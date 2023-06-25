library(shiny)
library(shinydashboard)
library(shinyWidgets)

library(tidyverse)

library(jpcity)
library(jpstat)

theme_set(theme_light())

# global ------------------------------------------------------------------

pref <- parse_pref(1:47)
appId <- Sys.getenv("appId")

has_pref_code <- function(code, pref_code) {
  str_starts(code, str_pad(pref_code, 2, pad = "0"))
}

# estat -------------------------------------------------------------------

pattern_item_name_pop <- str_c("_",
                               c(# "年齢中位数",
                                 "15歳未満人口", "15～64歳人口", "65歳以上人口",
                                 # "未婚者割合（15歳以上人口）",
                                 "将来推計人口（\\d+年）",
                                 "出生数", "死亡数",
                                 "転入者数", "転出者数")) |> 
  str_c(collapse = "|")

estat_pop_pref <- estat(appId = appId,
                        statsDataId = "0000010101") |> 
  
  activate(tab) |> 
  select() |> 
  
  activate(cat01) |> 
  rekey("item") |> 
  filter(str_ends(name, pattern_item_name_pop)) |> 
  select(name, unit) |> 
  
  activate(area) |> 
  select(code, name) |> 
  
  activate(time) |> 
  rekey("year") |> 
  select(name) |> 
  
  deactivate()

estat_pop_city <- estat(appId = appId,
                        statsDataId = "0000020201") |> 
  
  activate(tab) |> 
  select() |> 
  
  activate(cat01) |> 
  rekey("item") |> 
  filter(str_ends(name, pattern_item_name_pop)) |> 
  select(name, unit) |> 
  
  activate(area) |> 
  select(code, name) |> 
  
  activate(time) |> 
  rekey("year") |> 
  select(name) |> 
  
  deactivate()

tidy_data_pop <- function(data) {
  data <- data |> 
    mutate(item_name = item_name |> 
             str_remove("^\\w+_"),
           year = parse_number(year_name),
           n = parse_number(n),
           .keep = "unused") |> 
    relocate(year, starts_with("area"))
  
  area_code <- data$area_code[[1L]]
  area_name <- data$area_name[[1L]]
  
  data_pop <- data |> 
    filter(item_name %in% c("15歳未満人口", "15～64歳人口", "65歳以上人口")) |> 
    rename(pop = n) |> 
    mutate(prop = pop / sum(pop),
           .by = c(year, area_code))
  
  data_pop_total <- data_pop |> 
    summarise(pop_total = sum(pop),
              .by = c(year, starts_with("area")))
  
  data_pop_future <- data |>
    filter(str_starts(item_name, "将来推計人口")) |> 
    mutate(year_future = parse_number(item_name),
           .keep = "unused") |> 
    slice_max(year,
              by = c(year_future, area_code)) |> 
    select(!year) |> 
    rename(year = year_future,
           pop = n) |> 
    relocate(year)
  
  data_natural_increase <- data |> 
    filter(item_name %in% c("出生数", "死亡数")) |> 
    left_join(data_pop_total |> 
                rename(year_pop_total = year),
              by = join_by(closest(year >= year_pop_total), 
                           area_code, area_name)) |> 
    mutate(rate = n / pop_total) |> 
    select(!c(year_pop_total, pop_total))
  
  data_social_increase <- data |> 
    filter(item_name %in% c("転入者数", "転出者数")) |> 
    left_join(data_pop_total |> 
                rename(year_pop_total = year),
              by = join_by(closest(year >= year_pop_total), 
                           area_code, area_name)) |> 
    mutate(rate = n / pop_total) |> 
    select(!c(year_pop_total, pop_total))
  
  list(area_code = area_code,
       area_name = area_name,
       pop = data_pop,
       pop_total = data_pop_total,
       pop_future = data_pop_future,
       natural_increase = data_natural_increase,
       social_increase = data_social_increase)
}

# plot
plot_pop <- function(data_pop, year) {
  data_pop$pop |> 
    filter(between(year, .env$year[[1]], .env$year[[2]])) |> 
    drop_na(pop) |> 
    ggplot(aes(year, pop)) + 
    geom_col(aes(fill = fct_rev(as_factor(item_name)))) +
    geom_line(data = data_pop$pop_future,
              aes(color = "将来推計人口"),
              linetype = "dashed") +
    geom_point(data = data_pop$pop_future,
               aes(color = "将来推計人口")) +
    scale_x_continuous("",
                       limits = c(year[[1]] - 1, NA)) +
    scale_y_continuous("人口",
                       labels = scales::label_comma(),
                       breaks = scales::breaks_extended(n = 10)) +
    scale_fill_brewer("年齢階級",
                      palette = "Set2") +
    scale_color_manual("", 
                       values = c(`将来推計人口` = "red")) +
    labs(title = data_pop$area_name) +
    theme(legend.position = "bottom",
          legend.box = "vertical")
}

plot_pop_prop <- function(data_pop, year) {
  data_pop$pop |> 
    filter(between(year, .env$year[[1]], .env$year[[2]])) |> 
    drop_na(prop) |> 
    ggplot(aes(year, prop,
               fill = fct_rev(as_factor(item_name)))) + 
    geom_col() +
    scale_x_continuous("",
                       limits = c(year[[1]] - 1, NA)) +
    scale_y_continuous("割合",
                       labels = scales::label_percent()) +
    scale_fill_brewer("年齢階級",
                      palette = "Set2") +
    labs(title = data_pop$area_name) +
    theme(legend.position = "bottom")
}

plot_increase <- function(type, data_pop, year) {
  data_pop[[type]] |> 
    filter(between(year, .env$year[[1]], .env$year[[2]])) |> 
    drop_na(n) |> 
    ggplot(aes(year, n,
               color = as_factor(item_name))) +
    geom_line() +
    geom_point() +
    scale_x_continuous("",
                       limits = c(year[[1]] - 1, NA)) +
    scale_y_continuous("増減数",
                       labels = scales::label_comma(),
                       breaks = scales::breaks_extended(n = 10),
                       limits = c(0, NA)) +
    scale_color_brewer("",
                       palette = "Paired") +
    labs(title = data_pop$area_name) +
    theme(legend.position = "bottom")
}

plot_increase_rate <- function(type, data_pop, year) {
  data_pop[[type]] |> 
    filter(between(year, .env$year[[1]], .env$year[[2]])) |> 
    drop_na(rate) |> 
    ggplot(aes(year, rate,
               color = as_factor(item_name))) +
    geom_line() +
    geom_point() +
    scale_x_continuous("",
                       limits = c(year[[1]] - 1, NA)) +
    scale_y_continuous("増減率",
                       labels = scales::label_percent(),
                       breaks = scales::breaks_extended(n = 10),
                       limits = c(0, NA)) +
    scale_color_brewer("",
                       palette = "Paired") +
    labs(title = data_pop$area_name) +
    theme(legend.position = "bottom")
}
