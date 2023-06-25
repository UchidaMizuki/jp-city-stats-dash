
# server ------------------------------------------------------------------

function(input, output, session) {
  # estat meta data
  reactive_estat_pop <- reactive({
    list(nat = estat_pop_pref |> 
           
           activate(area) |> 
           filter(code == "00000") |> 
           
           deactivate(),
         
         pref = estat_pop_pref |> 
           
           activate(area) |> 
           filter(has_pref_code(code, input$pref_code)) |> 
           
           deactivate(),
         city = estat_pop_city |> 
           
           activate(area) |> 
           filter(has_pref_code(code, input$pref_code)) |> 
           
           deactivate())
  }) |> 
    bindCache(input$pref_code) |> 
    bindEvent(input$pref_code)
  
  # estat data
  reactive_data_pop_nat <- reactive({
    estat_pop <- reactive_estat_pop()
    
    estat_pop$nat |> 
      collect() |> 
      tidy_data_pop()
  }) |> 
    bindCache(input$pref_code) |> 
    bindEvent(input$run)
  
  reactive_data_pop <- reactive({
    estat_pop <- reactive_estat_pop()
    
    if (input$city_code == "") {
      estat_pop$pref |> 
        collect() |> 
        tidy_data_pop()
    } else {
      estat_pop$city |> 
        
        activate(area) |> 
        filter(code == input$city_code) |> 
        
        collect() |> 
        tidy_data_pop()
    }
  }) |> 
    bindCache(input$pref_code, input$city_code) |> 
    bindEvent(input$run)
  
  # ui
  output$city_code <- renderUI({
    city <- reactive_estat_pop() |>
      chuck("city") |> 
      activate(area) |> 
      as_tibble()
    
    pickerInput("city_code", "市区町村：",
                choices = city$code |> 
                  set_names(city$name),
                options = list(title = "全市区町村",
                               `live-search` = TRUE))
  }) |> 
    bindEvent(input$pref_code)
  
  output$year <- renderUI({
    year <- reactive_estat_pop() |>
      chuck("pref") |> 
      activate(year) |> 
      as_tibble() |> 
      pull(name) |> 
      parse_number()
    
    range_year <- range(year)
    sliderInput("year", "年度（範囲）：",
                min = range_year[[1L]],
                max = range_year[[2L]],
                value = c(range_year[[2L]] - 20, range_year[[2L]]),
                ticks = FALSE,
                sep = "")
  }) |> 
    bindEvent(input$pref_code)
  
  # plots
  output$plot_pop <- renderPlot({
    data_pop <- reactive_data_pop()
    plot_pop(data_pop = data_pop,
             year = input$year)
  },
  res = 90) |> 
    bindEvent(input$run, input$year)
  
  output$plot_pop_nat <- renderPlot({
    data_pop_nat <- reactive_data_pop_nat()
    plot_pop(data_pop = data_pop_nat,
             year = input$year)
  },
  res = 90) |> 
    bindEvent(input$run, input$year)
  
  output$plot_pop_prop <- renderPlot({
    data_pop <- reactive_data_pop()
    plot_pop_prop(data_pop = data_pop,
                  year = input$year)
  },
  res = 90) |> 
    bindEvent(input$run, input$year)
  
  output$plot_pop_prop_nat <- renderPlot({
    data_pop_nat <- reactive_data_pop_nat()
    plot_pop_prop(data_pop = data_pop_nat,
                  year = input$year)
  },
  res = 90) |> 
    bindEvent(input$run, input$year)
  
  output$plot_natural_increase <- renderPlot({
    data_pop <- reactive_data_pop()
    plot_increase(type = "natural_increase",
                  data_pop = data_pop,
                  year = input$year)
  },
  res = 90) |> 
    bindEvent(input$run, input$year)
  
  output$plot_natural_increase_nat <- renderPlot({
    data_pop_nat <- reactive_data_pop_nat()
    plot_increase(type = "natural_increase",
                  data_pop = data_pop_nat,
                  year = input$year)
  },
  res = 90) |> 
    bindEvent(input$run, input$year)
  
  output$plot_natural_increase_rate <- renderPlot({
    data_pop <- reactive_data_pop()
    plot_increase_rate(type = "natural_increase",
                       data_pop = data_pop,
                       year = input$year)
  },
  res = 90) |> 
    bindEvent(input$run, input$year)
  
  output$plot_natural_increase_rate_nat <- renderPlot({
    data_pop_nat <- reactive_data_pop_nat()
    plot_increase_rate(type = "natural_increase",
                       data_pop = data_pop_nat,
                       year = input$year)
  },
  res = 90) |> 
    bindEvent(input$run, input$year)
  
  output$plot_social_increase <- renderPlot({
    data_pop <- reactive_data_pop()
    plot_increase(type = "social_increase",
                  data_pop = data_pop,
                  year = input$year)
  },
  res = 90) |> 
    bindEvent(input$run, input$year)
  
  output$plot_social_increase_rate <- renderPlot({
    data_pop <- reactive_data_pop()
    plot_increase_rate(type = "social_increase",
                       data_pop = data_pop,
                       year = input$year)
  },
  res = 90) |> 
    bindEvent(input$run, input$year)
}
