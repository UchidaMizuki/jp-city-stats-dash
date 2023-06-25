
# ui ----------------------------------------------------------------------

header <- dashboardHeader(
  title = "市区町村ダッシュボード",
  titleWidth = 300
)

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    pickerInput("pref_code", "都道府県：",
                choices = pref_code(pref) |> 
                  set_names(pref_name(pref)),
                options = list(`live-search` = TRUE)),
    uiOutput("city_code"),
    actionButton("run", "決定"),
    hr(),
    uiOutput("year"),
    hr(),
    menuItem("人口ダッシュボード",
             tabName = "tab_pop")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      "tab_pop",
      fluidRow(
        column(
          width = 4,
          tabBox(
            title = "人口推移",
            width = 12,
            tabPanel("人口",
                     plotOutput("plot_pop"),
                     plotOutput("plot_pop_nat")),
            tabPanel("割合",
                     plotOutput("plot_pop_prop"),
                     plotOutput("plot_pop_prop_nat"))
          )
        ),
        column(
          width = 4,
          tabBox(
            title = "自然増減",
            width = 12,
            selected = "増減率（総人口比）",
            tabPanel("増減数",
                     plotOutput("plot_natural_increase"),
                     plotOutput("plot_natural_increase_nat")),
            tabPanel("増減率（総人口比）",
                     plotOutput("plot_natural_increase_rate"),
                     plotOutput("plot_natural_increase_rate_nat"))
          )
        ),
        column(
          width = 4,
          tabBox(
            title = "社会増減",
            width = 12,
            selected = "増減率（総人口比）",
            tabPanel("増減数",
                     plotOutput("plot_social_increase")),
            tabPanel("増減率（総人口比）",
                     plotOutput("plot_social_increase_rate"))
          ),
          box(
            title = "出典",
            width = 12,
            tags$a("社会・人口統計体系 / 市区町村データ / 基礎データ（廃置分合処理済）",
                   href = "https://www.e-stat.go.jp/stat-search/database?page=1&layout=datalist&toukei=00200502&tstat=000001111376&cycle=8&tclass1=000001111380&statdisp_id=0000020201&tclass2val=0",
                   target="_blank",
                   rel="noopener noreferrer")
          )
        ),
      )
    )
  )
)

dashboardPage(header, sidebar, body)
