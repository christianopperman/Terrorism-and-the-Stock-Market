dashboardPage(
    dashboardHeader(
        title = "Global Terrorism & the US Stock Market",
        titleWidth = 400,
        tags$li("Christian Opperman", 
                style = "padding-right: 15px; padding-top: 15px; font-weight: bold; font-size: 13px",
                class = "dropdown"),
        tags$li(a(href="https://github.com/christianopperman", icon("github-square")),
                style = "font-size: 20px",
                class = "dropdown"),
        tags$li(a(href="https://www.linkedin.com/in/christian-opperman/", icon("linkedin")),
                style = "font-size: 20px",
                class = "dropdown")
    ),
    
    dashboardSidebar(
        sidebarMenu(id = "sidebar",
                    menuItem("Home", tabName = "home"),
                    menuItem("Global Terrorism", tabName = "terrorism"),
                    shiny::conditionalPanel(condition="input.sidebar == 'terrorism'",   #Only display dropdown when terrorism tab is selected
                                            sliderInput("terrorismAttackYears",
                                                        label = "Year Range:",
                                                        min = 1990, max = 2017,
                                                        value = c(1990, 2017),
                                                        step = 1,
                                                        sep = ""),
                                            checkboxGroupInput(inputId = "terrorismAttackType",
                                                               label = "Type of Attack:",
                                                               choices = unique(terror_db$attacktype),
                                                               selected = unique(terror_db$attacktype)
                                                               )
                                            ),
                    menuItem("Volatility & the Stock Market", tabName = "volatilitystocks"),
                    menuItem("Terrorism & Volatility", tabName = "terrorvolatility"),
                    menuItem("Data", tabName = "data")
                    )
    ),
    
    dashboardBody(
        tags$style(type = "text/css", "#terrorismmap {height: calc(100vh - 80px) !important;}"),
        tabItems(
            tabItem(tabName = "home"),
            tabItem(tabName = "terrorism",
                    fluidRow(
                        leafletOutput("terrorismmap")
                    )),
            tabItem(tabName = "volatilitystocks",
                    tabsetPanel(
                        tabPanel(
                            "Visualization",
                            fluidRow(column(
                                12,
                                align = "center",
                                tags$h4("VIX Volatility Index and S&P500 Time Series"),
                                htmlOutput("vix_sandp_graph")
                            )),
                            tags$br(),
                            fluidRow(column(
                                12,
                                align = "center",
                                tags$h4("VIX Volatility Index vs. S&P500"),
                                htmlOutput("vix_vs_sandp_scatter")
                            ))
                        ),
                        tabPanel("Statistical Analysis")
                    )),
            tabItem(tabName = "terrorvolatility"),
            tabItem(tabName = "data",
                    tabsetPanel(
                        tabPanel("Global Terrorism Events",
                                 fluidRow(
                                     box(DT::dataTableOutput("terrorismdatatable"), width = 12)
                                     )
                                 ),
                        tabPanel("S&P 500",
                                 fluidRow(
                                     box(DT::dataTableOutput("sandp500datatable"), width = 12)
                                     )
                                 ),
                        tabPanel("VIX Volatility Index",
                                 fluidRow(
                                     box(DT::dataTableOutput("vixdatatable"), width = 12)
                                 )
                        )
                        ))
        )
    )
)