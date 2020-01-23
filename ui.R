dashboardPage(
    dashboardHeader(
        title = "Global Terrorism & the US Stock Market",
        titleWidth = 400,
        tags$li("Christian Opperman", 
                style = "padding-right: 15px; padding-top: 15px; font-weight: bold; font-size: 13px, span-color: #FFFFFF",
                class = "dropdown"),
        tags$li(a(href="https://github.com/christianopperman", icon("github-square"), title = "Christian's GitHub"),
                style = "font-size: 20px",
                class = "dropdown"),
        tags$li(a(href="https://www.linkedin.com/in/christian-opperman/", icon("linkedin"), title = "Christian's LinkedIn"),
                style = "font-size: 20px",
                class = "dropdown")
    ),
    
    # Removed sidebarUserPanel because it didn't play well with Leaflet. Possible futher work to reintroduce?
    
    dashboardSidebar(
        sidebarMenu(id = "sidebar",
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
                    menuItem("Data", tabName = "data"),
                    menuItem("About", tabName = "about")
                    )
    ),
    
    dashboardBody(
        #Add custom CSS styling  ###CUSTOM SHEET LINK DOES NOT SEEM TO WORK - CONFIRM WITH SOMEONE?
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$style(type = "text/css", "#terrorismmap {height: calc(100vh - 100px) !important;}")
            ),
        tabItems(
            
            # Tab containing a visualization of global terrorism events from 1990 to 2017
            tabItem(tabName = "terrorism",
                    fluidRow(
                        leafletOutput("terrorismmap")
                    )),
            
            # Tab containing a visualization of the S&P500 and VIX indices from 1990 to 2020 as well as
            # a basic visualization of the relationship between the two indices
            tabItem(tabName = "volatilitystocks",
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
            
            # Tab containing visualizations of how different terroism events affect the VIX index
            tabItem(tabName = "terrorvolatility"),
            
            # Tab containing a tab panel for each of the three datasets used in the project
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
                        )
                    ),
            
            # Tab containing information about the project and myself
            tabItem(tabName = "about",
                    tabsetPanel(
                        tabPanel("About the Project",
                                 box(width = 12,
                                     fluidRow(
                                         column(width = 6, offset = 3, align = "center",
                                                tagList(
                                                    tags$h4("About the Project"),
                                                    tags$br(),
                                                    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do 
                                                    eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim 
                                                    ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                                    aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit 
                                                    in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur 
                                                    sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt 
                                                    mollit anim id est laborum.",
                                                    tags$br(),
                                                    tags$br(),
                                                ))
                                         ),
                                     fluidRow(
                                         column(width = 6, offset = 3, align = "center",
                                                tagList(
                                                    tags$h4("About the Data"),
                                                    tags$br(),
                                                    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do 
                                                    eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim 
                                                    ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut 
                                                    aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit 
                                                    in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur 
                                                    sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt 
                                                    mollit anim id est laborum."
                                                ))
                                         )
                                     )
                                 ),
                        tabPanel("About the Author",
                                 box(width = 12,
                                     fluidRow(
                                         column(width = 6, offset = 3, align = "center",
                                                tagList(
                                                    tags$h4("About the Author"),
                                                    tags$br(),
                                                    tags$img(src = "Me.jpg",
                                                             width = "50%",
                                                             style="border-radius: 50%"),
                                                    tags$br(),
                                                    tags$br(),
                                                    "Christian Opperman is a data scientist and analyst based in New York City.
                                                    Originally from South Africa, he was raised in the Bay Area, California, and 
                                                    after college lived in Tokyo, Japan, working in the energy sector, for a number 
                                                    of years before moving back to the U.S.",
                                                    tags$br(),
                                                    tags$br(),
                                                    "Please feel free to explore Christian's ",
                                                    tags$a("GitHub Account", href = "https://github.com/christianopperman"),
                                                    "or ",
                                                    tags$a("LinkedIn Profile", href = "https://www.linkedin.com/in/christian-opperman/"),
                                                    "."
                                                    )
                                                )
                                         )
                                     )
                                 )
                        )
                    )
            )
        )
    )