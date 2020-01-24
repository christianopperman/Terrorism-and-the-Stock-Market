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
                    menuItem("Global Terrorism", tabName = "terrorism", icon=icon("fire", lib = "glyphicon")),
                    shiny::conditionalPanel(condition="input.sidebar == 'terrorism' & input.tabset == 'Map'",   #Only display dropdown when terrorism tab is selected
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
                    menuItem("Volatility & the Stock Market", tabName = "volatilitystocks", icon=icon("sort", lib = "glyphicon")),
                    menuItem("Terrorism & Volatility", tabName = "terrorvolatility", icon=icon("screenshot", lib = "glyphicon")),
                    menuItem("Data", tabName = "data", icon=icon("th", lib = "glyphicon")),
                    menuItem("About", tabName = "about", icon=icon("plus", lib = "glyphicon"))
                    )
    ),
    
    dashboardBody(
        #Add custom CSS styling
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$style(type = "text/css", "#terrorismmap {height: calc(100vh - 100px) !important;}")
            ),
        tabItems(
            
            # Tab containing a visualization of global terrorism events from 1990 to 2017 as well as 
            # graphs visualizing information about the events themselves
            tabItem(tabName = "terrorism",
                tabsetPanel(id = "tabset",
                    tabPanel("Map",  icon = icon("globe"),
                             fluidRow(leafletOutput("terrorismmap"))
                             ),
                    tabPanel("Attacks: Total", icon = icon("chart-line"),
                             #box(width = 12,
                                 fluidRow(
                                     column(width = 12, infoBoxOutput("maxBox", width = NULL))),
                                 fluidRow(
                                     box(width = 12, title = "Types of Attack", collapsible = T, collapsed = T, solidHeader = T, status = "primary",
                                         column(12, align = "center",
                                            htmlOutput("attacktypebarchart")))),
                                 fluidRow(
                                     box(width = 12, title = "Attacks by Region", collapsible = T, collapsed = T, solidHeader = T, status = "primary",
                                         column(12, align = "center",
                                            htmlOutput("attackregionbarchart")))),
                                 fluidRow(
                                     box(width = 12, title = "Attacks by Target", collapsible = T, collapsed = T, solidHeader = T, status = "primary",
                                     column(12, align = "center",
                                            htmlOutput("attacktargetsbarchart"))))
                             ),
                    tabPanel("Attacks: Yearly", icon = icon("chart-line"),
                                 fluidRow(
                                         column(width = 12, infoBoxOutput("avgBox", width = NULL))),
                                 fluidRow(
                                     box(width = 12, title = "Attacks by Type", collapsible = T, collapsed = T, solidHeader = T, status = "primary",
                                         column(12, align = "center", htmlOutput("yearlyattacktypelinechart")))),
                                 fluidRow(
                                     box(width = 12, title = "Attacks by Region", collapsible = T, collapsed = T, solidHeader = T, status = "primary",
                                         column(12, align = "center", htmlOutput("yearlyattackregionlinechart")))),
                                 fluidRow(
                                     box(width = 12, title = "Attacks by Target", collapsible = T, collapsed = T, solidHeader = T, status = "primary",
                                         column(12, align = "center", htmlOutput("yearlyattacktargetslinechart"))))
                             ),
                    tabPanel("Casualties", icon = icon("chart-line"),
                             fluidRow(
                                 column(width = 12, infoBoxOutput("avgCasualtyBox", width = NULL))),
                             fluidRow(
                                 box(width = 12, title = "Total Casualties", collapsible = T, collapsed = T, solidHeader = T, status = "primary",
                                     column(12, align = "center", htmlOutput("totalcasualtiesbarchart")))),
                             fluidRow(
                                 box(width = 12, title = "Casualties by Region", collapsible = T, collapsed = T, solidHeader = T, status = "primary",
                                     column(12, align = "center", htmlOutput("regionlcasualtiesbarchart")))),
                             fluidRow(
                                 box(width = 12, title = "Casualties by Attack Type", collapsible = T, collapsed = T, solidHeader = T, status = "primary",
                                     column(12, align = "center", htmlOutput("attacktypecasualtiesbarchart")))),
                             fluidRow(
                                 box(width = 12, title = "Casualties by Year", collapsible = T, collapsed = T, solidHeader = T, status = "primary",
                                     column(12, align = "center", htmlOutput("yearlycasualtieslinechart"))))
                             )
                    )
                ),
            
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
                                         column(width = 6, offset = 3,
                                                tagList(
                                                    tags$h4("About the Project", align = "center"),
                                                    "This project aims to visualize global terrorism events and those
                                                    events' impacts on the US stock market, as represented by the S&P 
                                                    500 index. Of particular interest is whether the region and/or type 
                                                    of event affects the stock market in different ways.",
                                                    tags$br(),
                                                    tags$br(),
                                                    "The underlying logic is that terror events effect changes to the VIX 
                                                    volatility index, which are in turn negatively correlated with changes 
                                                    in the S&P 500.",
                                                    tags$br(),
                                                    tags$br(),
                                                    tags$br(),
                                                ))
                                         ),
                                     fluidRow(
                                         column(width = 6, offset = 3,
                                                tagList(
                                                    tags$h4("About the Data", align = "center"),
                                                    "Data on terrorism events was sourced from the Global Terrorism Database 
                                                    (GTD), which covers the years 1970 to 2017, with the exception of 1993; 
                                                    that data can be found ",
                                                    tags$a("here.", href = "https://www.kaggle.com/START-UMD/gtd"),
                                                    tags$br(),
                                                    tags$br(),
                                                    "The GTD data was reduced to cover the period 
                                                    between 1990 and 2017, in order to line up with the ",
                                                    tags$a("VIX Volatility Index ", href = "https://finance.yahoo.com/quote/%5EVIX?p=^VIX&.tsrc=fin-srch"),
                                                    "data and ",
                                                    tags$a("S&P 500 Index ", href = "https://finance.yahoo.com/quote/%5EGSPC?p=^GSPC&.tsrc=fin-srch"),
                                                    "data sourced from Yahoo Finance."
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