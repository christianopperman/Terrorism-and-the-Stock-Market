dashboardPage(
    dashboardHeader(
        title = "Global Terrorism & the US Stock Market",
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
        sidebarUserPanel("Christian Opperman",
                         subtitle = "Fellow @ NYCDSA",
                         image = "Me.jpg"),
        
        sidebarMenu(id = "sidebar",
                    menuItem("Home", tabName = "home"),
                    menuItem("Global Terrorismn", tabName = "terrorism"),
                    menuItem("Volatility & the Stock Market", tabName = "volatilitystocks"),
                    menuItem("Terrorism & Volatility", tabName = "terrorvolatility"),
                    menuItem("Data", tabName = "data")
                    )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "home"),
            tabItem(tabName = "terrorism"),
            tabItem(tabName = "volatilitystocks",
                    fluidRow(
                        column(12, htmlOutput("vix_sandp_graph"), position = "center")
                        )
                    ),
            tabItem(tabName = "trerorvolatility"),
            tabItem(tabName = "data")
        )
    )
)