function(input, output){
  
  ########Global Terrorism Tab########
  
  ###Global map of terrorism events###
  # Filter dataset by criteria selected by user in the sidebar
  terrorism_attacks = reactive({
    terrorism_attacks = terror_db %>% 
      filter(., between(year(date), input$terrorismAttackYears[1], input$terrorismAttackYears[2]) &
             attacktype %in% input$terrorismAttackType)
  })
  
  output$terrorismmap = renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Esri.WorldStreetMap") %>% 
      addCircleMarkers(lng = terrorism_attacks()$longitude, lat = terrorism_attacks()$latitude, 
                       label = paste(terrorism_attacks()$date, terrorism_attacks()$attacktype, sep = ": "), 
                       radius = 1, fill = T, 
                       opacity = 0.3, fillOpacity = 0.3, 
                       color="red", fillColor="red") %>% 
      addControl(position = "topright", html = paste0("Number of attacks: ", formatC(nrow(terrorism_attacks()), big.mark=",")))
  })
  
  ###Graphs visualizing aspects of terrorism events###
  
  
  
      # Info Boxes showing country with most attacks and average attacks per year
      output$maxBox = renderInfoBox({
        max_country = terror_db %>% group_by(., country) %>% summarise(., Total=n()) %>% arrange(., desc(Total)) %>% top_n(., 1)
        valueBox(subtitle = paste("Country with Most Attacks (", max_country[[2]]," attacks total)", sep =""),
                 value = max_country[[1]], color = "black", icon = icon("long-arrow-alt-up")
        )
      })
      
      output$avgBox = renderInfoBox({
        avg_attacks = terror_db %>% group_by(., year(date)) %>% summarise(., Total = n())
        valueBox(subtitle = "Average Attacks per Year",
                 value = round(mean(avg_attacks[[2]], na.rm = T),0), color = "black", icon = icon("balance-scale") 
        )
      })
      
      # Graph showing count of attacks by attack type
      events_by_type = terror_db %>% 
        group_by(., attacktype) %>% 
        summarise(., Count = n())
      
      output$attacktypebarchart = renderGvis({
        gvisColumnChart(events_by_type, xvar = "attacktype", yvar = "Count",
                        options = list(legend = "{position: 'none'}"))
      })
      
      # Graph showing count of attacks by region
      events_by_region = terror_db %>% 
        group_by(., region) %>% 
        summarise(., Count = n())
      
      output$attackregionbarchart = renderGvis({
        gvisColumnChart(events_by_region, xvar = "region", yvar = "Count",
                        options = list(legend = "{position: 'none'}"))
      })
      
      # Graph showing count of attacks by attack target
      events_by_target = terror_db %>% 
        group_by(., targettype) %>% 
        summarise(., Count = n())
      
      output$attacktargetsbarchart = renderGvis({
        gvisColumnChart(events_by_target, xvar = "targettype", yvar = "Count",
                        options = list(legend = "{position: 'none'}"))
      })
      
      
      ###Yearly Graphs Tab
      # Graph showing count of attacks by attack type, segmented by year
      yearly_events_by_type = terror_db %>% 
        group_by(., Year = year(date), attacktype) %>% 
        summarise(., Count = n()) %>% 
        spread(., key = attacktype, value = Count)
      
      output$yearlyattacktypelinechart = renderGvis({
        gvisLineChart(yearly_events_by_type,
                      xvar = "Year",
                      yvar = unique(terror_db$attacktype),
                      options = list(hAxis = "{showTextEvery: 2, format: '0', ticks: data.getDistinctValues(0)}",
                                     explorer = "{ actions: ['dragToZoom', 'rightClickToReset']}")
        )
      })
      
      # Graph showing count of attacks by region, segmented by year
      yearly_events_by_region = terror_db %>% 
        group_by(., Year = year(date), region) %>% 
        summarise(., Count = n()) %>% 
        spread(., key = region, value = Count)
      
      output$yearlyattackregionlinechart = renderGvis({
        gvisLineChart(yearly_events_by_region,
                      xvar = "Year",
                      yvar = unique(terror_db$region),
                      options = list(hAxis = "{showTextEvery: 2, format: '0', ticks: data.getDistinctValues(0)}",
                                     explorer = "{ actions: ['dragToZoom', 'rightClickToReset']}")
        )
      })
      
      # Graph showing count of attacks by attack target, segmented by year
      yearly_events_by_target = terror_db %>% 
        group_by(., Year = year(date), targettype) %>% 
        summarise(., Count = n()) %>% 
        spread(., key = targettype, value = Count)
      
      output$yearlyattacktargetslinechart = renderGvis({
        gvisLineChart(yearly_events_by_target,
                      xvar = "Year",
                      yvar = unique(terror_db$targettype),
                      options = list(hAxis = "{showTextEvery: 2, format: '0', ticks: data.getDistinctValues(0)}",
                                     explorer = "{ actions: ['dragToZoom', 'rightClickToReset']}")
        )
      })  
    
  
  
  ########Volatility & the Stock Market Tab########
  
  #Transform stockmarket data for ease of graphing
  stockmarket_by_year_db = vix_sandp_db %>% gather(., key = "market", value = "value", -c(date, vix_price_change, sandp_price_change))
  
  #Output line graph showing VIX and S&P over specified time period. Graph has dual y-axis scales
  output$vix_sandp_graph = renderGvis({
    gvisAnnotationChart(
      data = stockmarket_by_year_db,
      datevar = "date",
      numvar = "value",
      idvar = "market",
      options=list(width = "98%",
                   scaleColumns = '[0,1]',
                   scaleType = 'allmaximized')
    )
  })
  
  output$vix_vs_sandp_scatter = renderGvis({
    gvisScatterChart(data = select(stockmarket_by_year_db, vix_price_change, sandp_price_change),
                     options = list(
                       fontName = "Georgia",
                       tooltip = "{trigger: 'selection'}",
                       hAxis = "{title: 'Daily Δ in VIX Closing Price',
                                 viewWindowMode: 'maximized'}",
                       vAxis = "{title: 'Daily Δ in S&P500 Closing Price',
                                 viewWindowMode: 'maximized'}",
                       series = "{0: {labelInLegend: 'Daily S&P500 Δ'}}",
                       trendlines = "{0: {color: 'black',
                       showR2: true,
                       visibleInLegend: true,
                       labelInLegend: 'Regression Line',
                       pointsVisible: false}}",
                       width = "97%",
                       height = "400px")
                     )
  })
  
  ########Data Tab########
  
  #Output data tables with filters to increase ease of searching
  output$terrorismdatatable = DT::renderDT(terror_db %>% select(., 
                                                           Date = date, 
                                                           Country = country, 
                                                           Region = region, 
                                                           "Province/State" = provstate,
                                                           City = city,
                                                           "Attack Type" = attacktype,
                                                           "Weapon Type" = weapontype,
                                                           Target = targettype,
                                                           Perpetrator = perpname,
                                                           Killed = nkill,
                                                           Wounded = nwound), 
                                            filter = list(position = "top", clear = FALSE, plain = FALSE),
                                            options = list(pageLength = 10))
  
  output$sandp500datatable = DT::renderDT(vix_sandp_db %>% select(., date, sandp_close, sandp_price_change),
                                           filter = list(position = "top", clear = FALSE, plain = FALSE),
                                           options = list(pageLength = 10))
  
  output$vixdatatable = DT::renderDT(vix_sandp_db %>% select(., date, vix_close, vix_price_change),
                                          filter = list(position = "top", clear = FALSE, plain = FALSE),
                                          options = list(pageLength = 10))
    
}