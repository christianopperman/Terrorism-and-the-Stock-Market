function(input, output){

  ########Global Terrorism Tab########
  
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
                   scaleColumns='[0,1]',
                   scaleType='allmaximized')
    )
  })
  
  output$vix_vs_sandp_scatter = renderGvis({
    gvisScatterChart(data = select(stockmarket_by_year_db, vix_price_change, sandp_price_change),
                     options = list(
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
  
}