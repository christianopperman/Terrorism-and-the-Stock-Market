function(input, output){
    
  output$vix_sandp_graph = renderGvis({
    gvisLineChart(terror_db,
                  xvar = "date",
                  yvar = c("vix_close", "gspc_close"),
                  options = list(series = "[{type: 'line',
                                             targetAxisIndex: 0},
                                            {type: 'line',
                                             targetAxisIndex: 1}]",
                                 vAxes = "[{title: 'VIX Volatility Index'},
                                           {title: 'S&P 500'}]"
                  ))
  })
  
}