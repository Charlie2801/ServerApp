server = function(input, output){
  
#-----------Overview Page-------------------------------
  output$Places_Overview = renderPlotly({
    ggplotly(map) 
  }
  )
  
  output$Median_Infections_Overview = renderPlotly({
    ColorMap(data.frame(AGS =map_overview_data$AGS, Data = map_overview_data$median_infections))
  })
  
  output$Mean_Infections_Overview = renderPlotly({
    ColorMap(data.frame(AGS =map_overview_data$AGS, Data = map_overview_data$mean_infections))
  })
  
  output$Variance_Overview = renderPlotly({
    ColorMap(data.frame(AGS =map_overview_data$AGS, Data = map_overview_data$max_infections - map_overview_data$min_infections))
  })
  
  output$InfectionsMunBox = renderPlotly({
    plotInfectionsMunBox()
  })
  
  output$InfectionsMunANOVA = renderPrint({
    summary(aov(full_overview$totalInfections ~ full_overview$name))
  })
  
  output$InfectionsRegBezBox = renderPlotly({
    plotInfectionsRegBezBox()
  })
  output$InfectionsRegBezANOVA = renderPrint({
    summary(aov(full_overview$totalInfections ~ full_overview$RegBez))
  })
  
  output$Density_Overview = renderPlotly({
    test#completeDensityMapNRW
  })
  

  
  
  
  
#-----------Correlations Page-------------------------------
  output$CorFullMatrix = renderPlot({
    plotCorMatrix()
  })
  
  observeEvent(c(input$valueA, input$valueB),{
    output$FullCorData = renderPrint((plotCorTable(input$valueA, input$valueB)))
    
    output$PlotLinReg = renderPlotly({
      plotCorLinReg(input$valueA, input$valueB)
    })
    
    
  })
  
#-----------Municipality Info Page-------------------------------  
  observeEvent(input$selected_munc, {
    
    output$InfAge = renderImage({
      retrieveImage(input$selected_munc, 'InfPerAge')
    }, deleteFile = FALSE)
  
    output$InfPercPerAge = renderImage({
      retrieveImage(input$selected_munc, 'InfPercPerAge')
    }, deleteFile = FALSE)
    
    
    output$muncMap = renderPlotly({
      getMuncMaps(input$selected_munc)
    })
    
    output$timeCases = renderPlotly({
      getTimeCases(input$selected_munc)

      })
    
    output$GeneralMunc = renderDataTable({
      singleMuncFrame(input$selected_munc)
    })
  })
  
  
  
}