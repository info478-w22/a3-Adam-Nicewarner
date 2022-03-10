function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    infrate = 0.3
    if(input$masks){
      infrate = infrate*0.7
    }
    if(input$distance){
      infrate = infrate*0.8
    }
    
    param <- param.dcm(inf.prob = infrate, act.rate = 1, rec.rate = 0.1)
    
    init <- init.dcm(s.num = 320000000, i.num = 4)
    control <- control.dcm(type = "SIS", nsteps = 500, dt = 0.5)
    mod <- dcm(param, init, control)
    
    plot(mod)
    })
  output$plot2 <- renderPlot({
    infrate = 0.3
    if(input$sto_masks){
      infrate = infrate*0.7
    }
    if(input$sto_distance){
      infrate = infrate*0.8
    }
    
    param <- param.icm(inf.prob = infrate, act.rate = 1)
    init <- init.icm(s.num = 5000, i.num = 4)
    control <- control.icm(type = "SI", nsims = 10, nsteps = 200)
    mod <- icm(param, init, control)
    plot(mod)
  })
  
  
  
}