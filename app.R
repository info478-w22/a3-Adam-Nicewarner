library(shiny)

# See above for the definitions of ui and server
ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("Covid Models"),
    
    tabsetPanel(
      tabPanel(
        "Introduction",
        titlePanel("Introduction:"),
        h4("We were assigned a project to create a Deterministic model of COVID, and a Stochastic Individual Contact model of the same.
         I ran into a few problems on the way, detailed below, but still created two models that will be able to model COVID spread. 
         One of my biggest assumptions going in is that due to how we cannot gain immunity to COVID like we can the chicken pox, COVID 
         will alwasy be an ongoing threat. Thus, all my models end up at an equilibrium, which should be roughly where the level of COVID
         ends up. "),
        h2("Deterministic Model"),
        h4("Deterministic Model: Shortly after starting my research trying to create a SIR model, I realized the fundamental flaw. 
         Covid is a disease where you can't really recover as we know. Instead, you simply fight off a wave, then have heightened resistance
         for the next few months, until it comes back. This is why I decided to use a SIS model instead. "),
        h2("Stochastic Model"),
        h4("Stochastic Model: During my Stochastic model creation, further problems emerged. My starting count was 320 million, roughly the population of the USA
        However, this was far too large for my Stochastic model, so I tried making it smaller. However, in order to get a functional number, 
         I had to decrease the s.num to 5000 in order to get a model that would actually load, so I will ask that you bear with the inaccuracies.")
      ),
      
      tabPanel(
        "Deterministic Model",
        sidebarLayout(
          sidebarPanel(
            checkboxInput("masks", "Facemasks worn by all", value = FALSE, width = NULL),
            checkboxInput("distance", "Social Distancing by all", value = FALSE, width = NULL),
          ),
          mainPanel(
            plotOutput("plot1")
          )
        )
      ),
      
      tabPanel(
        "Stochastic Model",
        sidebarLayout(
          sidebarPanel(
            checkboxInput("sto_masks", "Facemasks worn by all", value = FALSE, width = NULL),
            checkboxInput("sto_distance", "Social Distancing by all", value = FALSE, width = NULL),
          ),
          mainPanel(
            plotOutput("plot2")
          )
        )
      ),
      tabPanel(
        "Interpretation",
        titlePanel("Analysis and Conclusion"),
        h4("These two models are good, and their parameters are uselful. The act rate would be useful for manipulation if you were trying to track super-spreader events, like concerts and bars.
         This could also be useful for tracking STDs or other diseases that require a specific action to spread. The recovery rate is also helpful, although I would appreciate if there was a 
         way to link it to susceptibility, so we could have something similar to COVID in the model, where you are recovered, and it is harder go get sick for a long period, until your protection 
         wears off. The inital DCM seems useful in other models, where you want to model a specific population and time, like tracking infections over a business convention. However, for 
         general research like this it seems less helpful. The same general principle applies for the inital infected population, as well as the number of steps. "),
        h4("The differences between the models are relatively slight, but they are noticeable. The biggest one is the slight blur you can see around the ICM models. These represent how 
         the results of the ICM models are blurred, as they go to more effort to simulare individuals. This is likely the reason for the difficulty I had with simulating the entirety of
          the US population. "),
        h4("Finally, there are a few limitations. There is the fact that you cannot control time of recovery, which would better simulate the COVID recovery and subsequent vulnerability. In the 
         same way, It is much harder to simulate something like a population with differing attributes without a large amount of research. Something like America's problem, where we have a 
         large amount of people suffering from obesity is harder to manipulate due to the model assuming equality among all of the simulated population. Still, despite all of these problems, this 
         model is useful for predicting the end state of COVID, and with vaccines, I am hopeful that COVID will move into the same role as the flu-an annoyance, but a nonlethal one. ")
      ),
      tabPanel(
        "Bibliography",
        h2("Line Graph"),
        h4("
        Infection Rate:https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7976050/
        Rec rate: https://www.houstonmethodist.org/blog/articles/2020/jul/3-things-to-know-about-life-after-recovering-from-covid-19/
        Mask reduction in infection rate: https://www.cdc.gov/coronavirus/2019-ncov/science/science-briefs/masking-science-sars-cov2.html
        Social Distancing reduction in infection rate: https://www.pnas.org/doi/10.1073/pnas.2023131118#:~:text=The%20more%20participants%20practiced%20social,odds%20of%20contracting%20COVID%2D19.
          ")
      )
    ),
  ))

server <- function(input, output, session) {
  
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

shinyApp(ui = ui, server = server)