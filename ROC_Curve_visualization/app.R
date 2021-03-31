#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(pROC)
library(tidyverse)
library(shiny)
library(shinyjs)

Colors <- c("case" = "#FF0000", "control" = "#0000FF")


if(F){
  input <- list()
  input$m_case <- 0
  input$sd_case <- 1
  input$m_ctrl <- 0
  input$sd_ctrl <- 1
  
  input$n_sim_case <- 10
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  navbarPage(title = "ROC Curve Guide",
             tabPanel(title = "Plots",
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          withMathJax(helpText(
                            "Data from two groups is simulated: 
Cases, having the attribute and Controls not having the attribute.
Assmuing normal distribution.")),
                          actionButton(inputId = "action_sim",label = "Simulate new data!"),
                          #Cases
                          sliderInput("m_case",
                                      "Mean of Cases",
                                      min = -2.1,
                                      max = 2.1,
                                      value = 0),
                          sliderInput("sd_case",
                                      "Standard Deviation of Cases",
                                      min = 0,
                                      max = 10,
                                      value = 5),
                          #Controls
                          sliderInput("m_ctrl",
                                      "Mean of Controls",
                                      min = -2.1,
                                      max = 2.1,
                                      value = 0),
                          sliderInput("sd_ctrl",
                                      "Standard Deviation of Control",
                                      min = 0,
                                      max = 10,
                                      value = 5),
                          numericInput("n_sim_case",
                                       "Number of observations in Control group",
                                       min = 1,
                                       value = 5),
                          numericInput("n_sim_ctrl",
                                       "Number of observations in Control group",
                                       min = 1,
                                       value = 6)
                          
                          
                          
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          uiOutput("info_header"),
                          plotOutput("plot_groups2"),
                          uiOutput("threshold_slider"),

                          plotOutput("plot_roc"),
                          #verbatimTextOutput("sample"),
                          helpText("N_Sim simulated p-vals. Pvals Based on total group vs Subgroup"),
                          #plotOutput("plot_groups"),
                          verbatimTextOutput("out")
                        )
                      )
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  # Dynamic UI --------------------------------------------------------------

  
  output$threshold_slider <- renderUI({
    req(rv$sim_data)
    rv$n <- nrow(rv$sim_data)

    sliderInput("thr_slider",
                "Predictor Cut-Point, % of data points",
                min = 0, 
                max =  100, 
                value = round(rv$n/2)/rv$n*100, 
                step = round(1/rv$n*100,2), animate = T)
  })
  
  output$info_header <- renderUI({
    req(rv$sens, rv$spec)
    # min <- min(rv$roc_obj$thresholds)
    # max <- max(rv$roc_obj$thresholds)
    # len <- abs(min-max)
    #classified as Controll


   tagList(
     HTML("observations having predictor values", ifelse( rv$roc_obj$direction == "<", "<b>lower</b>", "<b>above</b>"), 
          "the chosen threshols value are classified as <font color=\"#0000FF\"><b>Controls</b></font>, resulting in",
          "<b>", ifelse( rv$roc_obj$direction == "<", rv$n_below, rv$n_above), "</b>",
          "clasified as controls, out of which <font color=\"#0000FF\">", rv$ntpcnt, "</font> are actual controls, given a <b> negative predictive value </b> of",
          paste0(rv$ntpcnt, "/",  ifelse( rv$roc_obj$direction == "<", rv$n_below, rv$n_above), "=", round( rv$npv,2) ),
          "and a <b> specificity </b> of",  paste0(rv$ntpcnt, "/", rv$n_control ),"=", round(rv$spec,2)),br(),
 hr(),
 HTML("observations having predictor values", ifelse( rv$roc_obj$direction == "<", "<b>above</b>", "<b>lower</b>"), 
      "the chosen threshols value are classified as <font color=\"#FF0000\"><b>Cases</b></font>, resulting in",
      "<b>", ifelse( rv$roc_obj$direction == "<",  rv$n_above, rv$n_below), "</b>",
      "clasified as Cases, out of which <font color=\"#FF0000\">", rv$ntpcs, "</font> are actual cases, given a <b> positive predictive value </b> of",
      paste0(rv$ntpcs, "/",  ifelse( rv$roc_obj$direction == "<", rv$n_above, rv$n_below), "=", round( rv$ppv,2) ),
      "and a <b> sensitivity </b> of",  paste0(rv$ntpcs, "/", rv$n_cases ),"=", round(rv$sens,2))
   )
    
  })
  

  
  

# Reactive values ---------------------------------------------------------




  rv <- reactiveValues()
  

  observeEvent(input$action_sim,{
    # Simulate data ----
    x <- rnorm(n = input$n_sim_case + input$n_sim_ctrl, 
               mean = c(rep(input$m_case, input$n_sim_case), rep(input$m_ctrl, input$n_sim_ctrl)),
               sd = c(rep(input$sd_case, input$n_sim_case), rep(input$sd_ctrl, input$n_sim_ctrl)))
    
    response <- rep(c("case","control"), c(input$n_sim_case,input$n_sim_ctrl))
    
    
    
    rv$sim_data <- tibble(x = x,
                          response = response)
    
    #Calculate pROC object ----
    print(levels(as.factor(rv$sim_data$response)))
    rv$roc_obj <- pROC::roc(response ~ x, data = rv$sim_data, levels = c("control", "case"))

  })
  

# calculate Sensitivity and Specificity, and other metrics from data and Threshold -----------

  
  observe({
    req(input$thr_slider, rv$sim_data, rv$roc_obj)
    
    quantile(1:3, probs = 0)
    
    
    #steps
    step <- input$thr_slider/100*rv$n +1
    rng <- range(rv$sim_data$x)
    x <- c( rng[1]-0.05*diff(rng), (sort(rv$sim_data$x)[1:(rv$n-1)] + sort(rv$sim_data$x)[2:rv$n])/2, rng[2]+0.05*diff(rng)) 
    
    rv$thr_slider <- x[step]
    data <- rv$sim_data
    
    rv$n_cases <- sum(data$response == "case")
    rv$n_control <- sum(data$response == "control")
    rv$n_below <- sum(data$x < rv$thr_slider)
    rv$n_above <- sum(data$x > rv$thr_slider)

    # "control" operator "case"
    if(rv$roc_obj$direction == "<"){
      
      rv$sens <-  sum(data$response == "case" &  (rv$thr_slider < data$x) ) / sum(data$response == "case") 
      rv$ntpcs<-  sum(data$response == "case" &  (rv$thr_slider < data$x ) )
      rv$ppv  <-  rv$ntpcs / sum(rv$thr_slider < data$x) 
      
      #control
      rv$ntpcnt <- sum(data$response == "control" &  (data$x < rv$thr_slider) )
      rv$spec <- sum(data$response == "control" &  (data$x < rv$thr_slider))/sum(data$response == "control") 
      rv$npv    <- sum(data$response == "control" &  (data$x < rv$thr_slider)) / sum(data$x < rv$thr_slider) 
      
      
    } else if(rv$roc_obj$direction == ">"){
      
      rv$sens <-  sum(data$response == "case" &  (rv$thr_slider > data$x))/sum(data$response == "case")
      rv$ntpcs<-  sum(data$response == "case" &  (rv$thr_slider > data$x ) )
      rv$ppv  <-  rv$ntpcs / sum(rv$thr_slider > data$x) 
      
      #control
      rv$ntpcnt <- sum(data$response == "control" &  (rv$thr_slider < data$x ) )
      rv$spec   <- sum(data$response == "control" &  (data$x > rv$thr_slider))/sum(data$response == "control") 
      rv$npv    <- sum(data$response == "control" &  (data$x > rv$thr_slider))/sum(data$x > rv$thr_slider) 
    }
    
    
    
    print(c(rv$sens, rv$spec))
    
  })
  

  
# plot roc curve  ---------------------------------------------------------

  
  output$plot_roc <- renderPlot({
    req(input$action_sim, rv$roc_obj)
    
    plot(rv$roc_obj)
    points(rv$spec, rv$sens, col = 2, cex = 2, pch = 8)
    #points(    rv$roc_obj$specificities, rv$roc_obj$sensitivities, col =2)
    
  })

# plot groups 2 -----------------------------------------------------------

  
  output$plot_groups2 <- renderPlot({
    req(input$action_sim, rv$sim_data, input$thr_slider)
    line <- rv$thr_slider
    #load data
    data <- rv$sim_data
    #set classification
    data <- 
      if(rv$roc_obj$direction == "<"){
        
        data %>% mutate(classification = ifelse(x < line,"control", "case"))
        
      }else if(rv$roc_obj$direction == ">"){
        
        data %>% mutate(classification = ifelse(x >= line,"control", "case"))
      }
    

    
    
    # Density plot with zones -------------------------------------------------


    p <- data %>% ggplot(aes(x = x, fill = factor(response))) + geom_density(alpha = 0.7) +
      scale_fill_manual(values = Colors) +
      geom_vline(aes(xintercept=line), linetype="dashed", size = 1.1) +
      #rug of cases
      geom_rug(data = subset(data, response == "case"), sides = "t", alpha = 0.5, colour = Colors[names(Colors)=="case"] ) + 
      geom_rug(data = subset(data, response == "case" & classification == "case"),  sides = "t", size = 1.1, colour = Colors[names(Colors)=="case"] ) + 
      #rug of controls
      geom_rug(data = subset(data, response == "control"), sides = "b", alpha = 0.5, colour = Colors[names(Colors)=="control"]) + 
      geom_rug(data = subset(data, response == "control" & classification == "control"), sides = "b", size = 1.1, colour = Colors[names(Colors)=="control"] ) 
    
    d <- ggplot_build(p)$data[[1]]
# color dependent on direction of roc-object: recall control >< Case
    
    if(rv$roc_obj$direction == "<"){
      #small values are classied as control
      p <- p + geom_area(data = subset(d, x < line & fill == Colors[names(Colors)=="control"]),
                    aes(x=x, y=y), fill=Colors[names(Colors)=="control"], alpha = 0.2) +
                geom_area(data = subset(d, x > line & fill == Colors[names(Colors)=="case"]),
                    aes(x=x, y=y), fill=Colors[names(Colors)=="case"], alpha = 0.2)

        
      
    } else if(rv$roc_obj$direction == ">"){
      #large values are classied as control, small as case
      
      p <- p + geom_area(data = subset(d, x > line & fill == Colors[names(Colors)=="control"]),
                         aes(x=x, y=y), fill=Colors[names(Colors)=="control"], alpha = 0.2) +
        geom_area(data = subset(d, x < line & fill == Colors[names(Colors)=="case"]),
                  aes(x=x, y=y), fill=Colors[names(Colors)=="case"], alpha = 0.2)
    }
    
    #return plot
    return(p)

    
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

