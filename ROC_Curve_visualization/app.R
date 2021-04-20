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
  rv <- list()
  
  input <- list()
  input$m_case <- 0
  input$sd_case <- 1
  input$m_ctrl <- 0
  input$sd_ctrl <- 1
  
  input$n_sim_case <- 5
  input$n_sim_ctrl <- 6
  
  
  
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
                                      min = -10,
                                      max = 10,
                                      value = 0),
                          sliderInput("sd_case",
                                      "Standard Deviation of Cases",
                                      min = 0.01,
                                      max = 10,
                                      value = 5),
                          #Controls
                          sliderInput("m_ctrl",
                                      "Mean of Controls",
                                      min = -10,
                                      max = 10,
                                      value = 0),
                          sliderInput("sd_ctrl",
                                      "Standard Deviation of Control",
                                      min = 0.01,
                                      max = 10,
                                      value = 5),
                          numericInput("n_sim_case",
                                       "Number of observations in Control group",
                                       min = 1,
                                       value = 5),
                          numericInput("n_sim_ctrl",
                                       "Number of observations in Control group",
                                       min = 1,
                                       value = 6),
                          checkboxInput(inputId = "youden_bt",label = "Show Youden Cutpoint"),
                          checkboxInput(inputId = "ctl_bt", label = "Show Closest Topleft cutpoint")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          fluidRow(uiOutput("info_header"),
                                   br(),
                                   uiOutput("roc_stats")),
                          column(width = 6,
                              plotOutput("plot_groups2"),
                              uiOutput("threshold_slider")
                          ),
                          column(width = 6, 
                               plotOutput("plot_roc"),
                               #verbatimTextOutput("sample"),
                               uiOutput("help_text_roc"),
                               verbatimTextOutput("out")       
                          )
                        )
                      )
             ),
             tabPanel(title = "Theory", "Coming Soon"),
             tabPanel(title = "About","Made by Niels Moctezuma Krarup, please report bugs to krarup.niels@gmail.com")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  # Dynamic UI --------------------------------------------------------------

  
  output$threshold_slider <- renderUI({
    req(rv$sim_data)
    #count no of rows in data
    rv$n <- nrow(rv$sim_data)
  
    #set slider as percent of data x-points
    sliderInput("thr_slider",
                "Predictor Cut-Point, No. of sorted data points",
                min = 0, 
                max =  rv$n, 
                #set vallue as half of points. if even = 50% otherwise nearest even cut percentage
                value = round(rv$n/2, digits = 0), 
                step = 1, 
                animate = T)
  })
  
  #Roc help text
  output$help_text_roc <- renderUI({
    req(rv$roc_obj)
    helpText("The ROC curve is created by plotting as a step-function, all the
            (Specificity, Senscitivity) pairs arrising from different cut-points.
             Cut points are taken to be the middle points between two consecutive predictor values.")

  }) 
  
  #Roc help text
  output$roc_stats <- renderUI({
    req(rv$roc_obj)
  
    tagList(
      h4("Roc Curve statistics:"),
      HTML("ROC AUC:", round(auc(rv$roc_obj),2)),
      br(),
      HTML("\nYouden cutpoint:", round(as.numeric(rv$youden_cut[1]), digits = 3), 
           "giving Sensitivity: ", round(rv$youden_cut[3] %>% as.numeric(),3), 
           "and Specificity: ", round(rv$youden_cut[2] %>% as.numeric(),digits = 3)),
      br(),
      HTML("Closest Topleft cutpoint:", round(rv$tl_cut[1] %>% as.numeric(),digits = 3), 
           "giving Sensitivity:", round(rv$tl_cut[3] %>% as.numeric(),3),
           "and Specificity: ", round(rv$tl_cut[2] %>% as.numeric(),digits = 3))
    )
    
  }) 
  
  
  output$info_header <- renderUI({
    req(rv$sens, rv$spec)

   tagList(
     HTML("observations having predictor values", ifelse( rv$roc_obj$direction == "<", "<b>lower</b>", "<b>above</b>"), 
          "the chosen threshols value are classified as <font color=\"#0000FF\"><b>Controls</b></font>, resulting in",
          "<b>", ifelse( rv$roc_obj$direction == "<", rv$n_below, rv$n_above), "</b>",
          "clasified as controls, out of which <font color=\"#0000FF\">", rv$ntpcnt, "</font> are actual controls, given a <b> negative predictive value </b> of",
          paste0(rv$ntpcnt, "/",  ifelse( rv$roc_obj$direction == "<", rv$n_below, rv$n_above), "=", round( rv$npv,2) ),
          "and a <b> specificity </b> of",  paste0(rv$ntpcnt, "/", rv$n_control ),"=", "<font color=\"#FFA500\"><b>" ,round(rv$spec,2), "</b></font>"),
 hr(),
 HTML("observations having predictor values", ifelse( rv$roc_obj$direction == "<", "<b>above</b>", "<b>lower</b>"), 
      "the chosen threshols value are classified as <font color=\"#FF0000\"><b>Cases</b></font>, resulting in",
      "<b>", ifelse( rv$roc_obj$direction == "<",  rv$n_above, rv$n_below), "</b>",
      "clasified as Cases, out of which <font color=\"#FF0000\">", rv$ntpcs, "</font> are actual cases, given a <b> positive predictive value </b> of",
      paste0(rv$ntpcs, "/",  ifelse( rv$roc_obj$direction == "<", rv$n_above, rv$n_below), "=", round( rv$ppv,2) ),
      "and a <b> sensitivity </b> of",  paste0(rv$ntpcs, "/", rv$n_cases ),"=", "<font color=\"#800080\"><b>", round(rv$sens,2), "</b></font>")
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
    
    #save youden and closests topleft
    #youden
    rv$youden_cut <- coords(
      roc = rv$roc_obj,
      x = 'best', best.method = "y",
      transpose = TRUE
    )
    #closest topleft
    rv$tl_cut <- coords(
      roc = rv$roc_obj,
      x = 'best',
      best.method = c("closest.topleft"),
      transpose = TRUE
    )
    

  })
  

# calculate Sensitivity and Specificity, and other metrics from data and Threshold -----------
  observe({
    req(input$thr_slider, rv$sim_data, rv$roc_obj)
    
    
    #steps; x value to get % of data
    step <- input$thr_slider
    rng <- range(rv$sim_data$x)
    #lowest/highest step is +-0.05 of spread/range of predictor (x) values, remaining is the middle point between two consecutive points.
    x_grid <- c( rng[1]-0.05*diff(rng), 
            (sort(rv$sim_data$x)[1:(rv$n-1)] + sort(rv$sim_data$x)[2:rv$n])/2, 
            rng[2]+0.05*diff(rng)) 
    
    #set reactive value to x-value that gives No. of data
    rv$thr_slider <- x_grid[step + 1]
    #set x_grid as RV for later fixing of axis in plot
    rv$x_grid <- x_grid
    
    data <- rv$sim_data
    
    #count basic outcomes
    rv$n_cases   <- sum(data$response == "case")
    rv$n_control <- sum(data$response == "control")
    rv$n_below   <- sum(data$x < rv$thr_slider)
    rv$n_above   <- sum(data$x > rv$thr_slider)

    # "control" operator "case"
    if(rv$roc_obj$direction == "<"){
      
      rv$sens <-  sum(data$response == "case" &  (rv$thr_slider < data$x) ) / sum(data$response == "case") 
      rv$ntpcs<-  sum(data$response == "case" &  (rv$thr_slider < data$x ) )
      rv$ppv  <-  rv$ntpcs / sum(rv$thr_slider < data$x) 
      
      #control
      rv$ntpcnt <- sum(data$response == "control" &  (data$x < rv$thr_slider) )
      rv$spec   <- sum(data$response == "control" &  (data$x < rv$thr_slider))/sum(data$response == "control") 
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

    
  })
  

  
# plot roc curve  ---------------------------------------------------------

  
  output$plot_roc <- renderPlot({
    req(input$action_sim, rv$roc_obj, rv$spec, rv$sens)
    
    plot(rv$roc_obj, main = "ROC Curve")

    #plot current specif, sens
    points(rv$spec, rv$sens, col = "grey", cex = 2, pch = 17)
    #make help lines for current values of 
    segments(x0 = rv$spec, y0 = 0, x1 = rv$spec, y1 = rv$sens, col = "orange", lty = 2) #specificity 
    segments(x0 = 2, y0 = rv$sens, x1 = rv$spec, y1 = rv$sens, col = "purple", lty = 2) #sensitivity
    
    #insert Youden --------
    if(input$youden_bt){
      points(x = rv$youden_cut[2], y = rv$youden_cut[3], cex = 1.1, pch = 16)

      abline(a = sum(rv$youden_cut[2:3]), b = -1, lty = 2)
      
    }
    #insert Closest Topleft --------
    if(input$ctl_bt){
      # pch 15 = square, 16 = dot, 17 = triangle
      points(x = rv$tl_cut[2], y = rv$tl_cut[3], pch = 15, col = 2)

      #first calculate distance to (1,1)
      r <- sqrt( sum( (1 - rv$tl_cut[2:3])^2 ) )
      segments(x0 = 1, y0 = 1, x1 = rv$tl_cut[2], y1 = rv$tl_cut[3], col = 2)
      
      spec_vec <- seq(from = 1, to = 1-r, length.out = 100)
      sens_vec <- 1 - sqrt( r^2 - (1 - spec_vec)^2)
      #curve
      points(x = spec_vec, y = sens_vec, type = "l", col = 2, cex = 1)
    }
    
  })
  
  

# plot groups 2 -----------------------------------------------------------

  
  output$plot_groups2 <- renderPlot({
    req(input$action_sim, rv$sim_data, input$thr_slider)
    line <- rv$thr_slider
    #load data
    isolate(data <- rv$sim_data)
    #set classification
    data <- 
      if(rv$roc_obj$direction == "<"){
        
        data %>% mutate(classification = ifelse(x < line,"control", "case"))
        
      }else if(rv$roc_obj$direction == ">"){
        
        data %>% mutate(classification = ifelse(x >= line,"control", "case"))
      }
    
    # Density plot with zones -------------------------------------------------

#initial ggplot
   isolate({
     p0 <- data %>% ggplot(aes(x = x, fill = factor(response))) + geom_density(alpha = 0.7) +
       scale_fill_manual(values = Colors) + xlim(range(rv$x_grid)) 
   })
    p <- p0 +
      geom_vline(aes(xintercept=line), linetype="dashed", size = 1.1, col = "grey") +
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
      if(nrow(tmp1 <- subset(d, x < line & fill == Colors[names(Colors)=="control"])) !=0 ){
        
        p <- p + geom_area(data = tmp1, aes(x=x, y=y), fill=Colors[names(Colors)=="control"], alpha = 0.2)
      } 
      if(nrow(tmp2 <- subset(d, x > line & fill == Colors[names(Colors)=="case"])) != 0){
        
          p <- p + geom_area(data = tmp2, aes(x=x, y=y), fill=Colors[names(Colors)=="case"], alpha = 0.2)
      }

    } else if(rv$roc_obj$direction == ">"){
      #large values are classied as control, small as case
      
      if(nrow(tmp1 <-  subset(d, x > line & fill == Colors[names(Colors)=="control"])) != 0 ){
        
        p <- p +geom_area(data = tmp1, aes(x=x, y=y), fill=Colors[names(Colors)=="control"], alpha = 0.2)
      } 
      if( nrow( tmp2 <- subset(d, x < line & fill == Colors[names(Colors)=="case"])) != 0){
                  
        p <- p + geom_area(data = tmp2, aes(x=x, y=y), fill=Colors[names(Colors)=="case"], alpha = 0.2)
      }

    }
    
    #add cut points
    if(input$youden_bt){
      p <- p + geom_vline(xintercept = rv$youden_cut[1], col = "black" )
    }
    if(input$ctl_bt){
      p <- p + geom_vline(xintercept = rv$tl_cut[1], col = "red" )
    }
    
    
    #return plot
    return(p)

  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

