# This was added from GITHUB online by Niels
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("mean1",
                     "Mean of group 1:",
                     min = 1,
                     max = 50,
                     value = 30),
        sliderInput("mean2",
                    "Mean of group 2:",
                    min = 1,
                    max = 50,
                    value = 30),
        sliderInput("thr",
                    "Threshold:",
                    min = 1,
                    max = 50,
                    value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        uiOutput("stat_calc"),
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  rv <- reactiveValues()
  observe({
    rv$p_g1 <- 1-pnorm(q = input$thr, mean = input$mean1)
    rv$p_g2 <- 1-pnorm(q = input$thr, mean = input$mean2)
    
  })
  lab <- "hej"
  
  funcShaded <- function(x, mean, sd = 1) {
    y <- dnorm(x, mean = 0.2, sd = 0.1)
    y[x < 0.2 | x > (0.2 + 4 * 0.1)] <- NA
    return(y)
  }
  
 # p9 <- p9 + stat_function(fun=funcShaded, geom="area", fill="#84CA72", alpha=0.2)


   output$distPlot <- renderPlot({
     
     x_range <-  c(min(qnorm(0.000001, mean = c(input$mean1, input$mean2))),
                      max(qnorm(0.999999, mean = c(input$mean1, input$mean2)))
                   )
      # generate bins based on input$bins from ui.R
     ggplot(data.frame(x = x_range), aes(x = x)) +
       stat_function(fun = dnorm, args = list(mean = input$mean1), colour = "#0000FF") + 
       stat_function(fun = function(x,...) ifelse(x > input$thr, dnorm(x,...), NA), 
                     n = 2^10, geom = "area", args = list(mean = input$mean1), fill = "#0000FF", alpha = 0.2) + 
       #group2
       stat_function(fun = dnorm, args = list(mean = input$mean2), colour = "#FF99CC") + 
       stat_function(fun = function(x,...) ifelse(x > input$thr, dnorm(x,...), NA), 
                     n = 2^10, geom = "area", args = list(mean = input$mean2), fill = "#FF99CC", alpha = 0.2) + 
       geom_vline(xintercept = input$thr, linetype = "dashed") + ggtitle(label = lab)
   })
   
   output$stat_calc <- renderUI({

     tagList(
       HTML("Proportion of <b><font color=\"#0000FF\"> group 1 </font></b> above threshold: <b>", round(rv$p_g1,3), "</b> <br/>", 
            "Proportion of <b><font color=\"#FF99CC\"> group 2 </font></b> above threshold: <b>", round(rv$p_g2,3), "</b> <br/>" ,
            "Proportion of population above threshold, coming from group 1:<font color=\"#0000FF\">", 
            round( rv$p_g1/(rv$p_g1 + rv$p_g2), 4) , "</font> xx <b> bold </b> of")

     )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

