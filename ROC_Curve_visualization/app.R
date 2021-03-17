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

                          uiOutput("n_sub_input"),
                          numericInput("n_sim_case",
                                       "Number of observations in Control group",
                                       min = 1,
                                       value = 5),
                          numericInput("n_sim_ctrl",
                                       "Number of observations in Control group",
                                       min = 1,
                                       value = 3)
                          
                          
                          
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          helpText("First sample, running p-vals"),
                          plotOutput("plot_groups"),
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
  
  output$n_sub_input <- renderUI({
    req(input$n_total)
 
    sliderInput("n_sub_grp",
                "Sub Group Size",
                min = 1, 
                max = input$n_total, 
                value = input$n_total/2, 
                step = 1)
    
  })
  
  output$n_sub_input <- renderUI({
    req(input$n_total)
    
    sliderInput("n_sub_grp",
                "Sub Group Size",
                min = 1, 
                max = input$n_total, 
                value = input$n_total/2, 
                step = 1)
    
  })
  
  output$threshold_slider <- renderUI({
    req(rv$roc_obj)
    
    sliderInput("n_sub_grp",
                "Sub Group Size",
                min = 1, 
                max = input$n_total, 
                value = input$n_total/2, 
                step = 1)
    
  })

# Reactive values ---------------------------------------------------------




  rv <- reactiveValues()
  
  sim_data <- observeEvent(input$action_sim,{
    #First cases
    x <- rnorm(n = input$n_sim_case + input$n_sim_ctrl, 
               mean = c(rep(input$m_case, input$n_sim_case), rep(input$m_ctrl, input$n_sim_ctrl)),
               sd = c(rep(input$sd_case, input$n_sim_case), rep(input$sd_ctrl, input$n_sim_ctrl)))
    
    respons <- rep(c("case","ctrl"), c(input$n_sim_case,input$n_sim_ctrl))
    
    
    
    rv$sim_data <- tibble(x = x,
                          respons = respons)
    
    #Calculate pROC
    print(levels(as.factor(rv$sim_data$respons)))
    rv$roc_obj <- pROC::roc(respons ~ x, data = rv$sim_data, levels = c("ctrl", "case"))

  })
  
  
  
  output$plot_roc <- renderPlot({
    req(input$action_sim)
    
    plot(rv$roc_obj)
  })
  
  
  
  output$plot_groups <- renderPlot({
    req(input$action_sim, rv$sim_data)
    data <- rv$sim_data
    #create limits for plots of groups
    min_lim <- isolate({
      min(min(data$x),qnorm(p = 0.001, mean = c(input$m_case, input$m_ctrl), sd = c(input$sd_case, input$sd_ctrl)))
      })
    max_lim <- isolate({
      max(max(data$x),qnorm(p = 0.999, mean = c(input$m_case, input$m_ctrl), sd = c(input$sd_case, input$sd_ctrl)))
      })
    
    
    ggplot(data = data, aes(x = x, col = respons, fill = respons)) + 
      geom_density(alpha = 0.5, outline.type = "upper", size = 0.25) + 
      geom_rug(data = subset(data, respons == "case"), sides = "t") + 
      geom_rug(data = subset(data, respons == "ctrl"), sides = "b") + 
      #geom_rug() + 
      xlim(min_lim, max_lim)
    
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)


