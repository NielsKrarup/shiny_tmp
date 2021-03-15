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
                          verbatimTextOutput("sample"),
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

# Reactive values ---------------------------------------------------------




  rv <- reactiveValues()
  
  sim_data <- eventReactive(input$action_sim,{
    #First cases
    x <- rnorm(n = input$n_sim_case + input$n_sim_ctrl, 
               mean = c(rep(input$m_case, input$n_sim_case), rep(input$m_ctrl, input$n_sim_ctrl)),
               sd = c(rep(input$sd_case, input$n_sim_case), rep(input$sd_ctrl, input$n_sim_ctrl)))
    
    grp <- rep(c("case","ctrl"), c(input$n_sim_case,input$n_sim_ctrl))
    
    
    
    tibble(x = x,
           grp = grp)

  })
  
  
  
  output$sample <- renderPrint({
    sim_data()
  })
  
  
  
  output$plot_groups <- renderPlot({
    req(input$action_sim)
    data <- sim_data()
    #create limits for plots of groups
    min_lim <- isolate({
      min(min(data$x),qnorm(p = 0.001, mean = c(input$m_case, input$m_ctrl), sd = c(input$sd_case, input$sd_ctrl)))
      })
    max_lim <- isolate({
      max(max(data$x),qnorm(p = 0.999, mean = c(input$m_case, input$m_ctrl), sd = c(input$sd_case, input$sd_ctrl)))
      })
    
    
    ggplot(data = data, aes(x = x, col = grp, fill = grp)) + 
      geom_density(alpha = 0.5, outline.type = "upper", size = 0.25) + 
      geom_rug(data = subset(data, grp == "case"), sides = "t") + 
      geom_rug(data = subset(data, grp == "ctrl"), sides = "b") + 
      #geom_rug() + 
      xlim(min_lim, max_lim)
    
  })
  
  qnorm(p = 0.1, mean = 1:2, sd = c(1/100, 100))
  
  output$out <- renderPrint({
    #since the result matrix is dependent on renderUI and other calculations, use REQ
    req(rv$res_mat)
    n_sim <- input$n_sim
    
    list(correlation  = cor(rv$res_mat[,1], rv$res_mat[,2]),
         Total_test   = table(rv$res_mat[,1]<0.05)/n_sim,
         Sub_grp_test = table(rv$res_mat[,2]<0.05)/n_sim,
         table(rv$res_mat[,1]<0.05 , rv$res_mat[,2] < 0.05, dnn = c("Total Test Rej", "Sub Test Rej"))/n_sim)
    
  })
  
  output$p_vals_running <- renderPlot({
    req(input$n_sub_grp)
    #first row of data
    data <- sim_data()[1,]
    # generate bins based on input$bins from ui.R
    p_vals_running <- vector("double", input$n_sub_grp-3)
    
    for(i in seq_len(input$n_sub_grp-3)){
      x_sub      <- data[1:(i+3)]
      p_val_sub   <- t.test(x_sub)$p.value
      
      p_vals_running[i] <- p_val_sub
    }
    # # draw the histogram with the specified number of bins
    plot(4:input$n_sub_grp, p_vals_running, type = "b", xlab = "Number of obs. used", ylab = "Test P value", ylim = c(0,1))
    abline(h = 0.05, lty = 2)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


