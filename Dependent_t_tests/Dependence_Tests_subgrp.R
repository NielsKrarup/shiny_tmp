#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Comment from Second Branch , loading other library
library(dplyr)

#Last test. This is a comment only on the local level

#This Is from Master Branch
library(ggplot2)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dependence of tests using subsets of same data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("A t-test is made on data x1,...,x_total"),
            helpText("Another t-test is made on subset of same data: x1,...,x_sub"),
            sliderInput("effect",
                        "Common Effect",
                        min = -2.1,
                        max = 2.1,
                        value = 0),
            sliderInput("n_total",
                        "Total size of sample",
                        min = 1,
                        max = 500,
                        value = 30),
            uiOutput("n_sub_input"),
            numericInput("n_sim",
                        "Number of simulations",
                        min = 1,
                        value = 1e2)

        ),

        # Show a plot of the generated distribution
        mainPanel(
          helpText("First sample, running p-vals"),
                  plotOutput("p_vals_running"),
          helpText("N_Sim simulated p-vals. Pvals Based on total group vs Subgroup"),
                  plotOutput("p_vals"),
                  verbatimTextOutput("out")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #renderUI
    output$n_sub_input <- renderUI({
        req(input$n_total)
        
        sliderInput("n_sub_grp",
                    "Sub Group Size",
                    min = 1, 
                    max = input$n_total, 
                    value = input$n_total/2, 
                    step = 1)
        
    })
    
    rv <- reactiveValues()
    
    sim_data <- eventReactive(c(input$n_total, input$effect, input$n_sim),{
       matrix(data =  rnorm(mean = input$effect, n = input$n_total*input$n_sim, sd = 1),
              nrow = input$n_sim,
              ncol = input$n_total)
    })
    
    
    output$sample <- renderPrint({
        sim_data()[1,]
    })
    


    output$p_vals <- renderPlot({
        req(input$n_sub_grp)
        data <- sim_data()
        print(dim(data))
        # generate bins based on input$bins from ui.R
        res_mat <- matrix(NA, nrow = input$n_sim, ncol = 2)
        
        for(i in 1:input$n_sim){
            x_total    <- data[i,]
            x_sub      <- x_total[1:input$n_sub_grp]

            p_val_total <- t.test(x_total)$p.value
            p_val_sub   <- t.test(x_sub)$p.value

            res_mat[i,] <- c(p_val_total, p_val_sub)
        }
        rv$res_mat <- res_mat
        # # draw the histogram with the specified number of bins
        plot(res_mat[,1], res_mat[,2], xlab = "Total Group Test Pval", ylab = "Sub Group Test P val")
        abline(h = 0.05, lty = 2, col = 2)
        #other branch, changed color to col = 3
        abline(v = 0.05, lty = 2, col = 3)

    })
    
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


