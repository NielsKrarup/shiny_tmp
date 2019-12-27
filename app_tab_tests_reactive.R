#test
library(shiny)

plot_fun_helper <- function(x,y){
    plot(x,y)
}

xy_dens_gen <- function(tab, input){
  if(tab == "Normal"){
      #if normal, plot around mean, pm 3 std
      x <- seq(from = input$normal_mean - 3*input$normal_std, 
               to = input$normal_mean + 3*input$normal_std,
               length.out = 1e3)
      y <- dnorm(x, mean = input$normal_mean, sd = input$normal_std)
    }
  else if(tab == "LogNormal"){
    #if LogNormal, plot from 0 to 3*CV
    x <- seq(from = 0, 
             to = 3*input$lognormal_CV,
             length.out = 1e3)
    y <- dlnorm(x, meanlog = 0, sdlog = 1)
  }
  else if(tab =="Exponential"){
    x <- seq(from = 0,
             to = 3/input$exp_rate,
             length.out = 1e3)
    y <- dexp(x, rate = input$exp_rate)
  }
  else stop("No method found")
  
  return(list(x = x,
              y = y))
}


# Define UI for application that draws a histogram
ui <- 
  navbarPage(
    title = 'reprex', id = "cur_tab", selected = 'Normal',
    # Normal Tab ----
    navbarMenu("Normal/LogNormal",
               tabPanel("Normal",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput(inputId = "normal_mean", "Choose mean", value = 1, min = -2 , max = 2),
                            sliderInput(inputId = "normal_std" , "Choose std" , value = 1, min = 0 , max = 2)
                          ),
                          mainPanel(
                            plotOutput("plot_normal")
                          )
                        )
               ),
               tabPanel("LogNormal",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput(inputId = "lognormal_CV", "Choose CV", value = 1, min = 0 , max = 20)
                            
                          ),
                          mainPanel(
                            plotOutput("plot_LogNormal")
                          )
                        )
               )
    ),
    
    # Normal Data Tab ----
    tabPanel('Exponential',       
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "exp_rate", "Choose rate", value = 1, min = 0, max = 10)
               ),
               mainPanel(
                  plotOutput("plot_exp")
                 
               )
             )#sidebar Layout
    )

  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    rvals <- reactiveValues()
    
    observe({
      rvals$x <- xy_dens_gen(tab = input$cur_tab, input = input)$x
      rvals$y <- xy_dens_gen(tab = input$cur_tab, input = input)$y
    })
    

    
    #Render print inside renderPLot
    output$plot_normal <- renderPlot({
      plot_fun_helper(x = rvals$x, y = rvals$y)
    })
    
    output$plot_LogNormal <- renderPlot({
      plot_fun_helper(x = rvals$x, y = rvals$y)
    })
    
    output$plot_exp <- renderPlot({
      plot_fun_helper(x = rvals$x, y = rvals$y)
    })


}

# Run the application 
shinyApp(ui = ui, server = server)


