#test
library(shiny)

plot_fun_helper <- function(x,y){
  df = data.frame(x=round(x,3),y=round(y,3))
  highcharter::hchart(df, type = "area", hcaes(x=x,y=y), color = "#009FDA", name = "Density")
}

xy_dens_gen <- function(tab, input){
  if(tab == "Normal"){
    #if normal, plot around mean, pm 3 std
    req(input$normal_mean)
    
    x <- seq(from = input$normal_mean - 3*input$normal_std, 
             to = input$normal_mean + 3*input$normal_std,
             length.out = 1e3)
    y <- dnorm(x, mean = input$normal_mean, sd = input$normal_std)
  }
  else if(tab == "LogNormal"){
    #if LogNormal, plot from 0 to 3*CV
    req(input$lognormal_CV)
    print(tab)
    x <- seq(from = 0, 
             to = 3*input$lognormal_CV,
             length.out = 1e3)
    y <- dlnorm(x, meanlog = 0, sdlog = 1)
  }
  else if(tab =="Exponential"){
    req(input$exp_rate)
    x <- seq(from = 0,
             to = 3/input$exp_rate,
             length.out = 1e3)
    y <- dexp(x, rate = input$exp_rate)
  }
  else stop("No method found")
  return(list(x = x,
              y = y))
}

ui <- 
  fluidPage(
    titlePanel("reprex"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput("cur_tab", "distribution",c("Normal","LogNormal","Exponential")),
        uiOutput("sidebar")
      ),
      uiOutput("plot")
    )
  )

server <- function(input, output) {
  
  rvals <- reactiveValues()
  
  observe({
    # Avoid calling dens_gen twice
    chose_density <- xy_dens_gen(tab = input$cur_tab, input = input)
    rvals$x <- chose_density$x
    rvals$y <- chose_density$y
    rvals$title <- input$cur_tab
  })
  
  output$sidebar <- renderUI({
    tab = input$cur_tab
    if(tab == "Normal"){
      div(
        sliderInput(inputId = "normal_mean", "Choose mean", value = 1, min = -2 , max = 2),
        sliderInput(inputId = "normal_std" , "Choose std" , value = 1, min = 0 , max = 2)
      )
    }
    else if(tab == "LogNormal"){
        sliderInput(inputId = "lognormal_CV", "Choose CV", value = 1, min = 0 , max = 20)
    }
    else if(tab =="Exponential"){
      sliderInput(inputId = "exp_rate", "Choose rate", value = 1, min = 0, max = 10)
    }
  })
  
  output$plot <- renderUI({
    if(is.null(rvals$x)) return(NULL)
    mainPanel(
      h1(rvals$title),
      plot_fun_helper(x = rvals$x, y = rvals$y)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)