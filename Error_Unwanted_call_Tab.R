#test
library(shiny)

#Helper plot function, only needed on plots in tabs: Special and Cool
plot_fun_helper <- function(x, y, tab){
    req(tab)
  print("calling plot_fun_helper")
    switch(tab,
           "Special" = {
             print("Special case in plot_fun_helper")
             
               plot(x,y, type = 'l', col = 'green')  
           },
           "Cool" = {
             print("Cool case in plot_fun_helper")
             
               plot(x,y, type = 'b', pch = y, col = y ) 
           },
           stop("Error. Only defined for Tab: Special and Cool"))
    
}

#function that returns the x- and y vektors for plotting, depending on the current tab.
xy_dens_gen <- function(tab){
  print("call to xy_dens_gen")
    req(tab)
    if(tab == "Normal"){
      print("normal case in xy_dens_gen")
        x <- 1:100
        y <- x^2
    }
    else if(tab == "Special"){
      print("Special case in xy_dens_gen")
        x <- 1:100
        y <- sin(x)
    }
    else if(tab =="Cool"){
      print("Cool case in xy_dens_gen")
        x <- 1:100
        y <- x %% 5
    }
    else stop("No method found")
    
    return(list(x = x,
                y = y))
}


# Define UI for application that draws the pdf
ui <- 
    navbarPage(
        title = 'reprex', id = "cur_tab", selected = 'Normal',
        # Normal Tab ----
        navbarMenu("Normal/Special",
                   tabPanel("Normal",
                                    plotOutput("plot_Normal")
                            ),
                   tabPanel("Special",
                                    plotOutput("plot_Special")
                   )
        ),
        
        # Cool Tab 
        tabPanel('Cool',      
                         plotOutput("plot_Cool")
        )
        
    )


# Server Logic
server <- function(input, output) {
    
    rvals <- reactiveValues()
    
    observe({
        xy_list <- xy_dens_gen(tab = input$cur_tab)
        rvals$x <- xy_list$x
        rvals$y <- xy_list$y
    })
    
    
    
    #Render print inside renderPLot
    output$plot_Normal <- renderPlot({
      if(input$cur_tab != "Normal") return(NULL)
      
        print("render PLot normal")
        #NB does not invoke the plot_fun_helper
        plot(rvals$x, rvals$y)
    })
    
    output$plot_Special <- renderPlot({
      if(input$cur_tab != "Special") return(NULL)
      
      print("render PLot Special")
      
        plot_fun_helper(x = rvals$x, y = rvals$y, tab = input$cur_tab)
    })
    
    output$plot_Cool <- renderPlot({
      if(input$cur_tab != "Cool") return(NULL)
      print("render PLot Cool")
      
        plot_fun_helper(x = rvals$x, y = rvals$y, tab = input$cur_tab)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


