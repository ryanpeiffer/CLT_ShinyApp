#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

#===========================================================================
# shiny ui function
#===========================================================================

ui <- fluidPage(

    # Application title
    titlePanel("The Central Limit Theorem on Display"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h5("Click the button below to get started!"),
            actionButton("sample", "Draw Sample")
            
        ),

        # Show a plot of the randomly generated values and the means of all sets of random values
        mainPanel(
           plotOutput("valsPlot"),
           h5(textOutput("meanText"), style="color:#539ed4"),
           plotOutput("meansPlot")
        )
    )
)

#===========================================================================
# shiny server function
#===========================================================================

server <- function(input, output) {
    
    #define some constants that we will use (maybe make these variable later)
    n_vals <- 10
    min_val <- 0
    max_val <- 10
    
    set.seed(as.numeric(Sys.time()))
    
    rv <- reactiveValues()
    
    
    #Take a new sample when the button is clicked.
    observeEvent(input$sample, {
        rv$n     <- input$sample
        rv$rands <- round(runif(n_vals, min_val, max_val))
        rv$cur_mean  <- round(mean(rv$rands), 1)
        rv$means <- append(rv$means, rv$cur_mean)
    })
    
    
    #build plot of our randomly generated values
    output$valsPlot <- renderPlot({
        #only plot after the button is pressed for the first time
        if (input$sample > 0) {
            ggplot(data.frame(vals = rv$rands), aes(vals)) +
                ggtitle(paste0(n_vals, " Random Draws from Unif[", min_val, ",", max_val, "]")) +
                theme(plot.title = element_text(hjust = 0.5)) +
                geom_dotplot(method = "histodot",
                             binwidth = 0.5,
                             fill = "#539ed4") +
                scale_y_continuous(NULL, breaks = NULL) +
                scale_x_continuous(NULL,
                                   breaks = c(0:10), 
                                   labels = as.character(c(0:10)),
                                   limits = c(0,10)) +
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      plot.title = element_text(hjust = 0.5))
            
            # hist(rv$rands, 
            #      breaks = seq(min_val, max_val, by = 1),
            #      main = paste("Sample Number", rv$n),
            #      xlab = paste0(n_vals, " random draws from Unif[", min_val, ",", max_val, "]"),
            #      ylab = "Count from Sample",
            #      xlim = range(min_val, max_val),
            #      ylim = range(0, 6),
            #      yaxt = 'n',
            #      labels = TRUE,
            #      col = 'darkgray', 
            #      border = 'white'
            # )
        }
    })
    
    
    #write in text the mean of our sample
    output$meanText <- renderText({
        #only show after the button is pressed for the first time
        if(input$sample > 0) {
            paste("Mean of sample ", rv$n, ":", rv$cur_mean)
        }
    })
    
    
    #plot graph of means of all samples
    output$meansPlot <- renderPlot({
        #only plot after the button is pressed for the first time
        if (input$sample > 0) {
            hist(as.numeric(rv$means),
                 main = paste("Plot of All Means"),
                 breaks = seq(min_val, max_val, by = 1), 
                 col = 'darkgray', 
                 border = 'white'
            )
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
