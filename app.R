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
           plotOutput("valsPlot", height = 200, width = 750),
           h5(textOutput("meanText"), style="color:#539ed4"),
           plotOutput("meansPlot", height = 300, width = 750)
        )
    )
)

#===========================================================================
# shiny server function
#===========================================================================

server <- function(input, output) {
   
     set.seed(as.numeric(Sys.time()))
    
    #define some constants that we will use for the random draws
    n_vals <- 10
    min_val <- 0
    max_val <- 20
    
    breaks_vec <- c(min_val:max_val)
    limits_vec <- c(min_val, max_val)
    
    #Take a new sample when the button is clicked.
    rv <- reactiveValues()
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
                geom_dotplot(method = "histodot",
                             binwidth = 0.5,
                             fill = "#539ed4",
                             color = NA) +
                scale_y_continuous(NULL, breaks = NULL) +
                scale_x_continuous(NULL,
                                   breaks = breaks_vec, 
                                   labels = as.character(breaks_vec),
                                   limits = limits_vec) +
                theme_bw() +
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      plot.title = element_text(hjust = 0.5)) +
                ggtitle(paste(n_vals, "Random Draws from", min_val, "to", max_val))
        }
    })
    
    
    #write in text the mean of our sample
    output$meanText <- renderText({
        #only show after the button is pressed for the first time
        if(input$sample > 0) {
            paste0("Mean of sample ", rv$n, ": ", rv$cur_mean)
        }
    })
    
    
    #plot graph of means of all samples
    output$meansPlot <- renderPlot({
        #only plot after the button is pressed for the first time
        if (input$sample > 0) {
            ggplot(data.frame(vals = rv$means), aes(vals)) +
                geom_histogram(aes(fill = (vals == rv$cur_mean)),
                               binwidth = 1,
                               boundary = 0,
                               color = 'white') +
                theme_bw() +
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank()) +
                scale_y_continuous(breaks = NULL) +
                scale_x_continuous(breaks = breaks_vec, limits = limits_vec) +
                labs(x = "Means", y = "Frequency") +
                scale_fill_manual(values = c(`TRUE` = "#539ed4", `FALSE` = "gray80"),
                                  guide = FALSE) +
                stat_function(fun = function(x) 
                    dnorm(x, mean = (min_val+max_val)/2,
                             sd = sd(rv$means)) * length(rv$means))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
