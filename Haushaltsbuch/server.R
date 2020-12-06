#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$dataframe1 <- renderTable({
        
        data_from_input <- input$file1
        
        raw_data <- read.csv2(data_from_input$datapath, 
                              sep=';', skip = 5, header = FALSE, dec=',')
        column_names <- c('Buchungstag', 'Wertstellung', 'Buchungstext', 'Auftraggeber / Beguenstigter', 'Verwendungszweck', 'Kontonummer', 
                          'BLZ', 'Betrag (EUR)', 'Glaeubiger-ID', 'Mandatsreferenz', 'Kundenreferenz')
        column_names <- make_clean_names(column_names)
        
        names(raw_data) <- column_names
        
        head(raw_data, 10)
        
    })
    
    

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
