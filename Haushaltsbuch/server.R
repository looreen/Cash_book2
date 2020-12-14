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
        
        if (is.null(data_from_input))
            return(NULL)
        
        raw_data <- read.csv2(data_from_input$datapath, 
                              sep=';', skip = 5, header = FALSE, dec=',')
        column_names <- c('Buchungstag', 'Wertstellung', 'Buchungstext', 'Auftraggeber / Beguenstigter', 'Verwendungszweck', 'Kontonummer', 
                          'BLZ', 'Betrag (EUR)', 'Glaeubiger-ID', 'Mandatsreferenz', 'Kundenreferenz')
        column_names <- make_clean_names(column_names)
        
        names(raw_data) <- column_names
        
        head(raw_data, 10)
        
    })
    
    output$chart1 <- renderPlot({
        
        data_from_input <- input$file1
        
        if (is.null(data_from_input))
            return(NULL)
        
        raw_data <- read.csv2(data_from_input$datapath, 
                              sep=';', skip = 5, header = FALSE, dec=',')
        column_names <- c('Buchungstag', 'Wertstellung', 'Buchungstext', 'Auftraggeber / Beguenstigter', 'Verwendungszweck', 'Kontonummer', 
                          'BLZ', 'Betrag (EUR)', 'Glaeubiger-ID', 'Mandatsreferenz', 'Kundenreferenz')
        column_names <- make_clean_names(column_names)
        
        names(raw_data) <- column_names
        
        prepared_data <- prepare_original_data(raw_data)
        
        graph_data <- prepared_data %>% 
            filter(betrag_eur<0) %>%
            group_by(short_name) %>% 
            summarise(betrag=sum(betrag_eur)) %>% 
            mutate(short_name=tolower(short_name))
        
        # graph_data %>% 
        #     ggplot(aes((-1)*betrag, reorder(short_name, -betrag))) + 
        #     geom_bar(stat = 'identity')
        
        new_graph_data <- graph_data %>% 
            left_join(categorisation_final, by=c('short_name'='auftrag'))
        
        new_graph_data %>% 
            group_by(category) %>% 
            summarise(betrag=sum(betrag)) %>% 
            ggplot(aes((-1)*betrag, reorder(category, -betrag))) + 
            geom_bar(stat = 'identity')
        
    })
})
