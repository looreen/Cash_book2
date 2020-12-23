
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
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
        
        categorisation <- tibble(auftrag=unique(prepared_data$short_name))
        categorisation_final <- categorisation %>% 
            mutate(category=case_when(auftrag %in% lebensmittel ~ 'lebensmittel',
                                      auftrag %in% auswaerts_essen ~ 'auswaerts_essen',
                                      auftrag %in% drogerie ~ 'drogerie',
                                      auftrag %in% auto ~ 'auto',
                                      auftrag %in% medizin ~ 'medizin',
                                      auftrag %in% strom ~ 'strom',
                                      auftrag %in% urlaub ~ 'urlaub',
                                      auftrag %in% geschenke ~ 'geschenke',
                                      auftrag %in% kinder ~ 'kinder',
                                      auftrag %in% internet ~ 'internet', 
                                      TRUE ~ 'anderes'))
        
        new_graph_data <- prepared_data %>% 
            filter(betrag_eur<0) %>%
            group_by(short_name) %>% 
            summarise(betrag=sum(betrag_eur)) %>% 
            mutate(short_name=tolower(short_name)) %>%
            left_join(categorisation_final, by=c('short_name'='auftrag'))
        
        new_graph_data %>% 
            group_by(category) %>% 
            summarise(betrag=sum(betrag)) %>% 
            arrange(desc(betrag)) %>% 
            mutate(category=str_to_title(category),
                   category=factor(category, levels = category)) %>%
            ggplot(aes(category, -betrag, label= (paste0(round(-betrag, 0), ' Euro')))) + 
            geom_text(nudge_y = 20) + 
            # geom_bar(stat = 'identity')+
            geom_segment(aes(xend=category, yend=0)) +
            geom_point(size=4, color="orange") +
            coord_flip()+
            theme_minimal()+
            labs(x='', y='',title = 'Ausgaben nach Kategorien')
        
     })
    
    output$table1 <- renderTable({
        data_from_input <- input$file1
        
        if (is.null(data_from_input))
            return(NULL)
        
        raw_data <- read.csv2(data_from_input$datapath, 
                              sep=';', skip = 5, header = FALSE, dec=',', 
                              encoding = 'latin1')
        column_names <- c('Buchungstag', 'Wertstellung', 'Buchungstext', 'Auftraggeber / Beguenstigter', 'Verwendungszweck', 'Kontonummer', 
                          'BLZ', 'Betrag (EUR)', 'Glaeubiger-ID', 'Mandatsreferenz', 'Kundenreferenz')
        column_names <- make_clean_names(column_names)
        
        names(raw_data) <- column_names
        
        prepared_data <- prepare_original_data(raw_data)
        
        categorisation <- tibble(auftrag=unique(prepared_data$short_name))
        categorisation_final <- categorisation %>% 
            mutate(category=case_when(auftrag %in% lebensmittel ~ 'lebensmittel',
                                      auftrag %in% auswaerts_essen ~ 'auswaerts_essen',
                                      auftrag %in% drogerie ~ 'drogerie',
                                      auftrag %in% auto ~ 'auto',
                                      auftrag %in% medizin ~ 'medizin',
                                      auftrag %in% strom ~ 'strom',
                                      auftrag %in% urlaub ~ 'urlaub',
                                      auftrag %in% geschenke ~ 'geschenke',
                                      auftrag %in% kinder ~ 'kinder',
                                      auftrag %in% internet ~ 'internet', 
                                      TRUE ~ 'anderes'))
        
        prepared_data_other <- prepared_data %>% 
            left_join(categorisation_final, by=c('short_name'='auftrag')) %>% 
            filter(is.na(category)|category=='anderes', 
                   betrag_eur<0) %>%
            select(buchungstag, auftraggeber_beguenstigter, verwendungszweck, betrag_eur, short_name)
        
        if (nrow(prepared_data_other)==0){
            print('Keine unkategorisierten Ausgaben!')
        } else {
            prepared_data_other
        }

    })
})
