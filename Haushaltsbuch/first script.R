library(tidyverse)
library(readxl)
library('janitor')

raw_data <- read.csv2('/Users/Constanze/Documents/GitHub/Cash-book/Gemeinsames Konto/2020/20 02.csv',
                      sep=';', skip = 5, header = FALSE, dec=',', encoding = 'latin1')
column_names <- c('Buchungstag', 'Wertstellung', 'Buchungstext', 'Auftraggeber / Beguenstigter', 'Verwendungszweck', 'Kontonummer',
                  'BLZ', 'Betrag (EUR)', 'Glaeubiger-ID', 'Mandatsreferenz', 'Kundenreferenz')
column_names <- make_clean_names(column_names)

names(raw_data) <- column_names

check_crucial_names <- function(data, crucial_names){
  names_data <- names(data)
  logical_indicator <- crucial_names %in% names_data
  
  return(every(logical_indicator, isTRUE))
}

prepare_original_data <- function(data){
  
    if (check_crucial_names(data, c('verwendungszweck', 'betrag_eur', 'auftraggeber_beguenstigter'))==FALSE){
    stop('Crucial columns are missing.')
  }

  data_prep <- data %>% 
    filter(!verwendungszweck=='Tagessaldo') %>% 
    mutate(short_name=str_extract_all(pattern = '^[[:alnum:]]*[[:blank:]][[:alnum:]]*', string = auftraggeber_beguenstigter, simplify = T),
           short_name=as.character(short_name),
           short_name=tolower(short_name),
           betrag_eur=str_replace_all(string = betrag_eur, pattern = '\\.', replacement = ''),
           betrag_eur=str_replace_all(string = betrag_eur, pattern = ',', replacement = '.'), 
           betrag_eur=as.numeric(betrag_eur))
  
  return(data_prep)
}

# prepared_data <- raw_data %>% 
#   filter(!verwendungszweck=='Tagessaldo') %>% 
#   mutate(short_name=str_extract_all(pattern = '^[[:alnum:]]*[[:blank:]][[:alnum:]]*', string = auftraggeber_beguenstigter),
#          betrag_eur=str_replace_all(string = betrag_eur, pattern = '\\.', replacement = ''),
#          betrag_eur=str_replace_all(string = betrag_eur, pattern = ',', replacement = '.'), 
#          betrag_eur=as.numeric(betrag_eur))


categorisation <- tibble(auftrag=graph_data$short_name)
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
