library(tidyverse)
library(readxl)
library('janitor')

raw_data <- read.csv2('/Users/Constanze/Documents/GitHub/Cash-book/Gemeinsames Konto/2020/20 02.csv', 
                      sep=';', skip = 5, header = FALSE, dec=',')
column_names <- c('Buchungstag', 'Wertstellung', 'Buchungstext', 'Auftraggeber / Beguenstigter', 'Verwendungszweck', 'Kontonummer', 
                  'BLZ', 'Betrag (EUR)', 'Glaeubiger-ID', 'Mandatsreferenz', 'Kundenreferenz')
column_names <- make_clean_names(column_names)

names(raw_data) <- column_names

prepared_data <- raw_data %>% 
  filter(!verwendungszweck=='Tagessaldo') %>% 
  mutate(short_name=str_extract_all(pattern = '^[[:alnum:]]*[[:blank:]][[:alnum:]]*', string = auftraggeber_beguenstigter),
         betrag_eur=str_replace_all(string = betrag_eur, pattern = '\\.', replacement = ''),
         betrag_eur=str_replace_all(string = betrag_eur, pattern = ',', replacement = '.'), 
         betrag_eur=as.numeric(betrag_eur))

graph_data <- prepared_data %>% 
  filter(betrag_eur<0) %>%
  group_by(short_name) %>% 
  summarise(betrag=sum(betrag_eur)) %>% 
  mutate(short_name=tolower(short_name))

graph_data %>% 
  ggplot(aes((-1)*betrag, reorder(short_name, -betrag))) + 
  geom_bar(stat = 'identity')

lebensmittel <- c("rewe sagt", "denns biomarkt", "steinecke s")
auswaerts_essen <- c("babo sushi")
drogerie <- c("dm fil")
auto <- c("bp tankstelle", "adeg 1093", "ah nempitz", "shell 6872", "shell 2404")
medizin <- c("steinbock apotheke")
strom <- c("evd energieversorgung")
urlaub <- c("erlebnisberg nagelkoepfel", "tauern spa")
geschenke <- c("gefeko berlin")
kinder <- c("tierpark berlin", "berliner instit", "bezirksamt friedrichshain")
internet <- c("tele columbus")

categorisation <- data_frame(auftrag=graph_data$short_name)
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

new_graph_data <- graph_data %>% 
  left_join(categorisation_final, by=c('short_name'='auftrag'))

new_graph_data %>% 
  group_by(category) %>% 
  summarise(betrag=sum(betrag)) %>% 
  ggplot(aes((-1)*betrag, reorder(category, -betrag))) + 
  geom_bar(stat = 'identity')
