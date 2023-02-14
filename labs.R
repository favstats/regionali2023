
library(tidyverse)

it <- read_csv("data/Regional Elections Annotation - Italy.csv")
l <- read_csv("data/Regional Elections Annotation - Lombardia.csv") %>%
    mutate(`Spend Money in Lombardia` = T) %>%
    filter(`Amount spent (EUR)` != "≤100")
laz <- read_csv("data/Regional Elections Annotation - Lazio.csv") %>%
    mutate(`Spend Money in Lazio` = T) %>%
    filter(`Amount spent (EUR)` != "≤100")

it %>%
    mutate(row_ss = row_number()) %>%
    left_join(l %>% select(`Page ID`, partymark = party, `Spend Money in Lombardia`)) %>%
    left_join(laz %>% select(`Page ID`, `Spend Money in Lazio`))  %>%
    distinct(row_ss, .keep_all = T) %>%
    mutate(`Spend Money in Lombardia` = ifelse(is.na(`Spend Money in Lombardia`), F, `Spend Money in Lombardia`)) %>%
    mutate(`Spend Money in Lazio` = ifelse(is.na(`Spend Money in Lazio`), F, `Spend Money in Lazio`)) %>%
    openxlsx::write.xlsx("data/mmid.xlsx")




it <- read_csv("data/Regional Elections Annotation - Italy.csv")



lab_dat <- it %>%
    mutate(coalition = case_when(
        str_detect(coalition, "[r|R]ight") ~ "Coalizione di centro-destra",
        str_detect(coalition, "[l|L]eft") ~ "Coalizione di centro-sinistra",
        str_detect(coalition, "[c|C]entre") ~ "Coalizione di centro",
        str_detect(coalition, "M5s|5|Five Star Movement coalition") ~ "Coalizione Movimente 5 Stelle",
        str_detect(coalition, "UP") ~ "Unione Popolare",
        str_detect(coalition, "PAE") ~ "Partito Animalista Europeoe",
        str_detect(coalition, "PRC") ~ "Partito della Rifondazione Comunista",
        T ~ coalition
        # T ~ "not-relevant"
    ))  %>%
    mutate(party = case_when(
        str_detect(party, "FdI|FdL") ~ "FdI",
        str_detect(party, "Civic Pact|Patto Civico per Majorino Presidente|Majorino Presidente") ~ "Majorino Presidente",
        str_detect(party, "Partito Democratico|PD|Pd") ~ "PD",
        str_detect(party, "M5s|5") ~ "M5S",
        # str_detect(party, "Verdi-Sinistra|Sinistra Verdi|Verdi Sinistra") ~ "Verdi-Sinistra",
        str_detect(party, "Udc|UdC") ~ "UdC",
        str_detect(party, "D'Amato List|Amato") ~ "Lista Civica D'Amato",
        str_detect(party, "Noi Moderati") ~ "Noi moderati e Rinascimento",
        str_detect(party, "Rocca") ~ "Lista Civica Rocca",
        str_detect(party, "Demos|Solidary Democracy") ~ "Demos",
        str_detect(party, "Europa|Volt|Radicali") ~ "Più Europa Radicali Volt",
        # str_detect(party, "Sinistra") ~ "Sinistra Italiana",
        str_detect(party, "Italia Viva") ~ "Azione – Italia Viva",
        str_detect(party, "Verde|Verdi|Sinistra|Sinistra Italiana") ~ "Verdi-Sinistra",
        str_detect(party, "Moratti") ~ "Moratti Presidente",
        str_detect(party, "UP") ~ "Unione Popolare",
        str_detect(party, "PAE") ~ NA_character_,
        str_detect(party, "PRC") ~ NA_character_,
        str_detect(party, "--") ~ "not-relevant",
        is.na(party) ~ "not-relevant",
        T ~ party
        # T ~ "not-relevant"
    ))  %>%
    drop_na() %>%
    mutate(coalition = ifelse(party == "Azione – Italia Viva" & election == "Lazio", "Coalizione di centro-sinistra", coalition)) %>%
    mutate(election = ifelse(str_detect(party, "Polo"), "Lazio", election)) %>%
    janitor::clean_names() %>%
    select(page_name, page_id, party, coalition, election) %>%
    mutate(page_id = as.character(page_id))
