




### Load packages
library(pacman)
p_load(haven, # Used for reading in STATA files
       rvest, # Used for webscraping
       janitor, # Used for data cleaning
       readxl, # Used for reading Excel files
       stringr, # Used for text manipulation
       tidyverse)




### Read in data
# The first dataset is from 434 local electoral contests in 11 cities in California 
# (Alameda, Anaheim, Berkeley, Richmond, Richmond, San Francisco, San Jose, San Leandro, 
# Santa Ana, Santa Clara, and Stockton). Each observation in the dataset is a seat that 
# was up for election between 1995 and 2014 in the selected cities. The dataset indicates 
# whether the city was a treatment city -- ie. whether it adopted the Alternative Vote 
# (aka instant runoff or ranked choice voting) and whether the contest occurred after AV 
# was adopted (the time variable). District- and city-level electoral structure, 
# socio-economic and demographic variables are included. 

# Paper for this is available here:
# https://www.sciencedirect.com/science/article/abs/pii/S0261379417304006?via%3Dihub
av <- haven::read_dta('Data/AV_database.dta')
av %>% head()



### Viz on which countries have which electoral systems
# 1. Proportional
#     a. Party-list Systems
#     b. Single Transferable Vote (STV)
# 2. Non-proportional
#     a. Plurality (First-Past-the-Post)
#     b. Majoritarian
#     c. Semi-proportional Systems

# Let's start by getting the list of voting systems from this table on Wikipedia:
# https://en.wikipedia.org/wiki/List_of_electoral_systems_by_country
# wiki_url <- 'https://en.wikipedia.org/wiki/List_of_electoral_systems_by_country'
# wiki_xpath <- '//*[@id="mw-content-text"]/div[1]/table[4]'

es_url <- 'https://aceproject.org/epic-en/CDTable?view=country&question=ES005'
es_xpath <- '//*[@id="tblData"]'
es <- es_url %>%
  read_html() %>%
  html_node(xpath = es_xpath) %>%
  html_table(fill = TRUE) %>%
  # Make the first row the headers
  janitor::row_to_names(1)

es_cleaned <- es %>% 
  janitor::clean_names() %>%
  # Replace any blanks
  dplyr::na_if("") %>% 
  # Countries can have multiple voting systems denoted. Break these apart by '.'
  tidyr::separate(answers, 
                  into = c('remove', 
                           'voting_system_1',
                           'voting_system_2',
                           'voting_system_3'),
                  sep = '[a-z]+\\. ',
                  remove = FALSE) %>% 
  # Drop the 'remove' column
  select(-'remove') %>% 
  # Pivot the data so the voting systems are one column
  tidyr::pivot_longer(cols = c('voting_system_1',
                               'voting_system_2',
                               'voting_system_3'),
                      names_to = 'remove',
                      values_to = 'voting_system',
                      values_drop_na = TRUE) %>% 
  select(-'remove') %>% 
  # Sometimes the word 'Vote' is replaced with just a 'V'. Let's replace these
  mutate(voting_system = stringr::str_replace(voting_system, ' V$', ' Vote'),
         voting_system = stringr::str_replace(voting_system, ' R$', ' Representation'))


# Let's see which voting systems were the most popular
es_bar <- es_cleaned %>%
  count(voting_system, sort = TRUE) %>% 
  # Start our visualization, creating our groups by party affiliation
  ggplot(aes(x = n,
             y = forcats::fct_reorder(voting_system, n))) +
  geom_bar(stat = "identity", fill = "slateblue", na.rm = T) +
  # Change the theme to classic
  theme_classic() +
  # Let's change the names of the axes and title
  xlab("Electoral System") +
  ylab("Count") +
  labs(title = "Number of Electoral Systems",
       subtitle = paste("Data is broken out across",
                        n_distinct(es_cleaned$country_territory), 
                        "countries/territories,"),
       caption = paste("Data is accredited to the work of the ACE Project\n",
                       "https://aceproject.org/epic-en?question=ES005&f=h")
  ) +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

es_bar
ggsave(here("Viz/Number of Electoral Systems.jpg"), es_bar)
