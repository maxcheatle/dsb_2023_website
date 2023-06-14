---
categories:  
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2023-05-03"
description: Connecting large databases to R for analysis, and webscraping public websites  # the title that will show up once someone gets to this page
draft: false
image: h3_cover.png # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: homework3 # slug is the shorthand URL address... no spaces plz
title: Databases and Web Scraping
---

```{r}
#| label: load-libraries
#| echo: false # This option disables the printing of code (only output is displayed).
#| message: false
#| warning: false

library(tidyverse)
library(wbstats)
library(tictoc)
library(skimr)
library(countrycode)
library(here)
library(DBI)
library(dbplyr)
library(arrow)
library(rvest)
library(robotstxt) # check if we're allowed to scrape the data
library(scales)
library(sf)
library(readxl)
library(stringr)

```

# Money in UK politics

## Open a connection to the database

The database made available by Simon Willison is an `SQLite` database

```{r}
sky_westminster <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = here::here("data", "sky-westminster-files.db")
)
```

How many tables does the database have?

```{r}
DBI::dbListTables(sky_westminster)
```

## Which MP has received the most amount of money?

```{r}

# First I am setting up the tables as database objects, so that I can manipulate them as dataframes

payments_db <- dplyr::tbl(sky_westminster, "payments")
members_db <- dplyr::tbl(sky_westminster, "members")

# Checking outputs.... looks good

#view(payments_db)
#view(members_db)

# We could jump straight to a join here, but I'll summarise on payments first to keep it easier to follow

id_payments <- payments_db %>% 
  
  # Grouping by member_id for summation
  group_by(member_id) %>% 
  
  # Summing the value of all payments to each member_id
  summarise(total_recieved = sum(value))
  
# Now I want to join to the members table, where payments_db$member_id = members_db$id

member_payments <- left_join(id_payments, members_db, by = c("member_id" = "id"))

# Now let's take that table to see who's getting the most cash

member_payments %>% 
  select(member_id, name, total_recieved) %>% 
  arrange(desc(total_recieved))

```

-   Looks like Theresa May is a favorite for donors!

## Any `entity` that accounts for more than 5% of all donations?

```{r}

# Let's find out which entity is splashing big cash

# First let me find the total amount of donations in our payments_db dataframe

total_donos <- payments_db %>% 
  summarise(total_donos = sum(value)) %>% 
  pull(total_donos)

# Then do the same, but on an entity-by-entity basis

entity_donos <- payments_db %>% 
  group_by(entity) %>% 
  summarise(entity_donos = sum(value))

# Finally, a simple caluclation to work out the percentages

entity_donos %>% 
  mutate(pct_donos = entity_donos/total_donos*100) %>% 
  arrange(desc(pct_donos)) 
  
```

-   Withers LLP's donations account for 5.3% of total donations!

## Do `entity` donors give to a single party or not?

```{r}

# First let's get a list of the distinct entities and count them

# Saving the value for use later
total_entities <- payments_db %>% 
  distinct(entity) %>% 
  summarise(entities = n()) %>% 
  pull(entities)

total_entities

# Now let's find out who only donates to one party

entity_parties <- payments_db %>% 
  
  # Joining payments and members, since members has party_id which we need
  left_join(members_db, by = c("member_id" = "id")) %>% 
  select(entity, party_id) %>% 
  arrange(desc(entity)) %>% 
  
  # Now selecting the distinct entity and party_id rows
  distinct(entity, party_id) %>% 
  
  # Grouping by entity for incoming calculation
  group_by(entity) %>% 
  
  # Counting the number of parties that each entity donates to
  summarise(parties_dono = n()) %>% 
  
  # Converting the number of parties donated to, to a categorical single/multiple variable
   mutate(parties_dono = case_when(
    parties_dono > 1 ~ "Multiple",
    TRUE ~ "Single"
  )) %>% 
  
  # Grouping by our new categorical variable
  group_by(parties_dono) %>% 
  
  # And counting the respective instances of single/multiple donors
  summarise(count = n())

 # Now presenting the answers

entity_parties %>% 
  
  # Creating a pct column, using the total_entities saved from the start of this chunk
  mutate(pct_multiple = as.numeric(count)/total_entities*100)

```

-   There are 2213 distinct donor entities, of which only 8% (177) donate to multiple parties

## Which party has raised the greatest amount of money in each of the years 2020-2022?

```{r out.width="80%"}
# knitr::include_graphics(here::here("images", "total_donations_table.png"), error = FALSE)

# First creating tables as databse objects

parties_db <- dplyr::tbl(sky_westminster, "parties")

# Now gathering the necessary data

yearly_payments <-
  
  payments_db %>% 
    
    # Opted to use sql() here, as I'm fluent in SQL and couldn't find a reliable way to do this using dplyr. I found a working method, but it then causes issues with the upcoming joins
    mutate(year = as.integer(sql("SUBSTR(date, -4)"))) %>% 
    filter(year >= 2020) %>% 

    # We now need the join with the members table where the party_id is stored
    left_join(members_db, by = c("member_id" = "id")) %>% 
  
    # Again, just reducing to the necessary cols  
    select(year, member_id, value, party_id) %>% 
   
    # Left joining to parties_db, which has the party names
    left_join(parties_db, by = c("party_id" = "id")) %>% 
    
    # Grouping by year and party name for sum
    group_by(year, name) %>%
    summarise(total_year_donatations = sum(value)) %>%
    
    # Now ungrouping to find total per year and calculate percentage
    ungroup(name) %>%
    mutate(prop = total_year_donatations / sum(total_year_donatations) * 100)

yearly_payments
  
# My numbers here are different, but I really can't understand why. I also noticed that some parties don't appear every year in the screenshot attached - perhaps this is where I'm going wrong. I've also iteratively consulted ChatGPT and it seems not be getting any different numbers.   
```

... and then, based on this data, plot the following graph.

```{r out.width="80%"}
# knitr::include_graphics(here::here("images", "total_donations_graph.png"), error = FALSE)

yearly_payments %>% 
  
    # Now plotting, making sure to reorder our fill
    ggplot(aes(y = total_year_donatations, x = year, fill = fct_reorder(name, -total_year_donatations))) +
  
    # position = "dodge" to prevent stacking
    geom_bar(stat = "identity", position = "dodge") +
  
    # Aesthetics
    theme_minimal() +
    labs(title = "Conservatives have captured the majority of political donations", subtitle = "Donations to public parties, 2020-2022", x = NULL, y = NULL, fill = "Party")

```

This uses the default ggplot colour pallete, as I dont want you to worry about using the [official colours for each party](https://en.wikipedia.org/wiki/Wikipedia:Index_of_United_Kingdom_political_parties_meta_attributes). However, I would like you to ensure the parties are sorted according to total donations and not alphabetically. You may even want to remove some of the smaller parties that hardly register on the graph. Would facetting help you?

Finally, when you are done working with the databse, make sure you close the connection, or disconnect from the database.

```{r}
dbDisconnect(sky_westminster)
```

# Anonymised Covid patient data from the CDC

## Obtain the data

```{r}
#| echo: false
#| message: false
#| warning: false


tic() # start timer
cdc_data <- open_dataset(here::here("data", "cdc-covid-geography"))
toc() # stop timer


glimpse(cdc_data)
```

```{r out.width="100%"}
# knitr::include_graphics(here::here("images", "covid-CFR-ICU.png"), error = FALSE)

# Let's give it a go by creating the necessary data frame first

cdc_data %>% 
  
  # Making sure we only count certain ICU admissions/non-admissions
  filter(icu_yn %in% c("Yes", "No")) %>% 
  
  # Making sure we only count certain deaths/non-deaths
  filter(death_yn %in% c("Yes", "No")) %>% 
  
  # Making sure we only count certain males/females
  filter(sex == "Male" | sex == "Female") %>% 
  
  # Removing rows without age_group variable  
  filter(age_group != "Missing") %>% 
  
  # Renaming for clarity
  mutate(icu_yn = case_when(
    icu_yn == "Yes" ~ "ICU Admission",
    TRUE ~ "No ICU Admission"
  )) %>%
  
  # Wanted to change death_yn column to a binary numerical value, so I can use sum() in the summarise without needing to filter
  mutate(death_yn = case_when(
    death_yn == "Yes" ~ "1", # Note here that 1 = Death 
    TRUE ~ "0"
  )) %>% 
  
  # Grouping by the categories given in the example plot    
  group_by(age_group, sex, icu_yn) %>% 
  
  # Calculating our inputs
  summarise(total_deaths = sum(as.numeric(death_yn)), total_cases = n()) %>%
  
  # Calculating our variable of interest  
  mutate(cfr = total_deaths/total_cases) %>% 

  # Collecting as data frame 
  collect() %>% 
  
  # And plotting
  ggplot(aes(x = cfr, y = age_group)) +
  geom_col(fill = "#ff8e7c") +
  
  # Faceting in grid format
  facet_grid(rows = vars(icu_yn), cols = vars(sex)) +
  
  # Now aesthetics
  geom_text(aes(label = round(cfr*100, 0), hjust = 1), size = 3) +
  labs(title = "Covid CFR % by age group, sex, and ICU Admission", caption = "Source: CDC", x = NULL, y = NULL) +
  theme_light() +
  scale_x_continuous(labels = percent_format())

```

The previous plot is an aggregate plot for all three years of data. What if we wanted to plot Case Fatality Ratio (CFR) over time? Write code that collects the relevant data from the database and plots the following

```{r out.width="100%"}
# knitr::include_graphics(here::here("images", "cfr-icu-overtime.png"), error = FALSE)

cdc_data %>% 
  
  # Making sure we only count certain ICU admissions/non-admissions
  filter(icu_yn %in% c("Yes", "No")) %>% 
  
  # Making sure we only count certain deaths/non-deaths
  filter(death_yn %in% c("Yes", "No")) %>% 
  
  # Making sure we only count certain males/females
  filter(sex == "Male" | sex == "Female") %>% 
  
  # Removing rows without age_group variable  
  filter(age_group != "Missing") %>% 
  
  # Renaming for clarity
  mutate(icu_yn = case_when(
    icu_yn == "Yes" ~ "ICU Admission",
    TRUE ~ "No ICU Admission"
  )) %>%
  
  # Wanted to change death_yn column to a binary numerical value, so I can use sum() in the summarise without needing to filter
  mutate(death_yn = case_when(
    death_yn == "Yes" ~ "1", # Note here that 1 = Death 
    TRUE ~ "0"
  )) %>% 
  
  # Grouping by the categories given in the example plot    
  group_by(case_month, age_group, sex, icu_yn) %>% 
  
  # Calculating our inputs
  summarise(total_deaths = sum(as.numeric(death_yn)), total_cases = n()) %>%
  
  # Calculating our variable of interest  
  mutate(cfr = total_deaths/total_cases) %>% 

  # Collecting as data frame 
  collect() %>% 
  
    # And plotting
  ggplot(aes(x = case_month, y = cfr)) +
  geom_line(aes(group = age_group, color = age_group)) +
  
  # Faceting in grid format
  facet_grid(rows = vars(icu_yn), cols = vars(sex)) +

  # Now aesthetics
  geom_text(aes(label = round(cfr*100, 0), hjust = 1, color = age_group), size = 3) +
  labs(title = "Covid CFR % by age group, sex, and ICU Admission", caption = "Source: CDC", x = NULL, y = NULL) +
  theme_light() +
  scale_y_continuous(labels = percent_format()) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 4))  +
  NULL

```

```{r}
urban_rural <- read_xlsx(here::here("data", "NCHSURCodes2013.xlsx")) %>% 
  janitor::clean_names()
```

```{r out.width="100%"}
# knitr::include_graphics(here::here("images", "cfr-county-population.png"), error = FALSE)

cdc_data %>% 
  
  # Making sure we only count certain ICU admissions/non-admissions
  filter(icu_yn %in% c("Yes", "No")) %>% 
  
  # Making sure we only count certain deaths/non-deaths
  filter(death_yn %in% c("Yes", "No")) %>% 
  
  # Making sure we only count certain males/females
  filter(sex == "Male" | sex == "Female") %>% 
  
  # Removing rows without age_group variable  
  filter(age_group != "Missing") %>% 
  
  # Renaming for clarity
  mutate(icu_yn = case_when(
    icu_yn == "Yes" ~ "ICU Admission",
    TRUE ~ "No ICU Admission"
  )) %>%
  
  # Wanted to change death_yn column to a binary numerical value, so I can use sum() in the summarise without needing to filter
  mutate(death_yn = case_when(
    death_yn == "Yes" ~ "1", # Note here that 1 = Death 
    TRUE ~ "0"
  )) %>% 
  
  # Now let's join the urban_rural table
  mutate(county_fips_code = as.double(county_fips_code)) %>% 
  left_join(urban_rural, by = c("county_fips_code" = "fips_code")) %>% 

  
  # Grouping by the categories given in the example plot    
  group_by(case_month, x2013_code) %>% 
  
  # Calculating our inputs
  summarise(total_deaths = sum(as.numeric(death_yn)), total_cases = n()) %>%
  
  # Calculating our variable of interest  
  mutate(cfr = total_deaths/total_cases) %>%

  # Collecting as data frame 
  collect() %>% 
  
  # Let's remove NAs
  filter(x2013_code != "NA") %>%
  filter(case_month != "2020-02") %>% 
  
  # And rename x2013_code names
  mutate(x2013_code = case_when(
    x2013_code == 1 ~ "Large central metro",
    x2013_code == 2 ~ "Large fringe metro",
    x2013_code == 3 ~ "Medium metro",
    x2013_code == 4 ~ "Small metro population",
    x2013_code == 5 ~ "Micropolitan",
    x2013_code == 6 ~ "Noncore"
  )) %>% 
  
  # And plotting
  ggplot(aes(x = case_month, y = cfr)) +
  geom_line(aes(group = x2013_code, color = x2013_code)) +
  
  # Faceting in grid format
  facet_wrap(~ x2013_code, nrow = 3, scales = "free") +

  # Now aesthetics
  geom_text(aes(label = round(cfr*100, 0), hjust = 1, color = x2013_code), size = 3) +
  labs(title = "Covid CFR % by county population", caption = "Source: CDC", x = NULL, y = NULL, color = NULL) +
  theme_light() +
  scale_y_continuous(labels = percent_format()) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 4))  +
  NULL

```

```{r out.width="100%"}
# knitr::include_graphics(here::here("images", "cfr-rural-urban.png"), error = FALSE)

cdc_data %>% 
  
  # Making sure we only count certain ICU admissions/non-admissions
  filter(icu_yn %in% c("Yes", "No")) %>% 
  
  # Making sure we only count certain deaths/non-deaths
  filter(death_yn %in% c("Yes", "No")) %>% 
  
  # Making sure we only count certain males/females
  filter(sex == "Male" | sex == "Female") %>% 
  
  # Removing rows without age_group variable  
  filter(age_group != "Missing") %>% 
  
  # Renaming for clarity
  mutate(icu_yn = case_when(
    icu_yn == "Yes" ~ "ICU Admission",
    TRUE ~ "No ICU Admission"
  )) %>%
  
  # Wanted to change death_yn column to a binary numerical value, so I can use sum() in the summarise without needing to filter
  mutate(death_yn = case_when(
    death_yn == "Yes" ~ "1", # Note here that 1 = Death 
    TRUE ~ "0"
  )) %>% 
  
  # Now let's join the urban_rural table
  mutate(county_fips_code = as.double(county_fips_code)) %>% 
  left_join(urban_rural, by = c("county_fips_code" = "fips_code")) %>% 
  
  # And rename x2013_code names
  mutate(x2013_code = case_when(
    x2013_code == 1 ~ "Urban",
    x2013_code == 2 ~ "Urban",
    x2013_code == 3 ~ "Urban",
    x2013_code == 4 ~ "Rural",
    x2013_code == 5 ~ "Rural",
    x2013_code == 6 ~ "Rural"
  )) %>% 
  
  # Grouping by the categories given in the example plot    
  group_by(case_month, x2013_code) %>% 
  
  # Calculating our inputs
  summarise(total_deaths = sum(as.numeric(death_yn)), total_cases = n()) %>%
  
  # Calculating our variable of interest  
  mutate(cfr = total_deaths/total_cases) %>%

  # Collecting as data frame 
  collect() %>% 
  
  # Let's remove NAs
  filter(x2013_code != "NA") %>%
  filter(case_month != "2020-02") %>% 
  
  # And plotting
  ggplot(aes(x = case_month, y = cfr)) +
  geom_line(aes(group = x2013_code, color = x2013_code)) +

  # Now aesthetics
  geom_text(aes(label = round(cfr*100,2), hjust = 1), color = "black",  size = 3) +
  labs(title = "Covid CFR % by rural and ubran areas", caption = "Source: CDC", x = NULL, y = NULL, color = NULL) +
  theme_light() +
  scale_y_continuous(labels = percent_format()) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 4))  +
  NULL

```

# Money in US politics

```{r}
#| label: allow-scraping-opensecrets
#| warning: false
#| message: false

library(rvest)
library(robotstxt)
paths_allowed("https://www.opensecrets.org")

base_url <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2022"

contributions_tables <- base_url %>%
  read_html() 

```

```{r}

# Scraping the table for all of its information
contributions <- data.frame(
  
  contributions_tables %>%
                              
  # Using inspect element to find the correct selector
  html_elements("#main > div.Main-wrap.l-padding.u-mt2 > div > div > div.l-primary > div:nth-child(1) > div > div:nth-child(5)") %>%
  html_table()
    ) %>% 
  
  # Cleaning names of our data frame
  janitor::clean_names()



# Write a function to parse_currency
parse_currency <- function(x){
  x %>%
    
    # remove dollar signs
    str_remove("\\$") %>%
    
    # remove all occurrences of commas
    str_remove_all(",") %>%
    
    # convert to numeric
    as.numeric()
}



# Applying function to contributions

contributions <- contributions %>% 
  
  # Using the function within a mutate of our existing data frame
  mutate(total = parse_currency(total),
         dems = parse_currency(dems),
         repubs = parse_currency(repubs))



# Clean country/parent co and contributions 
contributions <- contributions %>%
  separate(country_of_origin_parent_company, 
           into = c("country", "parent"), 
           sep = "/", 
           extra = "merge")

glimpse(contributions)

```

```{r}

# To create our function, with url as an input, we simply follow the same chain of commands as in the previous block
scrape_pac <- function(url) {

  # Process 1
  contributions_tables <- url %>% 
    read_html()
  
  # Process 2
  contributions <- data.frame(
  
  contributions_tables %>%
                              
  html_elements("#main > div.Main-wrap.l-padding.u-mt2 > div > div > div.l-primary > div:nth-child(1) > div > div:nth-child(5)") %>%
  html_table()
    ) %>% 
  
  janitor::clean_names()
  
  # Process 3 & 4
  contributions <- contributions %>% 
      separate(country_of_origin_parent_company, 
           into = c("country", "parent"), 
           sep = "/", 
           extra = "merge") %>% 
  
      mutate(total = parse_currency(total),
             dems = parse_currency(dems),
             repubs = parse_currency(repubs))
  
  contributions <- contributions %>% 
    mutate(year = str_sub(url, -4))
  
  glimpse(contributions)
  
  }

# Defining urls for 2000, 2020 and 2022
url_2022 <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2022"

url_2020 <-"https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2020"

url_2000 <-"https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2000"

# Creating a vector
os_urls <- c(url_2000, url_2020, url_2022)

# Running function over os_urls
contributions_all <- map_df(os_urls, scrape_pac)

# Accuracy check
contributions_all %>% 
  group_by(year) %>% 
  count() # Looks good

write.csv(contributions_all, file.path("~/Library/CloudStorage/OneDrive-LondonBusinessSchool/Electives/E628 Data Science/dsb2023_mcheatle/data", "contributions-all.csv"))

```

# Scraping consulting jobs

The website [https://www.consultancy.uk/jobs/](https://www.consultancy.uk/jobs) lists job openings for consulting jobs.

```{r}
#| label: consulting_jobs_url
#| eval: false

library(robotstxt)
paths_allowed("https://www.consultancy.uk") #is it ok to scrape?

base_url <- "https://www.consultancy.uk/jobs/page/1"

# First reading the html, so that we can use CSS selectors to retrieve attributes
listings_html <- base_url %>%
  read_html()

# Using the job selector to retrieve the job title
job <- listings_html %>%    
html_nodes(css = "span.title") %>%   
html_text2()

# Using the firm selector to retrieve the firm name
firm <- listings_html %>%    
html_nodes(css = ".hide-phone .row-link") %>%    
html_text2()

# Using the link selector, and concatenting the href on the end of the base url
link <- listings_html %>%    
html_nodes(css = ".hide-phone .row-link") %>%    
html_attr('href') %>%    
str_c("https://www.consultancy.uk", .)

# Using the functional area to selector to retrieve the job area
functional_area <- listings_html %>%    
html_elements(css = ".initial") %>%    
html_text2()

# Using the type selector to retrieve the type of job
type <- listings_html %>%    
html_nodes(css = ".hide-tablet-landscape .row-link") %>%    
html_text2() 

# Combining the above into a tibble
jobs_df <- tibble(job = job, 
                    firm = firm, 
                    functional_area = functional_area, 
                    type = type, 
                    link = link)

```

Identify the CSS selectors in order to extract the relevant information from this page, namely

1.  job - span.title
2.  firm - .hide-phone .row-link
3.  functional area - .initial
4.  type - .hide-tablet-landscape .row-link

```{r}


# To create our function, with url as an input, we simply follow the same chain of commands as in the previous block
scrape_jobs <- function(url) {

  listings_html <- url %>%
    read_html()
  
  job <- listings_html %>%    
  html_nodes(css = "span.title") %>%   
  html_text2()
  
  firm <- listings_html %>%    
  html_nodes(css = ".hide-phone .row-link") %>%    
  html_text2()
  
  link <- listings_html %>%    
  html_nodes(css = ".hide-phone .row-link") %>%    
  html_attr('href') %>%    
  str_c("https://www.consultancy.uk", .)
  
  functional_area <- listings_html %>%    
  html_elements(css = ".initial") %>%    
  html_text2()
  
  type <- listings_html %>%    
  html_nodes(css = ".hide-tablet-landscape .row-link") %>%    
  html_text2() 
  
  jobs_df <- tibble(job = job, 
                    firm = firm, 
                    functional_area = functional_area, 
                    type = type, 
                    link = link)
  
 # view(jobs_df) Only used this for checking

}

# Testing our function with page 2
scrape_jobs("https://www.consultancy.uk/jobs/page/2")

# Setting the base url, without a page number
base_url <- "https://www.consultancy.uk/jobs/page/"

# Setting url, as the base_url, concatenated with the numbers 1 to 8 (for all 8 pages of the website)
url <- str_c(base_url, 1:8)

all_consulting_jobs <- map_df(url, scrape_jobs)

write.csv(all_consulting_jobs, file.path("~/Library/CloudStorage/OneDrive-LondonBusinessSchool/Electives/E628 Data Science/dsb2023_mcheatle/data", "all_consulting_jobs.csv"))
```
