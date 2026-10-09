# set up 
library(tidyverse)
library(skimr)
library(lubridate)
library(dplyr)

# load data 
orders <- read_csv("customers.csv")

#data dictionary
data_dictionary <- read_csv("customers.csv")
View(data_dictionary)

# overview
glimpse(orders)
nrow(orders)
ncol(orders)
names(orders)
skim(orders)

#observe
customers %>%
  group_by(customer_record_id) %>%
  summarise(N = n()) %>%
  filter(N > 1)

#missing values
customers %>%
  filter(is.na(customer_record_id) | is.na(customer_id) | is.na(postal_area) |is.na(city) | is.na(region))

# how many records does one person have
customers %>%
  count(customer_id, name = "N") %>%
  arrange(desc(N)) %>%
  filter(N > 1)

customer_per_region <- customers %>%
  group_by(region) %>%
  summarise(Number_of_people = n_distinct(customer_id))
  
  





