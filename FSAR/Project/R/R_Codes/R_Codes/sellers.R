# set up 
library(tidyverse)
library(skimr)
library(lubridate)
library(dplyr)

# load data 
orders <- read_csv("sellers.csv")

#data dictionary
data_dictionary <- read_csv("sellers.csv")
View(data_dictionary)

# overview
glimpse(orders)
nrow(orders)
ncol(orders)
names(orders)
skim(orders)

#observation
sellers %>%
  group_by(seller_id) %>%
  summarise(N = n())

#missing value
sellers %>%
  filter(is.na(region) | is.na(city) | is.na(postal_area)) %>%
  summarise(N = n()) 

# sellers per region, city, postal area
sellers %>% group_by(region) %>% summarise(N = n()) %>% nrow()
sellers %>% group_by(city) %>% summarise(N = n()) %>% nrow()
sellers %>% group_by(postal_area) %>% summarise(N = n()) %>% nrow()

# seller with the most orders
unique_orders <- order_items %>%
  distinct(seller_id, order_id)
unique_orders

count_of_sellers <- sellers %>%
  left_join(unique_orders, by = "seller_id") %>% 
select(seller_id, order_id)

count_of_sellers %>%
  group_by(seller_id) %>%
  summarise(N = n()) %>%
  arrange(desc(N))
  
count_of_sellers %>%
  group_by(seller_id) %>%
  summarise(N = n()) %>%
  arrange(N)
