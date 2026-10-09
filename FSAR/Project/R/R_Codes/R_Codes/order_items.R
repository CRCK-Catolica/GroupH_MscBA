# set up 
library(tidyverse)
library(skimr)
library(lubridate)
library(dplyr)

# load data 
orders <- read_csv("order_items.csv")

#data dictionary
data_dictionary <- read_csv("order_items.csv")
View(data_dictionary)

# overview
glimpse(orders)
nrow(orders)
ncol(orders)
names(orders)
skim(orders)

#observe
order_items %>%
  group_by(item_number) %>%
  summarise(N = n()) %>%
  filter(N > 1)

# missing values
order_items %>%
  filter(is.na(order_id) | is.na(item_number)
         | is.na(product_id) | is.na(seller_id) | is.na (shipping_deadline)
         | is.na (item_price) | is.na (freight_cost))






