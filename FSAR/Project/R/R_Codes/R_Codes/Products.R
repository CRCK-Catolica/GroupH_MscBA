# set up 
library(tidyverse)
library(skimr)
library(lubridate)
library(dplyr)

# load data 
orders <- read_csv("products.csv")

#data dictionary
data_dictionary <- read_csv("products.csv")
View(data_dictionary)

# overview
glimpse(orders)
nrow(orders)
ncol(orders)
names(orders)
skim(orders)

#observation
products %>%
  group_by(product_id) %>%
  summarise(N = n()) %>%
  filter(N > 1)

#missing values
products %>%
  filter(is.na(product_category)) %>%
  summarise(N = n(),
            name_missing        = sum(is.na(name_length)),
            description_missing = sum(is.na(description_length)),
            photos_missing      = sum(is.na(photo_count)),
            weight_missing      = sum(is.na(weight_g)))

products %>% filter(is.na(weight_g))

# product categories
products_per_category <- products %>%
  group_by(product_category) %>%
  summarise(N = n())

nrow(products_per_category)   # number of categories (incl. NA)

products_per_category %>% arrange(desc(N)) %>% head(10)   # largest
products_per_category %>% arrange(N) %>% head(10)         # smallest

# link to order_items
items_per_product <- order_items %>%
  group_by(product_id) %>%
  summarise(n_sold = n())

# how often is a product sold
summary(items_per_product$n_sold)

# Best-selling products
items_per_product %>%
  left_join(products %>% select(product_id, product_category), by = "product_id") %>%
  arrange(desc(n_sold)) %>%
  head(10)

# Products that were never sold
products %>%
  left_join(items_per_product, by = "product_id") %>%
  filter(is.na(n_sold)) %>%
  nrow()