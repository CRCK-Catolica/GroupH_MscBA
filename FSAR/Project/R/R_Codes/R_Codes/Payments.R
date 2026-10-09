# set up 
library(tidyverse)
library(skimr)
library(lubridate)
library(dplyr)

# load data 
orders <- read_csv("payments.csv")

#data dictionary
data_dictionary <- read_csv("payments.csv")
View(data_dictionary)

# overview
glimpse(orders)
nrow(orders)
ncol(orders)
names(orders)
skim(orders)

#observe
payments %>%
  group_by(order_id) %>%
  summarise(N = n()) %>%
  filter(N > 1)

#missing values
payments %>%
  filter(is.na(order_id) | is.na(payment_sequence) | is.na(payment_method) |is.na(installments) | is.na(payment_amount))
