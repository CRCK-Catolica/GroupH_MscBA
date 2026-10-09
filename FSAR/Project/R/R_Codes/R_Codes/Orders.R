# set up 
library(tidyverse)
library(skimr)
library(lubridate)
library(dplyr)

# load data 
orders <- read_csv("orders.csv")

#data dictionary
data_dictionary <- read_csv("orders.csv")
View(data_dictionary)

# overview
glimpse(orders)
nrow(orders)
ncol(orders)
names(orders)
skim(orders)

# is every order item unique? for order_id:
orders %>%
  group_by(order_id) %>%
  summarise(N = n()) %>%
  filter(N > 1)

# is every order item unique? for customer_order_id:
orders %>%
  group_by(customer_record_id) %>%
  summarise(N = n()) %>%
  filter(N > 1)

#order status
orders %>%
  group_by(order_status) %>%
  summarise(N = n()) %>%
  mutate(share = round(N / sum(N) * 100, 2)) %>%
  arrange(desc(N))

# missing values
orders %>%
  group_by(order_status) %>%
  summarise(N           = n(),
            no_approval = sum(is.na(approval_timestamp)),
            no_carrier  = sum(is.na(carrier_timestamp)),
            no_delivery = sum(is.na(delivery_timestamp)))

# Cancelled, but delivered:
orders %>% 
  filter(order_status == "CANCELLED" & !is.na(delivery_timestamp))

# Completed, but no delivery date:
orders %>% 
  filter(order_status == "COMPLETED" & is.na(delivery_timestamp))

#order 
orders %>%
  summarise(approval_before_purchase = sum(approval_timestamp < purchase_timestamp, na.rm = TRUE),
            carrier_before_purchase  = sum(carrier_timestamp  < purchase_timestamp, na.rm = TRUE),
            carrier_before_approval  = sum(carrier_timestamp  < approval_timestamp, na.rm = TRUE),
            delivery_before_carrier  = sum(delivery_timestamp < carrier_timestamp,  na.rm = TRUE),
            delivery_before_purchase = sum(delivery_timestamp < purchase_timestamp, na.rm = TRUE)) %>%
glimpse()

# carrier before timestamp:
orders %>%
  filter(carrier_timestamp < purchase_timestamp) %>%
  mutate(year  = year(purchase_timestamp),
         month = month(purchase_timestamp),
         hours_too_early = as.numeric(purchase_timestamp - carrier_timestamp, units = "hours")) %>%
  group_by(year, month) %>%
  summarise(N = n(),
            median_hours = median(hours_too_early))

#carrier before approval:
orders %>%
  filter(carrier_timestamp < approval_timestamp) %>%
  mutate(year = year(purchase_timestamp)) %>%
  group_by(year) %>%
  summarise(N = n())

#orders per month and month with the most sales
orders %>%
  mutate(year = year(purchase_timestamp)) %>%
  mutate(month = month(purchase_timestamp)) %>%
  group_by(year, month) %>%
  summarise(Number_of_purchases = n()) %>%
  arrange(desc(Number_of_purchases), (year), (month)) %>%
  print(n = 30)

# dublicate orders
same_time <- orders %>%
  left_join(customers, by = "customer_record_id") %>%
  group_by(customer_id, purchase_timestamp) %>%
  summarise(N = n()) %>%
  filter(N > 1)

nrow(same_time)

same_time_2 <- orders %>%
  left_join(customers, by = "customer_record_id") %>%
  inner_join(same_time_2, by = c("customer_id", "purchase_timestamp")) %>%
  arrange(customer_id) %>%
  select(order_id, customer_id, order_status, purchase_timestamp, delivery_timestamp)
  # head(10)

same_time_2 %>%
  inner_join(distinct(order_items), by = "order_id") %>%
  group_by((customer_id), (purchase_timestamp), (product_id)) %>%
  summarise(N = n_distinct(order_id), order_ids = sort(unique(order_id)))

