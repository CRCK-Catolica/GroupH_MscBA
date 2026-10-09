library(tidyverse)
library(skimr)
library(lubridate)
library(ggplot2)

orders <-read.csv("/Data/orders.csv")

skim(orders)
library(dplyr)

orders %>%
  group_by(order_status) %>%
  summarise(Freq = n()) %>%
  arrange(desc(Freq))


dup_orders <- orders %>%
  left_join(customers, by = "customer_record_id") %>%          # add customer_id
  inner_join(distinct(items_order), by = "order_id") %>%       # add product_id
  group_by(customer_id, purchase_timestamp, product_id) %>%    # same person, same second, same product
  filter(n_distinct(order_id) > 1) %>%                         # ...appearing in more than one order
  ungroup() %>%
  distinct(order_id, customer_id, purchase_timestamp, product_id) %>%
  arrange(customer_id, purchase_timestamp)

dup_orders

difference_created_answered <- feedback %>%
  mutate(reponse_time = round(as.numeric(difftime(feedback_answered_timestamp, feedback_created_timestamp, units = "days")), 1 ))

#hisogramm of response time
ggplot(data = difference_created_answered, aes(x = reponse_time)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue4", color = "red")+
  # Set margins for x and y-axis titles and plot title
  theme(axis.title.x = element_text(margin = margin(t = 10, b = 10, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, b = 0, l = 10)),
        plot.title   = element_text(margin = margin(t = 20, b = 15, l = 0)))


