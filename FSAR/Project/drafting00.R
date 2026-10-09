library(readr)
library(dplyr)

cust_fdbck<-read.csv('./Data/customer_feedback.csv')
cust<-read.csv('./Data/customers.csv')
order_items<-read.csv('./Data/order_items.csv')
orders<-read.csv('./Data/orders.csv')
payments<-read.csv('./Data/payments.csv')
products<-read.csv('./Data/products.csv')
sellers<-read.csv('./Data/sellers.csv')

dim(cust)
colSums((is.na(cust)))

cust %>% summarise(
  row=n(),
  distinct_recrod_id=n_distinct(customer_record_id),
  distinct_customer_id = n_distinct(customer_id),
  exact_duplicate_rows=sum(duplicated(cust))
)