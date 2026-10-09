# 01_customers_id_check.R
# Checks whether the identifiers in customers.csv are unique,
# and how customer records link to orders.
# Run from the Project.Rproj root (relative paths).

library(readr)
library(dplyr)

customers <- read_csv("Data/customers.csv", show_col_types = FALSE)
orders    <- read_csv("Data/orders.csv",    show_col_types = FALSE)

# 1. Size and missing values
dim(customers)
colSums(is.na(customers))

# 2. Uniqueness of each ID
customers %>%
  summarise(
    rows                     = n(),
    distinct_record_id       = n_distinct(customer_record_id),
    distinct_customer_id     = n_distinct(customer_id),
    exact_duplicate_rows     = sum(duplicated(customers))
  )

# 3. How many records does each customer_id have?
records_per_customer <- customers %>% count(customer_id, name = "n_records")
records_per_customer %>% count(n_records, name = "n_customers")

# 4. Do repeat customers have different locations across their records?
customers %>%
  semi_join(filter(records_per_customer, n_records > 1), by = "customer_id") %>%
  group_by(customer_id) %>%
  summarise(n_postal = n_distinct(postal_area),
            n_region = n_distinct(region)) %>%
  summarise(multi_record_customers = n(),
            with_diff_postal_area  = sum(n_postal > 1),
            with_diff_region       = sum(n_region > 1))

# 5. Link to orders: is it one record per order?
orders %>%
  summarise(distinct_record_id_in_orders = n_distinct(customer_record_id),
            dup_record_id_in_orders      = sum(duplicated(customer_record_id)))

sum(!customers$customer_record_id %in% orders$customer_record_id)  # records with no order
sum(!orders$customer_record_id %in% customers$customer_record_id)  # orders with no record
