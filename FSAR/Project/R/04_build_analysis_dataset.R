# =============================================================================
# 04_build_analysis_dataset.R
# FSAR 2026 - Group H - Final analytical dataset (Scoping Package, section 1.2)
#
# Research question:
#   Are orders where seller and customer are in the same region delivered faster
#   and rated higher, and how much of the rating difference is explained by
#   shorter delivery times?
#
# Unit of observation : the order
# Analytical sample   : completed orders with a delivery date and a customer rating
# Outcomes            : delivery_days, rating (+ low_rating for robustness)
# Variable of interest: same_region (1 = all sellers of the order are in the
#                       customer's region)
# Controls            : customer_region, weight, freight_share, order_value,
#                       n_items, n_sellers, main_category, payment_method,
#                       promised_days, purchase_month
#
# Decisions implemented here are documented in the Data Decision Log (D1-D10).
# Run from the Project.Rproj root. Packages: readr, dplyr, stringr
# Output: output/analysis_orders.csv and output/analysis_orders.rds
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})
options(dplyr.summarise.inform = FALSE)

# ---- 1. Load the original CSV files (explicit types; timestamps in UTC) -------
dt <- col_datetime(format = "")
customers <- read_csv("Data/customers.csv", col_types = cols(.default = col_character()))
orders <- read_csv("Data/orders.csv", col_types = cols(
  .default = col_character(), purchase_timestamp = dt, approval_timestamp = dt,
  carrier_timestamp = dt, delivery_timestamp = dt, estimated_delivery_timestamp = dt))
items <- read_csv("Data/order_items.csv", col_types = cols(
  .default = col_character(), item_number = col_integer(), shipping_deadline = dt,
  item_price = col_double(), freight_cost = col_double()))
products <- read_csv("Data/products.csv", col_types = cols(
  .default = col_double(), product_id = col_character(), product_category = col_character()))
sellers <- read_csv("Data/sellers.csv", col_types = cols(.default = col_character()))
payments <- read_csv("Data/payments.csv", col_types = cols(
  .default = col_character(), payment_sequence = col_integer(),
  installments = col_integer(), payment_amount = col_double()))
feedback <- read_csv("Data/customer_feedback.csv", col_types = cols(
  .default = col_character(), rating = col_integer(), has_title = col_logical(),
  has_comment = col_logical(), comment_length = col_integer(),
  feedback_created_timestamp = dt, feedback_answered_timestamp = dt))

# ---- 2. Cleaning decisions at source-table level -----------------------------
# D3: remove the 60 exact duplicate item rows (payments match totals without them)
n_dup_items <- sum(duplicated(items))
items <- distinct(items)

# D8: harmonise category spellings (78 labels -> 73); missing category -> "UNKNOWN"
products <- products %>%
  mutate(category = product_category %>% toupper() %>%
           str_replace_all("[^A-Z]+", "_") %>% str_replace_all("_+", "_") %>%
           str_replace_all("^_|_$", "") %>%
           str_replace("^HOUSEHOLDGOODS$", "HOUSEHOLD_GOODS"),
         category = coalesce(category, "UNKNOWN"),
         # D9: implausible weights (0 g or > 100 kg) treated as missing
         weight_g_clean = if_else(weight_g > 0 & weight_g <= 100000, weight_g, NA_real_))

# ---- 3. Aggregate the 1:many tables to ONE ROW PER ORDER (D1) -------------------
# order_items (+ products, sellers) -> order level
items_order <- items %>%
  left_join(select(products, product_id, category, weight_g_clean), by = "product_id") %>%
  left_join(select(sellers, seller_id, seller_region = region), by = "seller_id") %>%
  group_by(order_id) %>%
  summarise(
    n_items        = n(),
    n_sellers      = n_distinct(seller_id),
    goods_value    = sum(item_price),
    freight_total  = sum(freight_cost),
    order_value    = goods_value + freight_total,
    freight_share  = freight_total / order_value,
    weight_kg      = sum(weight_g_clean) / 1000,          # NA if any item weight unknown
    main_category  = coalesce(category[which.max(item_price)], "UNKNOWN"),
    seller_regions = paste(sort(unique(seller_region)), collapse = "|"),
    n_seller_regions = n_distinct(seller_region),
    any_product_missing = any(!product_id %in% products$product_id)
  )

# payments -> order level: main method = method with the largest amount
payments_order <- payments %>%
  group_by(order_id) %>%
  summarise(payment_method = payment_method[which.max(payment_amount)],
            amount_paid    = sum(payment_amount))

# D4: feedback -> one rating per order = most recent feedback record
#     (mean rating and a conflict flag kept for robustness)
feedback_order <- feedback %>%
  arrange(order_id, desc(feedback_created_timestamp), desc(feedback_answered_timestamp)) %>%
  group_by(order_id) %>%
  summarise(n_feedback      = n(),
            rating_conflict = n_distinct(rating) > 1,
            rating_mean     = mean(rating),
            rating          = first(rating),
            feedback_date   = first(feedback_created_timestamp))

# ---- 4. Join everything to the order spine -------------------------------------
# D2: customers join 1:1 on customer_record_id (not customer_id)
orders_full <- orders %>%
  left_join(select(customers, customer_record_id, customer_id, customer_region = region),
            by = "customer_record_id") %>%
  inner_join(items_order,    by = "order_id") %>%
  left_join(payments_order,  by = "order_id") %>%
  left_join(feedback_order,  by = "order_id")
stopifnot(!any(duplicated(orders_full$order_id)))     # still one row per order

# ---- 5. Variable construction ----------------------------------------------------
orders_full <- orders_full %>%
  mutate(
    # outcomes
    delivery_days  = as.numeric(difftime(delivery_timestamp, purchase_timestamp, units = "days")),
    log_delivery_days = log(delivery_days),
    low_rating     = as.integer(rating <= 2),
    # variable of interest (D10): all sellers of the order are in the customer's region
    same_region    = as.integer(n_seller_regions == 1 & seller_regions == customer_region),
    # controls
    promised_days  = as.numeric(difftime(estimated_delivery_timestamp, purchase_timestamp, units = "days")),
    purchase_month = format(purchase_timestamp, "%Y-%m"),
    # D7: lateness compared by calendar date (estimate has no time component)
    days_late      = as.numeric(as.Date(delivery_timestamp) - as.Date(estimated_delivery_timestamp)),
    is_late        = as.integer(days_late > 0),
    # D5: flags kept for robustness checks (records are NOT dropped)
    flag_sequence_error = as.integer(
      (approval_timestamp < purchase_timestamp) %in% TRUE |
      (carrier_timestamp  < approval_timestamp) %in% TRUE |
      (carrier_timestamp  < purchase_timestamp) %in% TRUE |
      (delivery_timestamp < carrier_timestamp)  %in% TRUE),
    flag_feedback_before_purchase = as.integer(as.Date(feedback_date) < as.Date(purchase_timestamp)),
    flag_dominant_region = as.integer(customer_region == "REGION_ZAQW")
  )

# possible duplicate checkouts: same person, same second, same products (flag only)
dup_checkout <- items %>%
  left_join(select(orders, order_id, customer_record_id, purchase_timestamp), by = "order_id") %>%
  left_join(select(customers, customer_record_id, customer_id), by = "customer_record_id") %>%
  group_by(customer_id, purchase_timestamp, product_id) %>%
  filter(n_distinct(order_id) > 1) %>% ungroup() %>% distinct(order_id)
orders_full <- orders_full %>%
  mutate(flag_possible_duplicate = as.integer(order_id %in% dup_checkout$order_id))

# ---- 6. Analytical sample (D6) ----------------------------------------------------
sample_steps <- tibble(step = "All orders with items", orders = nrow(orders_full))
analysis <- orders_full %>% filter(order_status == "COMPLETED")
sample_steps <- add_row(sample_steps, step = "+ status COMPLETED", orders = nrow(analysis))
analysis <- analysis %>% filter(!is.na(delivery_timestamp), delivery_days > 0)
sample_steps <- add_row(sample_steps, step = "+ delivery date recorded", orders = nrow(analysis))
analysis <- analysis %>% filter(!is.na(rating))
sample_steps <- add_row(sample_steps, step = "+ customer rating available", orders = nrow(analysis))

analysis <- analysis %>%
  mutate(across(c(customer_region, main_category, payment_method, purchase_month), factor)) %>%
  select(order_id, customer_id, purchase_timestamp, purchase_month,
         delivery_days, log_delivery_days, rating, low_rating, rating_mean,   # outcomes
         same_region,                                                          # interest
         customer_region, weight_kg, freight_share, order_value, n_items,      # controls
         n_sellers, main_category, payment_method, promised_days,
         days_late, is_late,                                                   # descriptive
         flag_sequence_error, flag_possible_duplicate, flag_feedback_before_purchase,
         rating_conflict, flag_dominant_region, any_product_missing)           # robustness flags

# ---- 7. Checks and summary ---------------------------------------------------------
stopifnot(!any(duplicated(analysis$order_id)))
cat("\nDuplicate item rows removed:", n_dup_items, "\n\nSample construction:\n")
print(as.data.frame(sample_steps), row.names = FALSE)
cat("\nFinal analytical dataset:", nrow(analysis), "orders x", ncol(analysis), "variables\n\n")
print(analysis %>% group_by(same_region) %>%
        summarise(orders = n(), median_delivery_days = round(median(delivery_days), 1),
                  mean_rating = round(mean(rating), 2), share_low_rating = round(mean(low_rating), 3)) %>%
        as.data.frame(), row.names = FALSE)
cat("\nMissing values in model variables:\n")
print(colSums(is.na(select(analysis, delivery_days, rating, same_region, customer_region, weight_kg,
                           freight_share, order_value, main_category, payment_method, promised_days))))

# ---- 8. Save ------------------------------------------------------------------------
dir.create("output", showWarnings = FALSE)
write_csv(analysis, "output/analysis_orders.csv")
saveRDS(analysis, "output/analysis_orders.rds")
cat("\nSaved output/analysis_orders.csv and output/analysis_orders.rds\n")
