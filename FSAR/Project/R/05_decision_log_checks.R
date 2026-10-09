# =============================================================================
# 05_decision_log_checks.R
# FSAR 2026 - Group H - Evidence behind the Initial Data Decision Log (D1-D10)
# Each block reproduces the numbers quoted in Data_Decision_Log.xlsx.
# Run from the Project.Rproj root. Packages: readr, dplyr, stringr
# =============================================================================
suppressPackageStartupMessages({ library(readr); library(dplyr); library(stringr) })
options(dplyr.summarise.inform = FALSE)

customers <- read_csv("Data/customers.csv", show_col_types = FALSE)
orders    <- read_csv("Data/orders.csv", show_col_types = FALSE)
items     <- read_csv("Data/order_items.csv", show_col_types = FALSE)
products  <- read_csv("Data/products.csv", show_col_types = FALSE)
sellers   <- read_csv("Data/sellers.csv", show_col_types = FALSE)
payments  <- read_csv("Data/payments.csv", show_col_types = FALSE)
feedback  <- read_csv("Data/customer_feedback.csv", show_col_types = FALSE)
say <- function(...) cat(sprintf(...), "\n")

# ---- D1: rows per order in the 1:many tables, and join fan-out -----------------
cat("\n== D1 ==\n")
say("items per order:    %s", paste(range(count(items, order_id)$n), collapse = "-"))
say("payments per order: %s", paste(range(count(payments, order_id)$n), collapse = "-"))
say("feedback per order: %s", paste(range(count(feedback, order_id)$n), collapse = "-"))
naive <- orders %>%
  left_join(items, by = "order_id", relationship = "many-to-many") %>%
  left_join(payments, by = "order_id", relationship = "many-to-many") %>%
  left_join(feedback, by = "order_id", relationship = "many-to-many")
say("naive join rows: %d (orders: %d); payment total inflated by %.1f%%", nrow(naive), nrow(orders),
    100 * (sum(naive$payment_amount, na.rm = TRUE) / sum(payments$payment_amount) - 1))

# ---- D2: uniqueness of customer identifiers --------------------------------------
cat("\n== D2 ==\n")
say("rows %d | distinct customer_record_id %d | distinct customer_id %d", nrow(customers),
    n_distinct(customers$customer_record_id), n_distinct(customers$customer_id))
rec <- count(customers, customer_id)
say("people with >1 record: %d (max %d)", sum(rec$n > 1), max(rec$n))
say("customer_record_id unique in orders: %s", !any(duplicated(orders$customer_record_id)))

# ---- D3: duplicate item rows ---------------------------------------------------------
cat("\n== D3 ==\n")
say("rows %d | distinct order_id+item_number %d | exact duplicate rows %d",
    nrow(items), nrow(distinct(items, order_id, item_number)), sum(duplicated(items)))
dup_orders <- items %>% filter(duplicated(items)) %>% distinct(order_id)
paid <- payments %>% group_by(order_id) %>% summarise(paid = sum(payment_amount))
raw   <- items %>% semi_join(dup_orders, by = "order_id") %>% group_by(order_id) %>%
  summarise(raw = sum(item_price + freight_cost))
dedup <- items %>% distinct() %>% semi_join(dup_orders, by = "order_id") %>% group_by(order_id) %>%
  summarise(dedup = sum(item_price + freight_cost))
chk <- raw %>% left_join(dedup, by = "order_id") %>% left_join(paid, by = "order_id")
say("orders with duplicates: %d | payment matches de-duplicated: %d | matches raw: %d",
    nrow(chk), sum(abs(chk$paid - chk$dedup) <= 1), sum(abs(chk$paid - chk$raw) <= 1))

# ---- D4: feedback per order ------------------------------------------------------------
cat("\n== D4 ==\n")
say("feedback_ids on >1 row: %d", sum(count(feedback, feedback_id)$n > 1))
fo <- feedback %>% group_by(order_id) %>% summarise(n = n(), ratings = n_distinct(rating))
say("orders with >1 feedback: %d | with conflicting ratings: %d", sum(fo$n > 1), sum(fo$ratings > 1))

# ---- D5: timestamp sequence -----------------------------------------------------------
cat("\n== D5 ==\n")
seq_chk <- orders %>% transmute(
  carrier_before_approval  = carrier_timestamp  < approval_timestamp,
  carrier_before_purchase  = carrier_timestamp  < purchase_timestamp,
  delivery_before_carrier  = delivery_timestamp < carrier_timestamp,
  approval_before_purchase = approval_timestamp < purchase_timestamp)
print(colSums(seq_chk, na.rm = TRUE))
say("orders breaking any rule: %d", sum(rowSums(seq_chk, na.rm = TRUE) > 0))
cba <- orders %>% filter(carrier_timestamp < approval_timestamp)
say("share of carrier-before-approval in May/Jul/Aug 2024: %.0f%%",
    100 * mean(format(cba$carrier_timestamp, "%Y-%m") %in% c("2024-05", "2024-07", "2024-08")))

# ---- D6: missingness by status and sample size -----------------------------------------
cat("\n== D6 ==\n")
print(orders %>% group_by(order_status) %>%
        summarise(n = n(), no_approval = sum(is.na(approval_timestamp)),
                  no_delivery = sum(is.na(delivery_timestamp))) %>% as.data.frame(), row.names = FALSE)
say("CANCELLED with delivery date: %d | COMPLETED without: %d",
    sum(orders$order_status == "CANCELLED" & !is.na(orders$delivery_timestamp)),
    sum(orders$order_status == "COMPLETED" & is.na(orders$delivery_timestamp)))

# ---- D7: estimated delivery is a date ---------------------------------------------------
cat("\n== D7 ==\n")
print(table(format(orders$estimated_delivery_timestamp, "%H:%M:%S")))
d <- filter(orders, !is.na(delivery_timestamp))
say("late by timestamp %.1f%% | late by date %.1f%%",
    100 * mean(d$delivery_timestamp > d$estimated_delivery_timestamp),
    100 * mean(as.Date(d$delivery_timestamp) > as.Date(d$estimated_delivery_timestamp)))

# ---- D8: category labels and missing listing info ----------------------------------------
cat("\n== D8 ==\n")
norm <- products$product_category %>% toupper() %>% str_replace_all("[^A-Z]+", "_") %>%
  str_replace_all("_+", "_") %>% str_replace_all("^_|_$", "") %>% str_replace("^HOUSEHOLDGOODS$", "HOUSEHOLD_GOODS")
say("raw labels %d | harmonised %d | missing category %d",
    n_distinct(products$product_category, na.rm = TRUE), n_distinct(norm, na.rm = TRUE), sum(is.na(products$product_category)))
say("same products missing category AND description: %s",
    all(is.na(products$product_category) == is.na(products$description_length)))

# ---- D9: implausible weights -------------------------------------------------------------
cat("\n== D9 ==\n")
say("weight = 0: %d | weight > 100 kg: %d | max %s g | p99.9 %s g",
    sum(products$weight_g == 0, na.rm = TRUE), sum(products$weight_g > 100000, na.rm = TRUE),
    format(max(products$weight_g, na.rm = TRUE), big.mark = ","), quantile(products$weight_g, .999, na.rm = TRUE))

# ---- D10: multi-seller orders and regional concentration -----------------------------------
cat("\n== D10 ==\n")
say("orders with >1 seller: %d", sum(distinct(items, order_id, seller_id) %>% count(order_id) %>% pull(n) > 1))
top <- names(sort(table(customers$region), decreasing = TRUE))[1]
say("top region %s: %.0f%% of customer records, %.0f%% of sellers", top,
    100 * mean(customers$region == top), 100 * mean(sellers$region == top))
