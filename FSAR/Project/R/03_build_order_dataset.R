# =============================================================================
# 03_build_order_dataset.R
# Builds ONE order-level dataset (one row per order_id) from the 7 tables and
# uploads it to SQL Server.
#
# 1:1 relations (orders - customers) are joined directly.
# 1:many relations (order -> items, order -> payments, order -> feedback) are
# collapsed to one row per order in two ways:
#   - numeric summaries  (counts, sums, min/max, chosen rating)
#   - grouped list columns, values joined with "|"  (e.g. product_ids = "P1|P2")
# Product and seller attributes are attached to each item BEFORE aggregating.
#
# Cleaning applied here (documented in the decision log):
#   - exact duplicate rows in order_items are removed (payments confirm they are errors)
#   - product_category spelling variants are harmonised (raw label kept in the list column)
# Everything else is NOT removed, only flagged with flag_* columns, so the
# analysis can decide later.
#
# Run from the Project.Rproj root. Packages: readr, dplyr, tidyr, stringr, DBI, odbc
# SQL connection: R/sql_connect.R (credentials in .Renviron).
# =============================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr)
})
options(dplyr.summarise.inform = FALSE)

UPLOAD_TO_SQL <- TRUE           # set FALSE to only build the data frame
sql_table     <- "order_level_dataset"

# ---- 1. Load ----------------------------------------------------------------
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

collapse_ids <- function(x) paste(x, collapse = "|")                     # keeps order & repeats
collapse_set <- function(x) paste(sort(unique(na.omit(x))), collapse = "|")  # distinct values
normalise_cat <- function(x) {
  x %>% toupper() %>% str_replace_all("[^A-Z]+", "_") %>% str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "") %>% str_replace("^HOUSEHOLDGOODS$", "HOUSEHOLD_GOODS")
}

# ---- 2. Item level: de-duplicate, then attach product + seller attributes ----
n_dup <- sum(duplicated(items))
items_clean <- items %>% distinct()

products_clean <- products %>%
  mutate(category = normalise_cat(product_category),
         volume_cm3 = length_cm * height_cm * width_cm)

items_enriched <- items_clean %>%
  left_join(products_clean, by = "product_id") %>%
  left_join(sellers %>% rename(seller_postal_area = postal_area, seller_city = city,
                               seller_region = region), by = "seller_id") %>%
  arrange(order_id, item_number)

# ---- 3. Collapse order_items (1:many) to one row per order -------------------
items_order <- items_enriched %>%
  group_by(order_id) %>%
  summarise(
    n_items             = n(),
    n_distinct_products = n_distinct(product_id),
    n_sellers           = n_distinct(seller_id),
    n_categories        = n_distinct(category, na.rm = TRUE),
    items_price_total   = sum(item_price),
    items_freight_total = sum(freight_cost),
    items_value_total   = items_price_total + items_freight_total,
    item_price_max      = max(item_price),
    freight_share       = items_freight_total / items_value_total,
    weight_g_total      = sum(weight_g),            # NA if any item lacks a weight
    volume_cm3_total    = sum(volume_cm3),
    photo_count_mean    = mean(photo_count, na.rm = TRUE),
    description_length_mean = mean(description_length, na.rm = TRUE),
    shipping_deadline_first = min(shipping_deadline),
    shipping_deadline_last  = max(shipping_deadline),
    # main category = category of the most expensive item line
    main_category       = category[which.max(item_price)],
    # grouped (transposed) list columns
    product_ids         = collapse_ids(product_id),
    seller_ids          = collapse_set(seller_id),
    categories          = collapse_set(category),
    categories_raw      = collapse_set(product_category),
    seller_regions      = collapse_set(seller_region),
    item_prices         = collapse_ids(item_price),
    # flags
    flag_item_product_missing   = any(!product_id %in% products$product_id),
    flag_item_category_missing  = any(is.na(category)),
    flag_item_weight_implausible = any(weight_g == 0 | weight_g > 100000, na.rm = TRUE),
    flag_had_duplicate_item_rows = FALSE
  )
items_order$flag_had_duplicate_item_rows <- items_order$order_id %in% items$order_id[duplicated(items)]

# ---- 4. Collapse payments (1:many) -------------------------------------------
payments_order <- payments %>%
  arrange(order_id, payment_sequence) %>%
  group_by(order_id) %>%
  summarise(
    n_payments          = n(),
    payment_total       = sum(payment_amount),
    payment_main_method = payment_method[which.max(payment_amount)],
    payment_methods     = collapse_set(payment_method),
    installments_max    = max(installments),
    payment_amounts     = collapse_ids(payment_amount),
    flag_payment_anomaly = any(installments == 0 | payment_amount == 0 | payment_method == "UNKNOWN")
  )

# ---- 5. Collapse feedback (1:many, and feedback_id can span several orders) --
# Rule: the order's rating = the most recent feedback record (by created, then answered);
# mean/min/max and a conflict flag are kept so the choice can be challenged later.
feedback_order <- feedback %>%
  arrange(order_id, desc(feedback_created_timestamp), desc(feedback_answered_timestamp)) %>%
  group_by(order_id) %>%
  summarise(
    n_feedback            = n(),
    flag_rating_conflict  = n_distinct(rating) > 1,
    rating_mean           = mean(rating),
    rating_min            = min(rating),
    rating_max            = max(rating),
    ratings_all           = collapse_ids(rating),
    has_title             = first(has_title),
    has_comment           = first(has_comment),
    comment_length        = first(comment_length),
    feedback_created      = first(feedback_created_timestamp),
    feedback_answered     = first(feedback_answered_timestamp),
    feedback_ids          = collapse_ids(feedback_id),
    rating                = first(rating)           # latest record (computed LAST: it overwrites `rating`)
  ) %>%
  relocate(rating, .after = n_feedback) %>%
  left_join(feedback %>% count(feedback_id, name = "orders_per_feedback") %>%
              right_join(distinct(feedback, feedback_id, order_id), by = "feedback_id") %>%
              group_by(order_id) %>% summarise(flag_feedback_shared = any(orders_per_feedback > 1)),
            by = "order_id")

# ---- 6. Order spine + customer (1:1) + everything else -----------------------
order_df <- orders %>%
  left_join(customers %>% rename(customer_postal_area = postal_area, customer_city = city,
                                 customer_region = region), by = "customer_record_id") %>%
  left_join(items_order,    by = "order_id") %>%
  left_join(payments_order, by = "order_id") %>%
  left_join(feedback_order, by = "order_id") %>%
  left_join(customers %>% count(customer_id, name = "customer_n_orders"), by = "customer_id") %>%
  mutate(
    # time variables
    purchase_month      = format(purchase_timestamp, "%Y-%m"),
    delivery_days       = as.numeric(difftime(delivery_timestamp, purchase_timestamp, units = "days")),
    promised_days       = as.numeric(difftime(estimated_delivery_timestamp, purchase_timestamp, units = "days")),
    days_late           = as.numeric(as.Date(delivery_timestamp) - as.Date(estimated_delivery_timestamp)),
    is_late             = days_late > 0,               # calendar-date comparison
    approval_hours      = as.numeric(difftime(approval_timestamp, purchase_timestamp, units = "hours")),
    handling_days       = as.numeric(difftime(carrier_timestamp, purchase_timestamp, units = "days")),
    transit_days        = as.numeric(difftime(delivery_timestamp, carrier_timestamp, units = "days")),
    feedback_days_after_delivery = as.numeric(as.Date(feedback_created) - as.Date(delivery_timestamp)),
    low_rating          = rating <= 2,
    same_region         = str_detect(seller_regions, fixed(customer_region)),   # customer region among seller regions
    payment_minus_due   = round(payment_total - items_value_total, 2),
    # availability flags
    has_items           = !is.na(n_items),
    has_payment         = !is.na(n_payments),
    has_feedback        = !is.na(n_feedback),
    # data-quality flags (not removed, only marked)
    flag_sequence_error = (carrier_timestamp < approval_timestamp) %in% TRUE |
                          (carrier_timestamp < purchase_timestamp) %in% TRUE |
                          (delivery_timestamp < carrier_timestamp) %in% TRUE,
    flag_status_conflict = (order_status == "COMPLETED" & is.na(delivery_timestamp)) |
                           (order_status == "CANCELLED" & !is.na(delivery_timestamp)),
    flag_payment_mismatch = (abs(payment_minus_due) > 1) %in% TRUE,
    flag_feedback_before_purchase = (as.Date(feedback_created) < as.Date(purchase_timestamp)) %in% TRUE,
    flag_sparse_month   = purchase_month %in% names(which(table(purchase_month) < 100)),
    flag_payment_10x    = (abs(payment_total / items_value_total - 10) < 0.01) %in% TRUE
  ) %>%
  relocate(order_id, customer_id, customer_record_id, order_status, purchase_month) %>%
  # means over items with no listing data give NaN; store them as missing (NA) instead
  mutate(across(where(is.double), ~ replace(.x, !is.finite(.x), NA)))

# ---- 7. Integrity checks -------------------------------------------------------
stopifnot(nrow(order_df) == nrow(orders), !any(duplicated(order_df$order_id)))
stopifnot(isTRUE(all.equal(sum(order_df$payment_total, na.rm = TRUE), sum(payments$payment_amount))))
stopifnot(isTRUE(all.equal(sum(order_df$items_price_total, na.rm = TRUE), sum(items_clean$item_price))))

cat(sprintf("\nOrder-level dataset: %s rows x %d columns (one row per order)\n",
            format(nrow(order_df), big.mark = ","), ncol(order_df)))
cat(sprintf("Duplicate item rows removed: %d | category labels harmonised: %d -> %d\n",
            n_dup, n_distinct(products$product_category, na.rm = TRUE),
            n_distinct(products_clean$category, na.rm = TRUE)))
cat("Totals check passed: payments and item prices sum to the source tables.\n\nFlags:\n")
print(order_df %>% summarise(across(starts_with("flag_"), ~ sum(.x, na.rm = TRUE))) %>%
        pivot_longer(everything(), names_to = "flag", values_to = "orders") %>% as.data.frame(),
      row.names = FALSE)

# ---- 8. Upload to SQL Server ---------------------------------------------------
if (UPLOAD_TO_SQL) {
  source("R/sql_connect.R")
  con <- sql_connect("AuroraMarket")
  types <- sql_types(order_df, short_text = c("order_id", "customer_id", "customer_record_id",
                                              "order_status", "purchase_month", "main_category",
                                              "payment_main_method"))
  dbWriteTable(con, sql_table, sql_safe(order_df), overwrite = TRUE, field.types = types)
  dbExecute(con, sprintf("ALTER TABLE [%s] ALTER COLUMN [order_id] nvarchar(64) NOT NULL;", sql_table))
  dbExecute(con, sprintf("ALTER TABLE [%s] ADD CONSTRAINT [PK_%s] PRIMARY KEY ([order_id]);", sql_table, sql_table))
  n_sql <- dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM [%s]", sql_table))$n
  message(sprintf("Uploaded [AuroraMarket].[dbo].[%s]: %s rows (expected %s) %s", sql_table,
                  format(n_sql, big.mark = ","), format(nrow(order_df), big.mark = ","),
                  if (n_sql == nrow(order_df)) "OK" else "MISMATCH"))
  dbDisconnect(con)
}
