# =============================================================================
# 02_data_familiarization.R
# FSAR 2026 - Group H - Scoping Brief, section 1.1 "Data Familiarization"
#
# Explores all seven Aurora Market tables in depth:
#   dimensions & unit of observation, identifiers & keys, relationships,
#   missing values, duplicates, unusual numeric/categorical values,
#   and other consistency issues that could affect later analysis.
#
# It ONLY explores. It does not modify or clean any data.
# Run from the Project.Rproj root (relative paths).
# Packages: readr, dplyr, tidyr, stringr
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
})
options(width = 120, dplyr.summarise.inform = FALSE, pillar.sigfig = 6)

# ---- Printing helpers --------------------------------------------------------
header <- function(x) {
  cat("\n\n", strrep("=", 90), "\n", "  ", toupper(x), "\n", strrep("=", 90), "\n", sep = "")
}
section <- function(x) cat("\n--- ", x, " ", strrep("-", max(3, 84 - nchar(x))), "\n", sep = "")
say  <- function(...) cat("  ", sprintf(...), "\n", sep = "")
flag <- function(...) cat("  >> ", sprintf(...), "\n", sep = "")   # notable finding
show <- function(df, n = 50) {
  df <- as.data.frame(df)
  if (nrow(df) > n) { print(head(df, n), row.names = FALSE); say("... %d more rows not shown", nrow(df) - n) }
  else print(df, row.names = FALSE)
}
fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
pct <- function(a, b) sprintf("%.2f%%", 100 * a / b)

# Column-level profile: type, missing, distinct, example
profile_columns <- function(df) {
  tibble(
    column     = names(df),
    type       = vapply(df, function(x) class(x)[1], character(1)),
    n_missing  = vapply(df, function(x) sum(is.na(x)), integer(1)),
    pct_miss   = round(100 * n_missing / nrow(df), 2),
    n_distinct = vapply(df, function(x) n_distinct(x, na.rm = TRUE), integer(1)),
    example    = vapply(df, function(x) as.character(x[which(!is.na(x))[1]]), character(1))
  )
}

# Numeric summary with tails, zeros and negatives
numeric_summary <- function(df) {
  df %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything(), names_to = "variable") %>%
    group_by(variable) %>%
    summarise(
      n_na   = sum(is.na(value)),
      min    = min(value, na.rm = TRUE),
      p01    = quantile(value, .01, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      mean   = round(mean(value, na.rm = TRUE), 2),
      p99    = quantile(value, .99, na.rm = TRUE),
      max    = max(value, na.rm = TRUE),
      n_zero = sum(value == 0, na.rm = TRUE),
      n_neg  = sum(value < 0, na.rm = TRUE)
    )
}

# Frequency table with percentages
freq <- function(df, ...) {
  df %>% count(..., sort = TRUE) %>% mutate(pct = round(100 * n / sum(n), 2))
}

# Key check: is a (possibly composite) key unique?
check_key <- function(df, cols, label = paste(cols, collapse = " + ")) {
  n_keys <- nrow(distinct(df, across(all_of(cols))))
  dups   <- nrow(df) - n_keys
  if (dups == 0) say("Key [%s]: UNIQUE (%s rows = %s distinct)", label, fmt(nrow(df)), fmt(n_keys))
  else flag("Key [%s]: NOT unique - %s distinct values for %s rows (%s extra rows)",
            label, fmt(n_keys), fmt(nrow(df)), fmt(dups))
  invisible(dups)
}

# Referential integrity: child[col] -> parent[col]
check_fk <- function(child, child_name, parent, parent_name, by) {
  miss_rows <- sum(!child[[by]] %in% parent[[by]])
  miss_keys <- n_distinct(child[[by]][!child[[by]] %in% parent[[by]]])
  orphan_parents <- sum(!unique(parent[[by]]) %in% child[[by]])
  tibble(relationship = sprintf("%s.%s -> %s", child_name, by, parent_name),
         child_rows = nrow(child), unmatched_rows = miss_rows,
         unmatched_keys = miss_keys, parents_without_child = orphan_parents)
}

findings <- list()   # collected for the summary at the end

# =============================================================================
# 0. LOAD DATA (explicit column types; timestamps parsed as UTC date-times)
# =============================================================================
header("0. Loading data")

dt <- col_datetime(format = "")   # ISO-8601 with trailing Z -> POSIXct UTC
spec <- list(
  customers = cols(.default = col_character()),
  orders = cols(.default = col_character(),
                purchase_timestamp = dt, approval_timestamp = dt, carrier_timestamp = dt,
                delivery_timestamp = dt, estimated_delivery_timestamp = dt),
  order_items = cols(.default = col_character(), item_number = col_integer(),
                     shipping_deadline = dt, item_price = col_double(), freight_cost = col_double()),
  products = cols(.default = col_double(), product_id = col_character(),
                  product_category = col_character()),
  sellers = cols(.default = col_character()),
  payments = cols(.default = col_character(), payment_sequence = col_integer(),
                  installments = col_integer(), payment_amount = col_double()),
  customer_feedback = cols(.default = col_character(), rating = col_integer(),
                           has_title = col_logical(), has_comment = col_logical(),
                           comment_length = col_integer(),
                           feedback_created_timestamp = dt, feedback_answered_timestamp = dt)
)

d <- lapply(names(spec), function(t) read_csv(file.path("Data", paste0(t, ".csv")),
                                              col_types = spec[[t]], na = c("", "NA")))
names(d) <- names(spec)

parse_issues <- vapply(d, function(x) nrow(problems(x)), integer(1))
say("Parsing problems per table (0 = every value matched its declared type):")
show(tibble(table = names(parse_issues), parsing_problems = parse_issues))

customers <- d$customers; orders <- d$orders; items <- d$order_items
products <- d$products; sellers <- d$sellers; payments <- d$payments
feedback <- d$customer_feedback

# =============================================================================
# 1. OVERVIEW OF ALL TABLES
# =============================================================================
header("1. Overview of all seven tables")

overview <- tibble(
  table = names(d),
  rows = vapply(d, nrow, integer(1)),
  columns = vapply(d, ncol, integer(1)),
  cells_missing = vapply(d, function(x) sum(is.na(x)), integer(1)),
  exact_duplicate_rows = vapply(d, function(x) sum(duplicated(x)), integer(1)),
  intended_unit = c("customer record (one per order)", "order", "item line within an order",
                    "product listing", "seller", "payment transaction within an order",
                    "feedback record")
)
show(overview)

# =============================================================================
# 2. CUSTOMERS
# =============================================================================
header("2. customers")

section("2.1 Structure")
show(profile_columns(customers))

section("2.2 Identifiers")
check_key(customers, "customer_record_id")
dup_cust <- check_key(customers, "customer_id")
rec_per_cust <- customers %>% count(customer_id, name = "records")
say("Records per customer_id:")
show(rec_per_cust %>% count(records, name = "customers") %>% mutate(pct = round(100 * customers / sum(customers), 2)))
n_multi <- sum(rec_per_cust$records > 1)
flag("%s customers (%s) have more than one record; max = %d records for one customer",
     fmt(n_multi), pct(n_multi, nrow(rec_per_cust)), max(rec_per_cust$records))
findings$customers <- sprintf(
  "customer_record_id is unique, but customer_id is not: %s people hold %s records; %s (%s) have >1 record.",
  fmt(nrow(rec_per_cust)), fmt(nrow(customers)), fmt(n_multi), pct(n_multi, nrow(rec_per_cust)))

section("2.3 Location consistency for repeat customers")
loc <- customers %>%
  semi_join(filter(rec_per_cust, records > 1), by = "customer_id") %>%
  group_by(customer_id) %>%
  summarise(postal = n_distinct(postal_area), city = n_distinct(city), region = n_distinct(region))
say("Of %s multi-record customers: %s change postal_area, %s change city, %s change region across records",
    fmt(nrow(loc)), fmt(sum(loc$postal > 1)), fmt(sum(loc$city > 1)), fmt(sum(loc$region > 1)))
say("-> location is stored per record (likely the delivery address of that order), not per person.")

section("2.4 Geography hierarchy")
pa_city <- customers %>% distinct(postal_area, city) %>% count(postal_area) %>% filter(n > 1)
city_reg <- customers %>% distinct(city, region) %>% count(city) %>% filter(n > 1)
say("postal_area values: %s | city values: %s | region values: %s",
    fmt(n_distinct(customers$postal_area)), fmt(n_distinct(customers$city)), n_distinct(customers$region))
say("postal_areas mapping to >1 city: %d", nrow(pa_city))
if (nrow(city_reg) > 0) flag("%d city identifiers appear in more than one region (ambiguous city codes or data errors)", nrow(city_reg))

section("2.5 Region distribution (customer records)")
show(freq(customers, region))
top_reg <- freq(customers, region) %>% slice(1)
flag("Highly concentrated: %s alone holds %s%% of customer records", top_reg$region, top_reg$pct)

# =============================================================================
# 3. ORDERS
# =============================================================================
header("3. orders")

section("3.1 Structure")
show(profile_columns(orders))

section("3.2 Identifiers")
check_key(orders, "order_id")
check_key(orders, "customer_record_id")
say("-> each customer_record_id appears on exactly one order (1:1 with customers).")

section("3.3 Order status")
show(freq(orders, order_status))
say("Non-COMPLETED orders: %s (%s)", fmt(sum(orders$order_status != "COMPLETED")),
    pct(sum(orders$order_status != "COMPLETED"), nrow(orders)))

section("3.4 Missing timestamps by status (missingness is structured, not random)")
miss_status <- orders %>%
  group_by(order_status) %>%
  summarise(n = n(),
            no_approval = sum(is.na(approval_timestamp)),
            no_carrier = sum(is.na(carrier_timestamp)),
            no_delivery = sum(is.na(delivery_timestamp)),
            no_estimate = sum(is.na(estimated_delivery_timestamp))) %>%
  arrange(desc(n))
show(miss_status)
cmp_no_del <- sum(orders$order_status == "COMPLETED" & is.na(orders$delivery_timestamp))
canc_del   <- sum(orders$order_status == "CANCELLED" & !is.na(orders$delivery_timestamp))
flag("%d COMPLETED orders have no delivery timestamp; %d CANCELLED orders do have one", cmp_no_del, canc_del)

section("3.5 Time coverage")
say("Purchases from %s to %s", format(min(orders$purchase_timestamp)), format(max(orders$purchase_timestamp)))
monthly <- orders %>% count(month = format(purchase_timestamp, "%Y-%m"))
all_months <- format(seq(as.Date(paste0(min(monthly$month), "-01")),
                         as.Date(paste0(max(monthly$month), "-01")), by = "month"), "%Y-%m")
monthly <- tibble(month = all_months) %>% left_join(monthly, by = "month") %>% mutate(n = coalesce(n, 0L))
show(monthly, n = 40)
thin <- monthly %>% filter(n < 100)
flag("Thin/empty months (<100 orders): %s", paste0(thin$month, " (", thin$n, ")", collapse = ", "))
say("-> the series starts with a sparse ramp-up and ends abruptly; edge months are not comparable.")

section("3.6 Chronological consistency of the order lifecycle")
seq_checks <- tibble(
  check = c("approval before purchase", "carrier handover before approval",
            "carrier handover before purchase", "delivery before carrier handover",
            "delivery before purchase", "estimated delivery before purchase"),
  n_orders = c(sum(orders$approval_timestamp < orders$purchase_timestamp, na.rm = TRUE),
               sum(orders$carrier_timestamp < orders$approval_timestamp, na.rm = TRUE),
               sum(orders$carrier_timestamp < orders$purchase_timestamp, na.rm = TRUE),
               sum(orders$delivery_timestamp < orders$carrier_timestamp, na.rm = TRUE),
               sum(orders$delivery_timestamp < orders$purchase_timestamp, na.rm = TRUE),
               sum(orders$estimated_delivery_timestamp < orders$purchase_timestamp, na.rm = TRUE)))
show(seq_checks)
findings$order_sequence <- sprintf(
  "Lifecycle timestamps are not always in order: %s orders reach the carrier before approval, %s before purchase, %s are delivered before carrier handover.",
  fmt(seq_checks$n_orders[2]), fmt(seq_checks$n_orders[3]), fmt(seq_checks$n_orders[4]))

section("3.7 Delivery performance (orders with a delivery timestamp)")
del <- orders %>%
  filter(!is.na(delivery_timestamp)) %>%
  mutate(delivery_days = as.numeric(difftime(delivery_timestamp, purchase_timestamp, units = "days")),
         promised_days = as.numeric(difftime(estimated_delivery_timestamp, purchase_timestamp, units = "days")),
         days_vs_estimate = as.numeric(difftime(delivery_timestamp, estimated_delivery_timestamp, units = "days")),
         late = days_vs_estimate > 0)
show(numeric_summary(select(del, delivery_days, promised_days, days_vs_estimate)))
say("Late deliveries (delivered after estimate): %s of %s (%s)",
    fmt(sum(del$late)), fmt(nrow(del)), pct(sum(del$late), nrow(del)))
say("Deliveries taking > 60 days: %s", fmt(sum(del$delivery_days > 60)))
est_time <- table(format(orders$estimated_delivery_timestamp, "%H:%M:%S"))
say("Estimated delivery time-of-day values: %s -> the estimate is a DATE, the actual delivery is a precise time",
    paste(names(est_time), collapse = ", "))
say("-> 'late' should compare dates, otherwise same-day deliveries after 00:00 count as late.")
late_by_date <- mean(as.Date(del$delivery_timestamp) > as.Date(del$estimated_delivery_timestamp))
say("Late share using calendar dates instead: %.2f%%", 100 * late_by_date)
findings$delivery <- sprintf(
  "Of %s delivered orders, median delivery time is %.1f days and %.1f%% arrive after the estimated date; the estimate has no time component.",
  fmt(nrow(del)), median(del$delivery_days), 100 * late_by_date)

# =============================================================================
# 4. ORDER ITEMS
# =============================================================================
header("4. order_items")

section("4.1 Structure")
show(profile_columns(items))

section("4.2 Identifiers and duplicates")
check_key(items, c("order_id", "item_number"))
n_exact <- sum(duplicated(items))
flag("Exact duplicate rows (every column identical): %d", n_exact)
dup_keys <- items %>% group_by(order_id, item_number) %>% filter(n() > 1) %>% ungroup()
say("Every duplicated (order_id, item_number) pair is a fully identical row: %s",
    nrow(dup_keys) == 2 * n_exact && n_exact == nrow(items) - nrow(distinct(items, order_id, item_number)))
say("Example duplicated item line:")
show(head(dup_keys, 2))
findings$item_dups <- sprintf(
  "order_items has %d exact duplicate rows (same order, item number, product, price); payments match the de-duplicated totals, so they are recording errors to remove.", n_exact)

section("4.3 Item numbering")
numbering <- items %>% distinct(order_id, item_number) %>%
  group_by(order_id) %>% summarise(n = n(), max_no = max(item_number), min_no = min(item_number))
say("Orders whose item numbers are not 1..n (gaps): %d", sum(numbering$max_no != numbering$n | numbering$min_no != 1))

section("4.4 Order composition")
per_order <- items %>% distinct() %>% group_by(order_id) %>%
  summarise(items = n(), products = n_distinct(product_id), sellers = n_distinct(seller_id))
say("Items per order:"); show(freq(per_order, items) %>% arrange(items), n = 25)
say("Orders with >1 seller: %s (%s)", fmt(sum(per_order$sellers > 1)), pct(sum(per_order$sellers > 1), nrow(per_order)))
say("Orders with the same product repeated as several item lines: %s",
    fmt(sum(per_order$items > per_order$products)))
say("-> item_number counts units; quantity is represented by repeated lines, not a quantity column.")

section("4.5 Prices and freight")
show(numeric_summary(select(items, item_price, freight_cost)))
say("Free freight (freight_cost = 0): %s item lines", fmt(sum(items$freight_cost == 0)))
say("Freight greater than the item price: %s item lines (%s)",
    fmt(sum(items$freight_cost > items$item_price)), pct(sum(items$freight_cost > items$item_price), nrow(items)))
say("Most expensive item lines:")
show(items %>% arrange(desc(item_price)) %>% select(order_id, product_id, item_price, freight_cost) %>% head(5))
say("-> prices are strongly right-skewed (mean >> median); consider logs or robust statistics.")

section("4.6 Shipping deadline vs purchase date")
sd <- items %>% distinct() %>% left_join(select(orders, order_id, purchase_timestamp), by = "order_id") %>%
  mutate(deadline_days = as.numeric(difftime(shipping_deadline, purchase_timestamp, units = "days")))
show(numeric_summary(select(sd, deadline_days)))
flag("Deadlines more than 30 days after purchase: %s item lines (max %.0f days)",
     fmt(sum(sd$deadline_days > 30)), max(sd$deadline_days))

# =============================================================================
# 5. PRODUCTS
# =============================================================================
header("5. products")

section("5.1 Structure")
show(profile_columns(products))

section("5.2 Identifiers")
check_key(products, "product_id")

section("5.3 Missing-value pattern")
miss_pattern <- products %>%
  transmute(category_missing = is.na(product_category),
            listing_info_missing = is.na(name_length) & is.na(description_length) & is.na(photo_count),
            dimensions_missing = is.na(weight_g) & is.na(length_cm) & is.na(height_cm) & is.na(width_cm)) %>%
  count(category_missing, listing_info_missing, dimensions_missing)
show(miss_pattern)
say("-> category, name length, description length and photo count are missing TOGETHER (same %d products):",
    sum(is.na(products$product_category)))
say("   one missing block, probably listings created without metadata, not four independent problems.")

section("5.4 Product category: inconsistent spellings")
cat_raw <- products %>% filter(!is.na(product_category)) %>% distinct(product_category)
normalise_cat <- function(x) {
  x %>% toupper() %>% str_replace_all("[^A-Z]+", "_") %>% str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "") %>% str_replace("^HOUSEHOLDGOODS$", "HOUSEHOLD_GOODS")
}
cat_map <- cat_raw %>% mutate(normalised = normalise_cat(product_category))
variants <- cat_map %>% group_by(normalised) %>% filter(n() > 1) %>% arrange(normalised)
say("Raw category labels: %d -> after normalising case/separators: %d",
    nrow(cat_raw), n_distinct(cat_map$normalised))
flag("Labels that are the same category spelled differently:")
show(variants %>% left_join(count(products, product_category, name = "products"), by = "product_category"))
findings$categories <- sprintf(
  "product_category has %d raw labels but only %d real categories: case, separator and spelling variants (e.g. 'SPORTS AND RECREATION') must be harmonised; %d products have no category.",
  nrow(cat_raw), n_distinct(cat_map$normalised), sum(is.na(products$product_category)))

section("5.5 Category distribution (after normalising)")
show(products %>% mutate(category = normalise_cat(product_category)) %>% freq(category), n = 15)

section("5.6 Numeric attributes")
show(numeric_summary(select(products, -product_id, -product_category)))
flag("Products with weight_g = 0: %d", sum(products$weight_g == 0, na.rm = TRUE))
say("Heaviest products (check against their size):")
show(products %>% arrange(desc(weight_g)) %>%
       mutate(volume_l = length_cm * height_cm * width_cm / 1000) %>%
       select(product_id, product_category, weight_g, length_cm, height_cm, width_cm, volume_l) %>% head(8))
big <- sum(products$weight_g > 100000, na.rm = TRUE)
flag("%d products weigh > 100 kg while measuring at most ~55 cm per side; 9,375,000 g = 9.4 tonnes",
     big)
say("-> likely unit/entry errors (grams vs mg?) rather than real values. p99.9 is only %s g.",
    fmt(quantile(products$weight_g, .999, na.rm = TRUE)))
findings$weights <- sprintf(
  "weight_g contains %d zero weights and %d implausible values above 100 kg for small boxes (max 9.4 t).",
  sum(products$weight_g == 0, na.rm = TRUE), big)

# =============================================================================
# 6. SELLERS
# =============================================================================
header("6. sellers")

section("6.1 Structure")
show(profile_columns(sellers))

section("6.2 Identifiers")
check_key(sellers, "seller_id")

section("6.3 Region distribution")
show(freq(sellers, region))
say("Seller regions: %d (all of them also appear among customer regions: %s); customer regions: %d",
    n_distinct(sellers$region), all(unique(sellers$region) %in% customers$region), n_distinct(customers$region))

section("6.4 Seller activity (from order_items)")
seller_act <- items %>% distinct() %>% group_by(seller_id) %>%
  summarise(item_lines = n(), orders = n_distinct(order_id), revenue = sum(item_price))
show(numeric_summary(select(seller_act, item_lines, orders, revenue)))
top10 <- seller_act %>% arrange(desc(revenue)) %>% slice(1:ceiling(n() * 0.1))
say("Top 10%% of sellers account for %.1f%% of item revenue", 100 * sum(top10$revenue) / sum(seller_act$revenue))
say("Sellers with a single order: %s (%s)", fmt(sum(seller_act$orders == 1)),
    pct(sum(seller_act$orders == 1), nrow(seller_act)))
say("-> very long tail: seller-level estimates for small sellers will be noisy.")

# =============================================================================
# 7. PAYMENTS
# =============================================================================
header("7. payments")

section("7.1 Structure")
show(profile_columns(payments))

section("7.2 Identifiers")
check_key(payments, "order_id")
check_key(payments, c("order_id", "payment_sequence"))
pay_per_order <- payments %>% count(order_id, name = "payments")
say("Payments per order:"); show(freq(pay_per_order, payments) %>% arrange(payments), n = 15)
say("-> an order can be split over several payments (e.g. vouchers + card): payments is NOT one row per order.")

section("7.3 Payment method")
show(freq(payments, payment_method))

section("7.4 Instalments and amounts")
show(numeric_summary(select(payments, installments, payment_amount)))
show(payments %>% group_by(payment_method) %>%
       summarise(n = n(), min_inst = min(installments), mean_inst = round(mean(installments), 2),
                 max_inst = max(installments), zero_amount = sum(payment_amount == 0)))
flag("installments = 0 on %d payments (should be >= 1)", sum(payments$installments == 0))
flag("payment_amount = 0 on %d payments; %d payments have method UNKNOWN",
     sum(payments$payment_amount == 0), sum(payments$payment_method == "UNKNOWN"))
say("-> only CARD uses instalments; all other methods are always 1.")

section("7.5 Do payments add up to item price + freight?")
paid <- payments %>% group_by(order_id) %>% summarise(paid = sum(payment_amount))
due  <- items %>% distinct() %>% group_by(order_id) %>% summarise(due = sum(item_price + freight_cost))
due_raw <- items %>% group_by(order_id) %>% summarise(due_raw = sum(item_price + freight_cost))
recon <- inner_join(paid, due, by = "order_id") %>% left_join(due_raw, by = "order_id") %>%
  mutate(diff = round(paid - due, 2), diff_raw = round(paid - due_raw, 2))
say("Orders compared: %s", fmt(nrow(recon)))
say("Match within 1 currency unit (items de-duplicated): %s", pct(sum(abs(recon$diff) <= 1), nrow(recon)))
say("Match within 1 currency unit (raw items, duplicates kept): %s", pct(sum(abs(recon$diff_raw) <= 1), nrow(recon)))
say("Mismatches > 1 unit after de-duplication: %s orders (paid more: %s, paid less: %s)",
    fmt(sum(abs(recon$diff) > 1)), fmt(sum(recon$diff > 1)), fmt(sum(recon$diff < -1)))

say("Evidence on the duplicated item rows: for the orders that contain them, which total does the payment match?")
dup_orders <- items %>% filter(duplicated(items)) %>% distinct(order_id)
rd <- recon %>% semi_join(dup_orders, by = "order_id")
show(tibble(orders_with_duplicate_rows = nrow(rd),
            payment_matches_deduplicated = sum(abs(rd$diff) <= 1),
            payment_matches_raw = sum(abs(rd$diff_raw) <= 1)))
say("-> payments match the de-duplicated total: the duplicate rows are recording errors, not real extra units.")

say("Size of the remaining mismatches (ratio paid / due):")
mism <- recon %>% filter(abs(diff) > 1) %>% mutate(ratio = paid / due) %>%
  left_join(payments %>% group_by(order_id) %>%
              summarise(max_installments = max(installments),
                        methods = paste(sort(unique(payment_method)), collapse = "+")), by = "order_id")
show(mism %>% mutate(ratio_band = cut(ratio, c(0, 0.9, 0.99, 1.1, 2, 9.99, 10.01, Inf),
                                      labels = c("<0.90", "0.90-0.99", "1.01-1.10", "1.10-2", "2-9.99",
                                                 "exactly 10x", ">10x"))) %>%
       group_by(ratio_band) %>%
       summarise(orders = n(), share_card_with_instalments = round(mean(max_installments > 1), 2)))
ten_x <- sum(abs(mism$ratio - 10) < 0.01)
flag("%d orders were paid EXACTLY 10x the item+freight total -> likely a decimal-point error", ten_x)
show(mism %>% filter(abs(ratio - 10) < 0.01) %>% select(order_id, paid, due, ratio, methods) %>% head(5))
say("-> overpayments of 1-100%% occur almost only on card payments with instalments: consistent with")
say("   instalment interest, i.e. probably legitimate rather than errors.")
findings$payments <- sprintf(
  "payments has %s rows for %s orders (split payments). Paid totals match item+freight for %s of orders once duplicate item rows are removed; %d orders were paid exactly 10x the amount due (likely decimal errors).",
  fmt(nrow(payments)), fmt(n_distinct(payments$order_id)), pct(sum(abs(recon$diff) <= 1), nrow(recon)), ten_x)

# =============================================================================
# 8. CUSTOMER FEEDBACK
# =============================================================================
header("8. customer_feedback")

section("8.1 Structure")
show(profile_columns(feedback))

section("8.2 Identifiers: neither feedback_id nor order_id is unique")
check_key(feedback, "feedback_id")
check_key(feedback, "order_id")
check_key(feedback, c("feedback_id", "order_id"))
fid_multi <- feedback %>% group_by(feedback_id) %>% filter(n() > 1) %>%
  summarise(rows = n(), orders = n_distinct(order_id), ratings = n_distinct(rating))
say("feedback_ids used more than once: %d - they always point to different orders (%s) and carry the same rating (%s)",
    nrow(fid_multi), all(fid_multi$orders == fid_multi$rows), all(fid_multi$ratings == 1))
say("-> one feedback can cover several orders (e.g. a customer reviewing multiple orders at once).")
oid_multi <- feedback %>% group_by(order_id) %>% filter(n() > 1) %>%
  summarise(rows = n(), ratings = n_distinct(rating))
flag("Orders with more than one feedback record: %d; of those, %d have CONFLICTING ratings",
     nrow(oid_multi), sum(oid_multi$ratings > 1))
findings$feedback_keys <- sprintf(
  "Feedback is many-to-many with orders: %d feedback_ids cover several orders, and %d orders have several feedback records (%d with conflicting ratings). Need a rule (e.g. latest record) before joining.",
  nrow(fid_multi), nrow(oid_multi), sum(oid_multi$ratings > 1))

section("8.3 Rating distribution")
show(freq(feedback, rating) %>% arrange(rating))
say("Mean rating %.2f, but the distribution is J-shaped (many 5s, a second bump at 1).", mean(feedback$rating))
say("-> treat rating as ordinal / binary (e.g. low = 1-2) rather than as a normal continuous outcome.")

section("8.4 Text indicators")
say("has_comment vs comment_length > 0:")
show(count(feedback, has_comment, comment_positive = comment_length > 0))
say("has_title vs has_comment:")
show(count(feedback, has_title, has_comment))
say("Comment share by rating:")
show(feedback %>% group_by(rating) %>% summarise(n = n(), pct_with_comment = round(100 * mean(has_comment), 1),
                                                 mean_comment_length = round(mean(comment_length), 1)))

section("8.5 Feedback timing")
fbo <- feedback %>% left_join(select(orders, order_id, purchase_timestamp, delivery_timestamp, order_status),
                              by = "order_id") %>%
  mutate(days_after_delivery = as.numeric(difftime(feedback_created_timestamp, delivery_timestamp, units = "days")),
         answer_days = as.numeric(difftime(feedback_answered_timestamp, feedback_created_timestamp, units = "days")))
say("Time of day of feedback_created_timestamp (should be a pure date):")
show(feedback %>% count(time_of_day = format(feedback_created_timestamp, "%H:%M:%S"),
                        month = format(feedback_created_timestamp, "%m")) %>%
       group_by(time_of_day) %>% summarise(records = sum(n), months = paste(unique(month), collapse = ",")))
say("-> creation is recorded as a date; the few 01:00 values all fall in one month (likely a timezone/DST artifact).")
flag("Feedback created BEFORE the purchase date: %d records",
     sum(as.Date(fbo$feedback_created_timestamp) < as.Date(fbo$purchase_timestamp), na.rm = TRUE))
say("Feedback created before delivery date: %s records (%s) - possibly reviews of late/undelivered orders",
    fmt(sum(as.Date(fbo$feedback_created_timestamp) < as.Date(fbo$delivery_timestamp), na.rm = TRUE)),
    pct(sum(as.Date(fbo$feedback_created_timestamp) < as.Date(fbo$delivery_timestamp), na.rm = TRUE), nrow(fbo)))
say("Answered before created: %d", sum(fbo$answer_days < 0))
show(numeric_summary(select(fbo, answer_days)))
say("Feedback by order status of the reviewed order:")
show(freq(fbo, order_status))
say("Mean rating by order status:")
rating_status <- fbo %>% group_by(order_status) %>%
  summarise(n = n(), mean_rating = round(mean(rating), 2), pct_rating_1_2 = round(100 * mean(rating <= 2), 1)) %>%
  arrange(desc(n))
show(rating_status)
findings$rating <- sprintf(
  "Ratings are J-shaped (%.1f%% fives, %.1f%% ones). Completed orders average %.2f, non-completed orders %.2f or lower: fulfilment is visibly linked to satisfaction.",
  100 * mean(feedback$rating == 5), 100 * mean(feedback$rating == 1),
  rating_status$mean_rating[rating_status$order_status == "COMPLETED"],
  max(rating_status$mean_rating[rating_status$order_status != "COMPLETED"]))

# =============================================================================
# 9. RELATIONSHIPS BETWEEN TABLES
# =============================================================================
header("9. Relationships and join behaviour")

section("9.1 Referential integrity (does every child key exist in the parent table?)")
fk <- bind_rows(
  check_fk(orders,   "orders",      customers, "customers", "customer_record_id"),
  check_fk(items,    "order_items", orders,    "orders",    "order_id"),
  check_fk(items,    "order_items", products,  "products",  "product_id"),
  check_fk(items,    "order_items", sellers,   "sellers",   "seller_id"),
  check_fk(payments, "payments",    orders,    "orders",    "order_id"),
  check_fk(feedback, "customer_feedback", orders, "orders", "order_id")
)
show(fk)
say("Read: 'unmatched_rows' = child rows that would be lost in an inner join;")
say("      'parents_without_child' = parent keys with no matching row (e.g. orders with no items).")
missing_prod <- setdiff(unique(items$product_id), products$product_id)
flag("%d product_ids are sold in order_items but do not exist in products (%d item lines)",
     length(missing_prod), sum(items$product_id %in% missing_prod))

section("9.2 Orders missing from child tables, by status")
show(orders %>%
       mutate(has_items = order_id %in% items$order_id,
              has_payment = order_id %in% payments$order_id,
              has_feedback = order_id %in% feedback$order_id) %>%
       group_by(order_status) %>%
       summarise(orders = n(), no_items = sum(!has_items), no_payment = sum(!has_payment),
                 no_feedback = sum(!has_feedback)) %>% arrange(desc(orders)))
findings$coverage <- sprintf(
  "%d orders have no items (mostly UNAVAILABLE/CANCELLED), %d order has no payment, %d orders have no feedback, and %d sold product_ids are missing from products.",
  sum(!orders$order_id %in% items$order_id), sum(!orders$order_id %in% payments$order_id),
  sum(!orders$order_id %in% feedback$order_id), length(missing_prod))

section("9.3 Cardinality (rows per key on each side of a join)")
card <- function(x, key) { k <- count(x, .data[[key]]); c(min = min(k$n), mean = round(mean(k$n), 3), max = max(k$n)) }
show(bind_rows(
  c(join = "orders -> customers (customer_record_id)", card(customers, "customer_record_id")),
  c(join = "customer_id -> customers (person level)",  card(customers, "customer_id")),
  c(join = "orders -> order_items (order_id)",         card(items, "order_id")),
  c(join = "orders -> payments (order_id)",            card(payments, "order_id")),
  c(join = "orders -> customer_feedback (order_id)",   card(feedback, "order_id")),
  c(join = "products -> order_items (product_id)",     card(items, "product_id")),
  c(join = "sellers -> order_items (seller_id)",       card(items, "seller_id"))
))

section("9.4 Join fan-out: what happens if tables are joined naively")
j1 <- orders %>% left_join(items, by = "order_id", relationship = "many-to-many")
j2 <- j1 %>% left_join(payments, by = "order_id", relationship = "many-to-many")
j3 <- j2 %>% left_join(feedback, by = "order_id", relationship = "many-to-many")
show(tibble(step = c("orders", "+ order_items", "+ payments", "+ customer_feedback"),
            rows = c(nrow(orders), nrow(j1), nrow(j2), nrow(j3)),
            distinct_orders = c(n_distinct(orders$order_id), n_distinct(j1$order_id),
                                n_distinct(j2$order_id), n_distinct(j3$order_id))))
naive_rev <- sum(j3$payment_amount, na.rm = TRUE); true_rev <- sum(payments$payment_amount)
flag("Summing payment_amount after the naive join gives %s vs the true %s (+%s)",
     fmt(round(naive_rev)), fmt(round(true_rev)), pct(naive_rev - true_rev, true_rev))
say("-> each table must be aggregated to the chosen unit of observation BEFORE joining.")
findings$fanout <- sprintf(
  "Joining orders -> items -> payments -> feedback without aggregating grows %s orders to %s rows and inflates payment totals by %s.",
  fmt(nrow(orders)), fmt(nrow(j3)), pct(naive_rev - true_rev, true_rev))

# =============================================================================
# 10. SUMMARY OF KEY FINDINGS
# =============================================================================
header("10. Key findings for the Scoping Brief")
for (i in seq_along(findings)) cat(sprintf("\n %2d. %s\n", i, str_wrap(findings[[i]], width = 100, exdent = 5)))
cat("\n")
