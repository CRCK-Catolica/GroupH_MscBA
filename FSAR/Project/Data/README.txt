# Aurora Market Data

This folder contains the data for the Aurora Market Analytics Project.

Aurora Market is a multi-seller e-commerce platform. The database contains operational information about customers, orders, products, sellers, payments, deliveries, and customer feedback.

## Files

The database consists of seven CSV files:

- `customers.csv`
- `orders.csv`
- `order_items.csv`
- `products.csv`
- `sellers.csv`
- `payments.csv`
- `customer_feedback.csv`

Variable definitions are provided in:

- `data_dictionary.csv`

## Working with the Database

The seven files form a relational database. Different tables may represent different units of observation, and some entities may appear more than once within a table.

You should investigate the structure of the database before constructing an analytical dataset. In particular, do not assume that:

- identifiers are necessarily unique unless you have checked;
- every relationship between tables is complete;
- tables can be joined directly without affecting the unit of observation;
- unusual or extreme values are necessarily errors;
- missing observations should automatically be removed;
- every data-quality issue requires the same response.

The data dictionary describes the intended meaning of each variable. It does not guarantee that all recorded values are complete, internally consistent, or error-free.

## Monetary Variables

`item_price`, `freight_cost`, and `payment_amount` are expressed in Aurora Market currency units.

No conversion to another currency is required.

## Date and Time Variables

Variables ending in `_timestamp` or `shipping_deadline` contain date and time information.

When importing the data into R, check that these variables have been read using an appropriate date-time format before performing calculations involving durations or ordering of events.

## Boolean Variables

In `customer_feedback.csv`, the variables `has_title` and `has_comment` indicate whether the corresponding feedback content was present.

## Reproducibility

Your project code should begin with the CSV files supplied in this folder.

Do not manually modify the original data files. Any cleaning, recoding, filtering, aggregation, or other changes used in your analysis should be performed reproducibly in R.

Use relative file paths wherever possible so that your project can run on another computer.