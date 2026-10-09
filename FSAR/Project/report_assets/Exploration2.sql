Select count(1) from products 
where weight_g is NULL
Select * from products 
where weight_g is NULL



Select * from sellers
where postal_area is NULL or city is NULL or region is NULL


SELECT A.seller_id,B.order_id 

------------------------------------------------------
DROP TABLE #Unique_Orders
    Select distinct(order_id),seller_id
    into #Unique_Orders
    from order_items
    group by order_id,seller_id
Select * from #Unique_Orders

Select * from sellers
------------------------------------------------------
SELECT A.seller_id,B.order_id  
into #Sellers_Orders
from sellers as A
left join #Unique_Orders as B
    on A.seller_id=B.seller_id
Select seller_id, count(1) as FREQ from #Sellers_Orders
group by seller_id order by FREQ DESC
---------------------------------------------
Select * from #Sellers_Orders where seller_id = 'SELLER_IGLPZXDYVH'
    
    Select * from sellers
    -----------------------------------------
    DROP TABLE #Unique_Orders
    Select distinct(order_id),seller_id
    into #Unique_Orders
    from order_items
    group by order_id,seller_id
------------------------------------------------------
    Select distinct(order_id) from orders
    --641 ID do not appear in the order items table



SELECT A.order_id, COUNT(*) AS freq
FROM AuroraMarket.dbo.orders AS A
LEFT JOIN AuroraMarket.dbo.order_items AS B
       ON A.order_id = B.order_id
GROUP BY A.order_id
HAVING COUNT(*) = 1;

------------------------------------------------
-- Customer Feedback
------------------------------------------------
Select *,feedback_answered_timestamp-feedback_created_timestamp
 from customer_feedback
where feedback_answered_timestamp>feedback_created_timestamp


Select * from customer_feedback

-----------------------------
--PAYMENTS
-----------------------------
Select payment_method,count(1) from payments 
group by payment_method
order by count(1) desc

Select distinct(order_id) from payments
Select (order_id) from payments

SELECT order_id, COUNT(*) AS N
FROM AuroraMarket.dbo.payments
GROUP BY order_id
HAVING COUNT(*) > 1;

Select * from payments where 
order_id IS NULL OR
payment_sequence is NULL 
or payment_method is NULL or 
installments IS NULL OR 
payment_amount IS NULL
------------------------------------
-- CUSTOMERS
------------------------------------
Select customer_id,customer_record_id from customers

SELECT COUNT(*)                           AS total_rows,
       COUNT(DISTINCT customer_record_id) AS distinct_record_ids,
       COUNT(DISTINCT customer_id)        AS distinct_customer_ids
FROM AuroraMarket.dbo.customers;

Select region, from customers


Select * from order_items 
where 
order_id='ORDER_HNGQEPCZMN'
--item_number in (19,20)
order by order_id, item_number

Select * from products
where product_id='PRODUCT_TAADIZOHKD'

Select * from customers where customer_id='CUSTOMER_FVUPSLSBKN'

Select * from orders where order_id='ORDER_HNGQEPCZMN'
:((((((((((((((((((((((((((

--where item_number in (19,20)
-----------------
--ORDER ITEMS
-----------------
--ITEM NUMER contains de number of product relative to the order. 
--Order ID is unique 
--Product ID its nor ordered by the item_number, many can appear in a single order
-- Each Order only has ONE seller (seller_id)r
-- NO HAY NULOS WUUU

Select * from order_items where
order_id IS NULL or
item_number IS NULL or
product_id IS NULL or
seller_id IS NULL or
shipping_deadline IS NULL or
item_price IS NULL or
freight_cost IS NULL 

Select * from orders