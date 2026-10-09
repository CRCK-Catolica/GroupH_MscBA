Select * from AuroraMarket.dbo.orders

Select COUNT(distinct(order_id)) 
from AuroraMarket.dbo.orders

Select
count(1) from AuroraMarket.dbo.orders
where
approval_timestamp>delivery_timestamp

Select
case 
when approval_timestamp>delivery_timestamp then 1
end
into #Medidas
from AuroraMarket.dbo.orders

Select * from AuroraMarket.dbo.order_items
where order_id='ORDER_CKTYANBORB' OR order_id='ORDER_FGXMKGJABC'



Select count(order_id) Freq
from AuroraMarket.dbo.orders
where
count(1)>1

SELECT order_id, COUNT(*) AS freq
FROM AuroraMarket.dbo.orders
GROUP BY order_id
HAVING COUNT(*) = 1
ORDER BY freq DESC;


Select approval_timestamp from AuroraMarket.dbo.orders
group by approval_timestamp

Select * from AuroraMarket.dbo.orders 
where order_status='Cancelled'

Select * from AuroraMarket.dbo.orders 
where order_status is NULL
Select * from AuroraMarket.dbo.orders 
where carrier_timestamp is NULL
Select * from AuroraMarket.dbo.orders 
where delivery_timestamp is NULL
Select * from AuroraMarket.dbo.orders 
where estimated_delivery_timestamp is NULL

Select order_status, count(1) Freq
from AuroraMarket.dbo.orders
group by order_status
order by Freq desc

Select * from AuroraMarket.dbo.orders
where order_status='CANCELLED' AND
delivery_timestamp is not NULLde


WITH checks AS (
    SELECT order_id, order_status,
        CASE WHEN approval_timestamp < purchase_timestamp THEN 1 ELSE 0 END AS approval_before_purchase,
        CASE WHEN carrier_timestamp  < purchase_timestamp THEN 1 ELSE 0 END AS carrier_before_purchase,
        CASE WHEN carrier_timestamp  < approval_timestamp THEN 1 ELSE 0 END AS carrier_before_approval,
        CASE WHEN delivery_timestamp < purchase_timestamp THEN 1 ELSE 0 END AS delivery_before_purchase,
        CASE WHEN delivery_timestamp < approval_timestamp THEN 1 ELSE 0 END AS delivery_before_approval,
        CASE WHEN delivery_timestamp < carrier_timestamp  THEN 1 ELSE 0 END AS delivery_before_carrier
    FROM AuroraMarket.dbo.orders
)
SELECT v.rule_broken, SUM(v.flag) AS orders
FROM checks
CROSS APPLY (VALUES
    ('1. approval before purchase', approval_before_purchase),
    ('2. carrier before purchase',  carrier_before_purchase),
    ('3. carrier before approval',  carrier_before_approval),
    ('4. delivery before purchase', delivery_before_purchase),
    ('5. delivery before approval', delivery_before_approval),
    ('6. delivery before carrier',  delivery_before_carrier)
) v(rule_broken, flag)
GROUP BY v.rule_broken
UNION ALL
SELECT '7. ANY rule broken (distinct orders)',
       SUM(CASE WHEN approval_before_purchase + carrier_before_purchase + carrier_before_approval
                   + delivery_before_purchase + delivery_before_approval + delivery_before_carrier > 0
                THEN 1 ELSE 0 END)
FROM checks
ORDER BY rule_broken;

Select * from AuroraMarket.dbo.orders where carrier_timestamp  < purchase_timestamp
Select * from AuroraMarket.dbo.orders where carrier_timestamp  < approval_timestamp

Select year(purchase_timestamp) as YEAR, MONTH(purchase_timestamp) AS MONTH, count(1) as FREQ
from AuroraMarket.dbo.orders
group by YEAR, MONTH

-----------------------------------------
Select A.order_id, COUNT(*) AS freq
from AuroraMarket.dbo.orders as A
GROUP BY A.order_id
HAVING COUNT(*) = 1
left join AuroraMarket.dbo.order_items B
on
A.order_id=B.order_id
-------------------------------------------
SELECT A.order_id, COUNT(*) AS freq
FROM AuroraMarket.dbo.orders AS A
LEFT JOIN AuroraMarket.dbo.order_items AS B
       ON A.order_id = B.order_id
GROUP BY A.order_id
HAVING COUNT(*) = 1;
--------------------------
SELECT n_items, COUNT(*) AS orders
FROM (
    SELECT A.order_id, COUNT(B.item_number) AS n_items
    FROM AuroraMarket.dbo.orders AS A
    LEFT JOIN AuroraMarket.dbo.order_items AS B ON A.order_id = B.order_id
    GROUP BY A.order_id
) t
GROUP BY n_items
ORDER BY n_items;



Select * 
from AuroraMarket.dbo.order_items