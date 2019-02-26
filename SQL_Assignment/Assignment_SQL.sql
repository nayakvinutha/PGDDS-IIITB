/* Understanding the Data

The Data provided, consists of data related to super store which is  stored across 5 tables as follows :

1. cust_dimen
—————————————————
-> Stores data about customer like name and the region/province  they belong and the customer segment. 

PK -  cust_id  ( TEXT) -  Uniquely identifies each customer 
FK -  NA         


2. orders_dimen
———————
-> Stores data about each order like, the order date, priority and order_id and Ord_id. 

PK  -  Ord_id (TEXT)
FK  -  NA 

Ord_id              - should be Primary key as  it uniquely identifies an Order. 
order_id ( INT) -  doesn’t satisfy the requirement of the column values to be unique. 


3. prod_dimen
———————
-> Stores data about each Product like the category and sub category that it belongs to.

PK  - Prod_id  ( TEXT)  - Uniquely identifies each product.
FK  - NA


4. shipping_dimen
———————
-> Stores data about each shipment like shipment date, mode of shipment, Order_ID 

PK  - ship_id ( TEXT)   - Uniquely identifies each shipment 
FK  -  NA 

Order_ID  refers to the Order_ID of the order_dimension table, however its  NOT a foreign key as it has duplicate values in 
orders_dimen table.


5. Market_fact
———————
-> Stores data about market facts related like the Order_quantity, sales and other information

PK  -  composite key ( Ord_id,  Prod_id, Ship_id, Cust_id )
FK  -  Ord_id   -  Primary key in orders_dimen
          Prod_id  - PK in prod_dimen
          Ship_id, - PK in shipping_dimen
          Cust_id  - PK in cust_dimen
*/

#Task 2: Basic Analysis

# A. Find the total and the average sales (display total_sales and avg_sales)
Select sum(sales) as total_sales,  avg(sales) as avg_sales 	
from market_fact;


#B. Display the number of customers in each region in decreasing order of no_of_customers.
#The result should contain columns Region, no_of_customers 
select Region, count(*) as  no_of_customers
from cust_dimen
group by region 
order by no_of_customers desc;



#C. Find the region having maximum customers (display the region name and
#max(no_of_customers)
select Region, count(*) as  maximum_no_of_customers
from cust_dimen
group by region 
order by maximum_no_of_customers desc
Limit 1;


#D.Find the number and id of products sold in decreasing order of products sold (display
#product id, no_of_products sold)
select Prod_id as product_id , sum(Order_quantity)as no_of_products
from market_fact
group by Prod_id
order by no_of_products desc;


#E.Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and
#the number of tables purchased (display the customer name, no_of_tables
#`	purchased) 
select  cust_dimen.Customer_Name,  sum(Order_quantity) as no_of_tables 
from ( cust_dimen inner join market_fact on  cust_dimen.Cust_id = market_fact.Cust_id  
inner join prod_dimen on  market_fact.Prod_id = prod_dimen.Prod_id)
where Region = 'ATLANTIC' and Product_Sub_Category = 'TABLES'
group by cust_dimen.cust_id, cust_dimen.Customer_Name;



#Task 3: Advanced Analysis 

#A. Display the product categories in descending order of profits (display the product
#category wise profits i.e. product_category, profits)?
Select  Product_category, sum(Profit) as Profits
from  market_fact inner join prod_dimen on  market_fact.Prod_id = prod_dimen.Prod_id
group by Product_Category
order by Profits desc;


#B. Display the product category, product sub-category and the profit within each subcategory
#in three columns.
Select  Product_category, Product_sub_category, sum(Profit) as Profits
from  market_fact inner join prod_dimen on  market_fact.Prod_id = prod_dimen.Prod_id
group by Product_Category,Product_sub_category;



#C. Where is the least profitable product subcategory shipped the most? For the least
#profitable product sub-category, display the region-wise no_of_shipments and the
#profit made in each region in decreasing order of profits (i.e. region,
#no_of_shipments, profit_in_each_region)
#Note: You can hardcode the name of the least profitable product subcategory
Select Region, count(*) as no_of_shipments, sum(profit) as profit_in_each_region
from cust_dimen inner join market_fact        on     cust_dimen.Cust_id   = market_fact.Cust_id  
                              inner join prod_dimen        on    market_fact.Prod_id   = prod_dimen.Prod_id
                              inner join shipping_dimen on     market_fact.Ship_id   = shipping_dimen.Ship_id
where Product_Sub_Category = 'TABLES'
group by Region
order by  profit_in_each_region desc;