# Clearing all existing variables from the workspace
rm(list=ls())

# Loading necessary libraries
library(readr)
library(RSQLite)
library(dplyr)

# Establishing a connection to the SQLite database
my_connection <- dbConnect(SQLite(), "database/database.db")

# Creating the 'user' table
dbExecute(my_connection, "CREATE TABLE IF NOT EXISTS user(
  user_id INT,
  user_first_name VARCHAR(30) NOT NULL,
  user_last_name VARCHAR(30) NOT NULL,
  user_email VARCHAR(100) NOT NULL,
  user_password VARCHAR(20) NOT NULL,
  user_mobile_number VARCHAR(20) NOT NULL,
  user_membership_status VARCHAR(20) NOT NULL,
  address_id VARCHAR(30) NOT NULL,
  address_city VARCHAR(30) NOT NULL,
  address_country VARCHAR(30) NOT NULL,
  address_state VARCHAR(20) NOT NULL,
  address_postcode VARCHAR(20) NOT NULL,
  address_type VARCHAR(20) NOT NULL,
  PRIMARY KEY (user_id)
)")

# Creating the 'product' table
dbExecute(my_connection, "CREATE TABLE IF NOT EXISTS product(
  product_id INT,
  product_description VARCHAR(100) NOT NULL UNIQUE,
  product_code VARCHAR(20) NOT NULL UNIQUE,
  category_id INT,
  product_stock INT NOT NULL,
  product_price INT NOT NULL,
  product_name VARCHAR(30) NOT NULL,
  product_brand VARCHAR(30),
  product_weight INT NOT NULL,
  product_dimensions INT NOT NULL,
  product_published_datetime DATE NOT NULL,
  product_average_star_ratings DOUBLE(3,2),
  product_reviews VARCHAR(50),
  seller_id INT,
  PRIMARY KEY (product_id),
  FOREIGN KEY (category_id) REFERENCES product_category(category_id),
  FOREIGN KEY (seller_id) REFERENCES seller(seller_id)
)")

# Creating the 'product_category' table
dbExecute(my_connection, "CREATE TABLE IF NOT EXISTS product_category(
  category_id INT,
  category_name VARCHAR(30) NOT NULL,
  category_description VARCHAR(100) NOT NULL,
  PRIMARY KEY (category_id)
)")

# Creating the 'seller' table
dbExecute(my_connection, "CREATE TABLE IF NOT EXISTS seller(
  seller_id INT,
  seller_name VARCHAR(20) NOT NULL,
  seller_address VARCHAR(50) NOT NULL,
  seller_delivery_method VARCHAR(20) NOT NULL,
  PRIMARY KEY (seller_id)
)")

# Creating the 'order_details' table
dbExecute(my_connection, "CREATE TABLE IF NOT EXISTS order_details(
  order_detail_id INT,
  product_id INT,
  order_qty INT NOT NULL,
  order_price INT NOT NULL,
  order_status VARCHAR(30) NOT NULL,
  order_datetime DATETIME,
  payment_method VARCHAR(20) NOT NULL,
  user_id INT,
  PRIMARY KEY (order_detail_id, product_id, user_id),
  FOREIGN KEY (product_id) REFERENCES product(product_id),
  FOREIGN KEY (user_id) REFERENCES user(user_id)
)")

# Creating the 'delivery' table
dbExecute(my_connection, "CREATE TABLE IF NOT EXISTS delivery(
  delivery_id INT,
  delivery_type VARCHAR(20) NOT NULL,
  delivery_status VARCHAR(20) NOT NULL,
  delivery_datetime DATETIME,
  product_id INT,
  user_id INT,
  order_detail_id INT,
  shipper_id INT,
  PRIMARY KEY (delivery_id),
  FOREIGN KEY (product_id) REFERENCES address(product_id),
  FOREIGN KEY (user_id) REFERENCES user(user_id),
  FOREIGN KEY (order_detail_id) REFERENCES order_details(order_detail_id),
  FOREIGN KEY (shipper_id) REFERENCES shipper(shipper_id)
)")

# Creating the 'shipper' table
dbExecute(my_connection, "CREATE TABLE IF NOT EXISTS shipper(
  shipper_id INT,
  shipper_company VARCHAR(20) NOT NULL,
  shipper_contact VARCHAR(20) NOT NULL,
  PRIMARY KEY (shipper_id)
)")