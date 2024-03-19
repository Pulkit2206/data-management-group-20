
##Part 2.2

#Ingesting, Data Validation and Referential Integrity Checks

library(readr)
library(RSQLite)
library(dplyr)

# Ingest User Data Function
ingest_user_data <- function(df, connection) {
  required_columns <- c("user_id", "user_email", "user_password", "user_mobile_number", "address_id",
                    "address_city", "address_country", "address_state", "address_postcode", "address_type")
  
  # Filter out rows with NA in any of the required columns
  df <- df[!rowSums(is.na(df[required_columns])) > 0, ]
  
  for (i in 1:nrow(df)) {
    # Check for duplicate user_id
    existing_users <- dbGetQuery(connection, sprintf("SELECT user_id FROM user WHERE user_id = '%s'", df$user_id[i]))
    if (nrow(existing_users) > 0) {
      cat(sprintf("Skipping duplicate entry for user_id: %s\n", df$user_id[i]))
      next
    }
    
    # Check for valid email
    if (!grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", df$user_email[i])) {
      cat(sprintf("Skipping entry due to invalid email for user_id: %s\n", df$user_id[i]))
      next
    }
    
    # Prepare the INSERT statement including user and address information
    insert_query <- sprintf("INSERT INTO user (user_id, user_first_name, user_last_name, user_email, user_password, user_mobile_number, user_membership_status, address_id, address_city, address_country, address_state, address_postcode, address_type) VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')",
                            df$user_id[i], df$user_first_name[i], df$user_last_name[i], df$user_email[i],
                            df$user_password[i], df$user_mobile_number[i], df$user_membership_status[i],
                            df$address_id[i], df$address_city[i], df$address_country[i], df$address_state[i],
                            df$address_postcode[i], df$address_type[i])
    
    # Execute the INSERT statement and handle any errors
    tryCatch({
      dbExecute(connection, insert_query)
      cat(sprintf("Successfully inserted user_id: %s\n", df$user_id[i]))
    }, error = function(e) {
      cat(sprintf("Error in inserting user_id: %s, Error: %s\n", df$user_id[i], e$message))
    })
  }
}

# Ingest Product Data Function
ingest_product_data <- function(df, connection) {
  required_columns <- c("product_id", "product_name", "product_price", "product_description", "category_id", 
                        "product_stock", "product_code", "product_brand", "product_weight", "product_dimensions", 
                        "product_published_datetime", "product_average_star_ratings", "product_reviews")
  
  # Filter out rows with NA in any of the required columns
  df <- df[!rowSums(is.na(df[required_columns])) > 0, ]
  
  for(i in 1:nrow(df)) {
    # Check for duplicate product_id
    existing_ids <- dbGetQuery(connection, sprintf("SELECT product_id FROM product WHERE product_id = '%s'", df$product_id[i]))
    if(nrow(existing_ids) > 0) {
      cat(sprintf("Skipping duplicate entry for product_id: %s\n", df$product_id[i]))
      next
    }
    
    # Check for valid price
    if(!is.numeric(df$product_price[i]) || df$product_price[i] <= 0) {
      cat(sprintf("Skipping entry due to invalid price for product_id: %s\n", df$product_id[i]))
      next
    }
    
    # Prepare the INSERT statement with all required columns
    insert_query <- sprintf("INSERT INTO product (product_id, product_name, product_description, product_price, 
                             category_id, product_stock, product_code, product_brand, product_weight, 
                             product_dimensions, product_published_datetime, product_average_star_ratings, 
                             product_reviews) VALUES ('%s', '%s', '%s', %f, '%s', %d, '%s', '%s', %f, '%s', '%s', %f, '%s')",
                            df$product_id[i], df$product_name[i], df$product_description[i], df$product_price[i],
                            df$category_id[i], df$product_stock[i], df$product_code[i], df$product_brand[i], 
                            df$product_weight[i], df$product_dimensions[i], df$product_published_datetime[i], 
                            df$product_average_star_ratings[i], df$product_reviews[i])
    
    # Execute the INSERT statement and handle any errors
    tryCatch({
      dbExecute(connection, insert_query)
      cat(sprintf("Successfully inserted row: %d\n", i))
    }, error = function(e) {
      cat(sprintf("Error in inserting row: %d, Error: %s\n", i, e$message))
    })
  }
}



# Function to ingest product_category data
ingest_product_category_data <- function(df, connection) {
  required_columns <- c("category_id", "category_name", "category_description")
  df <- df[!rowSums(is.na(df[required_columns])) > 0, ]
  
  for(i in 1:nrow(df)) {
    # Check for duplicate category_id
    existing_ids <- dbGetQuery(connection, sprintf("SELECT category_id FROM product_category WHERE category_id = '%s'", df$category_id[i]))
    if(nrow(existing_ids) > 0) {
      cat(sprintf("Skipping duplicate entry for category_id: %s\n", df$category_id[i]))
      next
    }
    
    # Insert validated data into the database
    insert_query <- sprintf("INSERT INTO product_category (category_id, category_name, category_description) VALUES ('%s', '%s', '%s')",
                            df$category_id[i], df$category_name[i], df$category_description[i])
    tryCatch({
      dbExecute(connection, insert_query)
      cat(sprintf("Successfully inserted category_id: %s\n", df$category_id[i]))
    }, error = function(e) {
      cat(sprintf("Error in inserting category_id: %s, Error: %s\n", df$category_id[i], e$message))
    })
  }
}

# Function to ingest order_details data
ingest_order_details_data <- function(df, connection) {
  required_columns <- c("order_detail_id", "product_id", "order_qty", "order_price", 
                        "order_status", "order_datetime", "user_id", "category_id")
  df <- df[!rowSums(is.na(df[required_columns])) > 0, ]
  
  for(i in 1:nrow(df)) {
    # Check for duplicate order_detail_id
    existing_ids <- dbGetQuery(connection, sprintf("SELECT order_detail_id FROM order_details WHERE order_detail_id = '%s'", df$order_detail_id[i]))
    if(nrow(existing_ids) > 0) {
      cat(sprintf("Skipping duplicate entry for order_detail_id: %s\n", df$order_detail_id[i]))
      next
    }
    
    # Ensure referenced user_id, product_id, and category_id exist
    user_exists <- dbGetQuery(connection, sprintf("SELECT user_id FROM user WHERE user_id = '%s'", df$user_id[i]))
    product_exists <- dbGetQuery(connection, sprintf("SELECT product_id FROM product WHERE product_id = '%s'", df$product_id[i]))
    category_exists <- dbGetQuery(connection, sprintf("SELECT category_id FROM product_category WHERE category_id = '%s'", df$category_id[i]))
    
    if(nrow(user_exists) == 0 || nrow(product_exists) == 0 || nrow(category_exists) == 0) {
      cat(sprintf("Skipping entry due to non-existent user_id, product_id, or category_id for order_detail_id: %s\n", df$order_detail_id[i]))
      next
    }
    
    # Insert validated data into the database
    insert_query <- sprintf("INSERT INTO order_details (order_detail_id, product_id, order_qty, order_price, order_status, order_datetime, user_id, category_id) VALUES ('%s', '%s', %d, %f, '%s', '%s', '%s', '%s')",
                            df$order_detail_id[i], df$product_id[i], df$order_qty[i], df$order_price[i], df$order_status[i], df$order_datetime[i], df$user_id[i], df$category_id[i])
    tryCatch({
      dbExecute(connection, insert_query)
      cat(sprintf("Successfully inserted order_detail_id: %s\n", df$order_detail_id[i]))
    }, error = function(e) {
      cat(sprintf("Error in inserting order_detail_id: %s, Error: %s\n", df$order_detail_id[i], e$message))
    })
  }
}


# Function to ingest delivery data
ingest_delivery_data <- function(df, connection) {
  required_columns <- c("delivery_id", "delivery_type", "delivery_status", "user_id", "order_detail_id")
  df <- df[!rowSums(is.na(df[required_columns])) > 0, ]
  
  for(i in 1:nrow(df)) {
    # Check for duplicate delivery_id
    existing_ids <- dbGetQuery(connection, sprintf("SELECT delivery_id FROM delivery WHERE delivery_id = '%s'", df$delivery_id[i]))
    if(nrow(existing_ids) > 0) {
      cat(sprintf("Skipping duplicate entry for delivery_id: %s\n", df$delivery_id[i]))
      next
    }
    
    # Ensure referenced user_id, and order_detail_id exist
    user_exists <- dbGetQuery(connection, sprintf("SELECT user_id FROM user WHERE user_id = '%s'", df$user_id[i]))
    order_exists <- dbGetQuery(connection, sprintf("SELECT order_detail_id FROM order_details WHERE order_detail_id = '%s'", df$order_detail_id[i]))
    if(nrow(user_exists) == 0 || nrow(order_exists) == 0 || nrow(address_exists) == 0) {
      cat(sprintf("Skipping entry due to non-existent user_id, order_detail_id for delivery_id: %s\n", df$delivery_id[i]))
      next
    }
    
    # Insert validated data into the database
    insert_query <- sprintf("INSERT INTO delivery (delivery_id, delivery_type, delivery_status, user_id, order_detail_id) VALUES ('%s', '%s', '%s', '%s', '%s', '%s')",
                            df$delivery_id[i], df$delivery_type[i], df$delivery_status[i], df$user_id[i], df$order_detail_id[i])
    tryCatch({
      dbExecute(connection, insert_query)
      cat(sprintf("Successfully inserted delivery_id: %s\n", df$delivery_id[i]))
    }, error = function(e) {
      cat(sprintf("Error in inserting delivery_id: %s, Error: %s\n", df$delivery_id[i], e$message))
    })
  }
}

# Function to ingest seller data
ingest_seller_data <- function(df, connection) {
  required_columns <- c("seller_id", "seller_name", "seller_address", "seller_delivery_method")
  df <- df[!rowSums(is.na(df[required_columns])) > 0, ]
  
  for(i in 1:nrow(df)) {
    # Check for duplicate seller_id
    existing_ids <- dbGetQuery(connection, sprintf("SELECT seller_id FROM seller WHERE seller_id = '%s'", df$seller_id[i]))
    if(nrow(existing_ids) > 0) {
      cat(sprintf("Skipping duplicate entry for seller_id: %s\n", df$seller_id[i]))
      next
    }
    
    # Insert validated data into the database
    insert_query <- sprintf("INSERT INTO seller (seller_id, seller_name, seller_address, seller_delivery_method) VALUES ('%s', '%s', '%s', '%s')",
                            df$seller_id[i], df$seller_name[i], df$seller_address[i], df$seller_delivery_method[i])
    tryCatch({
      dbExecute(connection, insert_query)
      cat(sprintf("Successfully inserted seller_id: %s\n", df$seller_id[i]))
    }, error = function(e) {
      cat(sprintf("Error in inserting seller_id: %s, Error: %s\n", df$seller_id[i], e$message))
    })
  }
}

# Function to ingest shipper data
ingest_shipper_data <- function(df, connection) {
  required_columns <- c("shipper_id", "shipper_company", "shipper_contact")
  df <- df[!rowSums(is.na(df[required_columns])) > 0, ]
  
  for(i in 1:nrow(df)) {
    # Check for duplicate shipper_id
    existing_ids <- dbGetQuery(connection, sprintf("SELECT shipper_id FROM shipper WHERE shipper_id = '%s'", df$shipper_id[i]))
    if(nrow(existing_ids) > 0) {
      cat(sprintf("Skipping duplicate entry for shipper_id: %s\n", df$shipper_id[i]))
      next
    }
    
    # Insert validated data into the database
    insert_query <- sprintf("INSERT INTO shipper (shipper_id, shipper_company, shipper_contact) VALUES ('%s', '%s', '%s')",
                            df$shipper_id[i], df$shipper_company[i], df$shipper_contact[i])
    tryCatch({
      dbExecute(connection, insert_query)
      cat(sprintf("Successfully inserted shipper_id: %s\n", df$shipper_id[i]))
    }, error = function(e) {
      cat(sprintf("Error in inserting shipper_id: %s, Error: %s\n", df$shipper_id[i], e$message))
    })
  }
}


# Load the data from CSV files or data frames
user_df <- read_csv("Synthetic_Data_Generation/users_table.csv")
product_df <- read_csv("Synthetic_Data_Generation/products_table.csv")
product_category_df <- read_csv("Synthetic_Data_Generation/product_categories_table.csv")
order_details_df <- read_csv("Synthetic_Data_Generation/order_details_table.csv")
delivery_df <- read_csv("Synthetic_Data_Generation/deliveries_table.csv")
seller_df <- read_csv("Synthetic_Data_Generation/sellers_table.csv")
shipper_df <- read_csv("Synthetic_Data_Generation/shippers_table.csv")

# Establish a connection to the SQLite database
my_connection <- dbConnect(RSQLite::SQLite(), "database/database.db")

# Ingest the data into the database
ingest_user_data(user_df, my_connection)
ingest_product_category_data(product_category_df, my_connection)
ingest_product_data(product_df, my_connection)
ingest_order_details_data(order_details_df, my_connection)
ingest_delivery_data(delivery_df, my_connection)
ingest_seller_data(seller_df, my_connection)
ingest_shipper_data(shipper_df, my_connection)

# Close the database connection
dbDisconnect(my_connection)

