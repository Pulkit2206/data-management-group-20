library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)



# create table used for analysis
combined_data <- read.csv("Synthetic_Data_Generation/combined_mockaroo_with_synthetic.csv")
users_table <- read.csv("Synthetic_Data_Generation/users_table.csv")
products_table <- read.csv("Synthetic_Data_Generation/products_table.csv")
product_categories_table <- read.csv("Synthetic_Data_Generation/product_categories_table.csv")
order_details_table <- read.csv("Synthetic_Data_Generation/order_details_table.csv")

# 4 Data Analysis of User Portrail
# 4.1 Data Analysis Process
# 4.1.1 Data Preparation
# 4.1.2 Plot Coding
# Total sales of products by category in 2023.
# create new table with inner join function
data_ctg <- inner_join(order_details_table, product_categories_table, by = "category_id") %>%
  mutate(order_id = paste(order_detail_id, product_id, user_id, sep = "")) %>%
  select(order_id, order_datetime, category_name, order_qty, product_id)
# Extract year, month, and day components into separate columns and change to factors
data_ctg <- data_ctg %>%
  mutate(order_datetime = ymd_hms(order_datetime),
         order_year = as.factor(year(order_datetime)),
         order_month = as.factor(month(order_datetime)),
         order_day = as.factor(day(order_datetime)),
         order_monthf = factor(month.abb[order_month], levels = month.abb),
         order_id = as.factor(order_id),
         category_name = as.factor(category_name))
# plot the result
data_ctg_year <- data_ctg %>%
  group_by(category_name) %>%
  summarise(total_order_qty = sum(order_qty)) %>%
  ungroup() %>%
  mutate(category_name = factor(category_name, levels = rev(unique(category_name))))
p.total.sales <- ggplot(data_ctg_year) + 
  geom_col(aes(y = reorder(category_name, total_order_qty), x = total_order_qty), fill = "skyblue") +  
  geom_text(aes(y = reorder(category_name, total_order_qty), x = total_order_qty, label = total_order_qty), hjust = -0.2, size = 3, color = "darkgrey", fontface = "italic") +
  ylab("Category Name") +  
  xlab("Total Sales Quantity") +  
  theme_minimal()  +
  ggtitle("Sales by Category")
p.total.sales

# Distribution of sales of products by category by months in 2023.
data_ctg_month <- data_ctg %>%
  group_by(order_monthf,category_name) %>%
  summarise(total_order_qty = sum(order_qty))  %>%
  arrange(desc(total_order_qty))
color_palette <- c("brown","orange", 'lightgreen', "lightblue", "lightgrey", "darkgrey")
p.total.sales.month <- ggplot(data_ctg_month) + 
  geom_bar(aes(x = total_order_qty, y = order_monthf, fill = category_name), stat = "identity") +
  scale_fill_manual(values = color_palette) +
  ylab("Month") +
  xlab("Total Sales Quantity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Monthly Sales by Category")
p.total.sales.month

# Total customers by month in 2023.
# create new table with inner join function
data_orders <- inner_join(order_details_table, users_table, by = "user_id") %>%
  mutate(order_id = paste(order_detail_id, product_id, user_id, sep = "")) %>%
  select(order_id, user_id, order_datetime, order_price, order_qty, product_id, user_membership_status)
# Extract year, month, and day components into separate columns and change to factors
data_orders <- data_orders %>%
  mutate(order_datetime = ymd_hms(order_datetime),
         order_year = as.factor(year(order_datetime)),
         order_monthf = factor(month.abb[month(order_datetime)], levels = month.abb),
         order_day = as.factor(day(order_datetime)),
         order_id = as.factor(order_id))
# calculate the user number by month
total_unique_users <- data_orders %>%
  distinct(user_id, .keep_all = TRUE) %>%
  group_by(order_monthf) %>%
  summarise(total_unique_users = n_distinct(user_id))
# calculate the user with 'prime' by month
prime_unique_users <- data_orders %>%
  filter(user_membership_status == 'Prime') %>%
  distinct(user_id, .keep_all = TRUE) %>%
  group_by(order_monthf) %>%
  summarise(prime_unique_users = n_distinct(user_id))
# calculate the user status by month
unique_users_by_status <- data_orders %>%
  distinct(user_id, .keep_all = TRUE) %>%
  group_by(order_monthf, user_membership_status) %>%
  summarise(user_count = n_distinct(user_id))
# plot
color_palette <- c("lightblue","orange")
p.total.customers <- ggplot() +
  geom_bar(data = unique_users_by_status, aes(x = order_monthf, y = user_count, fill = user_membership_status), stat = "identity", position = "stack") +
  scale_fill_manual(values = color_palette) +
  geom_text(data = total_unique_users, aes(x = order_monthf, y = total_unique_users, label = total_unique_users), vjust = -0.5, color = "darkgrey", size = 3, fontface = 'italic') +  
  geom_line(data = total_unique_users, aes(x = order_monthf, y = total_unique_users, group = 1, color = "Total Unique Users"), linetype = "solid", size = 0.5) +  
  geom_line(data = prime_unique_users, aes(x = order_monthf, y = prime_unique_users, group = 1, color = "Prime Unique Users"), linetype = "solid", size = 0.2) +  
  xlab("Order Month") +
  ylab("Unique User Count") +
  theme_minimal() +
  scale_fill_manual(values = color_palette) +
  scale_color_manual(values = c("Total Users" = 'grey', "Prime Users" = "red")) +
  ggtitle("User Distribution by Membership Status by Month") +
  guides(fill = guide_legend(title = "Membership Status"), linetype = guide_legend(title = "Orders Count"))
p.total.customers

# The order frequency and price of customers by month.
# calculate the order number by month by membership
data_member <- data_orders %>%
  group_by(user_membership_status,order_id,order_year,order_monthf) %>%
  summarise(total_order = n(), total_price = sum(order_price)) 
# calculate the order number by month
total_orders_by_month <- data_member %>%
  group_by(order_monthf) %>%
  summarise(total_orders = n()) 
# plot
p.order.qty <- ggplot(data_member) + 
  geom_bar(aes(x = order_monthf, fill = user_membership_status)) + 
  labs(x = "Month", y = "Number of Orders", fill = "Membership Status") +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  ggtitle("Orders Count by Membership Status by Month") +
  geom_line(data = total_orders_by_month, aes(x = order_monthf, y = total_orders, group = 1, linetype = "Total Orders"), color = "darkgrey", size = 0.5) +
  geom_text(data = total_orders_by_month, aes(x = order_monthf, y = total_orders, label = total_orders), vjust = -0.5, color = "darkgrey", size = 2.5, fontface = 'italic') + 
  scale_linetype_manual(values = c("Total Orders" = "solid")) +
  guides(fill = guide_legend(title = "Membership Status"), linetype = guide_legend(title = "Orders Count"))
data_member_avg <- data_member %>%
  group_by(user_membership_status,order_year,order_monthf) %>%
  summarise(avg_price = mean(total_price)) 
p.order.price <- ggplot(data_member_avg) +
  geom_point(aes(x = order_monthf, y = avg_price, col = user_membership_status)) +
  geom_line(aes(x = order_monthf, y = avg_price, group = user_membership_status, col = user_membership_status)) +
  labs(x = "Month", y = "Average Total Price", col = "Membership Status") +
  theme_minimal() +
  ggtitle("Orders Price by Membership Status by Month")
p.order.qty
p.order.price

# Regional Distribution of Orders
# create new table with inner join function
data_locations <- inner_join(order_details_table, users_table, by = "user_id") %>%
  mutate(order_id = paste(order_detail_id, product_id, user_id, sep = "")) %>%
  select(order_id, address_state, user_id, order_datetime, order_price, order_qty, product_id)
# Extract year, month, and day components into separate columns and change to factors
data_locations <- data_locations %>%
  mutate(order_datetime = ymd_hms(order_datetime),
         order_year = as.factor(year(order_datetime)),
         order_monthf = factor(month.abb[month(order_datetime)], levels = month.abb),
         order_day = as.factor(day(order_datetime)),
         order_id = as.factor(order_id))
# plot the result
data_state <- data_locations %>%
  group_by(user_id,address_state) %>%
  summarise(total_order_qty = sum(order_qty), total_price = sum(order_price)) 
data_state$user_id <- as.factor(data_state$user_id)
data_state_count <- data_state %>% group_by(address_state) %>% count(address_state) %>% arrange(desc(n)) %>% head(10)
data_state_count$address_state <- as.factor(data_state_count$address_state)
p.region <- ggplot(data_state_count) + 
  geom_bar(aes(x = reorder(address_state, n), y = n), stat = "identity") +
  geom_text(aes(x = reorder(address_state, n), y = n, label = n), vjust = 0.5, hjust = -0.1, size = 2.5, color = 'black', fontface = 'italic') +
  labs(x = "State", y = "The number of customers") + 
  coord_flip() +
  theme_minimal() +
  ggtitle("TOP 10 Deliveried States")
p.region