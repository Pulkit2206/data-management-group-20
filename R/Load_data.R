library(readr)
library(RSQLite)

mock_data <- readr::read_csv("data_upload/final_ecommerce.csv")
my_connection<-RSQLite::dbConnect(RSQLite::SQLite(),"database/database.db")
RSQLite::dbWriteTable(my_connection,"customers",mock_data)