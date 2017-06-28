######################################################################################

# API CITYBIK (datos de sistemas de bicis publicas en el mundo)

######################################################################################


#  libraries
library(jsonlite)
library(dplyr)
library(RSQLite)
setwd("/home/albertogonzalez/Desktop/bestiario/Quadrigram/2016/monografias/bikes")

# Open DB connection

con <- dbConnect(SQLite(),dbname="bikes.sqlite3")
my_db <- src_sqlite( "bikes.sqlite3", create = FALSE)



# Function to get data

foo = function(url,id){
  input = as.character(url)
  read_input = jsonlite::fromJSON(input, simplifyDataFrame = TRUE)
  input_df_2 = read_input$network
  input_df_3 = input_df_2$stations
  input_df = input_df_3[c(1,3:8)]
  input_df$company_id = as.character(id)
  return (input_df)
}



# LISTA DE CIUDADES SELECCIONADAS + ID. CIUDADES EUROPEAS - 
# HELSINKI, BARCELONA, ISTANBUL, MILAN, PARIS, LONDRES, BUDAPEST, DUBLIN  

######################################################################################

# DUBLIN
u = "http://api.citybik.es/v2/networks/dublinbikes"
i = "dublin"

city_ind = foo(u,i)

head(city_ind)
dim(city_ind)

# Append rows to DB:
db_insert_into( con = my_db$con, table = "bikes_records_2", values = city_ind) # append rows


######################################################################################


######################################################################################

# HELSINKI
u = "http://api.citybik.es/v2/networks/citybikes-helsinki"
i = "helsinki"

city_ind = foo(u,i)

head(city_ind)
dim(city_ind)

# Append rows to DB:
db_insert_into( con = my_db$con, table = "bikes_records_2", values = city_ind) # append rows


######################################################################################



######################################################################################

# BARCELONA
u = "http://api.citybik.es/v2/networks/bicing"
i = "barcelona"

city_ind = foo(u,i)

head(city_ind)
dim(city_ind)

# Append rows to DB:
db_insert_into( con = my_db$con, table = "bikes_records_2", values = city_ind) # append rows


######################################################################################


######################################################################################

# ISTANBUL
u = "http://api.citybik.es/v2/networks/baksi-istanbul"
i = "istanbul"

city_ind = foo(u,i)

head(city_ind)
dim(city_ind)

# Append rows to DB:
db_insert_into( con = my_db$con, table = "bikes_records_2", values = city_ind) # append rows


######################################################################################


######################################################################################

# MILAN
u = "http://api.citybik.es/v2/networks/bikemi"
i = "milano"

city_ind = foo(u,i)

head(city_ind)
dim(city_ind)

# Append rows to DB:
db_insert_into( con = my_db$con, table = "bikes_records_2", values = city_ind) # append rows


######################################################################################


######################################################################################

# PARIS
u = "http://api.citybik.es/v2/networks/velib"
i = "paris"

city_ind = foo(u,i)

head(city_ind)
dim(city_ind)

# Append rows to DB:
db_insert_into( con = my_db$con, table = "bikes_records_2", values = city_ind) # append rows


######################################################################################


######################################################################################

# LONDON
u = "http://api.citybik.es/v2/networks/santander-cycles"
i = "london"

city_ind = foo(u,i)

head(city_ind)
dim(city_ind)

# Append rows to DB:
db_insert_into( con = my_db$con, table = "bikes_records_2", values = city_ind) # append rows


######################################################################################


######################################################################################

# BUDAPEST
u = "http://api.citybik.es/v2/networks/bubi"
i = "budapest"

city_ind = foo(u,i)

head(city_ind)
dim(city_ind)

# Append rows to DB:
db_insert_into( con = my_db$con, table = "bikes_records_2", values = city_ind) # append rows


######################################################################################


a = dbGetQuery(con,"SELECT Count(*) FROM bikes_records_2")
a

b = dbGetQuery(con,"SELECT * FROM bikes_records_2")
head(b)
tail(b)
dim(b)

######################################################################################





















