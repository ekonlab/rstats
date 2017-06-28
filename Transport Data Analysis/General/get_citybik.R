######################################################################################

# API CITYBIK (datos de sistemas de bicis publicas en el mundo)

######################################################################################

# 1.- Get list of bikes system
# 2.- For each system, get data (build DB and cron job)
# 3.- Read DB with stored data and calculate metrics
# 4.- Build the visual interface


# 1.- Get list of bikes system

library(jsonlite)

setwd("/home/albertogonzalez/Desktop/bestiario/Quadrigram/2016/monografias/bikes")

network_endpoint = "http://api.citybik.es/v2/networks"
network_endpoint_df <- jsonlite::fromJSON(network_endpoint, simplifyDataFrame = TRUE)

company_names = network_endpoint_df$networks
str(company_names)
head(company_names)

company_ids = company_names$id
company_data = company_names$location
company_url = company_names$href

suffix = "http://api.citybik.es"
company_url = paste(suffix,company_names$href,sep="")

company_df = cbind(company_ids,company_data,company_url)
head(company_df)

# Export list of companies
write.csv(company_df,file = "list_of_bikes_system_worldwide.csv",row.names = FALSE)

######################################################################################


# 2.- For each system, get data (build DB and cron job)

# Build a function to read, parse and store endpoint data
head(company_df)

bike_input = "http://api.citybik.es/v2/networks/wroclawski-rower-miejski"
bike_input_df = jsonlite::fromJSON(bike_input, simplifyDataFrame = TRUE)
bike_input_df_2 = bike_input_df$network
bike_input_df_3 = bike_input_df_2$stations
bike_df = bike_input_df_3[c(1,3:8)]
total_slots = bike_input_df_3$extra$slots
bike_df$total_slots = total_slots
head(bike_df)


head(company_df)

# Get id + url
input_array = company_df[c(1,6)]
head(input_array)

array_id = input_array$company_ids
array_id = as.data.frame(array_id)
array_url = input_array$company_url
array_url = as.data.frame(array_url)


foo = function(url,id){
  input = as.character(url)
  read_input = jsonlite::fromJSON(input, simplifyDataFrame = TRUE)
  input_df_2 = read_input$network
  input_df_3 = input_df_2$stations
  input_df = input_df_3[c(1,3:8)]
  #total_slots_df = input_df_3$extra$slots
  #input_df$total_slots = total_slots_df
  input_df$company_id = as.character(id)
  return (input_df)
}


# Test fuction for one:
aa = array_url[2, ] 
a = foo(aa,"bole")
head(a)

# Apply function to all the bike systems

u = array_url[1:452,]
i = array_id[1:452,]

foo_all <- lapply(u,foo,i)

################################################################################


# LISTA DE CIUDADES SELECCIONADAS + ID. CIUDADES EUROPEAS - 
# HELSINKI, BARCELONA, ISTANBUL, MILAN, PARIS, LONDRES, BUDAPEST, DUBLIN  

# Pruebas con las ciudades seleccionadas:

u = "http://api.citybik.es/v2/networks/dublinbikes"
i = "dublin"

city_ind = foo(u,i)

head(city_ind)
dim(city_ind)

################################################################################


# Create a DB (sqlite)

require(dplyr)

setwd("/home/albertogonzalez/Desktop/bestiario/Quadrigram/2016/monografias/bikes")

# Create DB & Table
my_db <- src_sqlite( "bikes.sqlite3", create = TRUE)   # crear DB          
copy_to( my_db, city_ind, "bikes_records_2", temporary = FALSE)   # crear tabla        

# Insert data
#db_insert_into( con = my_db$con, table = "bikes_records", values = city_ind) # append rows


################################################################################










 




