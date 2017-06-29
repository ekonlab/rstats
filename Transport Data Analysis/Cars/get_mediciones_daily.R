##-------- GET MEDICIONES DE TRAFICO DE SANTANDER AND SAVE THEM IN SQLITE ----------###

library(jsonlite)
library(plyr)
library(dplyr)
library(RSQLite)

## Mediciones

# Endpoint de mediciones de trafico: http://datos.santander.es/api/rest/datasets/mediciones.json

#####---------------------------------------------------#####

# GET DATA MEDICIONES
ur = "http://datos.santander.es/api/rest/datasets/mediciones.json?items=416"
mydf = fromJSON(ur)
str(mydf)
head(mydf$resources)
df = mydf$resources
df[3] = NULL
str(df)

# SAVE DATA
setwd("/home/albertogonzalez/Desktop/TDA/chapters/cars")

con <- dbConnect(SQLite(),dbname="santander.sqlite3")

my_db <- src_sqlite( "santander.sqlite3", create = FALSE)

db_insert_into( con = my_db$con, table = "mediciones", values = df) # append rows


a = dbGetQuery(con,"SELECT Count(*) FROM mediciones")
a




#####---------------------------------------------------#####

# dbListTables(con)
# 
# # GET DATA SENSORES RIEGO
# 
# ur_sensor = "http://datos.santander.es/api/rest/datasets/sensores_smart_irrigation.json?items=33"
# 
# mydf_sensor = fromJSON(ur_sensor)
# str(mydf_sensor)
# head(mydf_sensor$resources)
# df_sensor = mydf_sensor$resources
# 
# db_insert_into( con = my_db$con, table = "sensores_riego", values = df_sensor) # append rows
# 
# 
# b = dbGetQuery(con,"SELECT Count(*) FROM sensores_riego")
# b
# 
# 
# #####---------------------------------------------------#####
# 
# # GET DATA SENSORES AMBIENTALES
# ur_ambi = "http://datos.santander.es/api/rest/datasets/sensores_smart_env_monitoring.json?items=50&page=1"
# 
# 
# foo = function(ambi_url){
#   mydf_ambi = fromJSON(ambi_url)
#   df_ambi = mydf_ambi$resources
#   return(df_ambi)
# }
# 
# try_one = foo(ur_ambi)
# head(try_one)
# 
# # Range
# ra = seq(1:14)
# ra
# ra_ur = paste("http://datos.santander.es/api/rest/datasets/sensores_smart_env_monitoring.json?items=50&page=",ra,sep="")
# ra_ur
# 
# foo_all = lapply(ra_ur,foo)
# df_ambi <- ldply(foo_all, data.frame)
# 
# 
# 
# db_insert_into( con = my_db$con, table = "sensores_ambientales", values = df_ambi) # append rows
# 
# 
# c = dbGetQuery(con,"SELECT Count(*) FROM sensores_ambientales")
# c


#####---------------------------------------------------#####































