##################################################################################

##################################################################################

library(jsonlite)
library(dplyr)
library(RSQLite)
library(lubridate)
library(tidyr)
library(ggmap)
library(ggplot2)
library(ggthemes)

##################################################################################

setwd("/home/albertogonzalez/Desktop/bestiario/Quadrigram/2016/monografias/bikes")

# Open connection
con <- dbConnect(SQLite(),dbname="bikes.sqlite3")
my_db <- src_sqlite( "bikes.sqlite3", create = FALSE)

# Get all data
all_data = dbGetQuery(con,"SELECT * FROM bikes_records_2")
head(all_data)
str(all_data)

# Insert dates columns
#all_data$timestamp = as.Date(all_data$timestamp)
all_data$time_2 = ymd_hms(all_data$timestamp)
all_data$week_day = wday(all_data$time_2,label = TRUE)
all_data$hour = hour(all_data$time_2)
all_data$shift = cut(all_data$hour,c(0,7,15,23),c("night","morning","afternoon"))


# Calculo de total slots y  de Medidas de capacidad
all_data$total_slots = all_data$empty_slots + all_data$free_bikes

all_data$empty_status = (all_data$empty_slots / all_data$total_slots) * 100
all_data$empty_status_label = cut(all_data$empty_status,c(0,25,75,100),c("overflow","balanced","shortage"))

all_data$empty_status_label[all_data$empty_status==0] <- "overflow"

##################################################################################

# One City

# list of available cities:

head(all_data)
table(all_data$company_id)


ci = "london"

ci_df = all_data %>%
  filter(company_id == ci)

head(ci_df)
dim(ci_df)

ci_df = na.omit(ci_df)
dim(ci_df)

##################################################################################

## Group by weekday, hour and station - mean de used_capacity
ci_df_2 = ci_df %>%
  group_by(week_day,hour,name) %>%
  summarise(avg_used_capacity = mean(empty_status, na.rm=TRUE)) %>%
  arrange(desc(avg_used_capacity))


ci_df_ref = as.data.frame(ci_df_2)
head(ci_df_ref)


# Top and bottom stations
top_stations_weekday = ci_df_ref[c(1:250),]
bottom_stations_weekday = tail(ci_df_ref,250)


# Build wide matrix with stations ids as rows and hours as columns
# ci_df_wide = spread(ci_df_ref,hour,avg_used_capacity)
# head(ci_df_wide)

#ci_df_wide_m = data.matrix(ci_df_wide[2:23])

#h = heatmap(ci_df_wide_m)


## Group by station - mean de used_capacity
ci_df_3 = ci_df %>%
  group_by(name) %>%
  summarise(avg_used_capacity = mean(empty_status, na.rm=TRUE)) %>%
  arrange(desc(avg_used_capacity))

ci_df_3_ref = as.data.frame(ci_df_3)

# Top and Bottom stations
top_stations = ci_df_3_ref[c(1:50),]
bottom_stations = tail(ci_df_3_ref,50)

top_stations
bottom_stations

##################################################################################

## Geolocate main stations
## Geo table
ci_df_geo = ci_df[,c("name","latitude","longitude")] 
ci_df_geo_uni = unique(ci_df_geo)

top_stations_geo = merge(top_stations,ci_df_geo_uni,by = "name")
bottom_stations_geo = merge(bottom_stations,ci_df_geo_uni,by = "name")

top_stations_geo


m <- get_map("London",zoom=12,maptype="terrain-lines",source="stamen")

ggmap(m, base_layer = ggplot(aes(x = longitude, y = latitude), data = top_stations_geo))  + geom_point(color="green") + theme_tufte() + ggtitle("Top Stations")
ggmap(m, base_layer = ggplot(aes(x = longitude, y = latitude), data = bottom_stations_geo))  + geom_point(color="red") + theme_tufte() + ggtitle("Bottom Stations")


top_stations_weekday_geo = merge(top_stations_weekday,ci_df_geo_uni,by = "name")
bottom_stations_weekday_geo = merge(bottom_stations_weekday,ci_df_geo_uni,by = "name")

ggmap(m, base_layer = ggplot(aes(x = longitude, y = latitude), data = top_stations_weekday_geo))  + facet_grid(week_day ~ hour) + geom_point(color="green") + theme_tufte() + ggtitle("Top Stations by weekday and hour")
ggmap(m, base_layer = ggplot(aes(x = longitude, y = latitude), data = bottom_stations_weekday_geo))  + facet_grid(week_day ~ hour) + geom_point(color="red") + theme_tufte() + ggtitle("Bottom Stations by weekday and hour")


head(ci_df_3)
ci_df_3_geo = merge(ci_df_3,ci_df_geo_uni,by = "name")
head(ci_df_3_geo)

colnames(ci_df_3_geo) = c("name","avgcapacity","latitude","longitude")


t2 = ggmap(m,extent = "panel") + geom_point(data=ci_df_3_geo,aes(longitude,latitude,size = avgcapacity,alpha = 0.1)) + theme_tufte()
t2


##################################################################################

## Suma de empty slots = numero total de bicis en circulación
circulando = ci_df %>%
  group_by(hour) %>%
  summarise(bicis_circulando = sum(empty_slots, na.rm=TRUE)) %>%
  arrange(desc(bicis_circulando))

circulando_df = as.data.frame(circulando)
circulando_df$city = ci
head(circulando_df)

cir_plot = ggplot(circulando_df,aes(as.factor(hour),bicis_circulando)) + geom_point() + theme_tufte() + ggtitle("Total Bikes in movement by hour")
cir_plot 

##################################################################################

# Incorporación de las poblaciones en las ciudades
# lista de ciudades
cities = as.data.frame(unique(all_data$company_id))
str(cities)
names(cities) = c("city")
cities_pop = as.data.frame(c(553165,630752,1604555,14025646,1347707,2249975,8673713,1759407))
names(cities_pop) = "population"
cities_df = cbind(cities,cities_pop)
head(cities_df)

ggplot(cities_df,aes(city,population)) + geom_point() + theme_tufte()

##################################################################################

# En circulacion como % o tanto por mil habitantes
ci
cities_df

cities_df_pop = merge(circulando_df,cities_df,by = "city",all.y = FALSE)
cities_df_pop$popu_mil = cities_df_pop$population/1000
cities_df_pop$per_mil_capita = cities_df_pop$bicis_circulando / cities_df_pop$popu_mil

cities_df_pop

ggplot(cities_df_pop,aes(hour,per_mil_capita)) + geom_point() + theme_tufte() + ggtitle("Bikes per mil capita and hour")

##################################################################################

# Average capacity used per hour
ave_per_hour = ci_df %>%
  group_by(hour) %>%
  summarise(ave_cap_h = mean(empty_status,na.rm = TRUE)) %>%
  arrange(desc(ave_cap_h))

ave_per_hour = as.data.frame(ave_per_hour)

cities_df_ave = merge(cities_df_pop,ave_per_hour,by = "hour")

ggplot(cities_df_ave,aes(ave_cap_h,per_mil_capita)) + geom_point() + geom_smooth(method = lm) + theme_tufte() + ggtitle("Bikes per mil capita and Average used capacity")

# Summary
cities_df_ave
top_stations_geo
bottom_stations_geo
head(ci_df_wide,25)
head(ci_df)


############################################################################


############################################################################


#---------- Model 1 - overflow prediction - KNN - ------------- #

df1 = ci_df[c("empty_slots","free_bikes","latitude","longitude","hour","empty_status","empty_status_label","id","name","shift","week_day")]
head(df1)
df1$empty_status = as.integer(df1$empty_status)
df1 = na.omit(df1)
str(df1)
dim(df1)

# overflow table
table(df1$empty_status_label)

# normalize function
normalize = function(x){
  return ((x - min(x))/ (max(x) - min(x)))
}

df1_norm = as.data.frame(lapply(df1[1:5], normalize))
head(df1_norm)

# Split & model
n = nrow(df1_norm)

train = sample(1:n, size = round(0.7*n), replace=FALSE)

df1_norm_train = df1_norm[train,]
dim(df1_norm_train)

df1_norm_test = df1_norm[-train,]
dim(df1_norm_test)

df1_train_labels = df1$empty_status_label[train]
df1_test_labels = df1$empty_status_label[-train]

# Training the model
library(class)
library(gmodels)

# df1_test_pred = knn(df1_norm_train,df1_norm_test,cl = df1_train_labels, k = 3)
# CrossTable(df1_test_labels,df1_test_pred)

df1_test_pred_2 = knn(df1_norm_train,df1_norm_test,cl = df1_train_labels, k = 21)
CrossTable(df1_test_labels,df1_test_pred_2)


df1_test = df1[-train,]
df1_test$label_pred = df1_test_pred_2
head(df1_test,25)

table(df1_test$empty_status_label,df1_test$label_pred)


df1_test$pred = ifelse(df1_test$empty_status_label==df1_test$label_pred,1,0)
head(df1_test)
  
df1_test_wrong = df1_test[df1_test$pred == 0, ]
head(df1_test_wrong)
dim(df1_test_wrong)


# Select a given ID and view its prediction
head(df1_test)

# Generate a vector with the ID/Names list
places = unique(df1_test$id)
length(places)
places[1:10]

places_names = unique(df1_test$name)
places_names[1:10]

input_place = "217 - C/ RECTOR UBACH, 24"
input_hour = 10
input_weekday = "Mon"

selected_place = filter(df1_test,name == input_place & hour == input_hour & week_day == input_weekday)

table(selected_place$empty_status_label,selected_place$label_pred)
selected_place




##################################################################################
