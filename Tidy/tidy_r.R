##############################################
# https://rpubs.com/justmarkham/dplyr-tutorial
##############################################

# libraries
library(ggvis)
library(dplyr)
library(hflights)
library(RSQLite)
library(nycflights13)

data(hflights)
head(hflights)

flights <- tbl_df(hflights)

# table_df
flights <- tbl_df(hflights)
flights

# filter
# base R approach to view all flights on January 1
flights[flights$Month==1 & flights$DayofMonth==1,]

filter(flights, Month==1, DayofMonth==1)
filter(flights, UniqueCarrier=="AA" | UniqueCarrier=="UA")
filter(flights, UniqueCarrier %in% c("AA", "UA"))

# select
flights[, c("DepTime", "ArrTime", "FlightNum")]

select(flights, DepTime, ArrTime, FlightNum)
select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))

# pipeling
filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)

flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60)

# arrange
flights[order(flights$DepDelay), c("UniqueCarrier", "DepDelay")]

flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(desc(DepDelay))
          
# mutate (add new vars)
flights$Speed <- flights$Distance / flights$AirTime*60

flights[, c("Distance", "AirTime", "Speed")]

flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)

# summarise (reduce vars to vals)
head(with(flights, tapply(ArrDelay, Dest, mean, na.rm=TRUE)))

head(aggregate(ArrDelay ~ Dest, flights, mean))

flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE)) %>%
  arrange(desc(avg_delay))


flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean), Cancelled, Diverted)

flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE)), matches("Delay"))

flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>%
  arrange(desc(flight_count))

flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort = TRUE)

flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(TailNum))

flights %>%
  group_by(Dest) %>%
  select(Cancelled) %>%
  table() %>%
  head()

# One way to build pivot tables with groupby + summarise
flights %>% 
  group_by(UniqueCarrier) %>%
  summarise(ave.dely=mean(DepDelay),mean.dis=mean(Distance)) %>%
  arrange(desc(mean.dis))


# window functions
flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  filter(min_rank(desc(DepDelay)) <= 2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

flights %>%
  group_by(Month) %>%
  summarise(flight_count = n()) %>%
  mutate(change = flight_count - lag(flight_count))


# sample
flights %>% sample_n(5)
flights %>% sample_frac(0.25, replace=TRUE)
glimpse(flights)


# databases

# http://cran.r-project.org/web/packages/dplyr/vignettes/databases.html
my_db <- src_sqlite("my_db.sqlite3", create = T)
my_db
data(flights)
flights_sqlite <- copy_to(my_db, flights, temporary = FALSE, indexes = list(c("year", "month", "day"), "carrier", "tailnum"))
flights_sqlite <- tbl(nycflights13_sqlite(), "flights")
flights_sqlite


a = select(flights_sqlite, year:day, dep_delay, arr_delay)
a$query

b = filter(flights_sqlite, dep_delay > 240)
b$query

c = arrange(flights_sqlite, year, month, day)
explain(c)

mutate(flights_sqlite, speed = air_time / distance)
summarise(flights_sqlite, delay = mean(dep_time))

translate_sql(x == 1 && (y < 2 || z > 3))

flights_tbl <- tbl(my_db, "flights")
flights_tbl
flights_tbl %>%
  select(carrier,dep_delay) %>%
  arrange(desc(dep_delay)) %>%
  explain()



#------------------------------------------------------------#

# ggvis
# http://ggvis.rstudio.com/ggvis-basics.html
library(ggvis)
p <- ggvis(mtcars, x = ~wt, y = ~mpg)
layer_points(p)
layer_points(ggvis(mtcars, x = ~wt, y = ~mpg))
mtcars %>%
  ggvis(x = ~wt, y = ~mpg) %>%
  layer_points()

mtcars %>%
  ggvis(x = ~mpg, y = ~disp) %>%
  mutate(disp = disp / 61.0237) %>% # convert engine displacment to litres
  layer_points()

mtcars %>% ggvis(~mpg, ~disp, stroke = ~vs) %>% layer_points()
mtcars %>% ggvis(~mpg, ~disp, fill = ~vs) %>% layer_points()
mtcars %>% ggvis(~mpg, ~disp, size = ~vs) %>% layer_points()
mtcars %>% ggvis(~mpg, ~disp, shape = ~factor(cyl)) %>% layer_points()

mtcars %>% 
  ggvis(~wt, ~mpg, 
        size := input_slider(10, 100),
        opacity := input_slider(0, 1)
  ) %>% 
  layer_points()

#------------------------------------------------------------#


#########################################################################################################

# TIDYR CHEATSHEET SUMMARY
# http://www.gis-blog.com/data-management-with-r-tidyr-part-1/

library(EDAWR)
library(tidyr)

# INITIAL DATASETS
storms
cases
pollution

# RESHAPING DATA (GATHER, SPREAD, SEPARATE & UNITE)

# GATHER = WHEN THERE IS COLUMNS WHICH ARE NOT VARIABLES (FROM WIDE TO LONG)
# formula = (data,key,value,columns_to_gather)
tidy.cases = gather(cases, "year", "n", 2:4)
tidy.cases

# SPREAD = TAKES DATA THAT IS IN KEY VALUE FORMAT AND RETURNS A RECTANGULAR CELL FORMAT (LONG TO WIDE) 
# formula = (data,key,value)
tidy.pollution <- spread(pollution, size, amount)
tidy.pollution
pollution

# SEPARATE = SPLITS A COLUMNS INTO MULTIPLE COLUMNS USING A SEPARATOR 
# formula = (data,column_to_spread,names_new_columns,separator)
storms
storms.sep <- separate(storms, date, c("year", "month", "day"), sep="-")
storms.sep

# UNITE = GATHERS MULTIPLE COLUMNS INTO A SINGLE COLUMN
# formula = (data,column_to_add,columns_to_unite,separator)
unite(storms.sep, "date" , 4:6 , sep ="-")

#########################################################################################################
#########################################################################################################





