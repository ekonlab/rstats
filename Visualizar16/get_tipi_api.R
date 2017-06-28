############ API TIPI CIUDADANO ####################

# URL DE REFERENCIA: 

# ENDPOINT DE INICIATIVAS POR TOPIC: http://tipiciudadano.es/api/v1/stats/latest/infancia
# ENDPOINT DE INICIATIVAS POR DIPUTADO: http://tipiciudadano.es/api/v1/stats/bydeputies
# ENDPOINT DE INICIATIVAS POR DICCIONARIO: http://tipiciudadano.es/api/v1/tipis/

# /api/v1/tipis?offset=1&limit=1

library(jsonlite)

# BY TOPIC
url_by_topic = "http://tipiciudadano.es/api/v1/stats/latest/infancia"
by_topic_df <- jsonlite::fromJSON(url_by_topic, simplifyDataFrame = TRUE)

by_topic_df_1 = by_topic_df$items
head(by_topic_df_1)


# INICIATIVA A NIVEL GENERAL
url_by_dict = "http://tipiciudadano.es/api/v1/tipis?offset=1&limit=1000"
by_dict_df <- jsonlite::fromJSON(url_by_dict, simplifyDataFrame = TRUE)
head(by_dict_df)

##########################################################
# take one: just a tipi per initiative
##########################################################

# GET NODES (INITIATIVES, GROUPS AND TOPICS )
ini_id = as.data.frame(by_dict_df$ref)

ini_grupo = by_dict_df$autor_grupo
ini_grupo = sub("^$", "N", ini_grupo)
ini_grupo = as.data.frame(ini_grupo)

ini_tipi = as.data.frame(by_dict_df$dicts)
ini_tipi = sub("^$", "N", ini_tipi$tipi)

ini_df = cbind(ini_id,ini_tipi,ini_grupo)
names(ini_df) = c("id","tipi","grupo")
head(ini_df)
str(ini_df)


# SPLIT DE TIPI LIST PARA ASIGNAR ID A VARIOS TIPIS

library(dplyr)
library(tidyr)
library(stringr)

# SPLIT
df_5 = separate(ini_df,"tipi", sep = ",",into = c("a","b","c","d","e"),fill = "warn")
head(df_5)

# CLEAN BAD CHARACTERS IN TIPIS

# COLUMN A (first tipi)
c_out = gsub("c[:(:]","",df_5$a)
c_out = as.data.frame(c_out)
head(c_out)
table(c_out$c_out)

c_out_2 = gsub("\"","",c_out$c_out,fixed = TRUE)
c_out_2 = as.data.frame(c_out_2)
table(c_out_2$c_out_2)
names(c_out_2) = c("a")
str(c_out_2)

# COLUMN B
col_b = gsub("[:):]","",df_5$b)
col_b = as.data.frame(col_b)
col_b_2 = gsub("\"","",col_b$col_b,fixed = TRUE)
col_b_2 = as.data.frame(col_b_2)

# COLUMN C
col_c = gsub("[:):]","",df_5$c)
col_c = as.data.frame(col_c)
col_c_2 = gsub("\"","",col_c$col_c,fixed = TRUE)
col_c_2 = as.data.frame(col_c_2)

# COLUMN D
col_d = gsub("[:):]","",df_5$d)
col_d = as.data.frame(col_d)
col_d_2 = gsub("\"","",col_d$col_d,fixed = TRUE)
col_d_2 = as.data.frame(col_d_2)

# COLUMN E
col_e = gsub("[:):]","",df_5$e)
col_e = as.data.frame(col_e)
col_e_2 = gsub("\"","",col_e$col_e,fixed = TRUE)
col_e_2 = as.data.frame(col_e_2)

# BUILD A DATA FRAME WITH IDS, TIPIS & GRUPOS
head(ini_df,40)
str(ini_df)

final_df_id = ini_df[1]
final_df_tipis = cbind(c_out_2,col_b_2,col_c_2,col_d_2,col_e_2)
names(final_df_tipis) = c("a","b","c","d","e")
head(final_df_tipis)
final_df_group = ini_df[3]

final_df = cbind(final_df_id,final_df_group,final_df_tipis)
head(final_df,10)

# FROM WIDE TO LONG
# formula = (data,key,value,columns_to_gather)
final_df_long = gather(final_df, "tipi", "tipi_name", 3:7)
head(final_df_long,10)

# SANITY CHECK
f = subset(final_df_long,id =="184/000348")
f

# CLEAN FINAL DF
final_df_clean = final_df_long[c("id","grupo","tipi_name")]
final_df_superclean = subset(final_df_clean,tipi_name != "character(0)")
head(final_df_superclean)

f_2 = subset(final_df_superclean,id =="184/000348")
f_2

# PAIR TABLE OF_ INITIATIVE, TIPI AND GROUP
# TABLES NEEDED: ID Vs TIPI, ID Vs GRUPO Y TIPI Vs GRUPO
# TABLE ID & TIPI
id_tipi = final_df_superclean[c("id","tipi_name")]
names(id_tipi) = c("Origin","Destination")
head(id_tipi)

# TABLE ID & GRUPO
id_grupo = final_df_superclean[c("id","grupo")]
names(id_grupo) = c("Origin","Destination")
head(id_grupo)

# TABLE TIPI & GRUPO
tipi_grupo = final_df_superclean[c("tipi_name","grupo")]
names(tipi_grupo) = c("Origin","Destination")
head(tipi_grupo)

# RBIND TO BUILD THE FINAL PAIR TABLE
final_pair_table = rbind(id_tipi,id_grupo,tipi_grupo)
head(final_pair_table,20)
str(final_pair_table)

# PAIR TABLE JUST INITIATIVES AND TIPIS
final_pair_table_2 = id_tipi
head(final_pair_table_2)


###########################################################

# PROPERTIES TABLE
head(final_pair_table_2)
str(final_pair_table_2)

# INITIATIVES
uni_id = unique(as.character(final_pair_table_2$Origin))
uni_id = as.data.frame(uni_id)
names(uni_id) = c("node_id")
uni_id_length = length(uni_id$node_id)
uni_id_label = rep("ini",uni_id_length)

initiatives_df = cbind(uni_id,uni_id_label)
names(initiatives_df) = c("node","family")
head(initiatives_df)

# TIPIS
uni_tipi = unique(as.character(final_pair_table_2$Destination))
uni_tipi = as.data.frame(uni_tipi)
names(uni_tipi) = c("node_id")
uni_tipi_length = length(uni_tipi$node_id)
uni_tipi_label = rep("tipi",uni_tipi_length)

tipis_df = cbind(uni_tipi,uni_tipi_label)
names(tipis_df) = c("node","family")
head(tipis_df)

# MERGE BOTH
properties_table = rbind(initiatives_df,tipis_df)
head(properties_table)

# EXPORT TABLES
setwd("~/Desktop/visualizar16")
write.csv(properties_table,file = "tipi_properties_table.csv",row.names = FALSE)
write.csv(final_pair_table_2,file = "tipi_pair_table.csv",row.names = FALSE)

########################################################


















