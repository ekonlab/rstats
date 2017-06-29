##########################################################################################
# Scraping exhibitions in Paris Q1 2016
##########################################################################################

# http://es.parisinfo.com/donde-salir-por-paris/exposiciones-en-paris/%28show%29/all


install.packages("rvest","XML","RCurl","tm","plyr")


library(rvest)
library(XML)
library(RCurl)
library(tm)
library(plyr)


input_url = read_html("http://es.parisinfo.com/donde-salir-por-paris/exposiciones-en-paris/%28show%29/all")

u = htmlParse(input_url)

titulo =  getNodeSet(u,"//h5[@class='cartoucheTitreCommercial']/a/@title")
titulo = unlist(titulo)
titulo = as.data.frame(titulo)
head(titulo)

enlace =  getNodeSet(u,"//h5[@class='cartoucheTitreCommercial']/a/@href")
enlace = unlist(enlace)
enlace = as.data.frame(enlace)
head(enlace)

#resumen =  getNodeSet(u,"//div[@class='cartoucheListeContenuBars']/div[@class='summary']/text()")
#resumen = unlist(resumen)
#resumen = as.data.frame(resumen)
#head(resumen)

fechas = getNodeSet(u,"//div[@class='cartoucheListeContenuAcces']/p/text()")
fechas = unlist(fechas)



