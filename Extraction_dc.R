library(rvest)
library(XML) 
library(plyr)
library(tidyverse)

#Lecture & enregistrement de la page web
my_url<-#Lecture & enregistrement de la page web
my_url<-"https://www.data.gouv.fr/fr/datasets/fichier-des-personnes-decedees/"
my_html<-read_html(my_url)

#Extraction des urls présentes sur la page (avec balise a et href)
list_urls<-my_html%>%html_elements("a")%>%html_attr("href")

csv_urls<-grep("/fichier-des-personnes-decedees",list_urls,v=T)
csv_urls<-grep("2023-m",list_urls,v=T)

csv_list<-strsplit(csv_urls,"/")
csv_list_names<-do.call(rbind.data.frame,csv_list)[,7]


read_fwf("data/dc/deces-2023.txt",
         fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167),
                       c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC")
                       )
         )
