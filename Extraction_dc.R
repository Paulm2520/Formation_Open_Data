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
