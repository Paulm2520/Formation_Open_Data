library(rvest)
library(XML) 
library(plyr)
library(tidyverse)

#Lecture & enregistrement de la page web
my_url<-"https://www.data.gouv.fr/fr/datasets/election-presidentielle-des-10-et-24-avril-2022-resultats-definitifs-du-1er-tour/"
my_html<-read_html(my_url)

#my_html<-read_html("C:/Users/28717898/Desktop/Election présidentielle des 10 et 24 avril 2022 - Résultats définitifs du 1er tour - data.gouv.fr.html")

#on cherche dans la page les balises a et href carractéristiques des liens ()
list_url<-my_html%>%html_elements("a")%>%html_attr("href")

#toutes les balises a
liste_a<-html_elements(my_html,"a")
#Balises 'href' (=lien url)
list_urls<-html_attr(liste_a,"href")

csv_urls<-grep("\\.csv",list_urls,v=T)

csv_list<-strsplit(csv_urls,"/")
csv_list_names<-do.call(rbind.data.frame,csv_list)[,6]

lapply(seq_along(csv_urls),
       function(x)download.file(csv_urls[x],csv_list_names[x],mode="wb"))

#data/csv_list_names[x] pour enregistrer dans un dossier data
datasets<-lapply(csv_list_names,read.csv,sep=",",colClasses="character")
lapply(datasets,head)[1:3]

#do.call(rbind.data.frame,datasets)
#bind_rows
