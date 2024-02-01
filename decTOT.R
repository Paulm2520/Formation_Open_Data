library(tidyverse)
library(ggplot2)
library(lubridate)
mydatax%>%filter(ADEC%in%1990:2022&DEPDEC!="99"&AGEA_DC>0)%>%nrow
#mydata<-rbind(read.fwf("data/dc/deces-2022-t4.txt", widths=c(80, 1,4,2,2,2,3,30,30,4,2,2,2,3,9),colClasses = "character",
#                       col.names = c("NOM*PRENOM","Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","LIEUNAI","pays_nais","ADEC","MDEC","JDEC","DEPDEC","COMDC","numero")),
#read.fwf("data/dc/deces-2022-t3.txt", widths=c(80, 1,4,2,2,2,3,30,30,4,2,2,2,3,9),colClasses = "character",
#                       col.names = c("NOM*PRENOM","Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","LIEUNAI","pays_nais","ADEC","MDEC","JDEC","DEPDEC","COMDC","numero")),
#              read.fwf("data/dc/deces-2022-t2.txt", widths=c(80, 1,4,2,2,2,3,30,30,4,2,2,2,3,9),colClasses = "character",
#                       col.names = c("NOM*PRENOM","Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","LIEUNAI","pays_nais","ADEC","MDEC","JDEC","DEPDEC","COMDC","numero")),
#              read.fwf("data/dc/deces-2022-t1.txt", widths=c(80, 1,4,2,2,2,3,30,30,4,2,2,2,3,9),colClasses = "character",
#                       col.names = c("NOM*PRENOM","Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","LIEUNAI","pays_nais","ADEC","MDEC","JDEC","DEPDEC","COMDC","numero")),
#             )

#mydata<-rbind(mydata[,1:15],read.fwf("data/dc/deces-2019-2021.txt", widths=c(80, 1,4,2,2,2,3,30,30,4,2,2,2,3,9),colClasses = "character",
#                                     col.names = c("NOM*PRENOM","Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","LIEUNAI","pays_nais","ADEC","MDEC","JDEC","DEPDEC","COMDC","numero")),
#              read.fwf("data/dc/deces-2016-2018.txt", widths=c(80, 1,4,2,2,2,3,30,30,4,2,2,2,3,9),colClasses = "character",
#                       col.names = c("NOM*PRENOM","Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","LIEUNAI","pays_nais","ADEC","MDEC","JDEC","DEPDEC","COMDC","numero")),
#              read.fwf("data/dc/deces-2013-2015.txt", widths=c(80, 1,4,2,2,2,3,30,30,4,2,2,2,3,9),colClasses = "character",
#                       col.names = c("NOM*PRENOM","Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","LIEUNAI","pays_nais","ADEC","MDEC","JDEC","DEPDEC","COMDC","numero")),
#             )
#supprimer les doublons
#distinct(mydata,.keep_all = T)%>%nrow
#which(duplicated(mydata[,c(1,16,17)]))
#distinct(mydata%>%select(NOM.PRENOM,NAIS,DEC),.keep_all = T)%>%nrow
#45 doublons ou 10? 5793 homonymes?
test<-read_fwf("deces-2022-m12.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                            c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC")))
test2<-read_fwf("deces-2023-m01.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                       c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC")))
rbind(test,test2)
head(test2)
test%>%filter(ADEC==2023&MDEC=="01")%>%nrow
data%>%filter(ADEC==2023&MDEC==01)%>%nrow


mydata<-rbind(read_fwf("data/dc/deces-2019-2021.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2016-2018.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2013-2015.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2022-t4.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                  c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2022-t3.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                  c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2022-t2.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                  c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2022-t1.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                  c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2010-2012.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2007-2009.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2004-2006.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2000-2003.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC")))
)
#nouvelle version
mydata<-rbind(read_fwf("data/dc/deces-2023.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2022.2.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                 c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2019-2021.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2016-2018.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2013-2015.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2010-2012.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2007-2009.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2004-2006.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC"))),
              read_fwf("data/dc/deces-2000-2003.txt", fwf_positions(c(81,82,86,88,90,92,155,159,161,163,165), c(81,85,87,89,91,94,158,160,162,164,167), 
                                                                    c("Sexe","ANAIS","MNAIS","JNAIS","DEPNAI","comunenai","ADEC","MDEC","JDEC","DEPDEC","COMDC")))
)
#suppression des décès avant 2000
#mydata<-mydata%>%filter(ADEC>=2000)

head(mydata)
nrow(mydata)

#ajout date de naissance, date de décès age au décès et age au premier janvier de l'année de décès
mydata<-mydata%>%mutate(NAIS=as.Date(paste0(ANAIS,sep="/",MNAIS,sep="/",JNAIS)))
mydata<-mydata%>%mutate(DEC=as.Date(paste0(ADEC,sep="/",MDEC,sep="/",JDEC)))
mydata$MDC<-month(mydata$DEC,label=T)
mydata<-mydata%>%mutate(AGE_DC=floor(time_length(DEC-NAIS,unit="years")))
mydata<-mydata%>%filter(ANAIS!=0&AGE_DC<123)%>%mutate(AGEA_DC=case_when((ADEC-ANAIS-1)<100~(ADEC-ANAIS-1),(ADEC-ANAIS-1)>=100~99))

mydata<-mydata%>%mutate(CT=1)
mydata<-mydata%>%mutate(AGE_CLASS=case_when(AGE_DC<60~"60-",
                                        AGE_DC>=60&AGE_DC<70~"60-69",
                                        AGE_DC>=70&AGE_DC<80~"70-79",
                                        AGE_DC>=80&AGE_DC<90~"80-99",
                                        AGE_DC>=90~"90+",))
mydata<-mydata%>%mutate(SEXE=case_when(Sexe==1~"M",Sexe==2~"F"))

#Ajout de la région
mydata<-mydata%>%mutate(REGION=case_when(DEPDEC %in% c("01","03","07","15","26","38","42","43","63","69","73","74")~"Auvergne-Rhône-Alpes",
                                         DEPDEC %in% c("21","25","39","58","70","71","89","90")~"Bourgogne-Franche-Comté",
                                         DEPDEC %in% c("22","29","35","56")~"Bretagne",
                                         DEPDEC %in% c("2A","2B")~"Corse",
                                         DEPDEC %in% c("18","28","36","37","41","45")~"Centre-Val-de-Loire",
                                         DEPDEC %in% c("08","10","67","52","68","51","54","55","57","88")~"Grand Est",
                                         DEPDEC %in% c("02","59","60","62","80")~"Hauts-de-France",
                                         DEPDEC %in% c("91","92","75","77","93","94","95","78")~"Île-de-France",
                                         DEPDEC %in% c("14","27","50","61","76")~"Normandie",
                                         DEPDEC %in% c("16","17","19","23","79","24","33","87","40","47","64","86")~"Nouvelle-Aquitaine",
                                         DEPDEC %in% c("09","11","12","30","31","32","65","34","46","48","66","81","82")~"Occitanie",
                                         DEPDEC %in% c("44","49","53","72","85")~"Pays de la Loire",
                                         DEPDEC %in% c("04","06","13","05","83","84")~"Provence-Alpes-Côte d'Azur",
                                         DEPDEC =="99"~"Etranger",
                                         TRUE~"DOMTOM"
))

#Ajout de l'effectif au 1er janvier par age et/ou sexe
#par age et sexe (POPS)
#mydata<-mydata%>%left_join(data3[,1:4],by=c("ADEC"="ANNEE","SEXE","AGEA_DC"="AGE"))
#par age (POP)
#mydata<-mydata%>%left_join(data3%>%group_by(ANNEE,AGE)%>%summarise(POPT=sum(POP)),by=c("ADEC"="ANNEE","AGEA_DC"="AGE"))
head(data3)
#pop moyenne, 2000-2023
data3%>%filter(ANNEE>1999)%>%select(ANNEE,SEXE,AGE,POP)%>%group_by(SEXE,AGE)%>%summarise(sum(POP)/23)%>%tail
data3%>%filter(ANNEE>1999)%>%select(ANNEE,SEXE,AGE,POP)%>%group_by(AGE)%>%summarise(sum(POP)/23)%>%tail
#ajour POPR, de référence tout sexes
mydata<-mydata%>%left_join(data3%>%filter(ANNEE>=2000)%>%select(ANNEE,SEXE,AGE,POP)%>%group_by(AGE)%>%summarise(POPR=sum(POP)/23),
                           by=c("AGEA_DC"="AGE"))
#ajour POPRS, de référence par sexe
mydata<-mydata%>%left_join(data3%>%filter(ANNEE>=2000)%>%select(ANNEE,SEXE,AGE,POP)%>%group_by(SEXE,AGE)%>%summarise(POPRS=sum(POP)/23),
                           by=c("AGEA_DC"="AGE","SEXE"))

head(mydata)
mydata%>%filter(SEXE=="F"&AGEA_DC>=0)%>%group_by(AGEA_DC,DEC)%>%summarise(dcF=sum(CT),popF=max(POPS))%>%mutate(txF=(dcF/popF)*100000)

test<-full_join(mydata%>%filter(SEXE=="F"&AGEA_DC>=0)%>%group_by(AGEA_DC,DEC)%>%summarise(dcF=sum(CT),popF=max(POPS))%>%mutate(txF=(dcF/popF)*100000),
           mydata%>%filter(SEXE=="M"&AGEA_DC>=0)%>%group_by(AGEA_DC,DEC)%>%summarise(dcM=sum(CT),popM=max(POPS))%>%mutate(txM=(dcM/popM)*100000),by=c("DEC","AGEA_DC"))
test[is.na(test)] <- 0
test<-test%>%mutate(dcdiff=dcM-dcF,popdiff=popM-popF,txdiff=txM-txF)
head(test)

#graphique diff de mortalité h/f&ADEC>=2015

test%>%filter(AGEA_DC %in% 0:100)%>%
  ggplot(aes(x = DEC, y = AGEA_DC,fill=(txdiff))) +
  geom_raster(interpolate = T) +
  scale_fill_viridis_c(option = "H",n.breaks=8,limits=c(-25,25)) +
  coord_cartesian(expand=F)+
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y")+
  scale_y_continuous(breaks=seq(0,100,5))+
  labs(title="Différence de mortalité H-F",y="Age",fill="Décès")+
  theme_void()+ 
  theme(
    axis.text.x=element_text(angle=90, hjust = 1,vjust=0.5,color="white",face="bold",
                             margin=margin(t=18,b=8,unit="pt")),
    axis.text.y=element_text(angle=0, hjust = 0,vjust=0.5,size=12,color="white",face="bold",
                             margin=margin(l=4,r=4,unit="pt")),
    plot.title=element_text( hjust = 0.5,color="white", size=35,face="bold",
                             margin=margin(t=5,b=5,unit="pt")),
    axis.title.y = element_text( color="white", size=14, face="bold",angle=0,vjust = 1.03,hjust=3),
    legend.text = element_text(colour="white", size=12,face="bold"),
    legend.title = element_text(colour="white", size=14,face="bold"),
    legend.key.height= unit(2, 'cm'),legend.key.width= unit(1, 'cm'),
    axis.ticks = element_line( color="white",size = 1),axis.ticks.length = unit(.3, "cm"),
    plot.background=element_rect(fill="black"))+ 
  annotate("text", x = as.Date (c(paste0(seq(2000,2022),"-07-02"))),
           y = 0, label = c(seq(2000,2022)),size=6,color="white",hjust = 0.5,vjust=0)+
  geom_vline(xintercept = as.Date (c(paste0(seq(2000,2022),"-01-01"))), linetype="dotted", color = "white", size=1)

ggsave("graphs/diffHF.png",width=50.8,height=28.576,units="cm",dpi=600)

#Graphique mortalité brute,breaks=seq(0,80,10),limits=c(0,80),labels=c("0","10","20","30","40","50","60","70","80+")
mydata%>%filter(AGE_DC %in% 30:99&ADEC>=2015)%>%group_by(DEC,AGE_DC)%>%summarise(ADEC=max(ADEC),dc=sum(CT))%>%
  ggplot(aes(x = DEC, y = AGE_DC,fill=dc)) +
  geom_raster(interpolate = T) +
  scale_fill_viridis_c(option = "H",n.breaks=8,na.value = "#8b0f00") +
  coord_cartesian(expand=F)+
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y")+
  scale_y_continuous(breaks=seq(30,100,5))+
  labs(title="Evolution de la mortalité francaise",y="Age",fill="Décès")+
  theme_void()+ 
  theme(
    axis.text.x=element_text(angle=90, hjust = 1,vjust=0.5,color="white",face="bold",
                             margin=margin(t=18,b=8,unit="pt")),
    axis.text.y=element_text(angle=0, hjust = 0,vjust=0.5,size=12,color="white",face="bold",
                             margin=margin(l=4,r=4,unit="pt")),
    plot.title=element_text( hjust = 0.5,color="white", size=35,face="bold",
                             margin=margin(t=5,b=5,unit="pt")),
    axis.title.y = element_text( color="white", size=14, face="bold",angle=0,vjust = 1.03,hjust=3),
    legend.text = element_text(colour="white", size=12,face="bold"),
    legend.title = element_text(colour="white", size=14,face="bold"),
    legend.key.height= unit(2, 'cm'),legend.key.width= unit(1, 'cm'),
    axis.ticks = element_line( color="white",size = 1),axis.ticks.length = unit(.3, "cm"),
    plot.background=element_rect(fill="black"))+ 
  annotate("text", x = as.Date (c(paste0(seq(2015,2022),"-07-02"))),
           y = 30, label = c(seq(2015,2022)),size=6,color="white",hjust = 0.5,vjust=0)+
  geom_vline(xintercept = as.Date (c(paste0(seq(2015,2022),"-01-01"))), linetype="dotted", color = "white", size=1)


ggsave("graphs/M_bruteA201578.png",width=50.8,height=28.576,units="cm",dpi=600)


#Graphique mortalité "standardisée"
mydata%>%filter(AGEA_DC %in% 30:98&ADEC>=2000)%>%group_by(DEC,AGEA_DC)%>%summarise(ADEC=max(ADEC),POPT=max(POPT),POPR=max(POPR),dc=sum(CT),taux=dc/POPT,dc2=taux*POPR)%>%
  ggplot(aes(x = DEC, y = AGEA_DC,fill=dc2)) +
  geom_raster(interpolate = T) +
  scale_fill_viridis_c(option = "H",limits=c(0,150),na.value = "#8b0f00",breaks=c(0,25,50,75,100,125,150),labels=c("0","25","50","75","100","125","150+")) +
  coord_cartesian(expand=F)+
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y")+
  scale_y_continuous(breaks=seq(30,100,5))+
  labs(title="Evolution de la mortalité francaise",y="Age",fill="Décès")+
  theme_void()+ 
  theme(
    axis.text.x=element_text(angle=90, hjust = 1,vjust=0.5,color="white",face="bold",
                             margin=margin(t=18,b=8,unit="pt")),
    axis.text.y=element_text(angle=0, hjust = 0,vjust=0.5,size=12,color="white",face="bold",
                             margin=margin(l=4,r=4,unit="pt")),
    plot.title=element_text( hjust = 0.5,color="white", size=35,face="bold",
                             margin=margin(t=5,b=5,unit="pt")),
    axis.title.y = element_text( color="white", size=14, face="bold",angle=0,vjust = 1.03,hjust=3),
    legend.text = element_text(colour="white", size=12,face="bold"),
    legend.title = element_text(colour="white", size=14,face="bold"),
    legend.key.height= unit(2, 'cm'),legend.key.width= unit(1, 'cm'),
    axis.ticks = element_line( color="white",size = 1),axis.ticks.length = unit(.3, "cm"),
    plot.background=element_rect(fill="black"))+ 
  annotate("text", x = as.Date (c(paste0(seq(2000,2022),"-07-02"))),
           y = 30, label = c(seq(2000,2022)),size=6,color="white",hjust = 0.5,vjust=0)+
  geom_vline(xintercept = as.Date (c(paste0(seq(2000,2022),"-01-01"))), linetype="dotted", color = "white", size=1)

ggsave("graphs/M_ST.png",width=50.8,height=28.576,units="cm",dpi=600)

head(mydata)
#Graphique mortalité "standardisée"2015,limits=c(0,80),na.value = "#8b0f00",breaks=c(0,20,40,60,80),labels=c("0","20","40","60","80+")
mydata%>%filter(AGEA_DC %in% 30:98&ADEC>=2015)%>%group_by(DEC,AGEA_DC)%>%summarise(ADEC=max(ADEC),POPT=max(POPT),POPR15=max(POPR15),dc=sum(CT),taux=dc/POPT,dc2=taux*POPR15)%>%
  ggplot(aes(x = DEC, y = AGEA_DC,fill=dc2)) +
  geom_raster(interpolate = T) +
  scale_fill_viridis_c(option = "H") +
  coord_cartesian(expand=F)+
  scale_x_date(date_breaks = "4 months", date_labels = "%b/%y")+
  scale_y_continuous(breaks=seq(30,100,5))+
  labs(title="Evolution de la mortalité francaise",y="Age",fill="Décès")+
  theme_void()+ 
  theme(
    axis.text.x=element_text(angle=90, hjust = 1,vjust=0.5,color="white",face="bold",
                             margin=margin(t=18,b=8,unit="pt")),
    axis.text.y=element_text(angle=0, hjust = 0,vjust=0.5,size=12,color="white",face="bold",
                             margin=margin(l=4,r=4,unit="pt")),
    plot.title=element_text( hjust = 0.5,color="white", size=35,face="bold",
                             margin=margin(t=5,b=5,unit="pt")),
    axis.title.y = element_text( color="white", size=14, face="bold",angle=0,vjust = 1.03,hjust=3),
    legend.text = element_text(colour="white", size=12,face="bold"),
    legend.title = element_text(colour="white", size=14,face="bold"),
    legend.key.height= unit(2, 'cm'),legend.key.width= unit(1, 'cm'),
    axis.ticks = element_line( color="white",size = 1),axis.ticks.length = unit(.3, "cm"),
    plot.background=element_rect(fill="black"))+ 
  annotate("text", x = as.Date (c(paste0(seq(2015,2022),"-07-02"))),
           y = 30, label = c(seq(2015,2022)),size=6,color="white",hjust = 0.5,vjust=0)+
  geom_vline(xintercept = as.Date (c(paste0(seq(2015,2022),"-01-01"))), linetype="dotted", color = "white", size=1)

ggsave("graphs/M_152STA.png",width=50.8,height=28.576,units="cm",dpi=600)

###
head(mydata)
test<-mydata%>%filter(ADEC%in%2000:2023&AGEA_DC%in%0:99&!DEPDEC%in%c("Etranger","DOMTOM"))
test2<-test%>%group_by(ADEC,AGEA_DC,SEXE)%>%summarise(dc=n())
test2<-test2%>%mutate(cov20=case_when(ADEC==2020~1,T~0),cov21=case_when(ADEC==2021~1,T~0),cov22=case_when(ADEC==2022~1,T~0),cov23=case_when(ADEC==2023~1,T~0))
test3<-left_join(test2,pop%>%select(ADEC=ANNEE,SEXE,AGEA_DC=AGE,POP))
test3%>%group_by(ADEC)%>%summarise(sum(dc))

res<-glm(dc~AGEA_DC+SEXE+ADEC+cov20+cov21+cov22+cov23,offset = log(POP),data=test3%>%filter(ADEC>=2009),poisson)
exp(coef(res))
res<-glm(dc~factor(ADEC)+AGEA_DC+SEXE,offset = log(POP),data=test3,poisson)
ggcoef_model(res,exponentiate = T,conf.int = T,include = c( "factor(ADEC)","SEXE"))
ggcoef_model(res,exponentiate = T,conf.int = T)

library(tidyverse)
library(ggplot2)
library(lubridate)
library(GGally)
library(VGAM)
library(lme4)

