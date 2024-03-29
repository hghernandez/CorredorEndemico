library(tidyverse)

#Creo la lista para guardar los datos

dengue <- list()

#armo un vector de años
anios <- seq(2018,2022)

dengue[[1]] <- read.delim("http://datos.salud.gob.ar/dataset/ceaa8e87-297e-4348-84b8-5c643e172500/resource/255440b0-d8b2-4247-87d2-91dfec29eb0f/download/informacion-publica-dengue-zika-nacional-hasta-20181231.csv",
                          header = TRUE,sep=",")

dengue[[2]] <- openxlsx::read.xlsx("http://datos.salud.gob.ar/dataset/ceaa8e87-297e-4348-84b8-5c643e172500/resource/ab93dc4b-4e6c-499b-9335-c548a701a870/download/informacion-publica-dengue-zika-nacional-hasta-20191231_3.xlsx")

dengue[[3]] <- openxlsx::read.xlsx("http://datos.salud.gob.ar/dataset/ceaa8e87-297e-4348-84b8-5c643e172500/resource/3e11a05a-8287-4954-ab08-f698dc0ff3e1/download/informacion-publica-dengue-zika-nacional-hasta-20201231_1.xlsx")

dengue[[4]] <- openxlsx::read.xlsx("http://datos.salud.gob.ar/dataset/ceaa8e87-297e-4348-84b8-5c643e172500/resource/8275ff70-94e5-4ee0-9cbb-630587daa618/download/1648473440-6ff92f5b32926ebfb17725a0ab8e991652470725-informacion-publica-dengue-zika-nacional-ha.xlsx")

dengue[[5]] <- read.csv2("http://datos.salud.gob.ar/dataset/ceaa8e87-297e-4348-84b8-5c643e172500/resource/30d76bcb-b8eb-4bf3-863e-c87d41724647/download/informacion-publica-dengue-zika-nacional-anio-2022.csv",
                         fileEncoding = "Latin1", check.names = F)
#unifico los archivos


for(i in 1:5){
colnames(dengue[[i]])[1] <- 'departamento_id'  
colnames(dengue[[i]])[5] <- 'ano'
}

#Unifico las listas en un dataframe

dengue_df <- do.call(rbind,dengue)


#Veo la cantidad de NA

anios <- seq(2018,2022)
nulos <- c(rep(0,5))

se_nulls <- data.frame(anios,nulos)

for(i in 1:5){

se_nulls[i,2]  <- sum(is.na(as.numeric(dengue[[i]]$semanas_epidemiologicas)))

  
}

#Reasigno casos en 2020 para la semana los NULLS en semana epidemiologica

anio_2020 <- dengue_df %>%
       filter(ano== 2020) %>%
       mutate(semanas_epidemiologicas= as.numeric(semanas_epidemiologicas))%>%
       group_by(ano,semanas_epidemiologicas) %>%
       summarise(casos= sum(cantidad_casos)) %>%
       ungroup()%>%
       mutate(prop= casos/sum(casos[!is.na(semanas_epidemiologicas)])) %>%
       mutate(casos.y= prop * casos[is.na(semanas_epidemiologicas)]) %>%
       filter(!is.na(semanas_epidemiologicas)) %>%
       mutate(casos= round(casos+casos.y)) %>%
       select(-c(prop,casos.y))

#Calculo los casos para el resto de los años y unifico el dataframe


casos_dengue <- dengue_df %>%
  filter(ano != 2020) %>%
  mutate(semanas_epidemiologicas= as.numeric(semanas_epidemiologicas))%>%
  group_by(ano,semanas_epidemiologicas) %>%
  summarise(casos= sum(cantidad_casos)) %>%
  rbind(anio_2020) %>%
  arrange(ano)


#Completo un dataframe con las semanas epidemiologicas

anos <- c(rep(2018,52),
         rep(2019,53),
         rep(2020,53),
         rep(2021,52),
         rep(2022,52))

semanas_epi <- c(seq(1,52), #2018
                   seq(1,53), #2019
                   seq(1,53), #2020
                   seq(1,52), #2021
                   seq(1,52)) #2022

all_week <- data.frame(anos,semanas_epi)

#Joineo los casos de dengue por semana epidemiologica con el total de semanas
#por año

casos_dengue <- all_week %>%
  left_join(casos_dengue, by= c("anos"="ano","semanas_epi"="semanas_epidemiologicas")) %>%
  replace_na(list(casos=0))


save(casos_dengue, file="casos_dengue.RData")
load("casos_dengue.RData")

#Hacemos los cálculos

#Creamos el data frame con las poblaciones

anios <- seq(2018,2022)
poblacion <- c(44494502,44938712,45376763,45808747,46234830)

poblacion <- data.frame(anios,poblacion)

alfa=0.05


media_semanal <- casos_dengue %>%
  left_join(poblacion, by= c("anos"="anios")) %>%
  mutate(tasa= round((casos/poblacion*100000+1),4),
         lntasa= round(log(tasa),4)) %>%
    group_by(semanas_epi)%>%
  summarise(casos= sum(casos),
         lntasa_media= mean(lntasa),
         sd= sd(lntasa),
         n= n(),
         IC_INF= lntasa_media-(qt(1-alfa/2,n)*sd/sqrt(n)),
         IC_SUP= lntasa_media+(qt(1-alfa/2,n)*sd/sqrt(n)))

#Vuelvo a obtener los valores reales - 1

corredor_dengue <- media_semanal %>%
  mutate(media= exp(lntasa_media)-1,
         ic_inf= exp(IC_INF)-1,
         ic_sup= exp(IC_SUP)+1,
         media_casos= media*mean(poblacion$poblacion)/100000,
         ic_inf_casos= ic_inf*mean(poblacion$poblacion)/100000,
         ic_sup_casos= ic_sup*mean(poblacion$poblacion)/100000,
         new= max(c(casos,ic_sup_casos),na.rm=TRUE))


#Armo el gráfico

#graf_dengue <- 
ggplot(corredor_dengue,aes(x = semanas_epi#, fill=key
)) +
  geom_area(aes(x=semanas_epi, y=new), fill = "#981000", alpha=0.6, stat="identity")+
  geom_area(aes(x=semanas_epi, y= ic_sup_casos), fill = "#fee76a", stat="identity")+
  geom_area(aes(x=semanas_epi, y= media_casos), fill = "#3e9e39",  stat="identity")+
  geom_area(aes(x=semanas_epi, y= inf_to_media), fill = "grey",  stat="identity")+
  geom_area(aes(x=semanas_epi, y= ic_inf_casos), fill = "white", stat="identity") +
  geom_line(data= casos_dengue %>% filter(anos== 2020),mapping = aes(x=semanas_epi,y= casos, colour= "casos"), size=0.5)+
  theme_bw()+
  scale_x_continuous(breaks=seq(1,53, 1))+
  scale_color_manual(name="Casos 2020", values= c(casos="black"))+
  scale_y_continuous(label= scales::label_number_si())+
  labs(title= "Corredor endémico Dengue. Argentina 2018-2022",
       x= "Semanas Epidemiológicas",y="Casos")


plotly::ggplotly(graf_dengue)
#Armo las curvas epidemicas

RColorBrewer::brewer.pal.info
RColorBrewer::brewer.pal(5,"Set1")


graf_casos <- casos_dengue %>%
  ggplot(aes(x=semanas_epi, y=casos, color=as.factor(anos),linetype=as.factor(anos),group= anos))+
  geom_line(size=0.5)+
  labs(title= "Curvas epidémicas Dengue. Argentina 2018-2022",
       x= "Semanas Epidemiológicas",y="Casos", color= "Años",linetype= "Años")+
  scale_color_manual(values= c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00"))+
  scale_x_continuous(breaks=seq(1,53, 1))+
  scale_y_continuous(label= scales::label_number_si())+
  theme_bw()

plotly::ggplotly(graf_casos)


##%######################################################%##
#                                                          #
####           Armo Enfermedad Tipo Influenza           ####
#                                                          #
##%######################################################%##

ira <- list()

ira[[1]] <- read.delim("http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/f4096f8b-1692-4d5f-a6d8-09cae47931a4/download/vigilancia-respiratorias-agudas-2018-hasta-20200106.csv",
                          header = TRUE,sep=",")


ira[[1]] <- ira[[1]] %>% filter(anio %in% c(2018,2019)) %>%
  rename("departamento_id"= ï..departamento_id)



ira[[2]] <- openxlsx::read.xlsx("http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/cda1ba2a-955a-42b0-9d45-8ce4439d2106/download/informacion-publica-respiratorias-nacional-hasta-20220309.xlsx")

ira[[2]] <- ira[[2]] %>% filter(año %in% c(2020,2021)) %>%
  rename("anio"= año)


ira[[3]] <-openxlsx::read.xlsx("http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/37feeed4-dfcd-4f39-a403-93a6b4bd90d2/download/informacion-publica-respiratorias-nacional-hasta-20230706.xlsx")

ira[[3]] <- ira[[3]] %>% rename("anio"= año)


#Unifico el dataframe

ira_df <- do.call(rbind,ira)


casos_ira <- ira_df %>%
  group_by(anio,semanas_epidemiologicas)%>%
  summarise(casos= sum(cantidad_casos))


#Reasingo casos para la semana 1 a 17 de 2018

compl_2018 = ira_df %>%
  filter(semanas_epidemiologicas >= 1 & semanas_epidemiologicas <= 17 & anio > 2018 & anio < 2023)%>%
  group_by(anio,semanas_epidemiologicas) %>%
  summarise(casos= sum(cantidad_casos)) %>%
  group_by(semanas_epidemiologicas) %>%
  summarise(casos= median(casos)) %>%
  mutate(anio= 2018)



#Unifico todos

casos_ira <- rbind(compl_2018,
                   casos_ira)

save(casos_ira,file= "casos_ira.RData")
load("casos_ira.RData")
#Creamos el data frame con las poblaciones

anios <- seq(2018,2022)
poblacion <- c(44494502,44938712,45376763,45808747,46234830)

poblacion <- data.frame(anios,poblacion)

alfa=0.05


#Calculo la media semanal ETI

media_semanal_ira <- casos_ira %>%
  filter(anio < 2023) %>%
  left_join(poblacion, by= c("anio"="anios")) %>%
  mutate(tasa= round((casos/poblacion*100000+1),4),
         lntasa= round(log(tasa),4)) %>%
  group_by(semanas_epidemiologicas)%>%
  summarise(casos= sum(casos),
            lntasa_media= mean(lntasa),
            sd= sd(lntasa),
            n= n(),
            IC_INF= lntasa_media-(1*sd),
            IC_SUP= lntasa_media+(1*sd))

#Vuelvo a obtener los valores reales - 1

corredor_ira <- media_semanal_ira %>%
  mutate(media= exp(lntasa_media)-1,
         ic_inf= ifelse(exp(IC_INF)-1<0,0,exp(IC_INF)-1),
         ic_sup= exp(IC_SUP)-1,
         media_casos= media/100000*mean(poblacion$poblacion),
         ic_inf_casos= ifelse(ic_inf/100000*poblacion[5,2] < 0,0,ic_inf/100000*poblacion[5,2]),
         ic_sup_casos= ic_sup/100000*poblacion[5,2],
         new= max(c(casos,ic_sup_casos),na.rm=TRUE))



#Armo el gráfico

ggplot(corredor_ira,aes(x = semanas_epidemiologicas#, fill=key
)) +
  geom_area(aes(x=semanas_epidemiologicas, y=new), fill = "#981000", alpha=0.6, stat="identity")+
  geom_area(aes(x=semanas_epidemiologicas, y= ic_sup_casos), fill = "orange", stat="identity")+
  geom_area(aes(x=semanas_epidemiologicas, y= media_casos), fill = "#fee76a",  stat="identity")+
  geom_area(aes(x=semanas_epidemiologicas, y= ic_inf_casos), fill = "#3e9e39", stat="identity") +
  geom_line(data= casos_ira %>% filter(anio== 2023 & semanas_epidemiologicas <= 23),mapping = aes(x=semanas_epidemiologicas,y= casos, colour= "casos"), size=0.8)+
  theme_bw()+
  scale_x_continuous(breaks=seq(1,53, 1))+
  scale_color_manual(name="Casos 2023", values= c(casos="black"))+
  scale_y_continuous(label= scales::label_number_si())+
  labs(title= "Corredor endémico IRA. Argentina 2018-2022",
       x= "Semanas Epidemiológicas",y="Casos")



#Armo las curvas epidemicas

RColorBrewer::brewer.pal.info
RColorBrewer::brewer.pal(8,"Set1")


casos_ira %>%
  filter(anio < 2023) %>%
  ggplot(aes(x=semanas_epidemiologicas, y=casos, color=as.factor(anio),linetype=as.factor(anio),group= anio))+
  geom_line(size=0.9)+
  geom_line(data= casos_ira %>% filter(anio== 2023 & semanas_epidemiologicas <26),
            mapping = aes(x=semanas_epidemiologicas, y=casos, color=as.factor(anio),linetype=as.factor(anio)))+
  labs(title= "Curvas epidémicas IRA. Argentina 2018-2022",
       x= "Semanas Epidemiológicas",y="Casos", color= "Años",linetype= "Años")+
  scale_color_manual(values= c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#F781BF"))+
  scale_x_continuous(breaks=seq(1,53, 1))+
  scale_y_continuous(label= scales::label_number_si())+
  theme_bw()

