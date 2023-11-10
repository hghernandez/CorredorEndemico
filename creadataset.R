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

#Hacemos los cálculos

#Creamos el data frame con las poblaciones

anios <- seq(2018,2022)
poblacion <- c(44494502,44938712,45376763,45808747,46234830)

poblacion <- data.frame(anios,poblacion)

alfa=0.05


media_semanal <- casos_dengue %>%
  left_join(poblacion, by= c("anos"="anios")) %>%
  mutate(tasa= round((casos/poblacion*1000000+1),4),
         lntasa= round(log(tasa),4)) %>%
  group_by(semanas_epi)%>%
  summarise(lntasa_media= mean(lntasa),
         sd= sd(lntasa),
         n= n(),
         IC_INF= lntasa_media-(qt(1-alfa/2,n)*sd/sqrt(n)),
         IC_SUP= lntasa_media+(qt(1-alfa/2,n)*sd/sqrt(n)))

#Vuelvo a obtener los valores reales - 1

corredor_dengue <- media_semanal %>%
  mutate(media= exp(lntasa_media)-1,
         ic_inf= ifelse(exp(IC_INF)-1<0,0,exp(IC_INF)-1),
         ic_sup= exp(IC_SUP)-1,
         media_casos= media*poblacion[5,2]/100000,
         ic_inf_casos= ifelse(ic_inf*poblacion[5,2]/100000 < 0,0,ic_inf*poblacion[5,2]/100000),
         ic_sup_casos= ic_sup*poblacion[5,2]/100000,
         media_to_inf = media_casos-ic_inf_casos,
         media_to_sup= ic_sup_casos-media_casos)


"#D7381A","#DA89FE","#FBB999","#50007F","#EDE6DE"

ggplot(corredor_dengue,aes(as.numeric(semanas_epi),colour="#EDE6DE"))+
  geom_area(aes(,ic_sup_casos),color="sky blue",position = 'stack',fill = I("sky blue"))+
  geom_area(aes(,media_casos),color="#EDE6DE",position = 'stack',fill = I("#EDE6DE"))+
  geom_area(aes(,media_to_inf),color="#09C723",position = 'stack',fill = I("#09C723"))+
  geom_bar(aes(,media),color="black",position = 'stack',fill = I("blue"),stat = "identity")+
  geom_line(casos_dengue %>% filter(anos== 2020),mapping = aes(x=semanas_epi, y=casos),
            color= "black", linewidth= 1.2, linetype= "dashed")+
  theme(
    panel.grid.major = element_line(colour = "#F39276"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F39276")
  )+
  scale_x_continuous(breaks=seq(1,53, 1))+
  ggtitle("Corredor endémico Dengue. Argentina 2018-2022")+ 
  xlab("Semanas Epidemiológicas")+ ylab("Casos")

casos_dengue %>%
  ggplot(aes(x=semanas_epi, y=casos, color=as.factor(anos),linetype=as.factor(anos),group= anos))+
  geom_line()

##%######################################################%##
#                                                          #
####           Armo Enfermedad Tipo Influenza           ####
#                                                          #
##%######################################################%##

eti <- list()

eti[[1]] <- read.delim("http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/f4096f8b-1692-4d5f-a6d8-09cae47931a4/download/vigilancia-respiratorias-agudas-2018-hasta-20200106.csv",
                          header = TRUE,sep=",")

eti[[1]] <- eti[[1]] %>% filter(anio %in% c(2018,2019))

eti[[2]] <- openxlsx::read.xlsx("http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/cda1ba2a-955a-42b0-9d45-8ce4439d2106/download/informacion-publica-respiratorias-nacional-hasta-20220309.xlsx")

eti[[2]] <- eti[[2]] %>% filter(año %in% c(2020,2021)) %>%
  rename("anio"= año)


eti[[3]] <-openxlsx::read.xlsx("http://datos.salud.gob.ar/dataset/c553d917-36f4-4063-ac02-1686a9120e1c/resource/37feeed4-dfcd-4f39-a403-93a6b4bd90d2/download/informacion-publica-respiratorias-nacional-hasta-20230706.xlsx")

eti[[3]] <- eti[[3]] %>% rename("anio"= año)


#Unifico el dataframe

eti_df <- do.call(rbind,eti)

casos_eti <- eti_df %>%
  group_by(anio,semanas_epidemiologicas)%>%
  summarise(casos= sum(cantidad_casos))


#Reasingo casos para la semana 1 a 17 de 2018

compl_2018 = eti_df %>%
  filter(semanas_epidemiologicas >= 1 & semanas_epidemiologicas <= 17 & anio > 2018 & anio < 2023)%>%
  group_by(anio,semanas_epidemiologicas) %>%
  summarise(casos= sum(cantidad_casos)) %>%
  group_by(semanas_epidemiologicas) %>%
  summarise(casos= median(casos)) %>%
  mutate(anio= 2018)



#Unifico todos

casos_eti <- rbind(compl_2018,
                   casos_eti)

save(casos_eti,file= "casos_eti.RData")

