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


for(i in 2:5){
colnames(dengue[[i]])[5] <- 'ano'
}

dengue_df <- do.call("rbind",dengue)

#Veo las semanas epidemiologicas

View(dengue_df %>%
  mutate(semanas_epidemiologicas= as.numeric(semanas_epidemiologicas))%>%
  distinct(ano,semanas_epidemiologicas) %>%
  arrange(ano,semanas_epidemiologicas))

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

#Agrupo los casos por año y semana epid

View(dengue_df %>%
  mutate(semanas_epidemiologicas= as.numeric(semanas_epidemiologicas))%>%
  group_by(ano,semanas_epidemiologicas) %>%
  summarise(casos= sum(cantidad_casos)))

