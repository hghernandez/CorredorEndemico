library(tidyverse)

#Creo la lista para guardar los datos

dengue <- list()

#armo un vector de años
anios <- seq(2018,2022)



for(i in 1:length(anios)){
print(i) 

dengue[[i]] <- readr::read_csv(paste0("http://datos.salud.gob.ar/dataset/ceaa8e87-297e-4348-84b8-5c643e172500/resource/255440b0-d8b2-4247-87d2-91dfec29eb0f/download/informacion-publica-dengue-zika-nacional-hasta-",anios[i],"1231.csv"),
                       locale = readr::locale(encoding = "UTF-8"))
  
}  


