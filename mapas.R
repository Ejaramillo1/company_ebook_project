# Starting wit spatial data in R

library(easypackages)
my_packages <- c("tidyverse", "raster")
libraries(my_packages)


# Funcion para conocer cual es nuestro directorio de trabajo
getwd()

# Funcion para establecer el directoriod e trabajo 
# donde se encuentran los datos
setwd("~/proyecto_ebook/Mapa_Tijuana/020040001")

# Funcion para subir los datos de la DENUE al ambiente de trabajo en Rstudio

mapas <- shapefile("020040001a.shp")

# Filtramos datos que nos interesan en este momento

mapa_ageb <- plot(mapas)

dev.copy(png, "mapa_ageb.png")
dev.off()

tj <- mapas@data %>%
  filter(municipio %in% c("Tijuana") & codigo_act %in% c("236211", "236212")) %>%
  select(id, latitud, longitud)


library(rgeos)

mapas_f <- (mapas)

p <- ggplot(mapas@data, aes(id, per_ocu)) + geom_point()


p <- geom_point(aes(color = id))
