##################################################################################################
#Script para descargar informacion del INEGI con la APP de INEGI en R y luego hacer las graficas##
#Para el ebook####################################################################################
#### Primero subes los paquetes que necesitas


library(easypackages)
my_packages <- c("tidyverse", "readxl", "lubridate", "extrafont")
libraries(my_packages)
loadfonts()
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.22/bin/gswin64c.exe")
# Hacemos la paleta de colores
# Luego jalas las series de datos que necesitas

BIE_BIE20180126152150 <- read_excel("C:/Users/pvelazquez/Google Drive/Sector Industrial en Tijuana/Bases de datos/INDICADORES_ECONOMICOS_DE_COYUNTURA/BIE_BIE20180126152150.xlsx", 
                                    col_names = TRUE, skip = 1) %>%
  filter(!is.na(.[,2]))


datos <- BIE_BIE20180126152150 %>%
  rename_all(funs(str_to_lower(.) %>%
                    str_replace(., "proyectos especiales > tablero de indicadores económicos > ", ""))) %>%
  select(contains("construcción")) %>%
  rename( "constr_orig" = "indicadores de producción > índice de volumen físico de la construcción original p2 / f2/ (índice base 2013=100)", 
          "constr_calend" = "indicadores de producción > índice de volumen físico de la construcción original corregida por efectos de calendario f3/ (índice base 2013=100)", 
          "constr_esta" = "indicadores de producción > índice de volumen físico de la construcción desestacionalizada f3/ (índice base 2013=100)", 
          "constr_trend" = "indicadores de producción > índice de volumen físico de la construcción tendencia f3/ (índice base 2013=100)", 
          "constr_cicl" = "indicadores de producción > índice de volumen físico de la construcción ciclo f3/ (puntos)") %>%
  mutate(periodo = BIE_BIE20180126152150$Periodo,
         day = "01") %>%
  separate(periodo, into = c("year", "month"), sep = "\\/") %>%
  mutate(fecha = as_date(paste0(year, month, day)))



p <- datos %>%
  ggplot(aes(fecha, constr_calend)) +
  geom_line(colour = "#4f2584") + 
  theme(text = element_text(family = "Impact", size = 9)) +
  theme_minimal() +
  ggtitle("Este es el título")

 ggsave("ggplot_garamond.jpg", plot = p, width = 4, height=4)
embed_fonts("ggplot_garamond.jpg") 
 
 
datos %>%
  ggplot(aes(fecha, constr_calend)) + 
  geom_line(colour  = "#862884") + 
  geom_smooth(colour = "#6ebe4c") +
  xlab(label = NULL) + 
  ylab(label = "Índice") +
  theme_minimal()

datos %>%
  mutate(avg = caTools::runmin(IGAE, k = 1) ) %>%
  ggplot(aes(fecha, avg)) + geom_line() + geom_smooth(method = "loess") +
  theme_minimal() + ggtitle("Indicador global de la actividad Económica") + 
  xlab(label = "Fecha") + ylab(label = "IGAE") + scale_x_date(date_breaks = "1 year")




datos %>%
  ggplot(aes(fecha, log(IGAE))) + geom_line() geom_smooth(method = "loess") +
  theme_minimal()


lag(datos$IGAE)


nombres <- nombres %>% 
  rename("columna" = "colnames(BIE_BIE20180126152150)") %>%
  separate(columna, into = c("uno", "dos", "tres", "cuatro"), sep = "\\>")

nombres$columna <- if_else(is.na(nombres$cuatro), nombres$tres, nombres$cuatro) 

nombres$columna <- if_else(is.na(nombres$columna),nombres$uno, nombres$columna)

# otro cambio
