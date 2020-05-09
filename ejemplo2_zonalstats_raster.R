# Leer un conjunto de rásters de temperatura superficial en Kelvin
# Convertir unidades multiplicando por factor 0.02 y luego convertir a Celcius 
# Realizar estadísticas zonales por comuna para el mes de enero


# Cargar librerías --------------------------------------------------------
library(raster)
library(tidyverse)
library(sf)


# Hacer raster stack y cáculos --------------------------------------------

l <- list.files(path = "data/raster/LST", pattern = ".tif$", full.names = T)

lst <- stack(l)
lst <- (lst * 0.02) - 273.15

# Leer comunas ------------------------------------------------------------

comunas <- read_sf("data/comunas.shp") %>% 
  filter(Region == "Región de Valparaíso") %>% 
  st_transform(crs = st_crs(lst)) 

# Hacer estadística zonal -------------------------------------------------

max_ene <- comunas %>% 
  dplyr::select(Comuna, Provincia, Region) %>% 
  mutate(ene = raster::extract(lst$lst_01, comunas["Comuna"], fun = max, na.rm=T)) %>% 
  filter(!is.na(ene))
  

# Hacer gráfico -----------------------------------------------------------

p1 <- ggplot(max_ene)+
  geom_sf(aes(fill=ene)) +
  scale_fill_viridis_c()

## leaflet?
### Tarea: Intentar hacerlo con https://rstudio.github.io/leaflet/choropleths.html

