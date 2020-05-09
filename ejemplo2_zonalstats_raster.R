# Leer un conjunto de rásters de temperatura superficial en Kelvin
# Convertir unidades multiplicando por factor 0.02 y luego convertir a Celcius 
# Realizar estadísticas zonales por comuna para el mes de enero


# Cargar librerías --------------------------------------------------------
library(raster)
library(tidyverse)
library(sf)


# Hacer raster stack y cáculos --------------------------------------------

l <- list.files("data/raster/LST/", full.names = T)
lst <- stack(l)
lst <- (lst * 0.02) - 273.15

# Leer comunas ------------------------------------------------------------

comunas <- st_read("data/comunas.shp") %>% 
  filter(Region == "Región Metropolitana de Santiago") %>% 
  st_transform(crs=4326)


# Hacer estadística zonal -------------------------------------------------

max_ene <- comunas %>% 
  dplyr::select(Comuna, Region, Provincia) %>% 
  mutate(ene = raster::extract(lst$lst_01, comunas["Comuna"], fun=mean, na.rm=T))
  

# Hacer gráfico -----------------------------------------------------------

ggplot(max_ene) +
  geom_sf(aes(fill = ene )) +
  scale_fill_continuous(type = "viridis")

## leaflet?
### Tarea: Intentar hacerlo con https://rstudio.github.io/leaflet/choropleths.html

