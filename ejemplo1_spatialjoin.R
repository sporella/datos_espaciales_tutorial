# Leer puntos como tabla y convertir a formato espacial
# Atributar los puntos con su comuna correspondiente (spatial join)
# Generar estadísticas


# Cargar librerías --------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)


# Cargar datos con sf -----------------------------------------------------

tabla <- read_csv("data/reciclaje.csv")

reciclaje <- st_as_sf(tabla, coords = c("lon", "lat"), crs = 4326)

comunas <- read_sf("data/comunas.shp") %>% 
  filter(Region == "Región Metropolitana de Santiago")

# Corroborar CRS ----------------------------------------------------------

st_crs(reciclaje)$epsg
st_crs(comunas)$epsg

reciclaje_utm <- st_transform(reciclaje, crs = 32719)
comunas_utm <- st_transform(comunas, crs = 32719)

# Hacer spatial join y estadísticas ---------------------------------------

reciclaje_comuna <- comunas_utm %>% 
  st_join(reciclaje_utm) %>% 
  group_by(Comuna) %>% 
  summarise( n = n())

# Hacer gráfico ---------------------------------------------------------
## Espacial

ggplot(reciclaje_comuna) +
  geom_sf(aes(fill=n))
  
## No Espacial

tab_rec_com <- st_drop_geometry(reciclaje_comuna)

ggplot(reciclaje_comuna, aes(x=reorder(Comuna, n), y = n, fill = Comuna)) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip()

write_csv(tab_rec_com, "tabla.csv")
