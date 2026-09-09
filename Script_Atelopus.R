necesarios <- c("dplyr", "ggplot2", "terra", "tidyterra", "rnaturalearthdata",
                "rnaturalearth", "sf", "ggrepel", "ggspatial", "elevatr")
faltan <- necesarios[!necesarios %in% rownames(installed.packages())]

if (length(faltan) > 0) {
  problemas <- c(paste("Faltan paquetes:", paste(faltan, collapse = ", ")))
  cat("Paquetes faltantes:", paste(faltan, collapse = ", "), "\n")
} else {
  cat("Paquetes: todos instalados\n")
}

install.packages(faltan)


# 1. CARGAR LIBRERÍAS
library(dplyr)
library(ggplot2)
library(terra)
library(tidyterra)
library(rnaturalearth)
library(sf)
library(ggrepel)
library(ggspatial)
library(elevatr)

# 2. CARGAR DATOS DE OCURRENCIA

sp <- read.csv("Datos/registros_atelopus_colombia.csv", sep = ",", header = T)


# Revisar valores faltantes
sum(is.na(sp$decimalLongitude))
sum(is.na(sp$decimalLatitude))
sum(is.na(sp$year))



# Eliminar registros que no tengan coordenadas
sp <- sp %>%
  filter(
    !is.na(decimalLongitude),
    !is.na(decimalLatitude),
    !is.na(year)
  )


# 4. DEFINIR LA ESPECIE
genus_name <- "Atelopus"

# 5. DEFINIR LA REGIÓN DE TRABAJO

countries <- ne_countries(
  country = c(
    "Colombia",
    "Ecuador",
    "Peru",
    "Bolivia",
    "Venezuela",
    "Panama"
  ),
  returnclass = "sf",
  scale = "medium"
)


#6. ETIQUETAS DE PAÍSES
labels <- st_centroid(countries) %>%
  cbind(st_coordinates(.))

# 7. DEFINIR LA EXTENSIÓN GEOGRÁFICA DEL MAPA
map_xlim <- c(-82, -65)
map_ylim <- c(-8, 15)


# 8. DEFINIR LA VENTANA PARA DESCARGAR LA ELEVACIÓN

bbox_area <- st_as_sf(
  st_sfc(
    st_polygon(
      list(
        rbind(
          c(map_xlim[1], map_ylim[1]),
          c(map_xlim[1], map_ylim[2]),
          c(map_xlim[2], map_ylim[2]),
          c(map_xlim[2], map_ylim[1]),
          c(map_xlim[1], map_ylim[1])
        )
      )
    ),
    crs = 4326
  )
)

# 9. DESCARGAR MODELO DIGITAL DE ELEVACIÓN (DEM)

dem_raster <- get_elev_raster(
  locations = bbox_area,
  z = 6,
  clip = "bbox"
)


raster::writeRaster(
  dem_raster,
  filename = "Raster/Atelopus.tif",
  format = "GTiff",
  overwrite = TRUE
)

# Convertir a SpatRaster para compatibilidad con tidyterra
col_dem <- terra::rast("Raster/Atelopus.tif")


# 10. CONSTRUCCIÓN DEL MAPA (ggplot2)
# ------------------------------------------------------------------------------

map_plot <- ggplot() +
  
  # CAPA 1: MODELO DIGITAL DE ELEVACIÓN
  geom_spatraster(data = col_dem) +
  scale_fill_viridis_c(
    name = "Elevación (m)",
    option = "terrain",
    na.value = "transparent"
  ) +
  
  # CAPA 2: LÍMITES DE LOS PAÍSES
  geom_sf(
    data = countries,
    fill = NA,
    color = "black",
    size = 0.3
  ) +
  
  # CAPA 3: REGISTROS DE PRESENCIA
  geom_point(
    data = sp,
    aes(
      x = decimalLongitude,
      y = decimalLatitude,
      color = year
    ),
    size = 1.8,
    alpha = 0.8,
    show.legend = T
  ) + scale_color_viridis_c(option = "plasma") +
  
  # CAPA 4: NOMBRES DE LOS PAÍSES
  geom_text_repel(
    data = labels,
    aes(
      x = X,
      y = Y,
      label = admin
    ),
    size = 3.5,
    color = "black",
    fontface = "bold"
  ) +
  
  # ELEMENTOS CARTOGRÁFICOS: ESCALA Y NORTE
  annotation_scale(
    location = "bl",
    width_hint = 0.35,
    text_cex = 0.8
  ) +
  
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white")
    ),
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm")
  ) +
  
  # EXTENSIÓN Y BORDES DEL MAPA
  coord_sf(
    xlim = map_xlim,
    ylim = map_ylim,
    expand = FALSE
  ) +
  
  # TÍTULO (FORMATO CURSIVA CIENTÍFICA) Y EJES
  labs(
    title = bquote(italic(.(genus_name))),
    x = "Longitud",
    y = "Latitud"
  ) +
  
  # ESTILO GENERAL
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    plot.title = element_text(
      face = "bold",
      size = 13,
      hjust = 0.5
    )
  )


# EXPORTAR EL MAPA

ggsave(
  filename = "Mapas/mapa_Distribucion_ Atelopus.png",
  plot = map_plot,
  width = 7,
  height = 7,
  units = "in",
  dpi = 300
)




