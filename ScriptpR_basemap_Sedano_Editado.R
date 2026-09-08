# ==============================================================================
# TALLER 1: DEL DATO ESPACIAL AL MAPA BIOGEOGRÁFICO
# Curso de Biogeografía - Universidad del Valle
# ==============================================================================
#
# OBJETIVO DEL TALLER
#
# Este script es una plantilla para transformar datos de presencia de una
# especie en un mapa biogeográfico.
#
# La idea NO es simplemente reproducir este mapa. Utiliza esta plantilla como
# punto de partida y modifica el código para construir un mapa que te ayude
# a responder una pregunta biogeográfica.
#
# Puedes trabajar con uno o varios países de la región de los Andes tropicales,
# modificar la extensión geográfica, cambiar la especie, incorporar otras capas
# de información y modificar la simbología.
#
# PREGUNTA GUÍA:
# ¿Qué patrón biogeográfico puedo observar o comunicar mediante mi mapa?
#
# ==============================================================================

# Descargar librerias necesarias si no estan descargadas

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
# ------------------------------------------------------------------------------
# Las librerías contienen funciones que utilizaremos para leer, organizar,
# analizar y representar datos espaciales.

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
# ------------------------------------------------------------------------------
# El archivo 'sp.rds' contiene los registros de presencia de la especie.
#
# IMPORTANTE:
# El archivo debe estar guardado en la misma carpeta de trabajo que este
# script, o debes indicar correctamente la ruta al archivo.
#
# El objeto 'sp' debe contener, como mínimo, estas dos columnas:
#
#   decimalLongitude = longitud en grados decimales
#   decimalLatitude  = latitud en grados decimales
#
# Puedes revisar el contenido de tus datos con:
#
#   names(sp)
#   head(sp)
#   str(sp)

sp <- read.csv("Datos/IRO 32 data points in the Western Cordillera.txt", sep = "\t", header = F)

head(sp)

# 3. VERIFICAR LAS COORDENADAS
# ------------------------------------------------------------------------------
# Las coordenadas son fundamentales para construir un mapa.
#
# Comprueba que las columnas necesarias existan y que no haya valores faltantes.
# También revisa que los valores estén dentro de rangos geográficos razonables:
#
# Longitud: entre -180 y 180
# Latitud:  entre -90 y 90

names(sp) <- c("decimalLongitude", "decimalLatitude")

# Revisar valores faltantes
sum(is.na(sp$decimalLongitude))
sum(is.na(sp$decimalLatitude))

# Eliminar registros que no tengan coordenadas
sp <- sp %>%
  filter(
    !is.na(decimalLongitude),
    !is.na(decimalLatitude)
  )


# 4. DEFINIR LA ESPECIE
# ------------------------------------------------------------------------------
# Escribe aquí el nombre científico de la especie que estás utilizando.
# Cambia solamente el texto que aparece entre comillas.

species_name <- "Chlorostilbon mellisugus"


# 5. DEFINIR LA REGIÓN DE TRABAJO
# ------------------------------------------------------------------------------
# Puedes seleccionar UNO o VARIOS países.
# Los nombres deben escribirse en inglés porque así aparecen en rnaturalearth.

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
# ------------------------------------------------------------------------------
# Obtenemos los centroides de los países seleccionados para ubicar sus nombres.
#
# NOTA: Si prefieres mostrar las etiquetas únicamente de los países que tocan a 
# Colombia, puedes descomentar el bloque de código alternativo.

# Opción por defecto (Universal): Calcula centroides para TODOS los países descargados
labels <- st_centroid(countries) %>%
  cbind(st_coordinates(.))

# Opción alternativa (Fronterizos con Colombia):
# colombia_sf <- countries %>% filter(admin == "Colombia")
# neighbors_sf <- countries[st_touches(colombia_sf, countries, sparse = FALSE)[1, ], ]
# labels <- st_centroid(neighbors_sf) %>% cbind(st_coordinates(.))


# 7. DEFINIR LA EXTENSIÓN GEOGRÁFICA DEL MAPA
# ------------------------------------------------------------------------------
# Ajusta estos límites según la extensión de tu área de estudio.
#
# x = longitud (valores más negativos = más al oeste)
# y = latitud  (valores más positivos = más al norte)

map_xlim <- c(-82, -57)
map_ylim <- c(-23, 15)


# 8. DEFINIR LA VENTANA PARA DESCARGAR LA ELEVACIÓN
# ------------------------------------------------------------------------------
# Definimos el polígono (Bounding Box) correspondiente a la región del DEM.

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
# ------------------------------------------------------------------------------
# Descarga el terreno desde internet (z = 6 es ideal para alcance regional).

dem_raster <- get_elev_raster(
  locations = bbox_area,
  z = 6,
  clip = "bbox"
)

raster::writeRaster(
  dem_raster,
  filename = "Raster/demSedano.tif",
  format = "GTiff",
  overwrite = TRUE
)

# Convertir a SpatRaster para compatibilidad con tidyterra
col_dem <- terra::rast("Raster/demSedano.tif")


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
      y = decimalLatitude
    ),
    color = "darkblue",
    size = 1.8,
    alpha = 0.8
  ) +
  
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
    title = bquote(italic(.(species_name))),
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


# 11. VISUALIZAR EL MAPA
# ------------------------------------------------------------------------------
print(map_plot)


# 12. EXPORTAR EL MAPA
# ------------------------------------------------------------------------------
# Guarda la imagen en alta resolución (300 DPI).
# Puedes usar extensión .png, .jpeg o .pdf (vectorial).

ggsave(
  filename = "Mapas/mapa_biogeografia_final.png",
  plot = map_plot,
  width = 7,
  height = 7,
  units = "in",
  dpi = 300
)

