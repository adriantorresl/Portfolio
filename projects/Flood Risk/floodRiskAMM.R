#PAQUETERIAS
pacman::p_load(
  terra,
  elevatr,
  sf,
  geodata,
  tidyverse,
  rayshader,
  osmdata,
  tmap)
library(pacman)
library(terra)
library(elevatr)
library(sf)
library(geodata)
library(tidyverse)
library(rayshader)
#RUTA DE TRABAJO
path <- setwd('/Users/adriantorres/Documents/GitHub/Portfolio/projects/Flood Risk')
#ESTABLECER LIMITES PARA AREA DE TRABAJO
country_sf <- geodata::gadm(
  country = "MEX",
  level = 2,
  path = path) |>
  sf::st_as_sf()
country_sf <- sf::st_as_sf(country_sf)
country_sf <- sf::st_make_valid(country_sf)
#DESCARGAR CUENCAS 
url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip"
destfile <- basename(url)
download.file(
  url = url,
  destfile = destfile,
  mode = "wb")
unzip(destfile)
#IMPORTAR CUENCAS
filename <- list.files(
  path = "HydroRIVERS_v10_na_shp",
  pattern = ".shp",
  full.names = TRUE)
nuevo_leon_sf <- country_sf |> 
  dplyr::filter(NAME_1 == "Nuevo León")
#MUNICIPIOS DEL AMM
municipios_metropolitanos <- c(
  "Apodaca", "General Escobedo", "García", "Guadalupe", 
  "Juárez", "Monterrey", "San Nicolás de los Garza", 
  "San Pedro Garza García", "Santa Catarina", "Cadereyta Jiménez", 
  "El Carmen", "Pesquería", "Santiago", "Salinas Victoria")
#OBTENER GEOMETRIA
area_metropolitana_sf <- nuevo_leon_sf |>
  dplyr::filter(NAME_2 %in% municipios_metropolitanos)
country_bbox <- sf::st_bbox(area_metropolitana_sf)
#xmin       ymin       xmax       ymax 
#-100.86190   25.22379  -99.68620   26.39517 

bbox_wkt <- "POLYGON((
-100.86190   25.22379,
-100.86190 26.39517,
    -99.68620 26.39517,
    -99.68620 25.22379,
    -100.86190 25.22379
))"
#GRAFICAR LAS CUENCAS
cuencas <- sf::st_read(
  filename,
  wkt_filter = bbox_wkt) |>
  sf::st_intersection(
    area_metropolitana_sf)
plot(sf::st_geometry(cuencas))
#DARLE GROSOR A LAS CUENCAS
sort(
  unique(
    cuencas$ORD_FLOW))
crs_amm <- "+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

grosor <- cuencas |>
  dplyr::mutate(
    width = as.numeric(
      ORD_FLOW
    ),
    width = dplyr::case_when(
      width == 5 ~ 16, 
      width == 6 ~ 14,
      width == 7 ~ 12,
      width == 8 ~ 10,
      width == 9 ~ 6,
      TRUE ~ 0
    )
  ) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = crs_amm)
#DEM
dem <- elevatr::get_elev_raster(
  locations = area_metropolitana_sf,
  z = 9, clip = "locations")
dem_amm <- dem |>
  terra::rast() |>
  terra::project(crs_amm)
dem_matrix <- rayshader::raster_to_matrix(
  dem_amm)
#DESCARGAR EDIFICIOS OSM
edificios <- opq(bbox = country_bbox) |>
  add_osm_feature(key = "building") |>
  osmdata_sf()
#EXTRAER GEOMETRIA
edificios_sf <- edificios$osm_polygons |>
  sf::st_make_valid() |>
  sf::st_intersection(area_metropolitana_sf)
#PENDIENTE Y DIRECCIÓN DE FLUJO
slope <- terra::terrain(dem_amm, v = "slope", unit = "degrees")
flowdir <- terra::terrain(dem_amm, v = "flowdir")
#RASTERIZAR ORD_FLOW SOBRE EL DEM
cuencas_ras <- terra::rasterize(
  vect(grosor), dem_amm,
  field = "width", background = 0
)
#ACUMULACIÓN DE FLUJO
accum <- terra::flowAccumulation(flowdir)
accum[accum > quantile(values(accum), 0.99, na.rm = TRUE)] <- NA
#NORMALIZAR
accum_norm <- (accum - minmax(accum)[1]) / (minmax(accum)[2] - minmax(accum)[1])
#CALCULAR EL RIESGO COMBINANDO AMBOS FACTORES
riesgo_combinado <- accum_norm * cuencas_ras


# Paleta de colores para el riesgo
paleta_riesgo <- colorRampPalette(c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"))

# Crear la escena 3D básica
dem_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_overlay(
    # Superponer el riesgo
    height_shade(
      dem_matrix,
      texture = paleta_riesgo(256)[cut(as.vector(riesgo_matrix), breaks = 256)]
    ),
    alphalayer = 0.7
  ) %>%
  add_shadow(
    ray_shade(
      dem_matrix,
      sunaltitude = 30,
      sunangle = 315,
      zscale = 10
    ),
    max_darken = 0.3
  ) %>%
  add_shadow(
    ambient_shade(dem_matrix, zscale = 10),
    max_darken = 0.3
  ) %>%
  plot_3d(
    dem_matrix,
    zscale = 10,
    water = FALSE,
    shadowdepth = -50,
    windowsize = c(1200, 800),
    background = "white",
    theta = 230,
    phi = 35,
    zoom = 0.6,
    fov = 60
  )

# Añadir los ríos
render_path(
  grosor,
  extent = dem_amm,
  color = "blue",
  altitude = "relative",
  zscale = 10,
  clear_previous = FALSE
)

# Añadir los edificios (opcional, puede ser intensivo computacionalmente)
render_polygons(
  edificios_sf,
  extent = dem_amm,
  color = "#555555",
  height = 10,
  clear_previous = FALSE
)

# Añadir título
render_label(
  dem_matrix,
  text = "Riesgo de Inundación - Área Metropolitana de Monterrey",
  x = 0.5,
  y = 0.95,
  z = 1000,
  zscale = 10,
  relativez = FALSE,
  textsize = 2,
  linewidth = 2
)

# Añadir elementos del mapa
render_scalebar(
  limits = c(0, 5, 10),
  label_unit = "km",
  position = "bottomright",
  y = 50,
  z = 500,
  zscale = 10
)

render_compass(
  position = "topright",
  z = 1000,
  compass_radius = 100,
  zscale = 10
)











#MATRIZ DE ELEVACION
elmat <- rayshader::raster_to_matrix(dem_amm)
# Reescalar el raster de riesgo para que coincida con el tamaño del DEM
riesgo_resample <- terra::resample(riesgo_combinado, dem_amm)
riesgo_mat <- rayshader::raster_to_matrix(riesgo_resample)
riesgo_mat_norm <- riesgo_mat / max(riesgo_mat, na.rm = TRUE)
# Normalizar
riesgo_mat <- riesgo_mat / max(riesgo_mat, na.rm = TRUE)
# Convertir matriz de riesgo a overlay de colores
hillshade <- height_shade(elmat, texture = grey.colors(256))
library(RColorBrewer)
# Crear paleta YlOrRd invertida (más rojo = más riesgo)
paleta <- rev(brewer.pal(9, "YlOrRd"))
color_overlay <- height_shade(riesgo_mat_norm, texture = colorRampPalette(paleta)(256))

plot_3d(
  elmat,
  zscale = 30,
  hillshade=riesgo_mat_norm,
  windowsize = c(1000, 800),
  solid = FALSE,
  shadow = TRUE,
  theta = 225, phi = 45, zoom = 0.6,
  background = "white"
)

# 6. Añadir overlay de riesgo
add_overlay(color_overlay, alphalayer = 0.6)

# 7. Visor interactivo
rgl::rglwidget()










#DELIMITAR ZONAS DE RIESGO
umbral <- quantile(values(accum), 0.90, na.rm = TRUE)
zonas_riesgo <- accum > umbral
#POLÍGONOS DE RIESGO
zonas_riesgo_vect <- terra::as.polygons(zonas_riesgo, dissolve = TRUE)
zonas_riesgo_vect <- terra::intersect(zonas_riesgo_vect, vect(area_metropolitana_sf))



