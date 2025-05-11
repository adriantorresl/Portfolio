#PAQUETERIAS
pacman::p_load(
  terra,
  elevatr,
  sf,
  geodata,
  tidyverse,
  rayshader,
  osmdata,
  tmap,
  purrr)
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
    -100.86190  25.22379,
    -100.86190  26.39517,
    -99.68620   26.39517,
    -99.68620   25.22379,
    -100.86190  25.22379))"
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
      ORD_FLOW),
    width = dplyr::case_when(
      width == 5 ~ 16, 
      width == 6 ~ 14,
      width == 7 ~ 12,
      width == 8 ~ 10,
      width == 9 ~ 6,
      TRUE ~ 0)) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = crs_amm)
#ELEVACION
dem <- elevatr::get_elev_raster(
  locations = area_metropolitana_sf,
  z = 9, clip = "locations")
dem_amm <- dem |>
  terra::rast() |>
  terra::project(crs_amm)
dem_matrix <- rayshader::raster_to_matrix(dem_amm)
#PENDIENTE Y DIRECCIÓN DE FLUJO
slope   <- terra::terrain(dem_amm, v = "slope", unit = "degrees")
flowdir <- terra::terrain(dem_amm, v = "flowdir")
#RASTERIZAR ORD_FLOW SOBRE EL DEM
cuencas_ras <- terra::rasterize(
  vect(grosor), dem_amm,
  field = "width", background = 0)
#ACUMULACIÓN DE FLUJO
accum <- terra::flowAccumulation(flowdir)
#NORMALIZAR
accum_norm <- (accum - minmax(accum)[1]) / (minmax(accum)[2] - minmax(accum)[1])
#CALCULAR EL RIESGO COMBINANDO AMBOS FACTORES
riesgo_combinado <- accum_norm * cuencas_ras
#VISUALIZACION 2D
#NORMALIZAR EL RIESGO COMBINADO
riesgo_combinado[riesgo_combinado==0]<-NA
riesgo_raster <- riesgo_combinado / max(riesgo_combinado[], na.rm = TRUE)
curvas_nivel <- terra::as.contour(
  dem_amm, 
  levels = seq(0, 3000, by = 500)
) |> 
  st_as_sf()
#CREAR MAPA
tm_shape(riesgo_raster) +
  tm_raster(palette = "-RdYlGn", alpha = 1, title = "Riesgo") +
  tm_shape(curvas_nivel) +
  tm_lines(col = "black", lwd = 0.3) +  # Curvas de nivel
  tm_shape(grosor) +
  tm_lines(col = "lightblue", lwd = "ORD_FLOW", scale = 2) +
  tm_layout(legend.position = c("right", "top"))


