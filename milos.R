# 1. LIBRARIES
#-------------

install.packages("pacman")
install.packages("elevatr")
install.packages("rayshader")
pacman::p_load(
  terra,
  elevatr,
  sf,
  geodata,
  tidyverse,
  rayshader
)
library(pacman)
library(terra)
library(elevatr)
library(sf)
library(geodata)
library(tidyverse)
library(rayshader)

# 2. COUNTRY BORDERS
#-------------------

path <- setwd("/Users/adriantorres/Desktop/UNIVERSIDAD/milos")

country_sf <- geodata::gadm(
  country = "MEX",
  level = 2,
  path = path
) |>
  sf::st_as_sf()
country_sf <- sf::st_as_sf(country_sf)
country_sf <- sf::st_make_valid(country_sf)

# 3. DOWNLOAD RIVERS
#-------------------

url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip"
destfile <- basename(url)

download.file(
  url = url,
  destfile = destfile,
  mode = "wb"
)

unzip(destfile)

# 4. LOAD RIVERS
#---------------

filename <- list.files(
  path = "HydroRIVERS_v10_na_shp",
  pattern = ".shp",
  full.names = TRUE
)

nuevo_leon_sf <- country_sf |> 
  dplyr::filter(NAME_1 == "Nuevo León")

    #SOLUCIONANDO EL ERROR
    subset <- country_sf[, c("NAME_1")]
    head(subset)
    nuevo_leon_sf <- dplyr::filter(country_sf, NAME_1 == "Nuevo León")


# Lista de municipios del área metropolitana de Monterrey
municipios_metropolitanos <- c(
  "Apodaca", "Escobedo", "García", "Guadalupe", 
  "Juárez", "Monterrey", "San Nicolás de los Garza", 
  "San Pedro Garza García", "Santa Catarina", "Cadereyta Jiménez", 
  "El Carmen", "Pesquería", "Santiago"
)

# Filtrar geometrías de los municipios metropolitanos
area_metropolitana_sf <- nuevo_leon_sf |>
  dplyr::filter(NAME_2 %in% municipios_metropolitanos)
country_bbox <- sf::st_bbox(area_metropolitana_sf)

#xmin       ymin       xmax       ymax 
#-100.86190   25.22379  -99.68620   25.98348 

bbox_wkt <- "POLYGON((
-100.86190   25.22379,
-100.86190 25.98348,
    -99.68620 25.98348,
    -99.68620 25.22379,
    -100.86190 25.22379
))"

country_rivers <- sf::st_read(
  filename,
  wkt_filter = bbox_wkt
) |>
  sf::st_intersection(
    country_sf
  )

plot(sf::st_geometry(country_rivers))

# 5. RIVER WIDTH
#---------------

sort(
  unique(
    country_rivers$ORD_FLOW
  )
)

crs_country <- "+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

country_river_width <- country_rivers |>
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
  sf::st_transform(crs = crs_country)

# 6. DEM
#-------

dem <- elevatr::get_elev_raster(
  locations = area_metropolitana_sf,
  z = 9, clip = "locations"
)

dem_country <- dem |>
  terra::rast() |>
  terra::project(crs_country)

dem_matrix <- rayshader::raster_to_matrix(
  dem_country
)

# 7. RENDER SCENE
#----------------
dem_matrix |>
  rayshader::height_shade(
    texture = colorRampPalette(
      c(
        "#fcc69f",
        "#c67847"
      )
    )(128)
  ) |>
  rayshader::add_overlay(
    rayshader::generate_line_overlay(
      geometry = country_river_width,
      extent = dem_country,
      heightmap = dem_matrix,
      color = "#387B9C",
      linewidth = country_river_width$width,
      data_column_width = "width"
    ), alphalayer = 1
  ) |>
  rayshader::plot_3d(
    dem_matrix,
    zscale = 20,
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(600, 600),
    zoom = .5,
    phi = 89,
    theta = 0
  )


#para cambiar la escena
rayshader::render_camera(
  zoom = .75
)

# 8. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- basename(u)

download.file(
  url = u,
  destfile = hdri_file,
  mode = "wb"
)

file_name <- "nl-3d-elevation-rivers.png"

rayshader::render_highquality(
  filename = file_name,
  preview = TRUE,
  light = FALSE,
  environment_light = hdri_file,
  intensity_env = 1,
  interactive = FALSE,
  width = 3000,
  height = 3000
)
