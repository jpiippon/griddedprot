# Practicalities

# !!!! numeroi scriptit lopuksi: missä järjestyksessä tulee ajaa
# !!! kehittele funktio, joka laskee 99% quantilen kullekin layerille ja trimmaa rasteria sen mukaan?

# if (!dir.exists("Data")){ dir.create("Data") } 
# if (!dir.exists("Figures")){ dir.create("Figures") } # modify and add to packages etc
# if (!dir.exists("Data/Input")){ dir.create("Data/Input") }
# if (!dir.exists("Data/Intermediate_input")){ dir.create("Data/Intermediate_input") }
# if (!dir.exists("Data/Output")){ dir.create("Data/Output") }

# Packages etc

# packages <- c("tidyverse", "terra", "tmap", "scico",
#               "sf", "here", "tictoc", "rmapshaper",  "countrycode",
#               "readxl", "broom", "tidyr", "flextable", "gridExtra")
# 
# not_installed <- packages[!(packages %in% installed.packages()[,"Package"])]
# #if(length(not_installed)){install.packages(not_installed)}
# 
# if (length(not_installed) > 0) {
#   install.packages(not_installed)
# } # possible that "proxy" and "httpcode" (sf and flextable dependencies)
# # need to be installed separately
# 
# 
# lapply(packages, library, character.only = TRUE)

# same as 
library(tidyverse); library(terra); library(sf)
library(scico); library(tmap)
library(here); library(tictoc); library(rmapshaper)
library(countrycode);library(readxl)
library(broom); library(tidyr)
library(Rfast); library(matrixStats)
library(flextable); library(gridExtra)



# options, mainly for terra package
#terraOptions(tempdir= here("Temp_R"))
terraOptions()
terraOptions(memfrac=0.9,
             verbose = F)
mem_info(rast())
tempdir() # where are temporary files 
terraOptions() # free_RAM()/1e6 # amount of available RAM
# timestep
timestep_2000_2015 <- 2000:2015 # !! begins in 2000
timestep_2000_2020 <- 2000:2020
timestep_2001_2015 <- 2001:2015 # is this even needed?
timestep_2001_2020 <- 2001:2020
timestep_climnorm <- 1991:2020


template_rast_5arcmin <- rast(nrows=180*60/5, ncols= 360*60/5 , # 2160 rows 4320 cols
                              crs = "EPSG:4326")

template_rast_1arcmin <- rast(nrow=180*60, ncol=360*60, # 10800 rows 21600 cols
                              crs = "EPSG:4326") 

template_rast_30arcsec <- rast(nrows=180*60*2, ncols=360*60*2, # 21600 rows 43200 cols
                               crs = "EPSG:4326")



e <- ext(-180, 180, -90, 90)



# Polygons
adm_10m <- here("Data", "Input", "ne_10m_admin_0_countries.shp") |> 
  read_sf()

Finland_geom <- adm_10m %>% filter(ADMIN == "Finland") %>% dplyr::select(ADMIN) |> 
  as("Spatial") |>   vect()

#Finland_ext <- ext(21, 31.5, 60, 70)







# -------------------------------------------------------- create country raster
# adm10_simple <- ms_simplify(adm_10m) #  203 row -- was 258
# 
# # #change adm10 iso codes to fao codes
# adm10_simple_faoadded <- adm10_simple %>%
#   dplyr::select(ADMIN, NAME, SOVEREIGNT, ISO_A3_EH, REGION_UN) %>%
#   as.data.frame() %>%
#   st_drop_geometry()
# #
# adm10_simple_faoadded <- adm10_simple_faoadded %>%
#   mutate(fao_from_iso3eh = countrycode(.$ISO_A3_EH, origin = "iso3c", destination = "fao"),
#          # find fao code also using name of sovereignts (combine these cols later to get match)
#          fao_from_SOVEREIGNT = countrycode(.$SOVEREIGNT, origin = "country.name",  destination = "fao"))
# ## warns that some are missing. However, either fao_from_iso3eh or fao_from_SOVEREIGNT includes most
# 
# ## give Somaliland FAO code of Somalia
# adm10_simple_faoadded <- adm10_simple_faoadded %>%
#   rows_update(., tibble(
#     SOVEREIGNT = "Somaliland",
#     fao_from_SOVEREIGNT = (filter(adm10_simple_faoadded, SOVEREIGNT == "Somalia") %>%
#                              pull(fao_from_SOVEREIGNT))))
# 
# 
# 
# # ## combine cols fao_from_iso3eh and fao_from_SOVEREIGNT.
# # ## If any of the columns has value, this value will be the FAO_ID
# adm10_simple_faoadded <- adm10_simple_faoadded %>%
#   mutate(FAO_ID = coalesce(fao_from_iso3eh, fao_from_SOVEREIGNT)) %>%
#   filter(SOVEREIGNT != "Antarctica")  ## drop antarctica
# 
# ## give South Sudan FAO code of Sudan
# # adm10_simple_faoadded <- adm10_simple_faoadded %>%
# #   rows_update(., tibble(
# #     SOVEREIGNT = "South Sudan",
# #     FAO_ID = (filter(adm10_simple_faoadded, SOVEREIGNT == "Sudan") %>%
# #                              pull(FAO_ID))))
# 
# adm10_simple_faoadded <- st_as_sf(adm10_simple_faoadded)
# # save for intermediate use- --> note that South Sudan = Sudan code
# st_write(adm10_simple_faoadded,
#          here("Data", "Intermediate_input", "adm10_simple_faoadded.gpkg"),
#          append = FALSE)

# odd countries
# adm10_simple_faoadded %>%
#   filter(ISO_A3_EH == -99) %>% dplyr::select(SOVEREIGNT) %>% 
#   st_drop_geometry()


 ## to be bit faster save and read this file
# st_write(adm10_simple_faoadded, here("Data", "Intermediate_input", "adm10_simple_faoadded.gpkg"))
adm10_simple_faoadded <-
  here("Data", "Intermediate_input", "adm10_simple_faoadded.gpkg") %>%
  st_read()

adm10_simple_faoadded <- 
  adm10_simple_faoadded |> 
  mutate(ID = row_number())



## convert to raster
cntry_raster <- rasterize(vect(adm10_simple_faoadded),
                          template_rast_5arcmin, field = "FAO_ID")
#plot(cntry_raster, main = "Antarctica neede or not?")

  ## convert to rob



adm10_simple_faoadded_rob <-  adm10_simple_faoadded %>% 
  st_transform(., crs = "ESRI:54030")



# -------------------------------------------------------- not sure which data to use for countries
# -------------------------------------------------------- 
# admin borders originally created by Vili
# possible to derive from adm10m using e.g package "countryname"
# adm0_vili <-  here("Data", "Input", "adm0_polygons_025dgr.shp") %>%
#   read_sf() %>%
#   dplyr::select(ADMIN, ISO_3166_1, FAO_ID, NAME)
# 
# adm0_vili_vecto <- adm0_vili %>% as("Spatial") %>% vect() # 168 unique
# 
# #
# 
# 
# # combine adm10m data with fao id
# df_adm10m <- as.data.frame(adm_10m) %>%
#   dplyr::select(ADMIN, NAME, geometry)
# df_adm0_vili <- as.data.frame(adm0_vili)
# 
# # join. We want to preserve countries of df_adm0_vili but add more precise geometries
# df_adm_10m_fao_id <-
#   left_join(df_adm0_vili,
#             df_adm10m, by = c("ADMIN", "NAME" )) %>% # 176 obs.
#   # Combined by name as did not find country codes
#   dplyr::select(ADMIN, NAME, ISO_3166_1, FAO_ID,  geometry.y) %>%
#   rename(geometry = geometry.y)
# head(df_adm_10m_fao_id)
# 
# # df_adm0_vili %>% filter(ADMIN == "Finland") %>% head()
# # df_adm_10m_fao_id %>% filter(ADMIN == "Finland") %>% head()
# 
# 
# 
# adm_10m_fao_id_simple <- st_as_sf(df_adm_10m_fao_id) %>%
#   ms_simplify()
# adm_10m_fao_id_simple_rob <- adm_10m_fao_id_simple %>%
#   st_transform(., crs = "ESRI:54030")
# ---------------------------------------------------------------
# 
# 
# cntry_raster <- rasterize(vect(adm_10m_fao_id_simple),
#                           template_rast_5arcmin, field = "FAO_ID")
# 

# -------------------------------------------------------- 
# -------------------------------------------------------- 




 ## regions
reg <- here("Data", "Input", "reg_mollw.gpkg") |>  st_read()
reg_rob <- st_transform(reg, crs = "ESRI:54030")
reg_wgs <- st_transform(reg, crs = "EPSG:4326")
reg_wgs_vect <- vect(as(reg_wgs, "Spatial"))
# 
reg_rob <- reg_rob |>
  mutate(subregion = c("Australia and Oceania", "Central America",
                       "East Asia", "Eastern Europe and Central Asia",
                       "Ice", "South Asia", "South America", "Middle East",
                       "Sub-Saharan Africa", "North Africa", "North America",
                       "Southeast Asia", "Western Europe")) #|>   filter(subregion != "Ice") # maybe not needed ## it is needed for figs




# simplify only for plotting
# reg_rob_simple <- ms_simplify(reg_rob) # Remove ice or not?
# st_write(reg_rob_simple, here("Data", "Intermediate_input", "reg_rob_simple.gpkg"))

reg_rob_simple <- here("Data", "Intermediate_input", "reg_rob_simple.gpkg") %>% st_read()


## function for cropping and maskin
crop_and_mask <- function(r_data, df_cropmask_polygon){
  r_data |> 
    crop(df_cropmask_polygon) |> 
    mask(df_cropmask_polygon) 
}

## function for plotting
create_index_map <- function(r_index, index_label,index_main_title,
                             colorpal, breakvals,
                             breaknames = NULL,
                             color_midpoint = NULL, tocrs = NA, colorNA = NULL){
  if (!is.na(tocrs)){
    r_index <- project(r_index, tocrs, mask = TRUE)
  }
  index_map <- tm_shape(r_index) + # add here raster.downsample = 0 if needed
    tm_raster(palette = colorpal, # try style = "fixed", 
              breaks = breakvals,
              labels = breaknames,
              title = index_label,
              midpoint = color_midpoint,
              colorNA = colorNA,
              legend.is.portrait = FALSE) + # added 9.8.22
            #  legend.reverse = TRUE) + # deleted 9.8.22
    tm_layout(main.title = index_main_title,
              main.title.position = "center",
              main.title.size = 1,
              legend.bg.color = TRUE,
              legend.outside = TRUE,
              legend.title.size = 1,
              legend.text.size = 1,
              legend.outside.size = 0.2,
              legend.outside.position = "bottom", # added 9,8
              frame = FALSE)+
    tm_shape(adm10_simple_faoadded_rob) + # was reg_rob_simple
    tm_borders(col = NA,  lwd = 0.15)  # lwd was 0.33, col was "grey30",
  
  return (index_map)
} 




## plotting without contry borders (reg instead)
create_index_map_no_cntry <- function(r_index, index_label,index_main_title,
                             colorpal, breakvals,
                             breaknames = NULL,
                             color_midpoint = NULL, tocrs = NA, colorNA = NULL){
  if (!is.na(tocrs)){
    r_index <- project(r_index, tocrs, mask = TRUE)
  }
  index_map <- tm_shape(r_index) + 
    tm_raster(palette = colorpal, # try style = "fixed", 
              breaks = breakvals,
              labels = breaknames,
              title = index_label,
              midpoint = color_midpoint,
              legend.is.portrait = FALSE) + # added 9.8.22
    #  legend.reverse = TRUE) + # deleted 9.8.22
    tm_layout(main.title = index_main_title,
              main.title.position = "center",
              main.title.size = 1,
              legend.bg.color = TRUE,
              legend.outside = TRUE,
              legend.title.size = 1,
              legend.text.size = 1,
              legend.outside.size = 0.2,
              legend.outside.position = "bottom", # added 9,8
              frame = FALSE)+
    tm_shape(reg_rob_simple  ) + 
    tm_borders(col = NA,  lwd = 0.5)  # lwd was 0.33, col was "grey30",
  
  return (index_map)
} 





# Function to remove outliers. Only needed when calculating global sums
f_raster_without_outliers <- function(myraster_layer) {
  # Calculate quantiles for both 0.01 and 0.99
  quantiles <- quantile(values(myraster_layer), probs = c(0.01, 0.99), na.rm = T)
  
  # Define classification rules with "from-to-becomes" structure
  rcl_outliers <- matrix(c(-Inf, quantiles[1], quantiles[1],
                           quantiles[2], Inf, quantiles[2]), ncol = 3, byrow = T)
  
  # Classify raster values according to rules, with include.lowest and right set
  myraster_new <- classify(myraster_layer, rcl_outliers, include.lowest = TRUE, right = NA)
  
  return(myraster_new)
}








land_mask <- ifel(cntry_raster > 0, 1, NA)
#plot(land_mask)

