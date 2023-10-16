# Get intermediate data

# palettes
pal_npp <- scico(n = 5,begin = 0.6, end = 1, palette = "corkO")
pal_agb <- scico(n = 6, palette = "bamako", direction = -1)
pal_protein_lajolla <- scico(n = 6, palette = "lajolla", end = 0.85) 
pal_share_nuuk <- scico(n = 7, palette = "nuuk",  direction = -1) 
pal_fcr <- scico(n = 6, palette = "tokyo", direction = -1)
pal_lur <- scico(n =4, palette = "imola", direction = -1) 
pal_cv <-  scico(n = 6, palette = "nuuk", direction = -1)

# test
pal_protein_test <- 
  c("#66c2a5", "#a6d854", "#fee08b", "#fdae61", "#f46d43", "#d53e4f")



# ---------------------------------------------------------------- crops
# Crop production areas
r_physical_areas_crops_sum_ha_perpix <-
  here("Data", "Intermediate_input","r_physical_areas_crops_sum_ha_perpix.tif") %>% 
  rast()


# Crop total yields
r_global_production_crops_sum_kg_ha <-
  here("Data", "Intermediate_input","r_global_production_crops_sum_kg_ha.tif") %>% 
  rast()

# Crop protein 
r_prot_allcrops_sum_mt_perpix <- here("Data", "Intermediate_input","protein_production_27crops_sum_mt_perpix.tif") %>%
  rast() 

r_prot_allcrops_sum_kg_ha <- here("Data", "Intermediate_input","protein_production_27crops_sum_kg_ha.tif") %>%
  rast()


# ---------------------------------------------------------------- livestock
# Livestock protein and energy
r_protein_from_AGB_kg_perpix <- here("Data", "Intermediate_input","r_protein_from_AGB_kg_perpix.tif") %>% rast() 

r_protein_from_AGB_kg_ha <- here("Data", "Intermediate_input","r_protein_from_AGB_kg_ha.tif") %>% rast() 



# ------------------------------------------------------------------- fractions
# Fraction of crops in a cell
r_fraction_cl <-
  here("Data", "Intermediate_input","r_fraction_cl_with_27_spamcrops.tif") %>% 
  rast()
r_fraction_cl[r_fraction_cl >1] <- 1
r_fraction_cl_0toNA <- classify(r_fraction_cl, cbind(0,NA))
r_fraction_cl_NAto0 <- classify(r_fraction_cl, cbind(NA,0))



# r_fraction_gl <- 
#   here("Data", "Input", "from_harddrive", 
#        "fraction_of_cell_that_is_hyde_grazingland2010_0toNA.tif") %>%   #### changed 
#   rast() # includes corn belt as this is HYDE based gl

r_fraction_gl <- 
  here("Data", "Input", "from_harddrive", # NO Corn Belt
       "r_fraction_hyde_grazing_lands_masked_to_MODIS_IGBP_areas_0toNA.tif") %>%   #### changed  was HYDE area only
  rast()

  # these should be in the actual 5 and 6 scripts
r_fraction_gl[r_fraction_gl >1] <- 1
r_fraction_gl_0toNA <- classify(r_fraction_gl, cbind(0,NA))
r_fraction_gl_NAto0 <- classify(r_fraction_gl, cbind(NA,0))

r_fraction_gl_cl_total <- r_fraction_gl_NAto0 + r_fraction_cl_NAto0 # cannot be over 1!
r_fraction_gl_cl_total[r_fraction_gl_cl_total>1] <- 1
r_fraction_gl_cl_total_NAto0 <- classify(r_fraction_gl_cl_total, cbind(NA,0)) # totally same as above!
r_fraction_gl_cl_total_0toNA <- classify(r_fraction_gl_cl_total, cbind(0,NA))


# additional masks
r_cl_outside_gl <- 
  mask(r_fraction_cl_0toNA, r_fraction_gl_0toNA, inverse = T)

r_gl_outside_cl <- 
  mask(r_fraction_gl_0toNA, r_fraction_cl_0toNA, inverse = T)

# ------------------------------------------------------- +90% gl, mixed, +90cl

# --------------------------------------------------------------------- categories
# Määritä alkuarvot
threshold_gl_or_cl_shareover <- 90
threshold_mixed_share_gte <- 10

# Grazing land osuus maatalousmaasta
r_gl_share_of_agrland <-  100 * r_fraction_gl_NAto0 / r_fraction_gl_cl_total_0toNA 
r_gl_share_of_agrland_over_threshold <- terra::ifel(r_gl_share_of_agrland <= threshold_gl_or_cl_shareover, NA, r_gl_share_of_agrland)


# Croplandin osuus maatalousmaasta
r_cl_share_of_agrland <-  100 * r_fraction_cl_NAto0 / r_fraction_gl_cl_total_0toNA 
r_cl_share_of_agrland_over_threshold <- terra::ifel(r_cl_share_of_agrland <= threshold_gl_or_cl_shareover, NA, r_cl_share_of_agrland)


# Mixed: threshold_mixed_share_gte% tai enemmän maatalousmaasta molempia
r_gl_mask_over_threshold_mixed <- terra::ifel(r_gl_share_of_agrland >= threshold_mixed_share_gte, 1, NA)
r_cl_mask_over_threshold_mixed <- terra::ifel(r_cl_share_of_agrland >= threshold_mixed_share_gte, 1, NA)

# Yhdistä maskit
r_mixed_mask <- r_gl_mask_over_threshold_mixed * r_cl_mask_over_threshold_mixed

# Määritä mixed-alueet
r_gl_cl_share_mixed <- terra::ifel(r_mixed_mask == 1, 1, NA)


# plot(r_gl_share_of_agrland_over_threshold, main = paste("Grazing land share of agr land >", threshold_gl_or_cl_shareover, "%"))
# plot(r_gl_cl_share_mixed, main = paste("Mixed system: At least", threshold_mixed_share_gte, "% both gl and cl"))
# plot(r_cl_share_of_agrland_over_threshold, main = paste("Cropland share of agr land >", threshold_gl_or_cl_shareover, "%"))

# give new names
r_gl_share_of_agrland_over90 <- r_gl_share_of_agrland_over_threshold
r_cl_share_of_agrland_over90 <- r_cl_share_of_agrland_over_threshold


# --------------------------------------------------------- Suitability indices
SI_5arcmin <- 
  here("Data", "Input", "Zabel22_SI",
       "SI_5arcmin_overall_suitability_subset1to17_hist1980_2009_current_irr_areas_applied.tif") %>% 
  rast()

SI_5arcmin_range_1to10 <- SI_5arcmin
SI_5arcmin_range_1to10[SI_5arcmin_range_1to10 >= 10] <- NA
SI_5arcmin_range_1to10[SI_5arcmin_range_1to10 < 1] <- NA
# Create a vector of threshold values from 0 to 99
threshold_values <- 0:99


# ----------------------------------------------------------- 7 comparisons
r_protein_combined_kg_ha <- here("Data", "Intermediate_input", "r_protein_combined_kg_ha.tif") %>%
  rast()

r_protein_combined_kg_perpix <- 
  here("Data", "Intermediate_input", "r_protein_combined_kg_perpix.tif") %>% 
  rast()

r_protein_combined_kg_ha_wheremixed <-
  here("Data", "Intermediate_input", "r_protein_combined_kg_ha_wheremixed.tif") %>% 
  rast()

r_protein_combined_kg_ha_SIbetween1to10mask <- here("Data", "Intermediate_input", "r_protein_combined_kg_ha_SIbetween1to10mask.tif") %>%
  rast()



r_share_of_total_protein_livestock <- here("Data", "Intermediate_input", "r_share_of_total_protein_livestock.tif") %>%
  rast()

r_share_of_total_protein_livestock_mixed <- 
  here("Data", "Intermediate_input", "r_share_of_total_protein_livestock_mixed.tif") %>% 
  rast()

r_share_of_total_protein_livestock_SIbetween1to10mask <- here("Data", "Intermediate_input", "r_share_of_total_protein_livestock_SIbetween1to10mask.tif") %>%
  rast()



# --------------------------------------------------------------- regressions
r_predicted_AGB_based_protein_yield_kg_ha <- 
  here("Data", "Intermediate_input",
                "r_predicted_AGB_based_protein_yield_kg_ha.tif") %>% 
  rast()

r_predicted_AGB_based_protein_yield_on_cl_where_agb_prot_yield_not_known_kg_ha <-
  here("Data", "Intermediate_input",
       "r_predicted_AGB_based_protein_yield_on_cl_where_agb_prot_yield_not_known_kg_ha.tif") %>% 
  rast()


r_predicted_crop_protein_yield_kg_ha <-
  here("Data", "Intermediate_input","r_predicted_crop_protein_yield_kg_ha.tif") %>% rast()


r_predicted_crop_protein_yield_outside_cl_but_in_gl_areas_kg_ha  <-
  here("Data", "Intermediate_input","r_predicted_crop_protein_yield_outside_cl_but_in_gl_areas_kg_ha.tif") %>% 
  rast()


r_agb_prot_existing_plus_new_areas_kg_ha <-
  here("Data", "Intermediate_input", "r_agb_prot_existing_plus_new_areas_kg_ha.tif") %>% 
  rast()

r_crop_prot_existing_plus_new_areas_kg_ha <-
  here("Data", "Intermediate_input", 
       "r_crop_prot_existing_plus_new_areas_kg_ha.tif") %>% 
  rast()


# ------------------------------------------------------------------- LUR
LUR_SI_based_NAto0_positive <-
  here("Data", "Intermediate_input", "LUR_SI_based_NAto0_positive.tif") %>% 
  rast()


names(LUR_SI_based_NAto0_positive) <-
  paste0("LUR_SI_gte", threshold_values)


LUR_predicted_nlyr3 <- 
  here("Data", "Intermediate_input",
       "LUR_predicted.tif") %>% 
  rast()




pal_lur <- scico(n =4, palette = "imola", direction = -1) 
# 
pal_lur_inc_grey <- c("#A0A0A0", pal_lur) # add grey to -999 values
# 



