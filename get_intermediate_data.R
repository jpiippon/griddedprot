# Get intermediate data

# ---------------------------------------------------------------- crops
# Crop production areas
r_physical_areas_crops_sum_ha_perpix <-
  here("Data", "Intermediate_input","r_physical_areas_crops_sum_ha_perpix.tif") %>% 
  rast()


# Crop total yields
r_global_production_crops_sum_kg_ha <-
  here("Data", "Intermediate_input","r_global_production_crops_sum_kg_ha.tif") %>% 
  rast()

# Crop protein and energy
r_prot_allcrops_sum_mt_perpix <- here("Data", "Intermediate_input","protein_production_27crops_sum_mt_perpix.tif") %>%
  rast() 

r_prot_allcrops_sum_kg_ha <- here("Data", "Intermediate_input","protein_production_27crops_sum_kg_ha.tif") %>%
  rast()




# ---------------------------------------------------------------- livestock
# Livestock protein and energy
r_protein_from_065xAGB_kg_perpix <- here("Data", "Intermediate_input","r_protein_from_065xAGB_kg_perpix.tif") %>% rast() 

r_protein_from_065xAGB_kg_ha <- here("Data", "Intermediate_input","r_protein_from_065xAGB_kg_ha.tif") %>% rast() 


r_kcal_from_065xAGB_MM_ha <- here("Data", "Intermediate_input","r_kcal_from_065xAGB_MM_ha.tif") %>%  rast() 

r_kcal_from_065xAGB_MM_perpix <- 
  here("Data", "Intermediate_input","r_kcal_from_065xAGB_MM_perpix.tif") %>% 
  rast()




# ------------------------------------------------------------------- fractions
# Fraction of crops in a cell
r_fraction_cl <-
  here("Data", "Intermediate_input","r_fraction_cl_with_27_spamcrops.tif") %>% 
  rast()
r_fraction_cl[r_fraction_cl >1] <- 1
r_fraction_cl_0toNA <- classify(r_fraction_cl, cbind(0,NA))
r_fraction_cl_NAto0 <- classify(r_fraction_cl, cbind(NA,0))



r_fraction_gl <- 
  here("Data", "Input", "from_harddrive", 
       "fraction_of_cell_that_is_hyde_grazingland2010_0toNA.tif") %>%   #### changed 
  rast()
r_fraction_gl[r_fraction_gl >1] <- 1
r_fraction_gl_0toNA <- classify(r_fraction_gl, cbind(0,NA))
r_fraction_gl_NAto0 <- classify(r_fraction_gl, cbind(NA,0))

r_fraction_gl_cl_total <- r_fraction_gl_NAto0 + r_fraction_cl_NAto0 # cannot be over 1!
r_fraction_gl_cl_total[r_fraction_gl_cl_total>1] <- 1
r_fraction_gl_cl_total_NAto0 <- classify(r_fraction_gl_cl_total, cbind(NA,0)) # totally same as above!
r_fraction_gl_cl_total_0toNA <- classify(r_fraction_gl_cl_total, cbind(0,NA))


# --------------------------------------------------------- Suitability indices

SI_5arcmin <- 
  here("Data", "Input", "Zabel22_SI",
       "SI_5arcmin_overall_suitability_subset1to17_hist1980_2009_current_irr_areas_applied.tif") %>% 
  rast()