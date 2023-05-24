mada <-  adm_10m %>% filter(ADMIN == "Madagascar") %>% dplyr::select(ADMIN) |> 
  as("Spatial") |>   vect()

# crop and mask
LO_i_current_NAto0_mada <- crop_and_mask(LO_i_current_NAto0, mada)
r_optimised_protein_crops_mt_perpix_mada <- crop_and_mask(r_optimised_protein_crops_mt_perpix, mada)
r_SIbased_restricted_max_cropland_fraction_NAto0_mada <- crop_and_mask(r_SIbased_restricted_max_cropland_fraction_NAto0, mada)

r_optimised_protein_crops_kg_ha_mada <-
  (r_optimised_protein_crops_mt_perpix_mada * 1000) /
  (cellSize(r_optimised_protein_crops_mt_perpix_mada[[1]], unit = "ha") * 
     r_SIbased_restricted_max_cropland_fraction_NAto0_mada)

r_optimised_protein_crops_kg_ha_mada <-
  classify(r_optimised_protein_crops_kg_ha_mada,
           rclmat_neg_to_zeros, right = FALSE) 
# on Inf kun fractio lähestyy nollaa
r_optimised_protein_crops_kg_ha_mada_Infto0 <-
  classify(r_optimised_protein_crops_kg_ha_mada, cbind(Inf, 0))



HDP_i_SI_based_mada <- r_optimised_protein_crops_kg_ha_mada * 0.85

LUR_SI_based_mada <- 
  (LO_i_current_NAto0_mada * HDP_i_SI_based_mada) / 
  HDP_animal_prot_i_current

LUR_SI_based_mada_NAto0 <- classify(LUR_SI_based_mada, cbind(NA,0))
# mask to procution areas
LUR_SI_based_mada_NAto0 <- 
  mask(LUR_SI_based_mada_NAto0, crop_and_mask(r_fraction_gl_cl_total_0toNA, mada))


(plt_LUR_mada <- 
    create_index_map(r_index = LUR_SI_based_mada_NAto0[[10]], 
                     tocrs = "ESRI:54030",
                     index_main_title = "",
                     index_label = "[LUR]",
                     colorpal = pal_lur,
                     breakvals = c(0, 1, 10, 100,  Inf),
                     breaknames = c("0-1","1-10","10-100",">100"))) 


(plt_LUR_mada <- 
    create_index_map(r_index = LUR_SI_based_mada_NAto0[[95]], 
                     tocrs = "ESRI:54030",
                     index_main_title = "",
                     index_label = "[LUR]",
                     colorpal = pal_lur,
                     breakvals = c(0, 1, 10, 100,  Inf),
                     breaknames = c("0-1","1-10","10-100",">100"))) 


# miksi jotkut pikselit muuttuvat >100 kategoriaan? Eivät alun perin sitä ole


# testataan lähtödatalla
plot(r_optimised_protein_crops_kg_ha_mada[[10]])
plot(r_optimised_protein_crops_kg_ha_mada[[95]]) 
# plotti näyttää vain arvoja 0 koska ei ole SI>95 alueita, eli ei ole cl kun ollaan noin vaativia
# !! summary r_optimised_protein_crops_kg_ha_mada[[95]] näyttää kuitenkin myös inf arvoja??
plot(crop_and_mask(SI_5arcmin, mada )) # 85 = max SI


# r_optimised_protein_crops_kg_ha_mada
create_index_map(r_index = r_optimised_protein_crops_kg_ha_mada[[10]], 
                     tocrs = "ESRI:54030",
                     index_main_title = "",
                     index_label = "[kg]",
                     colorpal = pal_protein_lajolla,
                     breakvals = c(0, 10, 50, 100, 250, 500, Inf),
                     breaknames = c("0-10","10-50", "50-100",
                                    "100-250","250-500",  ">500"))

create_index_map(r_index = r_optimised_protein_crops_kg_ha_mada[[95]], 
                 tocrs = "ESRI:54030", 
                 index_main_title = "",
                 index_label = "[kg]", # Inf arvot jäävät näkyviin -- pitäisikö niiden olla 0?
                 colorpal = pal_protein_lajolla,
                 breakvals = c(0, 10, 50, 100, 250, 500, Inf),
                 breaknames = c("0-10","10-50", "50-100",
                                "100-250","250-500",  ">500"))

 #r_optimised_protein_crops_mt_perpix_mada ## PERPIX
create_index_map(r_index = r_optimised_protein_crops_mt_perpix_mada[[99]], 
                 tocrs = "ESRI:54030",
                 index_main_title = "",
                 index_label = "[mt perpix]", # ok -- ei juuri proteiinia kun SI > 99
                 colorpal = pal_protein_lajolla,
                 breakvals = c(0, 10, 50, 100, 250, 500, Inf),
                 breaknames = c("0-10","10-50", "50-100",
                                "100-250","250-500",  ">500"))
# vähäinen protsku mikä näkyy voi johtua siitä, että estimoitu sato laskee jotain oudosti tms

create_index_map(r_index = crop_and_mask(r_prot_allcrops_sum_mt_perpix, mada), # näkyy suht järkevästi
                 tocrs = "ESRI:54030",
                 index_main_title = "",
                 index_label = "[mt perpix]",
                 colorpal = pal_protein_lajolla,
                 breakvals = c(0, 10, 50, 100, 250, 500, Inf),
                 breaknames = c("0-10","10-50", "50-100",
                                "100-250","250-500",  ">500"))
# optimoitu proteiini laskettu väärin - sitä ei pitäisi olla ollenkaan jos ei ole cl maata tietyssä SI luokassa


kok <- r_optimised_protein_crops_kg_ha_mada
kok[kok > 500000] <- 500000
kok
plot(kok[[95]])


r_optimised_protein_crops_kg_ha_mada_Infto0


create_index_map(r_index = r_optimised_protein_crops_kg_ha_mada_Infto0[[95]], 
                 tocrs = "ESRI:54030", 
                 index_main_title = "",
                 index_label = "[kg]", # Inf arvot jäävät näkyviin -- pitäisikö niiden olla 0?
                 colorpal = pal_protein_lajolla,
                 breakvals = c(0, 10, 50, 100, 250, 500, Inf),
                 breaknames = c("0-10","10-50", "50-100",
                                "100-250","250-500",  ">500"))
