# Comparison SI
SI_rainfed <- 
  here("Data", "Input", "Zabel22_SI", 
       "overall_suitability_hist1980_2009_all_rainfed.bil") %>% ##### move subset 1-17 here
  rast()

SI_rainfed_agg <- 
  aggregate(SI_rainfed, fact = 10, na.rm = T) %>% 
  resample(., template_rast_5arcmin)

SI_current_irri <- 
  here("Data", "Input", "Zabel22_SI", 
       "overall_suitability_subset_1to17_hist1980_2009_current_irr_areas_applied.bil") %>% 
  rast()

SI_current_irri_agg <- 
  aggregate(SI_current_irri, fact = 10, na.rm = T) %>% 
  resample(., template_rast_5arcmin)


# Plot
r_both_si <- c(SI_rainfed_agg, SI_current_irri_agg)

(plt_SI_v3.0_twomaps <- create_index_map(r_index = r_both_si, tocrs = "ESRI:54030",
                                 index_main_title = "SI for food crops over 1980-2009: subset 1to17 with current irri and no subset with rainfed",
                                 index_label = "",
                                 colorpal = pal_agb,
                                 breakvals =  c(1, 33, 75, Inf),
                                 breaknames = c("Marginally suitable (1 < SI < 33 )", 
                                                "Moderately suitable (33 <= SI <= 75 )", "Highly suitable (SI > 75)")) )


(plt_SI_v3.0_twomaps2 <- create_index_map(r_index = r_both_si, tocrs = "ESRI:54030",
                                         index_main_title = "SI for food crops over 1980-2009\n subset 1to17 with current irri and\n no subset with rainfed",
                                         index_label = "",
                                         colorpal = pal_agb,
                                         breakvals =  c(1, 10, 20, 30, 40, 50, 60, 75, Inf),
                                         breaknames =  ))

tmap_save(plt_SI_v3.0_twomaps2,
          here("Delete", "SI_rainfedVSirrigated_5arcmin.pdf"))

# Plot 30 as
r_both_si_30as <- c(SI_rainfed, SI_current_irri)


plt_SI_v3.0_rainfed_30as <- create_index_map(r_index = SI_rainfed, tocrs = "ESRI:54030",
                                          index_main_title = "SI for food crops over 1980-2009\n subset 1to17 with current irri and\n no subset with rainfed",
                                          index_label = "",
                                          colorpal = pal_agb,
                                          breakvals =  c(1, 10, 20, 30, 40, 50, 60, 75, Inf),
                                          breaknames =  )
tmap_save(SI_rainfed,
          here("Delete", "SI_rainfed_30as.pdf"))
