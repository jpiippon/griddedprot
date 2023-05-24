# Create different git files

   # r_cropland_expansion_percent
plts_r_cropland_expansion_percent_list <-
  lapply(1:nlyr(r_cropland_expansion_percent), function(i) {
    create_index_map(r_index = r_cropland_expansion_percent[[i]],
                     tocrs = "ESRI:54030",
                     index_main_title = paste("Layer", i),
                     index_label = "[Cropland expansion]",
                     colorpal = pal_share_nuuk,
                     breakvals = c(-Inf, -50, -10,0, 10, 50, 80, Inf),
                     breaknames = c("-100 to -50",  "-50 to -10", 
                                    "-10 to 0", "0 to 10", "10 to 50","50 to 80", "more than 80"))
  })

tmap_animation(plts_r_cropland_expansion_percent_list,
                   width = 1200, height = 1000, delay = 20,dpi = 300,
                   filename = here("Figures", "plts_r_cropland_expansion_percent.gif"))
    
   # r_grazingland_reduction_percent
plts_r_grazingland_reduction_percent_list <-
  lapply(1:nlyr(r_grazingland_reduction_percent), function(i) {
    create_index_map(r_index = r_grazingland_reduction_percent[[i]],
                     tocrs = "ESRI:54030",
                     index_main_title = paste("Layer", i),
                     index_label = "[Grazingland reduction]",
                     colorpal = pal_share_nuuk,
                     breakvals = c(-Inf, -50, -10,0, 10, 50, 80, Inf),
                     breaknames = c("-100 to -50",  "-50 to -10", 
                                    "-10 to 0", "0 to 10", "10 to 50","50 to 80", "more than 80"))
    
  })

tmap_animation(plts_r_grazingland_reduction_percent_list,
               width = 1200, height = 1000, delay = 20,dpi = 300,
               filename = here("Figures", "plts_r_grazingland_reduction_percent.gif"))

