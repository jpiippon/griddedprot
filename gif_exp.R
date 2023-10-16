# Create different git files

# kok <- r_cropland_expansion_percent
# kok <- kok[[1:3]]

r_cropland_expansion_percent_NAto0 <-
  classify(r_cropland_expansion_percent_positive, cbind(NA,0))

r_cropland_expansion_percent_NAto0_agrlandmask <-
  mask(r_cropland_expansion_percent_NAto0,
       r_fraction_gl_cl_total_0toNA)

# test
# Chunk 5.1
r_cropland_expansion_percent_positive <- 
  here("Data", "Intermediate_input", "r_cropland_expansion_percent_positive.tif") %>%
  rast()

r_cropland_expansion_percent_positive <- r_cropland_expansion_percent_positive[[10:14]]

   # r_cropland_expansion_percent_positive ---- change classes (see figures RMD)
plts_r_cropland_expansion_percent_list <-
  lapply(1:nlyr(r_cropland_expansion_percent_positive), function(i) {
    create_index_map(r_index = r_cropland_expansion_percent_positive[[i]],
                     tocrs = "ESRI:54030",
                     index_main_title = paste("Cropland expansion when all grazing land with SI ≥", i, 
                                              "\ncould be used for crop production"),
                     index_label = "[Cropland expansion in %]",
                     colorpal = pal_share_nuuk,
                     breakvals = c(-Inf, -50, -10,0, 10, 50, 100, Inf),
                     breaknames = c("-100 to -50",  "-50 to -10", 
                                    "-10 to 0", "0 to 10", "10 to 50","50 to 80", "more than 100"))
  })






plts_r_cropland_expansion_percent_list <-
  lapply(1:nlyr(r_cropland_expansion_percent_NAto0_agrlandmask), function(i) {
    create_index_map(r_index = r_cropland_expansion_percent_NAto0_agrlandmask[[i]],
                     tocrs = "ESRI:54030",
                     index_main_title = paste("How much cropland area would increase, \nwhen all agricultural land with SI ≥", i, 
                                              "\ncould be used for crop production"),
                     index_label = "[Cropland expansion]",
                     colorpal = pal_share_nuuk,
                     breakvals = c(-Inf, -50, -10,0, 10, 50, 100, Inf),
                     breaknames = c("-100 to -50",  "-50 to -10", 
                                    "-10 to 0", "0 to 10", "10 to 50","50 to 80", "more than 100"))
  })

tmap_animation(plts_r_cropland_expansion_percent_list,
                   width = 1200, height = 1000, delay = 20,dpi = 300,
                   filename = here("Figures", "plts_r_cropland_expansion_percent.gif"))
    




# ------------------------------------------------------------------- LUR
LUR_SI_based_NAto0_rob <- project(LUR_SI_based_NAto0, "ESRI:54030") # 10 min


plts_lur_list <- lapply(1:nlyr(LUR_SI_based_NAto0_rob), function(i) {
  create_index_map(r_index = LUR_SI_based_NAto0_rob[[i]],
                   index_main_title = paste("Land use ratio (LUR)",
                                            "\nwhen all agricultural land with SI ≥", i,
                                            "\ncould be used for crop production"),
                   index_label = "[LUR]",
                   colorpal = pal_lur,
                   breakvals = c(0, 1, 10, 100, Inf),
                   breaknames = c("0-1", "1-10", "10-100", ">100")) # 1h
})



tmap_animation(plts_lur_list, width = 1200, height = 1000, delay = 20,dpi = 300,
               filename = here("Figures", "LUR_animation.gif"))



# Lataa magick-paketti
library(magick)

# Lataa molemmat gif-animaatiot kuvina
animation1 <- image_read(here("Figures", "plts_r_cropland_expansion_percent.gif"))
animation2 <- image_read(here("Figures", "LUR_animation.gif"))


# Yhdistä GIF-tiedostot sivuttain
combined_gif <- image_append(c(animation1, animation2))



# Tallenna yhdistetty GIF
image_write(combined_gif, here("Figures", "combined_animation.gif"))

combined_gif2 <- image_append(c(animation1, animation2), stack = TRUE)
image_write(combined_gif2, here("Figures", "combined_animation.gif"))
#    # r_grazingland_reduction_percent
# plts_r_grazingland_reduction_percent_list <-
#   lapply(1:nlyr(r_grazingland_reduction_percent), function(i) {
#     create_index_map(r_index = r_grazingland_reduction_percent[[i]],
#                      tocrs = "ESRI:54030",
#                      index_main_title = paste("How much grazing land area would decrese, \nwhen all agricultural land with SI ≥", i, 
#                                               "\ncould be used for crop production"),
#                      index_label = "[Grazingland reduction]",
#                      colorpal = pal_share_nuuk,
#                      breakvals = c(-Inf, -50, -10,0, 10, 50, 80, Inf),
#                      breaknames = c("-100 to -50",  "-50 to -10", 
#                                     "-10 to 0", "0 to 10", "10 to 50","50 to 80", "more than 80"))
#     
#   })
# 
# tmap_animation(plts_r_grazingland_reduction_percent_list,
#                width = 1200, height = 1000, delay = 20,dpi = 300,
#                filename = here("Figures", "plts_r_grazingland_reduction_percent.gif"))