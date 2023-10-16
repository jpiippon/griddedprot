# maskeja

# croplandit cl alueiden ulkopuolella
r_cl_outside_gl <- 
  mask(r_fraction_cl_0toNA, r_fraction_gl_0toNA, inverse = T)

r_gl_outside_cl <- 
  mask(r_fraction_gl_0toNA, r_fraction_cl_0toNA, inverse = T)





# Luo uusi väripaletti, jossa on harmaa ensimmäisenä värinä
pal_lur_inc_grey <- c("#A0A0A0", pal_lur)

# Tee kartta
plt_LUR_current_everywhere2 <-
  create_index_map(r_index = LUR_i_current, 
                   tocrs = "ESRI:54030",
                   index_main_title = "Current LUR",
                   index_label = "[LUR]",
                   colorpal = pal_lur_inc_grey,
                   breakvals = c(-999, 0, 1, 10, 100,  Inf),
                   breaknames = c("Not known", "0-1","1-10","10-100",">100"))
plt_LUR_current_everywhere2



