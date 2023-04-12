# testaa kuinka yhdistetään regressiotulokset sekä lasketaan crop protein sato alueille, joissa ei nyt ole cl
r_suitability_filled <- 
  rasterize(updated_regression_results, r_fraction_gl_declines_masked_to_gl, field="estimate_suitability_filled")
hist(r_suitability_filled)
plot(r_suitability_filled)

# kok <- updated_regression_results %>% filter(ADMIN == "Greece")
# kok1 <- updated_regression_results %>% filter(ADMIN == "North Macedonia")


r_intercept_filled <- 
  rasterize(updated_regression_results, r_fraction_gl_declines_masked_to_gl, field="estimate_intercept_filled")
plot(r_intercept_filled)


# Näiden perusteella proteiinisato olisi
#r_prot_based_on_SI <- r_intercept_filled + r_suitability_filled * SI_5arcmin

# # tuunataan
# r_prot_based_on_SI[r_prot_based_on_SI<0]<- NA
# test1 <- r_prot_allcrops_sum_mt_perpix
# test1[test1>920]<-920
# 
# plot(r_prot_based_on_SI, main = "estimoitu prot kasvit")
# plot(test1, main = "alk per prot sato kasvit")


# ----------------------------------------------------------------------------
r_optimized_protein_yield_crops_mt_perpix <-
  ifel(test = r_fraction_cl_NAto0 == 0  &  r_fraction_gl_declines_masked_to_gl > 0, # if there is possibility to expand cl on gl areas
       yes = (r_intercept_filled + r_suitability_filled * SI_5arcmin), # approximate yield to these areas
       no = ((r_fraction_cl_NAto0 + r_fraction_gl_declines_masked_to_gl) / r_fraction_cl_NAto0) * r_prot_allcrops_sum_mt_perpix)
# if cl > 0, then simply calculate the potential production on these areas