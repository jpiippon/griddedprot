# ------------------------------------------------------------------ AGB prot
r_protein_from_065xAGB_kg_ha <- 
  here("Data", "Intermediate_input","r_protein_from_065xAGB_kg_ha.tif") %>% 
  rast()



SI_grasses_5arcmin <- 
  here("Data", "Input", "from_harddrive",
       "SI_plants19_20_21_5arcmin.tif") %>% 
  rast()



# test with current herd structures
r_protein_and_SI <- c(r_protein_from_065xAGB_kg_ha$current_herd_str, # !!!!!!! note the name -- I prefer to use this one as it is also elsewhere in my cripts
                      SI_grasses_5arcmin)
names(r_protein_and_SI) <- c("current_herd_str", "SI_plant19", "SI_plant20", "SI_plant21")


remove_outliers_quantiles <- function(df, lower, upper) {
  lower_quantile <- quantile(df$current_herd_str, lower, na.rm = TRUE)
  upper_quantile <- quantile(df$current_herd_str, upper, na.rm = TRUE)
  return(c(lower_quantile, upper_quantile))
}


all_countries <- adm10_simple_faoadded

# create list of countries
country_sf_list <- all_countries %>%
  mutate(country_sf_list = map(ADMIN, ~filter(adm10_simple_faoadded, ADMIN == .x))) %>%
  pull(country_sf_list)
#country_sf_list[[53]] %>% plot() # FI

# find a relevant raster for each country by cropping and masking
country_raster_list <- map(country_sf_list, ~crop_and_mask(r_protein_and_SI, .x))
#country_raster_list[[53]] %>% plot() # FI

# Convert to data frame
country_df_list <- map(country_raster_list, ~as.data.frame(.x, xy = T) %>% as_tibble())
country_df_list[[53]] # suomen koordinaatit, current_herd_str_protein_kg_ha  ja SI for 3 grasses

f_outlier_level_list <- function(lower_bound, upper_bound) {
  outlier_level_list <-  map(country_df_list, ~ remove_outliers_quantiles(.x, lower_bound, upper_bound)) 
  return(outlier_level_list)
}


fit_lm_possibly <- function(data) {
  
  
  model_output <-
    lm(current_herd_str ~ SI_plant19  + SI_plant20  + SI_plant21, data = data)
  
  
  model_coef <- model_output %>% 
    broom::tidy() %>%
    dplyr::select(term, estimate,  p.value) %>% 
    pivot_wider(., names_from = term, values_from = c(estimate, p.value))
  
  
  model_stats <- model_output %>% 
    broom::glance()
  
  # Combine model_coef and model_stats into a single row data frame
  result <- model_coef %>%
    mutate(r.squared = model_stats$r.squared, # these from model stats
           adj.r.squared = model_stats$adj.r.squared,
           AIC = model_stats$AIC,
           BIC = model_stats$BIC,
           nobs = model_stats$nobs,
           sigma = model_stats$sigma,
           statistic = model_stats$statistic,
           p.value_Ftest = model_stats$p.value) %>% 
    rename(estimate_intercept = "estimate_(Intercept)")
  
  return(result)
}



# Safe version of the linear regression function
fit_lm_safe <- purrr::possibly(fit_lm_possibly, otherwise = NULL)




# function, maybe bit complicated but workds
perform_analysis <- function(bounds) {
  lower_bound <- bounds[1]
  upper_bound <- bounds[2]
  
  filtered_df <- all_countries %>%
    mutate(
      country_df_list = country_df_list,
      model_list = (
        map(
          map2(
            country_df_list,
            f_outlier_level_list(lower_bound, upper_bound),
            ~ .x %>%
              filter(current_herd_str >= .y[1] & current_herd_str <= .y[2]) %>%
              dplyr::select(-c(x, y))),
          fit_lm_safe)
      )
    ) %>%
    dplyr::select(ADMIN, ISO_A3_EH, REGION_UN, geom, model_list) %>%
    mutate(model_list = map(model_list, ~ as_tibble(.x))) %>%
    unnest(cols = model_list, keep_empty = T)
  
  med_adj_r_squared <- median(filtered_df$adj.r.squared, na.rm = T)
  med_AIC <- median(filtered_df$AIC, na.rm = T)
  med_BIC <- median(filtered_df$BIC, na.rm = T)
  med_pval_Ftest <-  median(filtered_df$p.value_Ftest, na.rm = T)
  sig_countries <- filtered_df %>% filter(p.value_Ftest < 0.05) %>% nrow()
  
  return(list(filtered_df, med_adj_r_squared, med_AIC,med_BIC, sig_countries, med_pval_Ftest))
}


bounds_list <- list(c(0.0, 1.0), c(0.00, 0.99), c(0.00, 0.98), c(0.00, 0.95), c(0.00, 0.90),
                    c(0.01, 1.0), c(0.01, 0.99), c(0.01, 0.98), c(0.01, 0.95), c(0.01, 0.90),
                    c(0.02, 1.0), c(0.02, 0.99), c(0.02, 0.98), c(0.02, 0.95), c(0.02, 0.90),
                    c(0.05, 1.0), c(0.05, 0.99), c(0.05, 0.98), c(0.05, 0.95), c(0.05, 0.90),
                    c(0.1, 1.0), c(0.1, 0.99), c(0.1, 0.98), c(0.1, 0.95), c(0.1, 0.90))

results_list <- map(bounds_list, perform_analysis)

# Tämän jälkeen voit käydä läpi `results_list` ja tallentaa tiedot tibbleen:

results_tibble <- tibble(
  bounds = sapply(bounds_list, paste, collapse = "-"),
  median_adj_r_squared = map_dbl(results_list, ~ .x[[2]]),
  median_AIC = map_dbl(results_list, ~ .x[[3]]),
  median_BIC = map_dbl(results_list, ~ .x[[4]]),
  Number_significant_countries = map_dbl(results_list, ~ .x[[5]]),
  median_pval_Ftest = map_dbl(results_list, ~ .x[[6]])
)

results_tibble






# ------------------------------------------------------------------ crops
# ------------------------------------------------------------------ crops
# ------------------------------------------------------------------ crops
SI_plant_spesific_5arcmin <- 
  here("Data", "Input", "Zabel22_SI",
       "plantspecific_suitabilititiesl_for_crops_1to17_1980_2009hist_5arcmin.tif") %>% 
  rast() # maybe kok[kok<1]<- NA

# this is not a good idea (plan was to run regression only for SI > 0 values but it does remove too much data)
#SI_plant_spesific_5arcmin <- classify(SI_plant_spesific_5arcmin, cbind(0, NA))

# Crop protein production yields
r_prot_allcrops_sum_kg_ha <-
  here("Data", "Intermediate_input", "protein_production_27crops_sum_kg_ha.tif") %>% 
  rast()


quantile(values(r_prot_allcrops_sum_kg_ha), probs = 0.95, na.rm = T) # 826.4333  

r_protein_and_SI_crops <- c(r_prot_allcrops_sum_kg_ha, SI_plant_spesific_5arcmin)




# päivitetty outlierien poistofunktio
remove_outliers_quantiles <- function(df, lower, upper) {
  lower_quantile <- quantile(df$crop_protein_kg_ha, lower, na.rm = TRUE)
  upper_quantile <- quantile(df$crop_protein_kg_ha, upper, na.rm = TRUE)
  return(c(lower_quantile, upper_quantile))
}


all_countries <- adm10_simple_faoadded

# create list of countries
country_sf_list <- all_countries %>%
  mutate(country_sf_list = map(ADMIN, ~filter(adm10_simple_faoadded, ADMIN == .x))) %>%
  pull(country_sf_list)
#country_sf_list[[53]] %>% plot() # FI

# find a relevant raster for each country by cropping and masking
country_raster_list <- map(country_sf_list, ~crop_and_mask(r_protein_and_SI_crops, .x))
#country_raster_list[[53]] %>% plot() # FI

# Convert to data frame
country_df_list <- map(country_raster_list, ~as.data.frame(.x, xy = T) %>% as_tibble())


fit_lm_possibly <- function(data) {
  
  model_output <- lm(crop_protein_kg_ha ~ 
                       plnt1 + plnt2 + plnt3 + plnt4 + plnt5 + 
                       plnt6 + plnt7 + plnt8 + plnt9 + plnt10 +
                       plnt11 + plnt12 + plnt13 + plnt14 + plnt15 +
                       plnt16 + plnt17,
                     data = data) 
  
  # model_coef <- model_output %>% 
  #   broom::tidy() %>%
  #   dplyr::select(term, estimate, p.value)
  
  model_coef <- model_output %>% 
    broom::tidy() %>%
    dplyr::select(term, estimate,  p.value) %>% 
    pivot_wider(., names_from = term, values_from = c(estimate, p.value))
  
  
  model_stats <- model_output %>% 
    broom::glance()
  
  # Combine model_coef and model_stats into a single row data frame
  result <- model_coef %>%
    mutate(r.squared = model_stats$r.squared, # these from model stats
           adj.r.squared = model_stats$adj.r.squared,
           AIC = model_stats$AIC,
           BIC = model_stats$BIC,
           nobs = model_stats$nobs,
           sigma = model_stats$sigma,
           statistic = model_stats$statistic,
           p.value_Ftest = model_stats$p.value) %>% 
    rename(estimate_intercept = "estimate_(Intercept)")
  
  
  
  return(result)
}




# Safe version of the linear regression function
fit_lm_safe <- purrr::possibly(fit_lm_possibly, otherwise = NULL)





# päivitetty analyysifunktio
perform_analysis <- function(bounds) {
  lower_bound <- bounds[1]
  upper_bound <- bounds[2]
  
  filtered_df <- all_countries %>%
    mutate(
      country_df_list = country_df_list,
      model_list = (
        map(
          map2(
            country_df_list,
            f_outlier_level_list(lower_bound, upper_bound),
            ~ .x %>%
              filter(crop_protein_kg_ha >= .y[1] & crop_protein_kg_ha <= .y[2]) %>%
              dplyr::select(-c(x, y))),
          fit_lm_safe)
      )
    ) %>%
    dplyr::select(ADMIN, ISO_A3_EH, REGION_UN, geom, model_list) %>%
    mutate(model_list = map(model_list, ~ as_tibble(.x))) %>%
    unnest(cols = model_list, keep_empty = T)
  
  med_adj_r_squared <- median(filtered_df$adj.r.squared, na.rm = T)
  med_AIC <- median(filtered_df$AIC, na.rm = T)
  med_BIC <- median(filtered_df$BIC, na.rm = T)
  med_pval_Ftest <-  median(filtered_df$p.value_Ftest, na.rm = T)
  sig_countries <- filtered_df %>% filter(p.value_Ftest < 0.05) %>% nrow()
  
  return(list(filtered_df, med_adj_r_squared, med_AIC,med_BIC, sig_countries, med_pval_Ftest))
}

# Määritellään bounds_list ja suoritetaan analyysi kaikille boundsille
bounds_list <- list(c(0.0, 1.0), c(0.00, 0.99), c(0.00, 0.98), c(0.00, 0.95), c(0.00, 0.90),
                    c(0.01, 1.0), c(0.01, 0.99), c(0.01, 0.98), c(0.01, 0.95), c(0.01, 0.90),
                    c(0.02, 1.0), c(0.02, 0.99), c(0.02, 0.98), c(0.02, 0.95), c(0.02, 0.90),
                    c(0.05, 1.0), c(0.05, 0.99), c(0.05, 0.98), c(0.05, 0.95), c(0.05, 0.90),
                    c(0.1, 1.0), c(0.1, 0.99), c(0.1, 0.98), c(0.1, 0.95), c(0.1, 0.90))
results_list <- map(bounds_list, perform_analysis)

# Tulosten tallennus tibbleen
results_tibble <- tibble(
  bounds = sapply(bounds_list, paste, collapse = "-"),
  median_adj_r_squared = map_dbl(results_list, ~ .x[[2]]),
  median_AIC = map_dbl(results_list, ~ .x[[3]]),
  median_BIC = map_dbl(results_list, ~ .x[[4]]),
  Number_significant_countries = map_dbl(results_list, ~ .x[[5]]),
  median_pval_Ftest = map_dbl(results_list, ~ .x[[6]])
)

results_tibble



bounds_list <- list(c(0.0, 1.0), c(0.00, 0.99), c(0.00, 0.95), c(0.00, 0.90),
                    c(0.01, 1.0), c(0.01, 0.99),  c(0.01, 0.95), c(0.01, 0.90),
                    c(0.05, 1.0), c(0.05, 0.99),  c(0.05, 0.95), c(0.05, 0.90),
                    c(0.1, 1.0), c(0.1, 0.99),  c(0.1, 0.95), c(0.1, 0.90))
  