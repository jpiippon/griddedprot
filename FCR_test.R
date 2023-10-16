# FCR
fcr_country <- 
  read_excel(here("Data", "Input", "fromKajsa", "FCRs_bycountry.xlsx")) %>% 
  filter(Region != is.na(Region))

# We tested that if we convert e.g. 1000 kg of AGB to protein, the protein output per country is the same if we use individual FCRs or total FCRs
# protein_from_AGB <- fcr_country %>% 
#   mutate(ProtKG_withtotal = 1000 / fcr_country$FCR_totalFeedDivTotalProtoutput)
# 
# # Now we use relative shares and FCRs designated for different animal species
# protein_from_AGB <- protein_from_AGB %>% 
#   mutate(ProtKG_varyingFCR = 
#            (1000 * share_dairycows)/  FCR_dairy_cattle +
#            (1000 * (share_dairyheifers + share_meat_bovines))/  FCR_beef_cattle +
#            (1000 * share_dairy_sg)/  FCR_small_rum_dairy +
#            (1000 * (share_dairy_recru_sg+share_meat_sg))/  FCR_small_rum_meat) 


# Therefore, it is ok to use one single FCR for a country and later we convert AGB raster to protein using that FCR

# Regional medians
fcr_regional_medians <- 
  fcr_country %>% 
  group_by(Region) %>% 
  summarise(FCR_reg_median = median(FCR_totalFeedDivTotalProtoutput, na.rm = T))


fcr_regional_medians 






# Clean data and convert NA and 0 to regional median values
fcr_country_clean <- fcr_country %>% 
  dplyr::select(Country, Region, FCR_totalFeedDivTotalProtoutput)


# Liitä alueelliset mediaaniarvot alkuperäiseen tietokehykseen
fcr_country_clean <- fcr_country_clean %>% 
  left_join(fcr_regional_medians, by = "Region")


# Replace NA and zero values with the regional median value
fcr_country_clean <- fcr_country_clean %>% 
  mutate(
    FCR_kgDMperkgProtein = case_when(
      is.na(FCR_totalFeedDivTotalProtoutput) ~ FCR_reg_median,  # Replace NA with regional median
      FCR_totalFeedDivTotalProtoutput == 0 ~ FCR_reg_median,   # Replace zeros with regional median
      .default = FCR_totalFeedDivTotalProtoutput)  # Otherwise, keep the original value
  )

# fcr_country_clean <- fcr_country_clean %>% 
#   mutate(FCR_kgDMperkgProtein = if_else(is.na(FCR_totalFeedDivTotalProtoutput),
#                                         FCR_reg_median,
#                                         FCR_totalFeedDivTotalProtoutput))


fcr_country_clean$FCR_kgDMperkgProtein %>% summary()

# Summary FCRs by regions 
fcr_country_clean %>% 
  group_by(Region) %>% 
  summarise(
    Min = min(FCR_kgDMperkgProtein, na.rm = TRUE),
    Max = max(FCR_kgDMperkgProtein, na.rm = TRUE),
    Mean = mean(FCR_kgDMperkgProtein, na.rm = TRUE),
    Median = median(FCR_kgDMperkgProtein, na.rm = TRUE),
    SD = sd(FCR_kgDMperkgProtein, na.rm = TRUE)
  )


# Find  outliers --- no need
# Funktio kvantiilien laskemiseen alueittain
# calculate_regional_quantiles <- function(data, column_name, lower = 0.01, upper = 0.99) {
#   result <- data %>% 
#     group_by(Region) %>% 
#     summarise(
#       q_lower = quantile(!!sym(column_name), probs = lower, na.rm = TRUE),
#       q_upper = quantile(!!sym(column_name), probs = upper, na.rm = TRUE)
#     ) 
#   
#   return(result)
# }
# 
# # Lasketaan kvantiilit vain viimeiselle muuttujalle ja alueittain
# quantile_FCR_by_region <- 
#   calculate_regional_quantiles(fcr_country_clean, "FCR_kgDMperkgProtein")
# 
# quantile_FCR_by_region
# 
# 
# # Liitä alueelliset kvantiilit alkuperäiseen tietokehykseen
# fcr_country_clean <- fcr_country_clean %>%
#   left_join(quantile_FCR_by_region, by = "Region") 
# 
# 
# 
# 
# 
# 
# # Replace values outside the 0.05 and 0.95 quantiles with the regional median value
# fcr_country_clean <- fcr_country_clean %>% 
#   mutate(
#     FCR_kgDMperkgProtein = case_when(
#       # If the value is greater than the upper quantile in that region, replace it with the regional median
#       FCR_kgDMperkgProtein > q_upper ~ FCR_reg_median,
#       # If the value is smaller than the lower quantile in that region, replace it with the regional median
#       FCR_kgDMperkgProtein < q_lower ~ FCR_reg_median,
#       .default = FCR_kgDMperkgProtein)  # Otherwise, keep the original value
#   )
# 
# fcr_country_clean # reg medians are same as previously but individual countries now sensible values



