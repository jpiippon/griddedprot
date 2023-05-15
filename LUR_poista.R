# kokeile tehdä LUR


## alue, joka tarvitaan tuottamaan 1kg ASF --  esimerkiksi 1kg naudanlihaa? 
  # onko mahdollista laskea tämä vain 1kg proteiinia? Eli silloin ASF ei tarvittaisi
  # kokeillaan per proteiinikilo ensin

# jos esim 5kg proteiinia per ha, tarvitaan 1/5 ha tuottamaan 1kg protskua
# jos 0.1 kg protskua per ha, tarvitaan 1/0.1 = 10 ha tuottamaan 1kg protskua

# poistetaan ensin nollat
test_agb_prot <- r_protein_from_065xAGB_kg_ha$current_herd_str
test_agb_prot[test_agb_prot < 0.1] <- 0.1
agb_area_needed_to_produce_1kg_animalprot <- 
  1 / test_agb_prot




LO_nodigest <- agb_area_needed_to_produce_1kg_animalprot # sulavuutta ei vielä huomioitu
summary(LO_nodigest)
plot(LO_nodigest)

# ----------------------------------------------------------------- kasvit HDP_i 
  # oletetaan että tämä on max proteiinimäärä, joka voidaan tuottaa tämänhetkisellä viljelyteknologialla

cropprot_from_same_area <- r_prot_allcrops_sum_kg_ha # tämän pitäisi olla SI perustuva proteiinituotanto
HDP_i_nodigest <- cropprot_from_same_area 
# tai oikeastaan ei ole from same area mutta kertoo kuinka paljon kasviprotskua voitaisiin tuottaa
# LO_nodigest solussa


# ---
test_numerator_howmuchCropprot_from_area_that_is_needed_to_prod_1kg_animalprot_from_AGB <- 
  LO_nodigest * HDP_i_nodigest
plot(test_numerator_howmuchCropprot_from_area_that_is_needed_to_prod_1kg_animalprot_from_AGB)



# ------------------------ HDP_dairy_prot
  # kokeillaan laskea paljonko kasveista tulevaa proteiinia on kilossa nautaprotskua (tässä sulamattomat määrät)

# fbs_animals_wide_filled %>% filter(Item == "Bovine Meat",
#                                    Area == "World") %>% 
#   pull(protein_fraction) # 0.14
HDP_animal_prot <- 0.14 # pitäisi olla sulava proteiini kussakin solussa eli kussakin solussa maakohtaiset arvot


LUR <- (LO_nodigest * HDP_i_nodigest) / HDP_animal_prot
summary(LUR) # min = 1.04 ja med 189




# ----------------------------------------- test LUR with old 100 SI rasters
 # !!!! tämän tilalla pitäisi olla r_optimised_protein_crops_mt_perpix!
r_optimised_protein_yield_whendairy_combined_with_crops_kg_perpix <-
  here("Data", "Intermediate_input",
       "r_optimised_protein_yield_whendairy_combined_with_crops_kg_perpix.tif") %>% 
  rast()

# tätä tarvitaan fractioksi
r_SIbased_restricted_max_cropland_fraction_NAto0 <- 
  here("Data", "Intermediate_input", 
       "Fraction_of_cropland_max_when_SI_ge_threshold_NAto0.tif") %>% 
  rast()

r_SIbased_restricted_max_cropland_fraction_NAto0_fi <-
  crop_and_mask(r_SIbased_restricted_max_cropland_fraction_NAto0,
                Finland_geom)

r_optimised_prot_kg_perpix_fi <-
  crop_and_mask(r_optimised_protein_yield_whendairy_combined_with_crops_kg_perpix,
                Finland_geom)

# Convert tp kg per ha
r_optimised_prot_kg_ha_fi<-
  r_optimised_prot_kg_perpix_fi /
  (cellSize(r_optimised_prot_kg_perpix_fi[[10]], unit = "ha") * 
     r_SIbased_restricted_max_cropland_fraction_NAto0_fi)


r_optimised_prot_kg_ha_fi[r_optimised_prot_kg_ha_fi < 0] <- 0
r_optimised_prot_kg_ha_fi[r_optimised_prot_kg_ha_fi > 1000] <- 0


plot(r_SIbased_restricted_max_cropland_fraction_NAto0_fi[[3]])
plot(r_SIbased_restricted_max_cropland_fraction_NAto0_fi[[30]])
plot(r_optimised_prot_kg_ha_fi[[3]]) # lähes koko suomi -- miksi valtavat arvot alun perin?
plot(r_optimised_prot_kg_ha_fi[[30]]) # vain pieni osa suomea
summary(r_optimised_prot_kg_ha_fi[[3]])
summary(r_optimised_prot_kg_ha_fi[[30]]) # tässä kovempi median koska vain parhaat alueet mukana
# pitää siis tehdä scripti jossa LUR menee nollaksi jos ei ole mahdollista olla crops tuotantoa
# LUR ei välttämättä ole juurikaan parempi indikaattori kuin nyt olemassaoleva, joka näyttää missä voi olla
# cl ja missä ei jos tiukenneteaan SI vaatimuksia

ka_proteiinisato <- global(r_optimised_prot_kg_ha_fi, fun = "mean", na.rm = T)
# kun pelkät kasvit mukana (nyt kasvit + elukat, keskimääräinen proteiinisato/ha pienenee
# kun SI kasvaa ja cl ala laskee? Jos siis lasketaan sato perpix / (cellsize * uusi_cl_frac)

# kokeillaan LUR
HDP_i_nodigest_SI_based <- r_optimised_prot_kg_ha_fi
LUR_SI_based <- (crop_and_mask(LO_nodigest, Finland_geom) * 
                   HDP_i_nodigest_SI_based) / HDP_animal_prot

LUR_SI_based_NAto0 <- classify(LUR_SI_based, cbind(NA,0))
# lasketaan keskiarvo -- kertoo kuinka paljon keskimääräinen LUR on kussakin luokassa
# nollat mukana , mutta oikeastaan pitäisi laittaa nollaksi vain ne solut, joissa on jotain tuotantoa
LUR_SI_based_NAto0 <- mask(LUR_SI_based_NAto0, 
                           r_optimised_prot_kg_ha_fi[[1]])

plot(LUR_SI_based_NAto0[[3]])
plot(LUR_SI_based_NAto0[[30]])

ka_LUR <- global(LUR_SI_based_NAto0, fun = "mean", na.rm = T) # pienenee

ggplot(ka_LUR, aes(x = 1:100,y = mean)) +
  geom_line() +
  ggtitle("LUR keskiarvo Suomelle kun ainoastaan pelto, jolla SI > x kelpaa viljelyyn") +
  theme_minimal()
# tee tällaiset käyrät joka maalle ja laita appendixiin!
# parempi näyttää kartta, josta näkyy, että LUR edelleen >> 1 alueilla, joilla on hyvin
# viljeltävää maata. Mutta LUR laskee nollaan kun sopiva maa-ala loppuu
# !!! entä jos cl = 0.1ha, mutta gl = 30ha solussa? Mitä LUR sitten sanoo?