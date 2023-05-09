# kokeile tehdä LUR


## alue, joka tarvitaan tuottamaan 1kg ASF --  esimerkiksi 1kg naudanlihaa? 
  # onko mahdollista laskea tämä vain 1kg proteiinia? Eli silloin ASF ei tarvittaisi
  # kokeillaan per proteiinikilo ensin

# jos esim 5kg proteiinia per ha, tarvitaan 1/5 ha tuottamaan 1kg protskua
# jos 0.1 kg protskua per ha, tarvitaan 1/0.1 = 10 ha tuottamaan 1kg protskua

# poistetaan ensin nollat
test_agb_prot_dairy <- r_protein_from_065xAGB_kg_ha$dairy
test_agb_prot_dairy[test_agb_prot_dairy < 0.1] <- 0.1
agb_area_needed_to_produce_1kg_dairyprot <- 
  1 / test_agb_prot_dairy




LO_nodigest <- agb_area_needed_to_produce_1kg_dairyprot # sulavuutta ei vielä huomioitu

summary(LO_nodigest)
plot(LO_nodigest)

# ------------------------- kasvit HDP_i 
  # oletetaan että tämä on max proteiinimäärä, joka voidaan tuottaa tämänhetkisellä viljelyteknologialla

cropprot_from_same_area <- r_prot_allcrops_sum_kg_ha # tämän pitäisi olla SI perustuva proteiinituotanto
HDP_i_nodigest <- cropprot_from_same_area


# ---
test_numerator_howmuchprot_from_area_that_is_needed_to_prod_1kg_dairyprot_from_AGB <- 
  LO_nodigest * HDP_i_nodigest
plot(test_numerator_howmuchprot_from_area_that_is_needed_to_prod_1kg_dairyprot_from_AGB)



# ------------------------ HDP_dairy_prot
  # kokeillaan laskea paljonko kasveista tulevaa proteiinia on kilossa nautaprotskua (tässä sulamattomat määrät)

# fbs_animals_wide_filled %>% filter(Item == "Bovine Meat",
#                                    Area == "World") %>% 
#   pull(protein_fraction) # 0.14
HDP_dairy_prot <- 0.14 # pitäisi olla sulava proteiini kussakin solussa eli kussakin solussa maakohtaiset arvot


LUR <- (LO_nodigest * HDP_i_nodigest) / HDP_dairy_prot
summary(LUR) # min = 1.04 ja med 189
