####################################
##### Tidy up GIS data - Kenya #####
####################################

# Load data
outlets <- read.csv("./Kenya/Food_Choices_Project_GIS_19Oct2017.csv")

# I was going to clean up the latitude and longitude coordinates but turns out that QGIS can load them in as they are - it simply ignores 
# any characters and takes the numbers as they are. You need to specify though that you are loading the points in 'DMS' format. Latitude is Y, longitude is X. 
# We need to add in the S and E coorindate parts first though since these were left off during data collection (else we end up in Poland!)
outlets$gis_longitude <- paste0("S01Â°", outlets$gis_longitude, "'", sep = "")
outlets$gis_latitude <- paste0("E036Â°", outlets$gis_latitude, "'", sep = "")

# Create advert variable
outlets$advert <- 0
outlets$advert[outlets$gis_itemname == 1 | outlets$gis_itemname == 3] <- 1

# Recode variables to numeric variables
outlets[,c(23:48,58:82)]<-sapply(outlets[,c(23:48,58:82)],function(x)ifelse(x==1 | x==2 | x==3,1,0))
outlets[,c(23:48,50:82)]<-sapply(outlets[,c(23:48,50:82)],function(x)ifelse(is.na(x),0,x))

# Create outlet type
outlets$outlet_w_advert <- 0
outlets$outlet_w_advert[outlets$gis_itemname == 1] <- 1
outlets$outlet_no_advert <- 0
outlets$outlet_no_advert[outlets$gis_itemname == 2] <- 1
outlets$advert_only <- 0
outlets$advert_only[outlets$gis_itemname == 3] <- 1

# Create individual outlet variables
outlets$supermarket <- 0
outlets$supermarket[outlets$gis_outlet == 1] <- 1
outlets$shop <- 0
outlets$shop[outlets$gis_outlet == 2] <- 1
outlets$kiosk <- 0
outlets$kiosk[outlets$gis_outlet == 3] <- 1
outlets$stand_table_top <- 0
outlets$stand_table_top[outlets$gis_outlet == 4] <- 1
outlets$local_vendor <- 0
outlets$local_vendor[outlets$gis_outlet == 5] <- 1
outlets$restaurant <- 0
outlets$restaurant[outlets$gis_outlet == 6] <- 1
outlets$bar_pub <- 0
outlets$bar_pub[outlets$gis_outlet == 9] <- 1
outlets$other_outlet <- 0
outlets$other_outlet[outlets$gis_outlet == 96] <- 1

# Rename variables #

# What outlets sell
names(outlets)[names(outlets)=="gis_food_item_1"] <- "grain_cereal"
names(outlets)[names(outlets)=="gis_food_item_2"] <- "fresh_meat_poultry"
names(outlets)[names(outlets)=="gis_food_item_3"] <- "fresh_fish_shellfish"
names(outlets)[names(outlets)=="gis_food_item_4"] <- "procssd_fried_meat_poultry"
names(outlets)[names(outlets)=="gis_food_item_5"] <- "procssd_fried_fish"
names(outlets)[names(outlets)=="gis_food_item_6"] <- "trad_mixed_dishes"
names(outlets)[names(outlets)=="gis_food_item_7"] <- "modern_mixed_dishes"
names(outlets)[names(outlets)=="gis_food_item_8"] <- "soups_stews"
names(outlets)[names(outlets)=="gis_food_item_9"] <- "legumes_pulses"
names(outlets)[names(outlets)=="gis_food_item_10"] <- "fried_roots_tubers_plntn_pots"
names(outlets)[names(outlets)=="gis_food_item_11"] <- "nonfried_roots_tubers_plntn_pots"
names(outlets)[names(outlets)=="gis_food_item_12"] <- "fruits"
names(outlets)[names(outlets)=="gis_food_item_13"] <- "vegetables"
names(outlets)[names(outlets)=="gis_food_item_14"] <- "milk"
names(outlets)[names(outlets)=="gis_food_item_15"] <- "cakes_sweets"
names(outlets)[names(outlets)=="gis_food_item_16"] <- "savoury_snacks_pies"
names(outlets)[names(outlets)=="gis_food_item_17"] <- "sodas_sweetened_bevs"
names(outlets)[names(outlets)=="gis_food_item_18"] <- "sugar_sweet_spreads"
names(outlets)[names(outlets)=="gis_food_item_19"] <- "condiments"
names(outlets)[names(outlets)=="gis_food_item_20"] <- "fresh_juices"
names(outlets)[names(outlets)=="gis_food_item_21"] <- "tea_coffee"
names(outlets)[names(outlets)=="gis_food_item_22"] <- "alcohol"
names(outlets)[names(outlets)=="gis_food_item_23"] <- "nuts_seeds"
names(outlets)[names(outlets)=="gis_food_item_24"] <- "fats_oils"
names(outlets)[names(outlets)=="gis_food_item_25"] <- "eggs"
names(outlets)[names(outlets)=="gis_food_item_26"] <- "other"
names(outlets)[names(outlets)=="gis_food_item_26_spy"] <- "other_specified_text"

# Advert type
names(outlets)[names(outlets)=="gis_advertisement_1"] <- "billboard_adv"
names(outlets)[names(outlets)=="gis_advertisement_2"] <- "poster_adv"
names(outlets)[names(outlets)=="gis_advertisement_3"] <- "onsite_adv"
names(outlets)[names(outlets)=="gis_advertisement_4"] <- "painting_adv"
names(outlets)[names(outlets)=="gis_advertisement_5"] <- "soc_markt_adv"
names(outlets)[names(outlets)=="gis_advertisement_96"] <- "other_adv"

# Foods advertised
names(outlets)[names(outlets)=="gis_adv_food_item_1"] <- "adv_grain_cereal"
names(outlets)[names(outlets)=="gis_adv_food_item_2"] <- "adv_fresh_meat_poultry"
names(outlets)[names(outlets)=="gis_adv_food_item_3"] <- "adv_fresh_fish_shellfish"
names(outlets)[names(outlets)=="gis_adv_food_item_4"] <- "adv_procssd_fried_meat_poultry"
names(outlets)[names(outlets)=="gis_adv_food_item_5"] <- "adv_procssd_fried_fish"
names(outlets)[names(outlets)=="gis_adv_food_item_6"] <- "adv_trad_mixed_dishes"
names(outlets)[names(outlets)=="gis_adv_food_item_7"] <- "adv_modern_mixed_dishes"
names(outlets)[names(outlets)=="gis_adv_food_item_8"] <- "adv_soups_stews"
names(outlets)[names(outlets)=="gis_adv_food_item_9"] <- "adv_legumes_pulses"
names(outlets)[names(outlets)=="gis_adv_food_item_10"] <- "adv_fried_roots_tubers_plntn_pots"
names(outlets)[names(outlets)=="gis_adv_food_item_11"] <- "adv_nonfried_roots_tubers_plntn_pots"
names(outlets)[names(outlets)=="gis_adv_food_item_12"] <- "adv_fruits"
names(outlets)[names(outlets)=="gis_adv_food_item_13"] <- "adv_vegetables"
names(outlets)[names(outlets)=="gis_adv_food_item_14"] <- "adv_milk"
names(outlets)[names(outlets)=="gis_adv_food_item_15"] <- "adv_cakes_sweets"
names(outlets)[names(outlets)=="gis_adv_food_item_16"] <- "adv_savoury_snacks_pies"
names(outlets)[names(outlets)=="gis_adv_food_item_17"] <- "adv_sodas_sweetened_bevs"
names(outlets)[names(outlets)=="gis_adv_food_item_18"] <- "adv_sugar_sweet_spreads"
names(outlets)[names(outlets)=="gis_adv_food_item_19"] <- "adv_condiments"
names(outlets)[names(outlets)=="gis_adv_food_item_20"] <- "adv_fresh_juices"
names(outlets)[names(outlets)=="gis_adv_food_item_21"] <- "adv_tea_coffee"
names(outlets)[names(outlets)=="gis_adv_food_item_22"] <- "adv_alcohol"
names(outlets)[names(outlets)=="gis_adv_food_item_23"] <- "adv_nuts_seeds"
names(outlets)[names(outlets)=="gis_adv_food_item_24"] <- "adv_fats_oils"
names(outlets)[names(outlets)=="gis_adv_food_item_25"] <- "adv_eggs"
names(outlets)[names(outlets)=="gis_adv_food_item_26"] <- "adv_other"
names(outlets)[names(outlets)=="gis_adv_food_item_26_spy"] <- "adv_other_specified_text"

# Other
names(outlets)[names(outlets)=="gis_location"] <- "location"
names(outlets)[names(outlets)=="gis_outlet"] <- "outlet_type"
names(outlets)[names(outlets)=="gis_outlet_spy"] <- "outlet_type_other_text"

## Save ##
write.csv(outlets, "./Kenya/final_kenya_data_cleaned.csv")
