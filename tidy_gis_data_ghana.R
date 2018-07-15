####################################
##### Tidy up GIS data - Ghana #####
####################################

# Load data
outlets <- read.csv("./Ghana/final_ghana_data_raw.csv")

# I was going to clean up the latitude and longitude coordinates but turns out that QGIS can load them in as they are - it simply ignores 
# any characters and takes the numbers as they are. You need to specify though that you are loading the points in 'DMS' format. Latitude is Y, longitude is X.

# # Add in missing variables (i.e. no reported values so blank but left out of data source)
outlets$gis_adv_food_item_23 <- 0

# Create advert variable
outlets$advert <- 0
outlets$advert[outlets$gis_itemname == "Advertising only" | outlets$gis_itemname == "Food outlet with advertising"] <- 1

# Recode variables to numeric variables
outlets[,c(15:41,52:77)]<-sapply(outlets[,c(15:41,52:77)],function(x)ifelse(x=="Uncooked" | x=="Cooked" | x=="Both" | x=="Yes",1,0))

# Create outlet type
outlets$outlet_w_advert <- 0
outlets$outlet_w_advert[outlets$gis_itemname == "Food outlet with advertising"] <- 1
outlets$outlet_no_advert <- 0
outlets$outlet_no_advert[outlets$gis_itemname == "Food outlet with no advertising"] <- 1
outlets$advert_only <- 0
outlets$advert_only[outlets$gis_itemname == "Advertising only"] <- 1

# Create individual outlet variables
outlets$supermarket <- 0
outlets$supermarket[outlets$gis_outlet == "Supermarket"] <- 1
outlets$shop <- 0
outlets$shop[outlets$gis_outlet == "Shop"] <- 1
outlets$kiosk <- 0
outlets$kiosk[outlets$gis_outlet == "Kiosk"] <- 1
outlets$stand_table_top <- 0
outlets$stand_table_top[outlets$gis_outlet == "Vegetable/fruit/food stand/table top"] <- 1
outlets$local_vendor <- 0
outlets$local_vendor[outlets$gis_outlet == "Local vendor"] <- 1
outlets$restaurant <- 0
outlets$restaurant[outlets$gis_outlet == "Restaurant"] <- 1
outlets$bar_pub <- 0
outlets$bar_pub[outlets$gis_outlet == "Drinking Bar/pub"] <- 1
outlets$other_outlet <- 0
outlets$other_outlet[outlets$gis_outlet == "Other (Specify)" | outlets$gis_outlet == "Cold stores (Ghana)" | outlets$gis_outlet == "Bakery"| 
                       outlets$gis_outlet == "Chop Bars (Ghana)" | outlets$gis_outlet == "Market"] <- 1

outlets$billboard_adv <- 0
outlets$billboard_adv[outlets$gis_advertisement_code_1 == "Billboard"| outlets$gis_advertisement_code_2 == "Billboard" | outlets$gis_advertisement_code_3 == "Billboard"] <- 1
outlets$poster_adv <- 0
outlets$poster_adv[outlets$gis_advertisement_code_1 == "Poster" | outlets$gis_advertisement_code_2 == "Poster" | outlets$gis_advertisement_code_3 == "Poster"] <- 1
outlets$onsite_adv <- 0
outlets$onsite_adv[outlets$gis_advertisement_code_1 == "On Site/Front of Outlet" | outlets$gis_advertisement_code_2 == "On Site/Front of Outlet" | outlets$gis_advertisement_code_3 == "On Site/Front of Outlet"] <- 1
outlets$painting_adv <- 0
outlets$painting_adv[outlets$gis_advertisement_code_1 == "Large scale painting on the outlet" | outlets$gis_advertisement_code_2 == "Large scale painting on the outlet" | outlets$gis_advertisement_code_3 == "Large scale painting on the outlet"] <- 1

# Rename variables #

# What outlets sell
names(outlets)[names(outlets)=="gis_food_item_1"] <- "grain_cereal"
names(outlets)[names(outlets)=="gis_food_item_2"] <- "fresh_meat_poultry"
names(outlets)[names(outlets)=="gis_food_item_3"] <- "fresh_fish_shellfish"
names(outlets)[names(outlets)=="gis_food_item_4"] <- "procssd_fried_meat_poultry"
names(outlets)[names(outlets)=="gis_food_item_5"] <- "procssd_fried_fish"
names(outlets)[names(outlets)=="gis_food_item_6"] <- "trad_mixed_dishes"
names(outlets)[names(outlets)=="gis_food_item_7"] <- "modern_mixed_dishes"
names(outlets)[names(outlets)=="gis_food_item_8"] <- "fried_roots_tubers_plntn_pots"
names(outlets)[names(outlets)=="gis_food_item_9"] <- "nonfried_roots_tubers_plntn_pots"
names(outlets)[names(outlets)=="gis_food_item_10"] <- "fruits"
names(outlets)[names(outlets)=="gis_food_item_11"] <- "vegetables"
names(outlets)[names(outlets)=="gis_food_item_12"] <- "cakes_sweets"
names(outlets)[names(outlets)=="gis_food_item_13"] <- "savoury_snacks_pies"
names(outlets)[names(outlets)=="gis_food_item_15"] <- "milk"
names(outlets)[names(outlets)=="gis_food_item_16"] <- "fresh_juices"
names(outlets)[names(outlets)=="gis_food_item_17"] <- "alcohol"
names(outlets)[names(outlets)=="gis_food_item_18"] <- "sugar_sweet_spreads"
names(outlets)[names(outlets)=="gis_food_item_19"] <- "sodas_sweetened_bevs"
names(outlets)[names(outlets)=="gis_food_item_20"] <- "tea_coffee"
names(outlets)[names(outlets)=="gis_food_item_21"] <- "fats_oils"
names(outlets)[names(outlets)=="gis_food_item_22"] <- "nuts_seeds"
names(outlets)[names(outlets)=="gis_food_item_23"] <- "legumes_pulses"
names(outlets)[names(outlets)=="gis_food_item_24"] <- "condiments"
names(outlets)[names(outlets)=="gis_food_item_25"] <- "eggs"
names(outlets)[names(outlets)=="gis_food_item_26"] <- "other"
names(outlets)[names(outlets)=="gis_food_item_26_spy"] <- "other_specified_text"

names(outlets)[names(outlets)=="gis_food_item_27"] <- "stews"
names(outlets)[names(outlets)=="gis_food_item_28"] <- "soups"
outlets$soups_stews <- 0 # Create single variable like in Kenyan data
outlets$soups_stews[outlets$stews == 1 | outlets$soups == 1] <- 1
outlets$stews <- NULL 
outlets$soups <- NULL

outlets <- outlets[c(1:21, 111, 22:110)] # reorder
  
# Foods advertised
names(outlets)[names(outlets)=="gis_adv_food_item_1"] <- "adv_grain_cereal"
names(outlets)[names(outlets)=="gis_adv_food_item_2"] <- "adv_fresh_meat_poultry"
names(outlets)[names(outlets)=="gis_adv_food_item_3"] <- "adv_fresh_fish_shellfish"
names(outlets)[names(outlets)=="gis_adv_food_item_4"] <- "adv_procssd_fried_meat_poultry"
names(outlets)[names(outlets)=="gis_adv_food_item_5"] <- "adv_procssd_fried_fish"
names(outlets)[names(outlets)=="gis_adv_food_item_6"] <- "adv_trad_mixed_dishes"
names(outlets)[names(outlets)=="gis_adv_food_item_7"] <- "adv_modern_mixed_dishes"
names(outlets)[names(outlets)=="gis_adv_food_item_8"] <- "adv_fried_roots_tubers_plntn_pots"
names(outlets)[names(outlets)=="gis_adv_food_item_9"] <- "adv_nonfried_roots_tubers_plntn_pots"
names(outlets)[names(outlets)=="gis_adv_food_item_10"] <- "adv_fruits"
names(outlets)[names(outlets)=="gis_adv_food_item_11"] <- "adv_vegetables"
names(outlets)[names(outlets)=="gis_adv_food_item_12"] <- "adv_cakes_sweets"
names(outlets)[names(outlets)=="gis_adv_food_item_14"] <- "adv_savoury_snacks_pies"
names(outlets)[names(outlets)=="gis_adv_food_item_16"] <- "adv_milk"
names(outlets)[names(outlets)=="gis_adv_food_item_17"] <- "adv_fresh_juices"
names(outlets)[names(outlets)=="gis_adv_food_item_18"] <- "adv_alcohol"
names(outlets)[names(outlets)=="gis_adv_food_item_19"] <- "adv_sugar_sweet_spreads"
names(outlets)[names(outlets)=="gis_adv_food_item_20"] <- "adv_sodas_sweetened_bevs"
names(outlets)[names(outlets)=="gis_adv_food_item_21"] <- "adv_tea_coffee"
names(outlets)[names(outlets)=="gis_adv_food_item_22"] <- "adv_fats_oils"
names(outlets)[names(outlets)=="gis_adv_food_item_23"] <- "adv_nuts_seeds"
names(outlets)[names(outlets)=="gis_adv_food_item_24"] <- "adv_legumes_pulses"
names(outlets)[names(outlets)=="gis_adv_food_item_25"] <- "adv_soups_stews"
names(outlets)[names(outlets)=="gis_adv_food_item_27"] <- "adv_condiments"
names(outlets)[names(outlets)=="gis_adv_food_item_26"] <- "adv_eggs"
names(outlets)[names(outlets)=="gis_adv_food_item_other"] <- "adv_other"
names(outlets)[names(outlets)=="gis_adv_food_item_26_spy"] <- "adv_other_specified_text"

#outlets <- outlets[c(1:59, 75, 60:74, 77, 76, 78:102)] # reorder

# Other
names(outlets)[names(outlets)=="gis_location"] <- "location"
names(outlets)[names(outlets)=="gis_outlet"] <- "outlet_type"
names(outlets)[names(outlets)=="gis_outlet_spy"] <- "outlet_type_other_text"

## Save ##
write.csv(outlets, "./Ghana/final_ghana_data_cleaned.csv")
