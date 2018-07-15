###############################
#### Tidy up poLCA results ####
###############################

# Class conditional probabilities

results <- NULL # Create blank object to store results

# Type of outlet
vars <- c("outlet_w_advert", "outlet_no_advert", "advert_only", "supermarket", "shop", "kiosk", "stand_table_top", "local_vendor", "restaurant", "bar_pub", "other_outlet", "billboard_adv", "poster_adv", "onsite_adv", "painting_adv")
i <- 1

for (i in 1:15) {
  
  hold <- data.frame(t(lca_all5$probs[[vars[i]]])) # Select probs for single variable and transpose output (i.e. swap rows and columsn around)
  colnames(hold) <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5") # Change column names
  hold2 <- data.frame(table(nairobi[[vars[i]]])) # Overall prevalence of values
  hold$Overall <- hold2$Freq / 499
  hold$Variable <- "Outlet Type" # Give details about variables
  hold$Value <- paste0(vars[i])
  hold <- hold[,c(7:8,1:6)] # Reorder results
  results <- rbind(results, hold[2,]) # Only keep the yes row

}

# Food sold by outlets
vars <- c("grain_cereal", "fresh_meat_poultry", "fresh_fish_shellfish", "procssd_fried_meat_poultry", "procssd_fried_fish", "trad_mixed_dishes", "modern_mixed_dishes", "soups_stews", "fried_roots_tubers_plntn_pots", "nonfried_roots_tubers_plntn_pots", "fruits", "vegetables", "cakes_sweets", "savoury_snacks_pies", "sodas_sweetened_bevs", "milk", "fresh_juices", "alcohol", "sugar_sweet_spreads", "tea_coffee", "fats_oils", "nuts_seeds", "legumes_pulses", "condiments", "eggs", "other")
i <- 1

for (i in 1:26) {
  
  hold <- data.frame(t(lca_all5$probs[[vars[i]]])) # Select probs for single variable and transpose output (i.e. swap rows and columsn around)
  colnames(hold) <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5") # Change column names
  hold2 <- data.frame(table(nairobi[[vars[i]]])) # Overall prevalence of values
  hold$Overall <- hold2$Freq / 499
  hold$Variable <- "Food sold" # Give details about variables
  hold$Value <- paste0(vars[i])
  hold <- hold[,c(7:8,1:6)] # Reorder results
  results <- rbind(results, hold[2,]) # Only keep the yes row
  
}

# Foods advertised
# "adv_nonfried_roots_tubers_plntn_pots" has been dropped from the LCA for some reason
vars <- c("adv_grain_cereal", "adv_fresh_meat_poultry", "adv_fresh_fish_shellfish", "adv_procssd_fried_meat_poultry", "adv_procssd_fried_fish", "adv_trad_mixed_dishes", "adv_modern_mixed_dishes", "adv_soups_stews", "adv_fried_roots_tubers_plntn_pots", "adv_fruits", "adv_vegetables", "adv_cakes_sweets", "adv_savoury_snacks_pies", "adv_sodas_sweetened_bevs", "adv_milk", "adv_alcohol", "adv_sugar_sweet_spreads", "adv_tea_coffee", "adv_fats_oils", "adv_nuts_seeds", "adv_legumes_pulses", "adv_condiments", "adv_eggs", "adv_other")
names <- c("grain_cereal", "fresh_meat_poultry", "fresh_fish_shellfish", "procssd_fried_meat_poultry", "procssd_fried_fish", "trad_mixed_dishes", "modern_mixed_dishes", "soups_stews", "fried_roots_tubers_plntn_pots", "fruits", "vegetables", "cakes_sweets", "savoury_snacks_pies", "milk", "alcohol", "sugar_sweet_spreads", "sodas_sweetened_bevs", "tea_coffee", "fats_oils", "nuts_seeds", "legumes_pulses", "condiments", "eggs", "other")
i <- 1

for (i in 1:24) {
  
  hold <- data.frame(t(lca_all5$probs[[vars[i]]])) # Select probs for single variable and transpose output (i.e. swap rows and columsn around)
  colnames(hold) <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5") # Change column names
  hold2 <- data.frame(table(nairobi[[vars[i]]])) # Overall prevalence of values
  hold$Overall <- hold2$Freq / 499
  hold$Variable <- "Food advertised" # Give details about variables
  hold$Value <- paste0(names[i])
  hold <- hold[,c(7:8,1:6)] # Reorder results
  results <- rbind(results, hold[2,]) # Only keep the yes row
  
}

# Class prevalence
hold <- data.frame(t(lca_all5$P))
colnames(hold) <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")
hold$Overall <- 1.00
hold$Variable <- "Class Prevalence" # Give details about variables
hold$Value <- ""
hold <- hold[,c(7:8,1:6)]
results <- rbind(results, hold)

# Tidy the table up
results[,3:8] <-round(results[,3:8],2) # Round the values to tidy up the table
rownames(results) <- NULL

rm(hold, hold2)
