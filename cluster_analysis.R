#################################################
#### Cluster Analysis of Stunted/Underweight ####
#################################################

# Load libraries
library(foreign)
library(poLCA)
library(mclust)
library(survey)

# Set working directory
setwd("M:/Documents/Papers/India - Underweight")

# Set seed just in case so reproducible
set.seed(268)

### Prepare data for analysis ###


# Load data
full_data <- read.dta("./SSM India RF Anthro Nov 2015/SSM India RF Anthro Nov 2015.dta")

# Keep variables
vars <- c("id", "caseid", "bidx", "lifestage", "sex", "wealth_5_new", "mum_educ", "dad_educ", "mom_ht_cat", "dad_ht_cat", "mom_bmi_c", "dad_bmi_c",
          "child_marriage", "diet_5", "ebreast5", "disease", "water", "safe_st", "sanit", "haq", "iodized",
          "fullv", "vita_supp", "who_stunt", "who_underwt", "who_waste", "who_htz", "who_wfl", "wt", "strata")

# Subset data
data <- full_data[vars]
rm(full_data)
rm(vars)

# Convert variables from factors to numeric
data$sex <- as.numeric(data$sex)
data$lifestage <- as.numeric(data$lifestage)
data$mum_educ <- as.numeric(data$mum_educ)
data$dad_educ <- as.numeric(data$dad_educ)
data$mom_ht_cat <- as.numeric(data$mom_ht_cat)
data$dad_ht_cat <- as.numeric(data$dad_ht_cat)
data$ebreast5 <- as.numeric(data$ebreast5)
data$water <- as.numeric(data$water)
data$sanit <- as.numeric(data$sanit)
data$haq <- as.numeric(data$haq)

# Recode Variables (PCA requires positive integers i.e. increment from 1)
data$mom_bmi_c[data$mom_bmi_c==2] <- 3 # Mother's BMI
data$mom_bmi_c[data$mom_bmi_c==1] <- 2
data$mom_bmi_c[data$mom_bmi_c==0] <- 1
data$dad_bmi_c[data$dad_bmi_c==2] <- 3 # Dad's BMI
data$dad_bmi_c[data$dad_bmi_c==1] <- 2
data$dad_bmi_c[data$dad_bmi_c==0] <- 1
data$child_marriage[data$child_marriage==1] <- 2 # Age at marriage
data$child_marriage[data$child_marriage==0] <- 1
data$disease[data$disease==1] <- 2 # Had infectious disease in past 2 weeks
data$disease[data$disease==0] <- 1
data$safe_st[data$safe_st==1] <- 2 # Safe disposal of stools
data$safe_st[data$safe_st==0] <- 1
data$iodized[data$iodized==1] <- 2 # Iodized salt used
data$iodized[data$iodized==0] <- 1
data$fullv[data$fullv==1] <- 2 # Fully vacinated
data$fullv[data$fullv==0] <- 1
data$vita_supp[data$vita_supp==1] <- 2 # Vitamin A supplementation
data$vita_supp[data$vita_supp==0] <- 1 

# Missing variables
data$vita_supp[data$vita_supp==8] <- "." 
data$dad_bmi_c[is.na(data$dad_bmi_c)] <- "."
data$dad_ht_cat[is.na(data$dad_ht_cat)] <- "."
data$dad_educ[is.na(data$dad_educ)] <- "."

# Subset data
stunted <- data[which(data$who_stunt==1),] # Stunted
notstunted <- data[which(data$who_stunt==0),] # Not stunted
# underweight <- data[which(data$who_underwt==1),] # Underweight

# # Sample characteristics
# stunted.w <- svydesign(ids = ~1, data = stunted, strata = stunted$strata, weights = stunted$wt)
# prop.table(svytable(~sex, design = stunted.w))
# prop.table(svytable(~wealth_5_new, design = stunted.w))
# prop.table(svytable(~lifestage, design = stunted.w))
# prop.table(svytable(~diet_5, design = stunted.w))
# prop.table(svytable(~ebreast5, design = stunted.w))
# prop.table(svytable(~disease, design = stunted.w))
# prop.table(svytable(~water, design = stunted.w))
# prop.table(svytable(~safe_st, design = stunted.w))
# prop.table(svytable(~sanit, design = stunted.w))
# prop.table(svytable(~haq, design = stunted.w))
# prop.table(svytable(~iodized, design = stunted.w))
# prop.table(svytable(~fullv, design = stunted.w))
# prop.table(svytable(~vita_supp, design = stunted.w))
# prop.table(svytable(~mum_educ, design = stunted.w))
# prop.table(svytable(~mom_ht_cat, design = stunted.w))
# prop.table(svytable(~mom_bmi_c, design = stunted.w))
# prop.table(svytable(~child_marriage, design = stunted.w))
# prop.table(svytable(~dad_bmi_c, design = stunted.w))
# prop.table(svytable(~dad_ht_cat, design = stunted.w))
# prop.table(svytable(~dad_educ, design = stunted.w))
# 
# data.w <- svydesign(ids = ~1, data = data, strata = data$strata, weights = data$wt)
# prop.table(svytable(~sex, design = data.w))
# prop.table(svytable(~wealth_5_new, design = data.w))
# prop.table(svytable(~lifestage, design = data.w))
# prop.table(svytable(~diet_5, design = data.w))
# prop.table(svytable(~ebreast5, design = data.w))
# prop.table(svytable(~disease, design = data.w))
# prop.table(svytable(~water, design = data.w))
# prop.table(svytable(~safe_st, design = data.w))
# prop.table(svytable(~sanit, design = data.w))
# prop.table(svytable(~haq, design = data.w))
# prop.table(svytable(~iodized, design = data.w))
# prop.table(svytable(~fullv, design = data.w))
# prop.table(svytable(~vita_supp, design = data.w))
# prop.table(svytable(~mum_educ, design = data.w))
# prop.table(svytable(~mom_ht_cat, design = data.w))
# prop.table(svytable(~mom_bmi_c, design = data.w))
# prop.table(svytable(~child_marriage, design = data.w))
# prop.table(svytable(~dad_bmi_c, design = data.w))
# prop.table(svytable(~dad_ht_cat, design = data.w))
# prop.table(svytable(~dad_educ, design = data.w))

# Save data in SAS format
write.csv(stunted, file="./SAS analysis/stunted_data.csv", row.names=FALSE, quote=FALSE)
write.csv(notstunted, file="./SAS analysis/notstunted_data.csv", row.names=FALSE, quote=FALSE)


# ## MClust ##
# hold <- stunted[complete.cases(stunted),] # drop missing data
# temp <- hold[,5:20] # drop all variables not included in analysis (only works with numeric data)
# fit <- Mclust(temp) # LCA
# # plot(fit) # for various plots
# 
# w <- hold$wt
# test <- me.weighted(modelName = "VVV", data = temp, z = unmap(temp), weights = w)
# 
# fitnew <- do.call("me.weighted", c(list(data=temp,weights=w), fit)) # fit model with sample weights
# print(fit$parameters$pro) # original component probabilities
# print(fitnew$parameters$pro) # weighted
# print(fit$parameters$mean) # original component means
# print(fitnew$parameters$mean) # weighted
  
### Latent Class Analysis ###

# Define variables to be used in LCA to estimate groups
f <- cbind(sex, wealth_5_new, mum_educ, mom_ht_cat, mom_bmi_c, lifestage, child_marriage, diet_5, ebreast5, 
           disease, water, safe_st, sanit, haq, iodized, fullv, vita_supp)~1

## All Individuals ##

# Identify best number of groups

numbers <- c(1:10)

for (i in numbers) {
  temp <- poLCA(f, data, nclass=i, nrep=10, maxiter=100000, verbose=FALSE, na.rm=F)
  assign(paste0("lca_all",i), temp)
  rm(temp)
}

# Compare solutions
AIC <- c(rbind(lca_all1$aic, lca_all2$aic, lca_all3$aic, lca_all4$aic, lca_all5$aic,
               lca_all6$aic, lca_all7$aic, lca_all8$aic, lca_all9$aic, lca_all10$aic))
BIC <- c(rbind(lca_all1$bic, lca_all2$bic, lca_all3$bic, lca_all4$bic, lca_all5$bic,
               lca_all6$bic, lca_all7$bic, lca_all8$bic, lca_all9$bic, lca_all10$bic))
Gsq <- c(rbind(lca_all1$Gsq, lca_all2$Gsq, lca_all3$Gsq, lca_all4$Gsq, lca_all5$Gsq,
               lca_all6$Gsq, lca_all7$Gsq, lca_all8$Gsq, lca_all9$Gsq, lca_all10$Gsq))
Chisq <- c(rbind(lca_all1$Chisq, lca_all2$Chisq, lca_all3$Chisq, lca_all4$Chisq, lca_all5$Chisq,
                 lca_all6$Chisq, lca_all7$Chisq, lca_all8$Chisq, lca_all9$Chisq, lca_all10$Chisq))
Groups <- c(1:10)
numb_grps_all <- cbind(Groups,AIC,BIC,Gsq,Chisq) # Put together in one table
write.table(numb_grps_all, "number_of_groups_all_data.txt", sep = "\t", row.names = FALSE) # Save

# Plot values
plot(AIC, xlab="Number of groups", ylab="AIC")
plot(BIC, xlab="Number of groups", ylab="BIC")
plot(Gsq, xlab="Number of groups", ylab="G-Squared Statistic")
plot(Chisq, xlab="Number of groups", ylab="Chi-squared goodness of fit")

# View results of chosen solution
lca_all4
save(lca_all4, file = "all_data_solution.RData")

## Presenting results
## http://statistics.ohlsen-web.de/latent-class-analysis-polca/ ##

## Stunted Individuals ##

# Identify best number of groups

numbers <- c(1:10)

for (i in numbers) {
  temp <- poLCA(f, stunted, nclass=i, nrep=10, maxiter=100000, verbose=FALSE, na.rm=F)
  assign(paste0("lca_stunt",i), temp)
  rm(temp)
}

# Compare solutions
AIC <- c(rbind(lca_stunt1$aic, lca_stunt2$aic, lca_stunt3$aic, lca_stunt4$aic, lca_stunt5$aic,
               lca_stunt6$aic, lca_stunt7$aic, lca_stunt8$aic, lca_stunt9$aic, lca_stunt10$aic))
BIC <- c(rbind(lca_stunt1$bic, lca_stunt2$bic, lca_stunt3$bic, lca_stunt4$bic, lca_stunt5$bic,
               lca_stunt6$bic, lca_stunt7$bic, lca_stunt8$bic, lca_stunt9$bic, lca_stunt10$bic))
Gsq <- c(rbind(lca_stunt1$Gsq, lca_stunt2$Gsq, lca_stunt3$Gsq, lca_stunt4$Gsq, lca_stunt5$Gsq,
               lca_stunt6$Gsq, lca_stunt7$Gsq, lca_stunt8$Gsq, lca_stunt9$Gsq, lca_stunt10$Gsq))
Chisq <- c(rbind(lca_stunt1$Chisq, lca_stunt2$Chisq, lca_stunt3$Chisq, lca_stunt4$Chisq, lca_stunt5$Chisq,
                 lca_stunt6$Chisq, lca_stunt7$Chisq, lca_stunt8$Chisq, lca_stunt9$Chisq, lca_stunt10$Chisq))
Groups <- c(1:10)
numb_grps_stunt <- cbind(Groups,AIC,BIC,Gsq,Chisq) # Put together in one table
write.table(numb_grps_stunt, "number_of_groups_stunted.txt", sep = "\t", row.names = FALSE) # Save

# Plot values
plot(AIC, xlab="Number of groups", ylab="AIC")
plot(BIC, xlab="Number of groups", ylab="BIC")
plot(Gsq, xlab="Number of groups", ylab="G-Squared Statistic")
plot(Chisq, xlab="Number of groups", ylab="Chi-squared goodness of fit")

# View results of chosen solution
lca_stunt4
save(lca_stunt4, file = "stunted_solution.RData")


## Underweight Individuals ##

# Identify best number of groups

numbers <- c(1:10)

for (i in numbers) {
  temp <- poLCA(f, underweight, nclass=i, nrep=10, maxiter=100000, verbose=TRUE, na.rm=F)
  assign(paste0("lca_undrwt",i), temp)
  rm(temp)
}

# Compare solutions
AIC <- c(rbind(lca_undrwt1$aic, lca_undrwt2$aic, lca_undrwt3$aic, lca_undrwt4$aic, lca_undrwt5$aic,
               lca_undrwt6$aic, lca_undrwt7$aic, lca_undrwt8$aic, lca_undrwt9$aic, lca_undrwt10$aic))
BIC <- c(rbind(lca_undrwt1$bic, lca_undrwt2$bic, lca_undrwt3$bic, lca_undrwt4$bic, lca_undrwt5$bic,
               lca_undrwt6$bic, lca_undrwt7$bic, lca_undrwt8$bic, lca_undrwt9$bic, lca_undrwt10$bic))
Gsq <- c(rbind(lca_undrwt1$Gsq, lca_undrwt2$Gsq, lca_undrwt3$Gsq, lca_undrwt4$Gsq, lca_undrwt5$Gsq,
               lca_undrwt6$Gsq, lca_undrwt7$Gsq, lca_undrwt8$Gsq, lca_undrwt9$Gsq, lca_undrwt10$Gsq))
Chisq <- c(rbind(lca_undrwt1$Chisq, lca_undrwt2$Chisq, lca_undrwt3$Chisq, lca_undrwt4$Chisq, lca_undrwt5$Chisq,
                 lca_undrwt6$Chisq, lca_undrwt7$Chisq, lca_undrwt8$Chisq, lca_undrwt9$Chisq, lca_undrwt10$Chisq))
Groups <- c(1:10)
numb_grps_undrwt <- cbind(Groups,AIC,BIC,Gsq,Chisq) # Save this table and add column names
c

# Plot values
plot(AIC, xlab="Number of groups", ylab="AIC")
plot(BIC, xlab="Number of groups", ylab="BIC")
plot(Gsq, xlab="Number of groups", ylab="G-Squared Statistic")
plot(Chisq, xlab="Number of groups", ylab="Chi-squared goodness of fit")

# View results of chosen solution
lca_undrwt4
save(lca_undrwt4, file = "underweight_solution.RData")

## Compare solutions ##

load(file = "stunted_solution.RData")

temp_all <- as.data.frame(cbind(data$id, lca_all4$predclass))
temp_all$grp_all <- temp_all$V2

temp_st <- as.data.frame(cbind(stunted$id, lca_stunt4$predclass))
temp_st$grp_st <- temp_st$V2

temp_uw <- as.data.frame(cbind(underweight$id, lca_undrwt4$predclass))
temp_uw$grp_uw <- temp_uw$V2

library(plyr)
joined_uw_st <- join(temp_st, temp_uw, by = "V1", type = "full", match = "all")
joined_all_st <- join(temp_all, temp_st, by = "V1", type = "full", match = "all")

table(joined_uw_st$grp_st, joined_uw_st$grp_uw)
table(joined_all_st$grp_all, joined_all_st$grp_st)

