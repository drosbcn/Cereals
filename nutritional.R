setwd("~/Desktop/Cereals_Project/")
agg_data <- read.csv("cereals.csv")

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape)
library(lars)

# Character vector of all health variables
health.vars <- c("totalfatg", "saturatedfatg", "sodiummg", "totalcarbohydratesg", "dietaryfiberg", "sugarsg", "proteing", "vitaminamg", "ironmg", 
                  "vitaminciu", "calciummg") 

# Subsetting some health variables 
healthy.data <- subset(agg_data, select = c(totalfatg, saturatedfatg, sodiummg, totalcarbohydratesg, dietaryfiberg, sugarsg, proteing, vitaminamg, ironmg, vitaminciu, calciummg))

# Making cereals db
cereals <- mutate(agg_data, quant_mon = quant_mon*qty, price_mon = price_mon/qty,
                  tot_sales = price_mon*quant_mon) %>%
  filter(month > 0)

# Final sub.cereals db which we wil use for LASSO reg avec health data and total sales
sub.cereals <- select(cereals, month, brandname, manufacturername, tot_sales, totalfatg, saturatedfatg, sodiummg, totalcarbohydratesg, dietaryfiberg, sugarsg, proteing, vitaminamg, ironmg, vitaminciu, calciummg) %>%
  filter(!is.na(tot_sales)) %>%
  group_by(manufacturername, month)

# Extra names just in case
manu.names <- unique(sub.cereals$manufacturername)
manu.names
brand.names <- unique(sub.cereals$brandname)
brand.names

# Start LASSO
X <- as.matrix(subset(sub.cereals, select = health.vars))
t <- sub.cereals$tot_sales

lasso <- lars(X, t, type = "lasso", trace = FALSE, normalize = TRUE, intercept = TRUE )
plot(lasso)

# Get coefficients
lasso_cv <- cv.lars(X,t, type = "lasso")
bestfraction <- lasso_cv$index[which.min(lasso_cv$cv)]
coef.lasso <- predict(lasso,X,s=bestfraction,type="coefficient",mode="fraction")

# Check optimal coefficients equal coefficients found in last step of LASSO
coef.lasso$coefficients == tail(coef(lasso),1)
