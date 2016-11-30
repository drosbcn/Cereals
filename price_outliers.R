setwd("~/Desktop/Cereals_Project/")
agg_data <- read.csv("cereals.csv")

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape)
library(lars)
library(tsoutliers)

# Subset relevant data
anti.data <- select(cereals, month, brandname, quant_mon, price_mon) %>%
  filter(!is.na(quant_mon), !is.na(price_mon)) %>%
  group_by(month, brandname) %>%
  summarise(quant=sum(quant_mon), price=mean(price_mon)) 

# Subset some relevant variables
manu.names <- unique(anti.data$manufacturername)
manu.names
brand.names <- unique(anti.data$brandname)
brand.names

## Testing anti competitve model with cornflakes, by looking at time series outliers
 test.flakes <- subset(anti.data, brandname = "DOM Corn Flakes")
 head(test.flakes)
 flakes.ts <- ts(test.flakes$price, frequency = 1)
 flakes.outliers <- tso(flakes.ts)
 plot(flakes.outliers)
 
 # We see that DOM Corn FLakes has no time series outliers, thus no major shift in price
 # was detected. We need to design an algorithm to find if ANY brands have outliers. 
 
 possible.brands <- vector(mode = "character")
 j <- 0
 for (i in 1:length(brand.names)) {
   
   outlier.data <- subset(anti.data, brandname = brand.names[i])
   ts.data <- ts(outlier.data$price, frequency = 1)
   outliers <- tso(ts.data)
   
   if(nrow(outliers$outliers) !=  0){
     j <- j + 1
     possible.brands[j] <-  brand.names[i]
     }
 }
   
 possible.brands 
 
 # Seems like there is no brandname time series outliers for price
