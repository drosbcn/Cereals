
# Set working directory and import data
setwd("~/Documents/Econ/PS 2/")
agg_data <- read.csv("cereals.csv")

# Import ggplot and dplyr
library(ggplot2)
library(dplyr)
library(tidyr)

# Convert agg_data to a tbl format to use dplyr on it.
agg_data <- tbl_df(agg_data)

# Create a new variable for sales value by multiplying quantity and price
# Create a measure of price per serving
# Create a measure of quantity sold in serving size
cereals <- mutate(agg_data, tot_sales = price_mon*quant_mon) %>%
  filter(month > 0)

# Plot market shares per manufacturername, for each month
# First create a table with appropriate data
select(cereals, month, manufacturername, tot_sales) %>%
  filter(!is.na(tot_sales)) %>%
  group_by(manufacturername, month) %>%
  summarise(sales = sum(tot_sales)) %>%
  # Plot the data
  ggplot(aes(x=month, y=sales, fill=manufacturername)) +
  geom_area(stat="identity",position="fill", alpha=0.5) +
  theme_classic() +
  labs(title="Market shares by manufacturer", y="Market share")

# Plot market shares per firm_id, for each month
# First create a table with appropriate data
select(cereals, month, firm_id, tot_sales) %>%
  mutate(firm_id = factor(firm_id)) %>%
  filter(!is.na(tot_sales)) %>%
  group_by(firm_id, month) %>%
  summarise(sales = sum(tot_sales)) %>%
  # Plot the data in shares using geom_area
  ggplot(aes(x=month, y=sales, fill=firm_id)) +
  geom_area(stat="identity",position="fill", alpha=0.5) +
  theme_classic() +
  labs(title="Market shares by firm ID", y="Market share")

# Create a variable giving total sales only for holidays
holidays <- select(cereals, month, tot_sales, christmas_mon, halloween_mon, 
                   thanksgiving_mon, newyear_mon, easter_mon, july4th_mon) %>%
  group_by(month) %>%
  summarise(sales=sum(tot_sales),hols=max(christmas_mon,halloween_mon,thanksgiving_mon,
                     newyear_mon,easter_mon,july4th_mon)*sales) %>%
  filter(hols!=0)


# Create a plot of total sales per month, along with holiday dates.
# First create a table with necessary data.
select(cereals, month, tot_sales) %>%
  filter(!is.na(tot_sales)) %>%
  group_by(month) %>%
  summarise(sales = sum(tot_sales)) %>%
  # Plot the data
  ggplot(aes(x=month, y=sales)) +
  geom_line() +
  geom_point(inherit.aes = FALSE, data=holidays, aes(x=month, y=hols, col=I("red"), 
                                                     size=1, alpha=0.5)) +
  theme(legend.position = "none") +
  labs(title="Total sales of cereal, with holidays as red points")


# Create a plot of quantity sold vs price
# First create a table with necessary data
select(cereals, month, descrip, quant_mon, price_mon) %>%
  filter(!is.na(quant_mon), !is.na(price_mon)) %>%
  group_by(month, descrip) %>%
  summarise(quant=sum(quant_mon), price=mean(price_mon)) %>%
  # Plot the data
  ggplot(aes(x=quant, y=price)) +
  geom_point(shape=1, size=1) +
  geom_smooth(method="lm", se=FALSE, col="red") +
  labs(title = "Price of cereals vs units sold, by product and month", x = "Units sold", 
       y = "Price")

# Create a table to run the first step of 2-step least squares regression
price_IV <- select(cereals, store, month, brandname, price_mon, distance_gasoline, brands_factory, wheat_g_price, corn_g_price,
                   rice_g_price, oat_g_price, barley_g_price, sugar_g_price, 
                   retailprofperquant_mon, foldingpaperboard_ppi, electricity_chi, 
                   advertising_chi, earnings_tradetransport, earnings_foodmanuf) %>%
  mutate(ingred_g_price = wheat_g_price + corn_g_price +
         rice_g_price + oat_g_price + barley_g_price + sugar_g_price) %>%
  select(store, month, brandname, price_mon, distance_gasoline, brands_factory, ingred_g_price, 
         retailprofperquant_mon, foldingpaperboard_ppi, electricity_chi, 
         advertising_chi, earnings_tradetransport, earnings_foodmanuf)

# Standardise our variables
for(i in 4:ncol(price_IV)) {
  mean_col <- mean(price_IV[[i]], na.rm = TRUE)
  sd_col <- sd(price_IV[[i]])
  price_IV[[i]] <- (price_IV[[i]] - mean_col)/sd_col
}

# Test whether our variables have mean 0 and standard deviation of 1:
# mean_col <- c()
# sd_col <- c()
# for(i in 2: ncol(price_IV)) {
#   mean_col[i] <- mean(price_IV[[i]], na.rm = TRUE)
#   sd_col[i] <- sd(price_IV[[i]])
# }

# Run the first step of our regression
beta_IV <- lm(price_mon ~ distance_gasoline + brands_factory + ingred_g_price + 
                retailprofperquant_mon + foldingpaperboard_ppi + electricity_chi + 
                advertising_chi + earnings_tradetransport + earnings_foodmanuf
              , data = price_IV)

# Store the coefficients of our regression
beta_IV <- summary(beta_IV)$coefficients[-1,1]
# Calculate a fitted value of price
price_IV$price_hat <- as.matrix(price_IV[,5:13]) %*% beta_IV

# Insert back the fitted values of price in our main "cereals" table
join_price_IV <- price_IV %>%
    select(store, month, brandname, price_hat)
cereals <- left_join(cereals, join_price_IV, by = c("store" , "month", "brandname"))

# Calculate market shares
total <- select(cereals, store, month, brandname, tot_sales) %>%
  group_by(store, brandname, month) %>%
  summarise(sales = sum(tot_sales))

total <- total %>%
  group_by(store, month) %>%
  summarise(market_size = sum(sales)) %>%
  full_join(total, by = c("store", "month")) %>%
  mutate(market_share = sales/market_size) %>%
  select(store, month, brandname, market_share)

cereals <- left_join(cereals, total, by = c("store", "month", "brandname"))



#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################




# Set up the quantity sold data to use in regression
 quant_reg <- select(cereals, month, manufacturername, quant_mon) %>%
  group_by(month, manufacturername) %>%
  summarise(quant_mon=sum(quant_mon)) %>%
  spread(manufacturername,quant_mon)
 
 # Set up the price data to use in regression
 price_reg <- select(cereals, month, manufacturername, price_mon) %>%
   group_by(month, manufacturername) %>%
   summarise(price_mon=mean(price_mon)) %>%
   spread(manufacturername,price_mon)
 
reg_ <- merge(quant_reg, price_reg, by = "month") 


 