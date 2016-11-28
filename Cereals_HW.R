
# Set working directory and import data
setwd("~/Documents/Econ/PS 2/")
agg_data <- read.csv("cereals.csv")

# Import ggplot and dplyr
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape)

# Convert agg_data to a tbl format to use dplyr on it.
agg_data <- tbl_df(agg_data)

# Create a new variable for sales value by multiplying quantity and price
# Create a measure of price per serving
# Create a measure of quantity sold in serving size
cereals <- mutate(agg_data, quant_mon = quant_mon*qty, price_mon = price_mon/qty,
                  tot_sales = price_mon*quant_mon) %>%
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

# Plot market shares per manufacturername, for each month, this time with quantity
# First create a table with appropriate data
select(cereals, month, manufacturername, quant_mon) %>%
  filter(!is.na(quant_mon)) %>%
  group_by(manufacturername, month) %>%
  summarise(sales = sum(quant_mon)) %>%
  # Plot the data
  ggplot(aes(x=month, y=sales, fill=manufacturername)) +
  geom_area(stat="identity",position="fill", alpha=0.5) +
  theme_classic() +
  labs(title="Market shares by manufacturer", y="Market share")

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
select(cereals, month, brandname, quant_mon, price_mon) %>%
  filter(!is.na(quant_mon), !is.na(price_mon)) %>%
  group_by(month, brandname) %>%
  summarise(quant=sum(quant_mon), price=mean(price_mon)) %>%
  # Plot the data
  ggplot(aes(x=quant, y=price)) +
  geom_point(shape=1, size=1) +
  geom_smooth(method="lm", se=FALSE, col="red") +
  labs(title = "Price of cereal brands vs units sold, by product and month", x = "Units sold", 
       y = "Price")

# Create a table with dummy variable for kids cereal to us for join in next step
kids_sugars_table <- select(cereals, month, brandname, kids, sugarsg)

# Create plot facetted for adult and kids' cereal
select(cereals, month, brandname, quant_mon, price_mon) %>%
  filter(!is.na(quant_mon), !is.na(price_mon)) %>%
  group_by(month, brandname) %>%
  summarise(quant=sum(quant_mon), price=mean(price_mon)) %>%
  left_join(kids_sugars_table, by = c("month", "brandname")) %>%
  # Plot the data
  ggplot(aes(x=quant, y=price, col = sugarsg)) +
  geom_point(shape=1, size=1) +
  geom_smooth(method="lm", se=FALSE, col="red") +
  facet_grid(. ~ kids) +
  labs(title = "Price  vs units sold, for adult (left) and kids (right) cereal", x = "Units sold", y = "Price")

# Graph crop prices. Use for loop to rescale as an index starting in month 1.
price_us <- select(cereals, month, contains("_price_us")) %>%
  filter(!duplicated(month))
      for(j in 2:length(price_us)){
        price_us[,j] <- 100 * price_us[,j] / price_us[1,j]
      }
  melt(price_us, id = "month") %>%
  ggplot(aes(x = month, y = value, col = variable)) +
    geom_line(size = 1) +
    labs(title = "Crop prices per bushel", y = "Index: week 1 = 100")
  

# Create plots with ingredient inputs per cereal, divided between adult and kids  
ing_input <- select(cereals, kids, brandname, ends_with("_g")) %>%
  group_by(kids, brandname) %>%
  filter(!duplicated(brandname))  %>%
  mutate(total_inputs = sum(corn_g, wheat_g, rice_g, barley_g, oat_g)) %>%
  as.data.frame()
  melt(ing_input, id = c("kids", "total_inputs", "brandname")) %>%
  ggplot(aes(x = reorder(brandname, -total_inputs), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
    facet_grid(. ~kids) +
    labs(title = "Ingredients per 100g, adult (left) vs kids (right) cereal", x = "") +
    theme(axis.text.x = element_blank())

# Create a table to run the first step of 2-step least squares regression
price_IV <- select(cereals, month, kids, brandname, price_mon, 
                   distance_gasoline, wheat_g_price, corn_g_price,
                   rice_g_price, oat_g_price, barley_g_price, sugar_g_price, 
                   retailprofperquant_mon) %>%
  filter(kids == 0) %>%
  mutate(ingred_g_price = wheat_g_price + corn_g_price +
         rice_g_price + oat_g_price + barley_g_price + sugar_g_price) %>%
  group_by(month, brandname) %>%
  summarise(price_mon = mean(price_mon), distance_gasoline = mean(distance_gasoline),
          retail_markup = mean(retailprofperquant_mon), 
          ingred_g_price = mean(ingred_g_price))

# Standardise our variables
for(i in 3:ncol(price_IV)) {
  mean_col <- mean(price_IV[[i]], na.rm = TRUE)
  sd_col <- sd(price_IV[[i]])
  price_IV[[i]] <- (price_IV[[i]] - mean_col)/sd_col
}

# Test whether our variables have mean 0 and standard deviation of 1:
# mean_col <- c()
# sd_col <- c()
# for(i in 3: ncol(price_IV)) {
#   mean_col[i] <- mean(price_IV[[i]], na.rm = TRUE)
#   sd_col[i] <- sd(price_IV[[i]])
# }
# mean_col
# sd_col

# Run the first step of our regression
beta_IV <- lm(price_mon ~ distance_gasoline + retail_markup + ingred_g_price, data = price_IV)
beta_IV
# Store the coefficients of our regression
beta_IV <- summary(beta_IV)$coefficients[-1,1]
# Calculate a fitted value of price
price_IV$price_hat <- as.matrix(price_IV[,4:6]) %*% beta_IV 
price_IV <- select(price_IV, month, brandname, price_hat, price_mon)

# Calculate market shares, to be stored in total_2
total <- select(cereals, kids, month, brandname, quant_mon) %>%
  filter(kids == 0) %>%
  group_by(brandname, month) %>%
  summarise(quant_mon = sum(quant_mon))
m_shares <- total %>%
  group_by(month) %>%
  summarise(market_size = sum(quant_mon))
m_shares <- full_join(m_shares,total, by = "month") %>%
  mutate(market_share = quant_mon/market_size) %>%
  select(month, brandname, market_share)

# Test whether adding up market shares each month sums to 1
  # group_by(m_shares, month) %>%
  # summarise(market = sum(market_share))

# Create a table for the final regression
reg_table <- cereals %>%
  select(kids, month, brandname, shredding, flaking, puffing, baking, extrusion) %>%
  filter(kids == 0) %>%
  select(-kids) %>%
  filter(!duplicated(month, brandname)) %>%
  left_join(m_shares, by = c("month", "brandname")) %>%
  left_join(price_IV, by = c("month", "brandname"))

# Standardise our variables
for(i in 3:ncol(reg_table)) {
  mean_col <- mean(reg_table[[i]], na.rm = TRUE)
  sd_col <- sd(reg_table[[i]])
  reg_table[[i]] <- (reg_table[[i]] - mean_col)/sd_col
}

# Test whether our variables have mean 0 and standard deviation of 1:
# mean_col <- c()
# sd_col <- c()
# for(i in 3: ncol(reg_table)) {
#   mean_col[i] <- mean(reg_table[[i]], na.rm = TRUE)
#   sd_col[i] <- sd(reg_table[[i]])
# }
# mean_col
# sd_col

# Run the regression to estimate alpha, the impact of price on market share
reg1 <- lm(market_share ~ shredding + flaking + puffing + baking + extrusion + price_hat,
           data = reg_table)
alpha <- summary(reg1)$coefficients[6,1]

# Calculate own-price elasticities
own_elast <-  

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


 