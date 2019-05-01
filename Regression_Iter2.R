# Join sales and sales price data
# Create a separate dataframe for each item
# Stack columns for features: -customer-price, -customer-promotion_days, etc.
# FIND OUT IF THERE RETAIL CHAINS LOOK AT WHAT OTHER CHAINS ARE PROMOTING TO TAKE DECISIONS!!!!!!

library (dplyr)
library (lubridate)
library(tidyr)
library(zoo)
library(GGally)
library(forecast)
library(ggplot2)

period <- "month"


#defining a function that creates a dataframe with all periods necessary for tseries, using first and last observations
create_period_df <- function(dataframe,period="month") {
  first_observation <- dataframe$init_period_date %>% min() %>% floor_date(period) %>% as_date()
  last_observation <-  dataframe$init_period_date %>% max() %>% floor_date(period) %>% as_date()
  
  c1 <- first_observation
  period_df <- data.frame(Period = c1)
  
  n_periods <- time_length (interval (first_observation, last_observation), unit= period)
  for (n in 2: (n_periods+1)) {
    period_df [n,"Period"] <- (first_observation %m+% months(n-1))
  }
  period_df <- period_df %>% 
    mutate(Period = as_date(Period))
  return (period_df)
} 

#FIRST APPROACH IS TO GET A SUM OF NUM PROMO&PROMO PRICE DAYS IN A MONTH & MEAN OF UNIT PRICE
#Trial 1 is done with item R240092 ---------------------------------------------------------------------------------
#This product is only served in certain seasons, so the model should work horribly

#filtering sales dataframe for given item
filtered_sales <- sales_df_item %>%
  filter(`Item No_` == top_items$`Item No_`[1])

#filtering sales price dataframe for given item, and grouping by date to get sums of promo days, and a mean of price
#very gross features, but they seem to work better than expected
filtered_sales_price <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[1]) %>%
  group_by(Period) %>% 
  summarise(Num_Days_Price_Promo = sum(Num_Days_Price_Promo), 
            Num_Days_Promo = sum(Num_Days_Promo),
            Quarter_Low_Price = sum(Quarter_Low_Price),
            Unit_Price =  mean(Unit_Price)) %>% 
  ungroup()

#creating a period_df for given item
period_df <- create_period_df(filtered_sales)

item_ts <- period_df %>% 
  left_join(filtered_sales, by = c("Period" = "init_period_date")) %>% 
  left_join(filtered_sales_price, by = c("Period")) %>%
  left_join(full_calendar_df, by = c("Period")) %>% 
  select(-`Item No_`)

#filling nas with zeros
item_ts [,2:ncol(item_ts)] <- item_ts [,2:ncol(item_ts)] %>% na.fill(0)

#plotting a scatterplot matrix
item_ts %>% 
  select(-Period) %>%
  ggpairs()


item_ts <- item_ts %>% 
  ts()

fit.trial1 <- tslm(Sum_Quantity ~  
                     Num_Days_Promo +
                     Num_Days_Price_Promo + 
                     Unit_Price,
                   data=item_ts)

#plotting fitted trial_1 and actual_data
autoplot(item_ts[,'Sum_Quantity'], series="Data") +
  autolayer(fitted(fit.trial1), series="Fitted") +
  xlab("Period") + ylab("") +
  guides(colour=guide_legend(title=" "))


#plotting fitted trial_1 vs actual_data
cbind(Data = item_ts[,"Sum_Quantity"],
      Fitted = fitted(fit.trial1)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("R240092") +
  geom_abline(intercept=0, slope=1)

checkresiduals(fit.trial1)

#the model is PRETTY BAD!!!


#Trial 2 is done with item R240271 ---------------------------------------------------------------------------------

#filtering sales dataframe for given item
filtered_sales <- sales_df_item %>%
  filter(`Item No_` == top_items$`Item No_`[2])

#filtering sales price dataframe for given item, and grouping by date to get sums of promo days, and a mean of price
filtered_sales_price <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>%
  group_by(Period) %>% 
  summarise(Num_Days_Price_Promo = sum(Num_Days_Price_Promo), 
            Num_Days_Promo = sum(Num_Days_Promo),
            Quarter_Low_Price = sum(Quarter_Low_Price),
            Unit_Price =  mean(Unit_Price)) %>% 
  ungroup()

#creating a period_df for given item
period_df <- create_period_df(filtered_sales)

item_ts <- period_df %>% 
  left_join(filtered_sales, by = c("Period" = "init_period_date")) %>% 
  left_join(filtered_sales_price, by = c("Period")) %>%
  left_join(full_calendar_df, by = c("Period")) %>% 
  select(-`Item No_`)

#filling nas with zeros
item_ts [,2:ncol(item_ts)] <- item_ts [,2:ncol(item_ts)] %>% na.fill(0)

#plotting a scatterplot matrix
item_ts %>% 
  select(-Period) %>%
  ggpairs()

item_ts <- item_ts %>% 
  ts()

fit.trial1 <- tslm(Sum_Quantity ~  
                     Num_Days_Promo +
                     Num_Days_Price_Promo + 
                     Unit_Price +
                     Quarter_Low_Price,
                   data=item_ts)

#plotting fitted trial_1 and actual_data
autoplot(item_ts[,'Sum_Quantity'], series="Data") +
  autolayer(fitted(fit.trial1), series="Fitted") +
  xlab("Period") + ylab("") +
  guides(colour=guide_legend(title=" "))


#plotting fitted trial_1 vs actual_data
cbind(Data = item_ts[,"Sum_Quantity"],
      Fitted = fitted(fit.trial1)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("R240271") +
  geom_abline(intercept=0, slope=1)

checkresiduals(fit.trial1)

#LOOKING A BIT BETTER HERE. ERRORS FOLLOW A TREND THAT IS CERTAINLY NOT CAPTURED BY THE MODEL
#LOOKS LIKE THERE IS A BIT OF AUTOCORR TOWARDS THE END
#HISTOGRAM OF ERRORS IS A BIT SKEWED...


#Trial 3 is done with item R242231 ---------------------------------------------------------------------------------

#filtering sales dataframe for given item
filtered_sales <- sales_df_item %>%
  filter(`Item No_` == top_items$`Item No_`[3])

#filtering sales price dataframe for given item, and grouping by date to get sums of promo days, and a mean of price
#very gross features, but they seem to work better than expected
filtered_sales_price <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[3]) %>%
  group_by(Period) %>% 
  summarise(Num_Days_Price_Promo = sum(Num_Days_Price_Promo), 
            Num_Days_Promo = sum(Num_Days_Promo),
            Quarter_Low_Price = sum(Quarter_Low_Price),
            Unit_Price =  mean(Unit_Price)) %>% 
  ungroup()

#creating a period_df for given item
period_df <- create_period_df(filtered_sales)

item_ts <- period_df %>% 
  left_join(filtered_sales, by = c("Period" = "init_period_date")) %>% 
  left_join(filtered_sales_price, by = c("Period")) %>%
  left_join(full_calendar_df, by = c("Period")) %>% 
  select(-`Item No_`)

#filling nas with zeros
item_ts [,2:ncol(item_ts)] <- item_ts [,2:ncol(item_ts)] %>% na.fill(0)

#plotting a scatterplot matrix
item_ts %>% 
  select(-Period) %>%
  ggpairs()

item_ts <- item_ts %>% 
  ts()

fit.trial1 <- tslm(Sum_Quantity ~  
                     Num_Days_Promo +
                     Num_Days_Price_Promo + 
                     Unit_Price,
                   data=item_ts)

#plotting fitted trial_1 and actual_data
autoplot(item_ts[,'Sum_Quantity'], series="Data") +
  autolayer(fitted(fit.trial1), series="Fitted") +
  xlab("Period") + ylab("") +
  guides(colour=guide_legend(title=" "))


#plotting fitted trial_1 vs actual_data
cbind(Data = item_ts[,"Sum_Quantity"],
      Fitted = fitted(fit.trial1)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("R240271") +
  geom_abline(intercept=0, slope=1)

checkresiduals(fit.trial1)

#LOOKING A BIT BETTER HERE. ERRORS FOLLOW A TREND THAT IS CERTAINLY NOT CAPTURED BY THE MODEL
#LOOKS LIKE THERE IS A BIT OF AUTOCORR TOWARDS THE END
#HISTOGRAM OF ERRORS IS A BIT SKEWED...


#Trial 4 is done with item R240371 ---------------------------------------------------------------------------------

#filtering sales dataframe for given item
filtered_sales <- sales_df_item %>%
  filter(`Item No_` == top_items$`Item No_`[4])

#filtering sales price dataframe for given item, and grouping by date to get sums of promo days, and a mean of price
#very gross features, but they seem to work better than expected
filtered_sales_price <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[3]) %>%
  group_by(Period) %>% 
  summarise(Num_Days_Price_Promo = sum(Num_Days_Price_Promo), 
            Num_Days_Promo = sum(Num_Days_Promo),
            Quarter_Low_Price = sum(Quarter_Low_Price),
            Unit_Price =  mean(Unit_Price)) %>% 
  ungroup()

#creating a period_df for given item
period_df <- create_period_df(filtered_sales)

item_ts <- period_df %>% 
  left_join(filtered_sales, by = c("Period" = "init_period_date")) %>% 
  left_join(filtered_sales_price, by = c("Period")) %>%
  left_join(full_calendar_df, by = c("Period")) %>% 
  select(-`Item No_`)

#filling nas with zeros
item_ts [,2:ncol(item_ts)] <- item_ts [,2:ncol(item_ts)] %>% na.fill(0)

#plotting a scatterplot matrix
item_ts %>% 
  select(-Period) %>%
  ggpairs()

item_ts <- item_ts %>% 
  ts()

fit.trial1 <- tslm(Sum_Quantity ~  
                     Num_Days_Promo +
                     Num_Days_Price_Promo + 
                     Unit_Price,
                   data=item_ts)

#plotting fitted trial_1 and actual_data
autoplot(item_ts[,'Sum_Quantity'], series="Data") +
  autolayer(fitted(fit.trial1), series="Fitted") +
  xlab("Period") + ylab("") +
  guides(colour=guide_legend(title=" "))


#plotting fitted trial_1 vs actual_data
cbind(Data = item_ts[,"Sum_Quantity"],
      Fitted = fitted(fit.trial1)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("R240271") +
  geom_abline(intercept=0, slope=1)

checkresiduals(fit.trial1)

#LOOKING A BIT BETTER HERE. ERRORS FOLLOW A TREND THAT IS CERTAINLY NOT CAPTURED BY THE MODEL
#LOOKS LIKE THERE IS A BIT OF AUTOCORR TOWARDS THE END
#HISTOGRAM OF ERRORS IS A BIT SKEWED...

