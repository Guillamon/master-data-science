#install.packages("DataCombine")
library (dplyr)
library (lubridate)
library(tidyr)
library(zoo)
library(GGally)
library(forecast)
library(ggplot2)
library(DataCombine)

# INITIALIZING VARIABLES, DECLARING FUNCTIONS, AND READING FILES --------------------------------------------------

period <- "month"


#function that creates a dataframe with all periods necessary for tseries, using first and last observations
create_period_df <- function(dataframe,period_column= Period, period="month") {
  first_observation <- dataframe$Period %>% min() %>% floor_date(period) %>% as_date()
  last_observation <-  dataframe$Period %>% max() %>% floor_date(period) %>% as_date()
  
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

#function that takes a vector of variables and creates a TRUE/FALSE dframe with all possible combinations
create_combination_df <- function(variables) {
  n_variables <- length(variables)
  variable_vector <- c(TRUE,FALSE)
  variable_list <- list()
  
  
  for (n in (1:n_variables)) {
    variable_list[[n]] <- variable_vector
  }
  combination_df <- (expand.grid(variable_list))
  combination_df <- combination_df[1:nrow(combination_df)-1, ]
  colnames(combination_df) <- variables
  
  return(combination_df)
}

#PREPARING DATAFRAME WITH FEATURES THAT WILL BE USED FOR TRYING MODELS--------------------------------------------------

#In this iteration we will only use item R240271 
#filtering sales dataframe for given item
filtered_sales <- sales_df_item %>%
  filter (year(Period) >= 2015 ) %>% 
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

#creating a dataframe with all periods in timespan for given item
period_df <- create_period_df(filtered_sales)

#creating a dframe with sales of the class
#excludes sales of the forecast item
class_sales_df <- sales_df_item %>%
  filter (Period >= '2014-12-01') %>% #doing this for later using lagged values
  left_join(class_key_df, by =c("Item No_"="No_")) %>%
  inner_join(top_items) %>%
  drop_na() %>%
  filter(`Item No_` != top_items$`Item No_`[2]) %>% #getting rid of item sales
  group_by(Period, Class) %>% 
  summarise(Class_Sales = sum(Sum_Quantity)) %>%
  filter(Class == class_key_df[ match(top_items$`Item No_`[2],class_key_df$No_) ,"Class"]) %>% 
  select(-Class) %>% 
  ungroup()

lagged_df <- class_sales_df %>%
mutate( Period= Period %m+% months(+1)) %>% 
rename( Lag_Class_Sales = Class_Sales)

#spreading promo days by customer
promo_spread <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>%
  select(Period, Customer_Code, Num_Days_Promo) %>%
  spread(Customer_Code,Num_Days_Promo) %>% 
  rename_at(vars(-Period), funs(paste0("Days_Promo_", .))) %>%
  replace(is.na(.), 0)

#spreading price promo days by customer
price_promo_spread <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>%
  select(Period, Customer_Code, Num_Days_Price_Promo) %>%
  spread(Customer_Code,Num_Days_Price_Promo) %>%
  rename_at(vars(-Period), funs(paste0("Days_Price_Promo_", .))) %>%
  replace(is.na(.), 0)

#creating a dataframe with all features
item_df <- period_df %>% 
  left_join(filtered_sales, by = c("Period")) %>% 
  left_join(filtered_sales_price, by = c("Period")) %>%
  left_join(full_calendar_df, by = c("Period")) %>%
  left_join(class_sales_df, by = c("Period")) %>%
  left_join(promo_spread, by = c("Period")) %>%
  left_join(price_promo_spread, by = c("Period")) %>%
  left_join(lagged_df, by = c("Period")) %>% 
  replace(is.na(.), 0) %>% #fill nas with zeros
  select(-`Item No_`)

#creating a timeseries object with data
item_ts <- item_df %>% 
  #filter( Period <='2019-01-01') %>% 
  select(-Period) %>% 
  ts(start = c(2015,1), frequency = 12)

#creating a train data set that is a timeseries object
item_ts_train <- item_df %>% 
  filter( Period <'2019-01-01') %>% 
  select(-Period) %>% 
  ts(start = c(2015,1), frequency = 12)

#creating a test data set that is a dataframe (argument new data in forecast requires df)
item_df_test <- item_df %>% 
  filter( Period >= '2019-01-01')

#creating a timeseries test data set for plotting (autoplot requires it)
item_ts_test <- item_df %>% 
  filter( Period >= '2019-01-01') %>% 
  #filter( Period < '2019-04-01') %>% 
  select(-Period) %>% 
  ts(start = c(2019,1), frequency = 12)

#creating a timeseries that uses data from june onwards
item_ts2 <- item_df %>% 
  filter(Period >= "2015-6-1") %>%
  select(-Period) %>% 
  ts(start = c(2015,1), frequency = 12)


#spreading sales df by item (this will not be included as feature)
spread_sales_df <- sales_df_item %>% spread(`Item No_`,Sum_Quantity)
spread_sales_df <- spread_sales_df[ 2:nrow(spread_sales_df) -1, -which(colMeans(is.na(spread_sales_df)) > 0.5)] #getting rid of columns that have more than half nas
spread_sales_df [,2:ncol(spread_sales_df)] <- spread_sales_df [,2:ncol(spread_sales_df)] %>% na.fill(0) #filling nas with zeros


full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>% 
  select(Period, Customer_Code, Unit_Price) %>% 
  spread(Customer_Code, Unit_Price) %>% 
  View()

joined_sales %>% 
  select(Customer_Code, `Item No_`)

####





item_df3 <- period_df %>% 
  left_join(filtered_sales, by = c("Period" = "Period")) %>% 
  left_join(promo_spread, by = c("Period")) %>%
  left_join(price_promo_spread, by = c("Period")) %>%
  left_join(class_sales_df,by = c("Period" = "Period")) %>% 
  left_join(full_calendar_df, by = c("Period")) %>%
  replace(is.na(.), 0) %>%
  select(-`Item No_`)


item_ts3 <- item_df3 %>% 
  filter( Period <='2019-01-01') %>% 
  select(-Period) %>% 
  ts(start = c(2015,1), frequency = 12)




#EXPLORING BEHAVIOUR AND RELATIONSHIPS OF REGRESSAND AND POSSIBLE REGRESSORS-------------------------

#plotting a scatterplot matrix to find correlation between features
item_df %>% 
  select(-Period) %>%
  ggpairs()

#looking for seasonality
ggseasonplot(item_ts[,"Sum_Quantity"])
ggseasonplot(item_ts[,"Sum_Quantity"], polar = TRUE)
ggsubseriesplot(item_ts[,"Sum_Quantity"])

#checking autocorrelation
gglagplot(item_ts_target)
ggAcf(item_ts_target)
autoplot(diff(item_ts_target))
ggAcf(diff(item_ts_target))
Box.test(diff(item_ts_target), lag=24, type = "Ljung")
Box.test((item_ts_target), lag=24, type = "Ljung")


spread_sales_df %>% 
  select(-Period) %>%
  ggpairs()

#lagged_trial with items-- j
#ust trying to see if lagged+/-1 target item has any relation with other items
spread_sales_df %>% select(Period, R240471) %>% #taking only period and item
  mutate( Period= Period %m+% months(-1)) %>% #shifting period back a month
  right_join(spread_sales_df, by= c("Period")) %>% #joining with original dataframe
  select(-Period) %>% 
  ggpairs()

spread_sales_df %>% select(Period, R240471) %>%
  mutate( Period= Period %m+% months(+1)) %>%
  right_join(spread_sales_df, by= c("Period")) %>%
  select(-Period) %>% 
  ggpairs()

#finding items that correlate most with other items
round(spread_sales_df[,2:ncol(spread_sales_df)] %>% cor, 2) %>% colMeans()

#finding correlation among sales of  item classes
sales_df_item %>% 
  left_join(class_key_df, by =c("Item No_"="No_")) %>%
  inner_join(top_items) %>%
  group_by(Period,Class) %>% 
  summarise(Sum_Quantity = sum(Sum_Quantity)) %>% 
  ungroup() %>% 
  drop_na() %>%
  spread(Class,Sum_Quantity) %>%
  replace(is.na(.), 0) %>%
  select (-Period) %>%
  ggpairs(combo = "facethist")

round(class_sales_df[,2:ncol(class_sales_df)] %>% cor, 2) %>% colMeans()

#finding correlation in a class... most have a pretty clear positive correlation, except for those that sell few items
#strong class are clearly 750 and 250
#most probably letting the model see the sales of the class will help improving it
#remember to filter out the sales of the item from class sales variable
sales_df_item %>% 
  left_join(class_key_df, by =c("Item No_"="No_")) %>%
  inner_join(top_items) %>%
  filter(Class == "750ML") %>% 
  select(-c(Class, sum_sales)) %>%
  spread(`Item No_`,Sum_Quantity) %>% select (-Period) %>%  
  replace(is.na(.), 0) %>%
  #pairs()
  ggpairs()

#NEXT STEP CHECKING RELATION BETWEEN SALES TO RETAIL CHAINS... FIRST IN GENERAL, THEN IN ITEMS
customer_sales <- joined_sales %>%
  inner_join(top_items) %>% 
  mutate(Period = floor_date(`Shipment Date`,period) %>% as_date()) %>%
  group_by(Period, Customer_Code) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>%
  ungroup() %>% 
  spread(Customer_Code, Sum_Quantity) %>% 
  replace(is.na(.), 0)

#It looks like DL, CP, CB and TR sales have a solid relation them (a threshold of ca.0.7)
#On the other hand, BL, KF, MT have no relation among them or others
#Here it would be really interesting to see if there is any relation between lagged values
customer_sales %>%
  select(-Period) %>% ggpairs()


#TRYING MODELS WITH BASIC REGRESSORS --------------------------------------------------
#No train-test here, or model optimization at all. Just seeing how basic models behave
fit.trial2 <- tslm(Sum_Quantity ~  
                     Num_Days_Promo +
                     Num_Days_Price_Promo + 
                     Unit_Price +
                     Quarter_Low_Price +
                     Num_Holidays +
                     Easter,
                   data=item_ts)

#same model but with %diff
fit.trial3 <- tslm(Sum_Quantity ~  
                        Num_Days_Promo +
                        Num_Days_Price_Promo + 
                        Unit_Price +
                        Quarter_Low_Price +
                        Num_Holidays +
                        Easter,
                      data=diff(item_ts))

summary(fit.trial2)
summary(fit.trial3)

#plotting fitted trial_1 and actual_data
autoplot(item_ts[,'Sum_Quantity'], series="Data") +
  autolayer(fitted(fit.trial1), series="Fitted") +
  xlab("Period") + ylab("") +
  guides(colour=guide_legend(title=" "))

autoplot(item_ts[,'Sum_Quantity'], series="Data") +
  autolayer(fitted(fit.trial2), series="Fitted") +
  xlab("Period") + ylab("") +
  guides(colour=guide_legend(title=" "))


#just checking naive forecast
naive(item_ts_target, h=2) %>% autoplot()
naive(diff(item_ts_target), h=2) %>% autoplot()

#plotting fitted trial_1 vs actual_data
cbind(Data = item_ts[,"Sum_Quantity"],
      Fitted = fitted(fit.trial2)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("R240271") +
  geom_abline(intercept=0, slope=1)

checkresiduals(fit.trial2)
checkresiduals(fit.trial3)
#residuals are skewed to the negative side of the histogram, which means the model is "blind" to high peaks
#also there is a significant negative acf near lag 15



#TRYING TO FIND WHEN TO CUT THE DATA-----------------------------------------------------------

# because the brand started up in 2014, 
# it is possible that taking the earliest data from 2015 could skew the errors because it had not yet picked up motion
# so here is a procedure that finds the best month to start from according to MASE
# After many trials, playing around with the model, all results point to june 2015

min <- 1
candidate_vec <- c()
for (y in 2015:2017) {
    for (m in 1:12) {
     item_ts2 <- item_df %>% 
      filter(Period > paste0(y, "-", m, "-","1")) %>%
      select(-Period) %>% 
      ts(start = c(y,m), frequency = 12)
      candidate <- accuracy(tslm(Sum_Quantity ~
                                   Num_Days_Promo +
                                   Num_Days_Price_Promo + 
                                   Unit_Price +
                                   Quarter_Low_Price +
                                   Num_Holidays+
                                   Easter,
                                  data=item_ts2))[6]
      if (candidate < min) {
        min <- candidate
        candidate_vec <- c(m,y,min)
        }
      }
  }
candidate_vec


item_ts2 <- item_df %>% 
  filter(Period > "2015-06-01") %>%
  select(-Period) %>% 
  ts(start = c(2015,6), frequency = 12)
  
fit.trial4 <- tslm(Sum_Quantity ~
                             Num_Days_Promo +
                             Num_Days_Price_Promo + 
                             Unit_Price +
                             Quarter_Low_Price +
                             Num_Holidays+
                             Easter,
                           data=item_ts2)

summary(fit.trial4)

autoplot(item_ts2[,'Sum_Quantity'], series="Data") +
  autolayer(fitted(fit.trial4), series="Fitted") +
  xlab("Period") + ylab("") +
  guides(colour=guide_legend(title=" "))
checkresiduals(fit.trial4)


fit.trial5 <- tslm(Sum_Quantity ~
          #Num_Days_Promo +
          #Num_Days_Price_Promo + 
          #Unit_Price +
          #Quarter_Low_Price +
          #Num_Holidays+
          #Easter+
          Class_Sales,
          data=item_ts)


autoplot(item_ts[,'Sum_Quantity'], series="Data") +
  autolayer(fitted(fit.trial5), series="Fitted") +
  xlab("Period") + ylab("") +
  guides(colour=guide_legend(title=" "))

#plotting fitted trial_1 vs actual_data
cbind(Data = item_ts[,"Sum_Quantity"],
      Fitted = fitted(fit.trial5)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("R240271") +
  geom_abline(intercept=0, slope=1)

min <- 1
candidate_vec <- c()
for (y in 2015:2017) {
  for (m in 1:12) {
    item_ts2 <- item_df %>% 
      filter(Period > paste0(y, "-", m, "-","1")) %>%
      select(-Period) %>% 
      ts(start = c(y,m), frequency = 12)
    candidate <- accuracy(tslm(Sum_Quantity ~
                                 Num_Days_Promo +
                                 Num_Days_Price_Promo + 
                                 Unit_Price +
                                 Quarter_Low_Price +
                                 Num_Holidays+
                                 Easter+
                                 Class_Sales,
                               data=item_ts2))[6]
    if (candidate < min) {
      min <- candidate
      candidate_vec <- c(m,y,min)
    }
  }
}
#Period that minimizes MSE is april-2015, though I´m not going to change it. Just checking
candidate_vec

accuracy(fit.trial4)
accuracy(fit.trial5)

checkresiduals(fit.trial4)
checkresiduals(fit.trial5)

#Checking if predictors are randomly scattered when plotted against residuals
df <- as.data.frame(item_ts) #creating a dataframe for plot
df[,"Residuals"]  <- as.numeric(residuals(fit.trial5)) #adding a column with residuals from fitting

ggplot(df, aes(x=Num_Days_Promo, y=Residuals)) +
  geom_point()
ggplot(df, aes(x=Num_Days_Price_Promo, y=Residuals)) +
  geom_point()
ggplot(df, aes(x=Unit_Price, y=Residuals)) +
  geom_point()
ggplot(df, aes(x=Quarter_Low_Price, y=Residuals)) +
  geom_point()
ggplot(df, aes(x=Num_Holidays, y=Residuals)) +
  geom_point()
ggplot(df, aes(x=Easter, y=Residuals)) +
  geom_point()
ggplot(df, aes(x=Class_Sales, y=Residuals)) +
  geom_point()





checkresiduals(fit.trial5)


CV(fit.trial4)
CV(fit.trial5)

#FINDING BEST MODELS WITHOUT PROPER TRAIN TEST -------------------------------------------------------------------------------------------------------------------

#TRYING MODEL WITH BASIC REGRESSORS
#create variable name vector
variable_names <- item_df %>% 
  select(Num_Days_Price_Promo, Num_Days_Promo, Quarter_Low_Price,
         Unit_Price, Easter, Num_Holidays,Class_Sales) %>% 
  colnames()

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)

#passing the boolean dataframe to the variable name vector to create a dataframe with the code for all possible combinations
model_list <- apply(combo_df, 1, function(x) as.formula(
  paste("Sum_Quantity ~ ",
          paste(c( variable_names[x]) ,collapse=" + "))))
          
#using the code stored in previous dataframe to pass it to a CV(tslm()) function and storing result in df
model_results_df <- lapply(model_list,
                           function(x) CV(tslm(x, data=item_ts))) %>%
  as.data.frame() %>%
  t() %>% 
  as_tibble()

#saving index for mutating in next lines
rankcv_index <- length(combo) + length(model_results_df) +1

#binding previous dataframe with boolean dataframe, and sorting by best score
full_model_results_df <- cbind (combo, model_results_df) %>% 
  mutate(RankCV = rank(CV), RankAIC = rank(AIC), RankAICc = rank(AICc), RankBIC = rank(BIC)) %>% 
  mutate(Mean_Rank = rowMeans(.[, rankcv_index:(rankcv_index+3)])) %>% 
  arrange(Mean_Rank) 
  

# Class Sales and Num_Days_Promo are certainly 2 most important predictors!!!!
# Easter, Unit Price and Quarter_Low_Price must improve as features or be forgotten

#storing row of winner as vector
winner <- full_model_results_df[1,1:length(variable_names)] %>%
  t() %>%
  as.vector

#parsing winner vector as formula to generate single script
winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[winner]) ,collapse=" + ")))

fit.trial6 <- tslm(winner_script, data=item_ts)

autoplot(item_ts[,'Sum_Quantity'], series="Data") +
  autolayer(fitted(fit.trial6), series="Fitted") +
  xlab("Period") + ylab("") +
  guides(colour=guide_legend(title=" "))

checkresiduals(fit.trial6)



#TRYING MODEL WITH "SPREAD" REGRESSORS
#create variable name vector
variable_names <- item_df %>%
  select(-c(Period, Sum_Quantity, Num_Days_Price_Promo, 
            Num_Days_Promo, Quarter_Low_Price, Unit_Price, Easter,Lag_Class_Sales)) %>% 
  colnames()

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)

#passing the boolean dataframe to the variable name vector to create a dataframe with the code for all possible combinations
model_list <- apply(combo_df, 1, function(x) as.formula(
  paste("Sum_Quantity ~ ",
        paste(c( variable_names[x]) ,collapse=" + "))))

#using the code stored in previous dataframe to pass it to a CV(tslm()) function and storing result in df
model_results_df2 <- lapply(model_list,
                           function(x) CV(tslm(x, data=item_ts))) %>%
  as.data.frame() %>%
  t() %>% 
  as_tibble()

#saving index for mutating in next lines
rankcv_index <- length(combo_df) + length(model_results_df2) +1

#binding previous dataframe with boolean dataframe, and sorting by best score
full_model_results_df2 <- cbind (combo_df, model_results_df2) %>% 
  mutate(RankCV = rank(CV), RankAIC = rank(AIC), RankAICc = rank(AICc), RankBIC = rank(BIC)) %>% 
  mutate(Mean_Rank = rowMeans(.[, rankcv_index:(rankcv_index+3)])) %>%
  #mutate(Mean_Rank = rowMeans(.[,23:26])) %>% 
  arrange(Mean_Rank)

#saving the winner model
winner <- full_model_results_df2[5,1:length(variable_names)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[winner]) ,collapse=" + ")))

fit.trial7 <- tslm(winner_script, data=item_ts)

autoplot(item_ts[,'Sum_Quantity'], series="Data") +
  autolayer(fitted(fit.trial7), series="Fitted") +
  xlab("Period") + ylab("") +
  guides(colour=guide_legend(title=" "))

checkresiduals(fit.trial7)



#FINDING BEST MODELS WITH BASIC TRAIN-TEST ---------------------------------------------

#TRAINING MODEL WITH BASIC REGRESSORS
#create variable name vector
variable_names <- item_df %>% 
  select(Num_Days_Price_Promo, Num_Days_Promo, Quarter_Low_Price,
         Unit_Price, Num_Holidays, Lag_Class_Sales) %>% 
  colnames()

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)

#passing the boolean dataframe to the variable name vector to create a dataframe with the code for all possible combinations
model_list <- apply(combo_df, 1, function(x) as.formula(
  paste("Sum_Quantity ~ ",
        paste(c( variable_names[x]) ,collapse=" + "))))

#using the code stored in previous dataframe to pass it to a CV(tslm()) function and storing result in df
model_results_df <- lapply(model_list,
                           function(x) CV(tslm(x, data=item_ts_train))) %>%
  as.data.frame() %>%
  t() %>% 
  as_tibble()

#saving index for mutating in next lines
rankcv_index <- length(combo_df) + length(model_results_df) +1

#binding previous dataframe with boolean dataframe, and sorting by best score
full_model_results_df <- cbind (combo_df, model_results_df) %>% 
  mutate(RankCV = rank(CV), RankAIC = rank(AIC), RankAICc = rank(AICc), RankBIC = rank(BIC)) %>% 
  mutate(Mean_Rank = rowMeans(.[, rankcv_index:(rankcv_index+3)])) %>% 
  arrange(Mean_Rank) 

winner <- full_model_results_df[1,1:length(variable_names)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[winner]) ,collapse=" + ")))

fit.trial8 <- tslm(winner_script, data=item_ts_train)

checkresiduals(fit.trial8)

new_data <- item_df_test#item_df %>% filter(Period>'2019-01-01' & Period <'2019-04-01')
fc.trial8 <- forecast (fit.trial8, newdata = new_data)

autoplot(fc.trial8) +
  autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.trial8)



# TRAINING MODEL WITH "SPREAD" REGRESSORS
#create variable name vector
variable_names <- item_df %>%
  select(-c(Period, Sum_Quantity, Num_Days_Price_Promo, 
            Num_Days_Promo, Quarter_Low_Price, Unit_Price, Easter,Class_Sales)) %>% 
  colnames()

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)

#passing the boolean dataframe to the variable name vector to create a dataframe with the code for all possible combinations
model_list <- apply(combo_df, 1, function(x) as.formula(
  paste("Sum_Quantity ~ ",
        paste(c( variable_names[x]) ,collapse=" + "))))

#using the code stored in previous dataframe to pass it to a CV(tslm()) function and storing result in df
model_results_df3 <- lapply(model_list,
                            function(x) CV(tslm(x, data=item_ts_train))) %>%
  as.data.frame() %>%
  t() %>% 
  as_tibble()

#saving index for mutating in next lines
rankcv_index <- length(combo_df) + length(model_results_df3) +1

#binding previous dataframe with boolean dataframe, and sorting by best score
full_model_results_df3 <- cbind (combo_df, model_results_df3) %>% 
  mutate(RankCV = rank(CV), RankAIC = rank(AIC), RankAICc = rank(AICc), RankBIC = rank(BIC)) %>% 
  mutate(Mean_Rank = rowMeans(.[, rankcv_index:(rankcv_index+3)])) %>%
  #mutate(Mean_Rank = rowMeans(.[,23:26])) %>% 
  arrange(Mean_Rank)

#saving the winner model
winner <- full_model_results_df3[17,1:length(variable_names)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[winner]) ,collapse=" + ")))

fit.trial9 <- tslm(winner_script, data=item_ts_train)

checkresiduals(fit.trial9)

new_data <- item_df_test#item_df %>% filter(Period>'2019-01-01' & Period <'2019-04-01')
fc.trial9 <- forecast (fit.trial9, newdata = new_data, h=4)

autoplot(fc.trial9) +
  autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.trial9)



#Adding Lagged Class Sales improves the forecast accuracy (MASE) by 2 points

#Let´s try to see if there is a better model than the one that is ranked first

winner_index <- 0
candidate_accuracy <- 1
for (i in (1:20)) {
  
  candidate <- full_model_results_df3[i,1:(length(variable_names))] %>%
    t() %>%
    as.vector
  
  candidate_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[candidate]) ,collapse=" + ")))
  
  fit.trial10 <- tslm(candidate_script, data=item_ts_train)
  
  new_data <- item_df_test %>% filter(Period>'2019-01-01' & Period <'2019-04-01')
  fc.trial10 <- forecast (fit.trial10, newdata = new_data)
  print(paste0("loop ", i))
  print(accuracy(fc.trial10))
  
  if (accuracy(fc.trial10)[6] < candidate_accuracy) {
    candidate_accuracy <- accuracy(fc.trial10)[6]
    winner_index <- i
  }
  
}
winner_index
candidate_accuracy

#It looks like number of holidays improves accuracy slightly
#If the final purpose were to get monthly predictions, a good idea would be to add information with num of working days

#It would be useful to find a possible explanation to why only certain retail chains are relevant to model
#A suspicion is that there are 2 factors:
# --total sales during promotional periods
# --retail chains like Billa & Kaufland might be stretching Promo Price Periods to get overall lower purchase costs 
