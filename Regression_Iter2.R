#install.packages("DataCombine")
#install.packages("TSA")
#install.packages("seasonal")
library (dplyr)
library (lubridate)
library (tidyr)
library (zoo)
library (GGally)
library (forecast)
library (ggplot2)
library (DataCombine)
library (stringr)
library (TSA)
library (seasonal)
library(data.table)

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


columns_match <- function(dataframe, string) {
  colnames(dataframe) [str_detect (colnames(dataframe),string)]  
}


#function that finds in a df with models, arranged by best, the most accurate model according to MASE
#won´t work it item_ts_train and item_ts_test are not saved in environment, so careful
most_accurate <- function(dframe, n_trials, target_string)
  
{  winner_index <- 0
candidate_accuracy <- 1
for (i in (1:n_trials)) {
  
  candidate <- dframe[i,1:(length(variable_names))] %>%
    t() %>%
    as.vector
  
  candidate_script <- as.formula(paste(target_string, paste(c( variable_names[candidate]) ,collapse=" + ")))
  
  fit <- tslm(candidate_script, data=item_ts_train)
  
  new_data <- item_df_test
  fc <- forecast (fit, newdata = new_data)
  
  if (accuracy(fc,x=item_ts_test[,1])[2,6] < candidate_accuracy) {
    candidate_accuracy <- accuracy(fc,x=item_ts_test[,1])[2,6]
    winner_index <- i
  }
}
return(c(winner_index, candidate_accuracy))
}

#vectorized function for extracting weekdays
nweekdays <- Vectorize(function(date1, date2) 
  sum(!wday(seq(date1, date2, "days")) %in% c(6,1)))


#function that finds outliers using specified percentiles, but discards treating those within the last 15 months
#then replaces outliers by taking the maximum values (inside interpercentile) of its own year or the next one
moderate_outliers <- function(dframe,
                              percentile_hi =95,
                              percentile_lo =5,
                              target_name = "Sum_Quantity",
                              period_name = "Period") {
  
  max_date <- dframe[,period_name] %>% max()
  
  outlier_df <- dframe %>%
    select(!!period_name,!!target_name) %>%
    mutate (Index = row.names(.) %>% as.numeric()) %>% #store an index for later replacement
    mutate (Percentile = ntile(dframe[,2], 100)) %>%  #create a column with percentile of value
    filter (Percentile <= percentile_lo | Percentile >= percentile_hi) %>% #keep only values beyond specified percentile range
    filter (difftime (max_date, get(period_name)) %>% #keep only outliers that are previous to last 15 months (year is a bit liquid in ts)
              time_length("months") %>% #difftime is passed to timelength
              round() >15 )
  
  #create a dataframe with maximum and minimum values per year within specified percentile range
  min_max_df <- dframe %>%
    select(!!period_name,!!target_name) %>%
    mutate (Percentile = ntile(dframe[,2], 100)) %>%
    filter (!Percentile <= percentile_lo & !Percentile >= percentile_hi) %>% 
    group_by(Year = year(get(period_name))) %>%
    summarise(Max = max(get(target_name)), Min = min(get(target_name)))
  
  #Initiliaze an empty vector for loop
  replace_vector <- c()
  
  #For each outlier, find values to replace and store them in vector
  for (i in 1:nrow(outlier_df)) {
    outlier_year <- year( outlier_df[i,period_name] )
    outlier_percentile <- outlier_df[i,"Percentile"]
    
    
    if (outlier_percentile >= percentile_hi) {
      max_val <- min_max_df %>% 
        filter (Year ==outlier_year | Year ==outlier_year+1) %>%
        select(Max) %>% 
        max()
      replace_vector <- append(replace_vector,max_val)
      
    } else { 
      min_val <- min_max_df %>% 
        filter (Year ==outlier_year | Year ==outlier_year+1) %>%
        select(Min) %>% 
        min()
      replace_vector <- append(replace_vector,min_val)
    }
  }
  
  replaced <- dframe
  #replacing values
  replaced[,target_name] <- replace(dframe[,target_name], outlier_df$Index, replace_vector)
  
  return(replaced)
}



# Function that gets the naive component of the MASE calculation. Specific for finding cross-validation accuracy
# The ts object that is passed to it is that of the target value
get_naive_cv_comp <- function (ts_obj, h=2, initial) {
  
  tsCV (ts_obj, naive, h=h, initial = initial) %>%
    as.data.frame() %>%
    abs() %>% 
    drop_na() %>%
    mutate(Sum_Errors = rowSums(.)) %>% 
    select(Sum_Errors) %>%
    colMeans()
}

# Function that calculates MASE. Specific for finding cross-validation
# The ts object that is passed is that of the errors of the forecast
# Very important to make sure that the naive benchmark that is passed has the same parameters of the evaluated model
get_mase_cv <- function (ts_obj, h=2, naive=naive_error) {
  
  ts_obj %>% 
    as.data.frame() %>% 
    #mutate(Index = row.names(.)) %>% 
    drop_na() %>%
    abs() %>% 
    mutate(Sum_Errors = rowSums(.)) %>%
    mutate(Q =  Sum_Errors / naive_error) %>%
    select(Q) %>%
    rename(MASE = Q) %>% 
    colMeans()
  
}




#PREPARING DATAFRAME WITH FEATURES THAT WILL BE USED FOR TRYING MODELS--------------------------------------------------

#In this iteration we will only use item R240271 
#filtering sales dataframe for given item
filtered_sales <- sales_df_item %>%
  filter (year(Period) >= 2015 ) %>% 
  filter(`Item No_` == top_items$`Item No_`[2]) %>% 
  select(-`Item No_`)

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


#Creating a dataframe with the months of the seasons
Summer <- c(7,8,9)
Autumn <- c(10,11,12)
Winter <- c(1,2,3)
Spring <- c(4,5,6)



seasons_df <- data.frame(Summer,Autumn,Winter,Spring) %>% gather(key="Season", value="Month")
seasons_df$Season <- factor(seasons_df$Season, levels= c("Winter","Spring","Summer","Autumn"))

#Creating a dataframe with sales grouped by seasons, to be used in exploration only
season_sales <- period_df %>% 
  mutate(Month = month(Period)) %>%
  filter(year(Period)>2014) %>%
  filter(Period < '2019-04-01') %>% 
  left_join(seasons_df, by = c("Month")) %>% 
  left_join(filtered_sales, by = c("Period")) %>% 
  select(-Month) %>% 
  group_by(Year = year(Period),Season) %>%
  summarise(Sum_Quantity = sum(Sum_Quantity)) %>% 
  ungroup()

#Creating a dataframe with Bank Holidays
month_bank_holidays_df <- bank_holidays_df %>% 
  mutate(Period = floor_date(start, period) %>%  as_date()) %>% #Creating period
  group_by(Period) %>%
  summarise(Easter = sum(Easter), Num_Holidays= n()) %>% 
  ungroup() %>% 
  mutate(Easter = ifelse (Easter>0,1,0))

#Creating a dataframe with school_holidays
month_school_holidays_df <- school_cal_df %>% 
  expand_time_int(start_col="Start", end_col="End", line_col="Line", n_days_col="Num_Days", period=period) %>% 
  mutate(Period = floor_date(Start, period) %>%  as_date()) %>%
  mutate(Period = Period %m+% months(Line-1)) %>% 
  arrange(Period) %>% 
  #select(Period,Num_Days) %>%
  group_by(Period) %>% 
  summarise(Num_Days = sum(Num_Days)) %>% 
  ungroup() %>% 
  rename(Num_School_Holidays = Num_Days)


#creating a dframe with sales of the class
#excludes sales of the forecast item
class_sales_df <- sales_df_item %>%
  filter (Period >= '2014-11-01') %>% #doing this for later using lagged values
  left_join(class_key_df, by =c("Item No_"="No_")) %>%
  inner_join(top_items) %>%
  drop_na() %>%
  filter(`Item No_` != top_items$`Item No_`[2]) %>% #getting rid of item sales
  group_by(Period, Class) %>% 
  summarise(Class_Sales = sum(Sum_Quantity)) %>%
  filter(Class == class_key_df[ match(top_items$`Item No_`[2],class_key_df$No_) ,"Class"]) %>% 
  select(-Class) %>% 
  ungroup()

#creating a df for lagged class sales
lagged_class_df <- class_sales_df %>%
mutate(Period= Period %m+% months(+1)) %>% 
rename(Lag_Class_Sales = Class_Sales)

#creating a df for lagged item sales
lagged_item_sales <- sales_df_item %>%
  filter (Period >= "2014-11-01" ) %>% 
  filter(`Item No_` == top_items$`Item No_`[2]) %>% 
  select(Period, Sum_Quantity) %>% 
  mutate(Period= Period %m+% months(+2)) %>% 
  rename(Lag_Item_Sales = Sum_Quantity)


#creating a df for lagged price promo days (2 months lag)
lagged_price_promo_df <- filtered_sales_price %>%
  select(Period, Num_Days_Price_Promo) %>%
  mutate( Period= Period %m+% months(+2)) %>% 
  rename(Lag_Price_Promo = Num_Days_Price_Promo)
  

#spreading promo days by customer
promo_spread <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>%
  select(Period, Customer_Code, Num_Days_Promo) %>%
  spread(Customer_Code,Num_Days_Promo) %>% 
  rename_at(vars(-Period), funs(paste0("Days_Promo_", .))) #%>%
  replace(is.na(.), 0)

#spreading price promo days by customer
price_promo_spread <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>%
  select(Period, Customer_Code, Num_Days_Price_Promo) %>%
  spread(Customer_Code,Num_Days_Price_Promo) %>%
  rename_at(vars(-Period), funs(paste0("Days_Price_Promo_", .))) #%>%
  replace(is.na(.), 0)

#spreading unit price by customer
unit_price_spread <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>%
  filter(year(Period) >= 2015) %>% 
  select(Period, Customer_Code, Unit_Price) %>% 
  spread(Customer_Code, Unit_Price) %>% 
  rename_at(vars(-Period), funs(paste0("Unit_Price_", .))) %>%
  replace(is.na(.), 10)

#creating a dataframe with all features
item_df <- period_df %>% 
  filter(Period < as_date('2019-05-01')) %>% 
  mutate(Month=month(Period)) %>% 
  left_join(filtered_sales, by = c("Period")) %>% 
  left_join(filtered_sales_price, by = c("Period")) %>%
  left_join(month_bank_holidays_df, by = c("Period")) %>%
  left_join(month_school_holidays_df, by = c("Period")) %>%
  left_join(class_sales_df, by = c("Period")) %>%
  left_join(promo_spread, by = c("Period")) %>%
  left_join(price_promo_spread, by = c("Period")) %>%
  left_join(unit_price_spread, by = c("Period")) %>%
  left_join(lagged_class_df, by = c("Period")) %>%
  left_join(lagged_item_sales, by = c("Period")) %>%
  left_join(lagged_price_promo_df, by = c("Period")) %>%
  left_join(seasons_df) %>%
  replace(is.na(.), 0) %>% #fill nas with zeros
  mutate(Winter = as.numeric(Season=="Winter")) %>% 
  mutate(Summer = as.numeric(Season=="Summer")) %>%
  mutate(Spring = as.numeric(Season=="Spring")) %>%
  mutate(Autumn = as.numeric(Season=="Autumn")) %>% 
  mutate(Num_Business_Days = nweekdays(Period, Period %m+% months(+1)-1) -Num_Holidays) %>% 
  select(-c("Month","Season"))
  
  

#saving a vector with names of columns that have all zeros
all_zero <- names( item_df [apply(item_df, MARGIN = 2, FUN = function(x) sum(x==0)/nrow(item_df) ==1)] )

#getting rid of columns that have all zeros
item_df <- item_df %>%
  select(-all_zero)

#creating a timeseries object with data
item_ts <- item_df %>% 
  #filter( Period <='2019-01-01') %>% 
  select(-Period) %>% 
  ts(start = c(2015,1), frequency = 12)

#creating a train data set that is a timeseries object
item_ts_train <- item_df %>% 
  filter( Period <as_date('2019-01-01')) %>% 
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



#spreading sales df by item (this will not be included as feature- just for exploration)
spread_sales_df <- sales_df_item %>% spread(`Item No_`,Sum_Quantity)
spread_sales_df <- spread_sales_df[1:nrow(spread_sales_df), -which(colMeans(is.na(spread_sales_df)) > 0.5)] #getting rid of columns that have more than half nas
spread_sales_df [,2:ncol(spread_sales_df)] <- spread_sales_df [,2:ncol(spread_sales_df)] %>% na.fill(0) #filling nas with zeros


#creating a dataframe with customer sales for top items (just for exploration)
customer_sales_df <- joined_sales %>%
  inner_join(top_items) %>% 
  mutate(Period = floor_date(`Shipment Date`,period) %>% as_date()) %>%
  group_by(Period, Customer_Code) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>%
  ungroup() %>% 
  spread(Customer_Code, Sum_Quantity) %>% 
  replace(is.na(.), 0)



#EXPLORING BEHAVIOUR AND RELATIONSHIPS OF REGRESSAND AND POSSIBLE REGRESSORS-------------------------

item_df %>% 
  ggplot( aes(x= Sum_Quantity))+
  geom_histogram(bins=10, binwidth = 400)

item_df %>%
  select(Sum_Quantity) %>% 
  log1p() %>% 
  ggplot( aes(x= Sum_Quantity))+
  geom_histogram(bins=10, binwidth = 0.2)

#plotting a scatterplot matrix to find correlation between engineered features
item_df %>% 
  select(-Period) %>%
  ggpairs()

#CHECKING AUTOCORRELATION
#Plot doesn´t show anything interesting
gglagplot(item_ts_target)

#Lag 2 shows high correlation, could make up for a good predictor
ggAcf(item_ts_target)

autoplot(diff(item_ts_target))
ggAcf(diff(item_ts_target))
Box.test(diff(item_ts_target), lag=24, type = "Ljung")
Box.test((item_ts_target), lag=24, type = "Ljung")

#Checking acf of predictors
ggAcf(item_ts[,"Lag_Item_Sales"])
ggAcf(item_ts[,"Lag_Class_Sales"])
ggAcf(item_ts[,"Class_Sales"])



#LOOKING FOR SEASONALITY

#Seems like there is no monthly seasonality
ggseasonplot(item_ts[,"Sum_Quantity"], polar = TRUE)
ggsubseriesplot(item_ts[,"Sum_Quantity"])

#From previous plots it looks like there could be quarterly seasonality that follows natural seasons
#Let´s plot sales by quarters:
season_sales %>% 
  select (Sum_Quantity) %>% 
  ts(start = c(2015,2), frequency = 4) %>% 
  ggseasonplot(polar = TRUE)


season_sales %>% 
  select (Sum_Quantity) %>% 
  ts(start = c(2015,2), frequency = 4) %>% 
  autoplot(series="Quarterly") + 
  autolayer(item_ts[,1], series="Monthly")


#Let´s take a look at a boxplot of seasons
#From the boxplot it looks like grouping seasons by months works doesn´t work too badly
#Any way This boxplot doesn´t help much because it has so little points
season_sales %>%
  filter(Year < 2019) %>% 
  ggplot(aes(x=Season, y=Sum_Quantity, fill=Season)) + 
  geom_boxplot()

#Let´s try a scatterplot better
#Nothing really conclusive can be said yet, except that summer is certainly a peak season
season_sales %>% 
  filter(Year < 2019) %>% 
  ggplot(aes(x=Season, y=Sum_Quantity,label=Year, color=Year)) + 
  geom_point()+
  geom_text(aes(label=Year),hjust=1, vjust=-0.7)


# let´s take a look at a boxplot of months
# March has a great dispersion, but April does not, so it doesn´t look like an Easter effect
filtered_sales %>%
  filter(year(Period) > 2015) %>% 
  mutate(Month = month(Period, label =TRUE)) %>%
  ggplot(aes(x=Month, y=Sum_Quantity, fill=Month)) + 
  geom_boxplot()

#same, scatterplot for months
#this just leaves clear that the previous boxplot gives no conclusive insights-
#- except that autumn looks like the most coherent season, and that months do have some difference in levels,
#- but not the same ones among years.
#So, creating a feature for month could help a bit, but it doesn´t look like too much -
#- here treating outliers would be a good idea
filtered_sales %>%
  filter(year(Period) > 2016) %>%
  mutate(Month = month(Period, label =TRUE)) %>%
  ggplot(aes(x=Month, y=Sum_Quantity, color= year(Period))) + 
  geom_point(aes(size=0.2))
  
#filtered_sales %>%
#  filter(year(Period) > 2015) %>% 
#  mutate(Month = month(Period, label =TRUE)) %>%
#  ggplot(aes(x=Sum_Quantity)) + 
#  geom_histogram(binwidth = 30) +
#  facet_wrap(~Month )

# A smoothed timeseries with a moving average of order 4 shows that there is certain seasonality
ma(item_ts[,1], 4) %>% autoplot(series="Monthly Smoothed",size=2) +
  autolayer(item_ts[,1], series="Monthly")

season_sales %>% 
  select (Sum_Quantity) %>% 
  ts(start = c(2015,2), frequency = 4) %>% 
  autoplot(series="Quarterly") + 
  autolayer(item_ts[,1], series="Monthly") +
  autolayer(ma(item_ts[,1], 4), series="Monthly Smoothed", size=2)

# My theory is that in 2015-2016 refrigerated juices were an entirely new product, 
# - and the market was adjusting its behaviour
# Taking a look at the behaviour of the class, or at other important items could help understand

class_ma_ts <- class_sales_df %>%
  filter(year(Period) > 2014 ) %>% 
  select(Class_Sales) %>% 
  ts(start = c(2015,1), frequency = 12) %>%
  ma(4)
  
class_ma_ts %>% 
  autoplot(series="Class Sales Smoothed")
  
brand_ma_ts <- sales_df_item %>%
  filter (year(Period) >= 2015 ) %>% 
  filter(!`Item No_` == top_items$`Item No_`[2]) %>% #filter out forecast item
  filter(!`Item No_` %in% c('R240092', 'R240091')) %>% #filter out seasonal products
  left_join(period_df) %>%
  group_by(Period) %>% 
  summarise(Sum_Quantity = sum(Sum_Quantity)) %>%
  select(Sum_Quantity) %>% 
  ts(start = c(2015,1), frequency = 12) %>%
  ma(4)

brand_ma_ts %>% 
  autoplot(series="Brand Sales Smoothed")

#All three follow a similar pattern, although in 2018, class sales seems to lead a month or two, 
#- and thus, brand sales has a rounder peak in 2018
#Also it does look like the market has adjusted a bit,
#- and we could expect that peaks and troughs look more stable in the future
#Bear in mind that neither brand or class sales have forecast item in their data!
#From class sales, we can see that the brand pattern is not extensible to all items, 
#- so we should be careful when forecasting other items
#joined_ma_ts <- cbind(brand_ma_ts, class_ma_ts, ma(item_ts[,1],4))
smoothed_comparison <- cbind(BRAND = brand_ma_ts , CLASS = class_ma_ts, ITEM = ma(item_ts[,1],4))
smoothed_comparison %>% 
autoplot(facets =TRUE)


# Another approach to finding seasonality: Fourier transformations

# Periodogram returns a list of frequencies at which the observations were vectorized, and the power of these
pg_list <- item_df %>% select(Sum_Quantity) %>% periodogram()

# Creating a dataframe with the results from previous step
pg_df <- data.frame(Frequency=pg_list$freq, Power=pg_list$spec) %>%
  mutate(Days = 1/Frequency) %>% #transforming frequency to unit days
  arrange(desc(Power))

# Checking top 3 power frequencies
# Seems like top scorer is 54 days by far
pg_df %>% head(5)

# And plotting an approximation of 54 days in a polar seasonal plot
# Perhaps this makes more sense when looking at weekly data
item_df %>%
  mutate(Bimonthly = lubridate::floor_date(Period, "2 months")) %>%
  group_by(Bimonthly) %>%
  summarise(Sum_Quantity = sum(Sum_Quantity)) %>%
  select(-Bimonthly) %>%
  ts(start = c(2015,1), frequency = 6) %>%
  ggseasonplot(polar=TRUE)

item_df %>%
  mutate(Bimonthly = lubridate::floor_date(Period, "2 months")) %>%
  group_by(Bimonthly) %>%
  summarise(Sum_Quantity = sum(Sum_Quantity)) %>%
  select(-Bimonthly) %>%
  ts(start = c(2015,1), frequency = 6) %>%
  ggseasonplot()


#Let´s try some proper methods to decompose our timeseries

#X11 returns the expected seasonality: troughs in autumn, peaks in summer
#Trend also behaves as expected
x11 <- item_ts[, 1] %>% seas(x11="")
x11 %>% autoplot()
x11$data[, "seasonal"] %>% autoplot()

#Seats however is unable to find seasonality, returns irregular instead (no wonder)
#It can only be used with quarterly and monthly data, and still nothing
#However it has captured the trend pretty well
seats <- item_ts[, 1] %>% seas()
seats$data %>% 
  autoplot(facets=TRUE)

#After much toggling with windows, Loess looks like X11 decomposition
# "Both t.window and s.window should be odd numbers"
# "t.window is the number of consecutive observations to be used when estimating the trend-cycle [...]"
loess <- item_ts[, 1] %>%
  stl(t.window=5, s.window=5, robust=TRUE)
loess %>% 
  autoplot()



#SO WE´VE SEEN SEASONALITY, BUT LET´S TAKE A LOOK AT WHAT PROBABLY CAUSES IT: PROMOTIONS


#creating a dataframe with promo price dates to plot a gantt chart
price_promo_df <- joined_sales_price %>%
  filter(`Item No_`== top_items$`Item No_`[2]) %>% 
  select(`Sales Code`, Customer_Code, Starting_Date_Price_Promo, Ending_Date_Price_Promo) %>% 
  rename(Start = Starting_Date_Price_Promo,
         End = Ending_Date_Price_Promo) %>% 
  gather("State", "Date", 3:4) %>%
  drop_na() %>% 
  mutate (Type = "Price Promo")

#creating a dataframe with promo dates to plot a gantt chart
promo_df <- joined_sales_price %>%
  filter(`Item No_`== top_items$`Item No_`[2]) %>% 
  select(`Sales Code`, Customer_Code, Starting_Date_Promo, Ending_Date_Price_Promo) %>% 
  rename(Start = Starting_Date_Promo,
         End = Ending_Date_Price_Promo) %>% 
  gather("State", "Date", 3:4) %>%
  drop_na() %>% 
  mutate (Type = "Promo") 

rbind(promo_df,price_promo_df)

nrow(promo_df)+nrow(price_promo_df)

#creating a faceted gantt chart for both promo and promo price
#First important conclusion: features related to promotions and customers CB00, and MT00 are useless
#CB00 and BL00 are promotionally very active, features giving info related to this could be very helpful
rbind(promo_df, price_promo_df) %>%    
  ggplot(aes(x=Date, y=Customer_Code, color=Customer_Code, group=`Sales Code`)) +
  geom_line(size = 10) +
  labs(x="", y=NULL, title="Promotion Timeline") +
  facet_grid(Type ~ .)

#After the gantt chart we know what customers are really active in promotions,
#So now let´s take a look and see in which months do we have most promotions,
# -and how that has evolved along the years


#Creating a faceted bar chart to show number of days per month with both promo & promo prices
item_df %>%
  mutate(Month = month(Period)) %>%
  rename(Promo = Num_Days_Promo,
         Price_Promo = Num_Days_Price_Promo) %>% 
  gather ("Type", "Num_Days", 3:4) %>%
  ggplot(aes(x=Month, y=Num_Days, fill=as.factor(Month))) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position="none") +
  scale_x_discrete(limits=seq(1,12)) +
  facet_grid(Type ~ .)


#let´s see how the number of days per month has evolved along years
#because just promos will probably mean a loss of information when forecasting, let´s look only at promo prices
item_df %>%
  mutate(Year = year(Period)) %>% 
  mutate(Month = month(Period)) %>%
  rename(Promo = Num_Days_Promo,
         Price_Promo = Num_Days_Price_Promo) %>% 
  gather ("Type", "Num_Days", 3:4) %>%
  filter(Type=="Price_Promo") %>% 
  ggplot(aes(x=Month, y=Num_Days, fill=as.factor(Month))) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position="none") +
  scale_x_discrete(limits=seq(1,12)) +
  facet_grid(Year ~ .)

#Bar chart with number of price promo days per quarter
#It seems like promotions are getting a more balanced distribution along the year,
#so it is unclear what will happen with seasonality
item_df %>%
  mutate(Year = year(Period)) %>% 
  mutate(Quarter = quarter(Period)) %>%
  rename(Promo = Num_Days_Promo,
         Price_Promo = Num_Days_Price_Promo) %>% 
  gather ("Type", "Num_Days", 3:4) %>%
  filter(Type=="Price_Promo") %>% 
  ggplot(aes(x=Quarter, y=Num_Days, fill=as.factor(Quarter))) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position="none") +
  scale_x_discrete(limits=seq(1,12)) +
  facet_grid(Year ~ .)


item_df %>%
  select(Num_Days_Price_Promo) %>% 
  ggAcf()
item_df %>%
  select(Num_Days_Price_Promo) %>% 
  ggPacf()





cor(lagged_price_promo[,"Num_Days_Promo"],lagged_price_promo[,"Sum_Quantity"])
cor(item_df[,"Num_Days_Price_Promo"],item_df[,"Sum_Quantity"])  

lagged_price_promo %>%
  autoplot(facets=TRUE)



#NEXT STEP, LET´S LOOK FOR RELATIONS AMONG ITEMS
#Because there are so many, it will be more practical to use ggpairs
spread_sales_df %>% 
  select(-Period) %>%
  ggpairs()

#Lagged_trial with items-- j
#Just trying to see if lagged+/-1 target item has any relation with other items
#No strong correlation
spread_sales_df %>% select(Period, R240271) %>% #taking only period and item
  mutate( Period= Period %m+% months(+1)) %>% #shifting period back a month
  right_join(spread_sales_df, by= c("Period")) %>% #joining with original dataframe
  select(-Period) %>% 
  ggpairs()

ggAcf(item_ts[,1])

#finding items that correlate most with other items
round(spread_sales_df[,2:ncol(spread_sales_df)] %>% cor, 2) %>% colMeans()

#finding correlation among sales of  item classes
#Surprise: the sales of the class of the item (excluding its own sales) has a high correlation to its sales
sales_df_item %>% 
  left_join(class_key_df, by =c("Item No_"="No_")) %>%
  inner_join(top_items) %>%
  group_by(Period,Class) %>% 
  summarise(Sum_Quantity = sum(Sum_Quantity)) %>% 
  ungroup() %>% 
  drop_na() %>%
  spread(Class,Sum_Quantity) %>%
  left_join(item_df %>% select(Period,Sum_Quantity) , by = c("Period")) %>% 
  replace(is.na(.), 0) %>%
  select (-Period) %>% 
  ggpairs()

#Let´s take a look at autocorrelation in class sales- this could make a good predictor
#
ggAcf(class_sales_df$Class_Sales)
ggPacf(class_sales_df$Class_Sales)

item_ts [,c("Sum_Quantity","Lag_Class_Sales")] %>% cor()

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




#NEXT STEP CHECKING RELATION BETWEEN SALES TO RETAIL CHAINS... 
item_df %>% 
  select(Sum_Quantity, colnames(item_df) [str_detect (colnames(item_df), "Unit_Price")]) %>% 
  ggpairs


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
         Unit_Price, Easter, Num_Holidays,Class_Sales,
         Winter, Spring, Summer) %>% 
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
rankcv_index <- length(combo_df) + length(model_results_df) +1

#binding previous dataframe with boolean dataframe, and sorting by best score
full_model_results_df <- cbind (combo_df, model_results_df) %>% 
  mutate(RankCV = rank(CV), RankAIC = rank(AIC), RankAICc = rank(AICc), RankBIC = rank(BIC)) %>% 
  mutate(Mean_Rank = rowMeans(.[, rankcv_index:(rankcv_index+3)])) %>% 
  arrange(Mean_Rank) 
  

# Class Sales and Num_Days_Promo are certainly 2 most important predictors!!!!
# Easter, Unit Price and Quarter_Low_Price must improve as features or be forgotten

#storing row of winner as vector
winner <- full_model_results_df[7,1:length(variable_names)] %>%
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
            Num_Days_Promo, Quarter_Low_Price, Unit_Price, 
            Easter, Lag_Class_Sales, Winter, Spring, Summer)) %>% 
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
  select(Num_Days_Price_Promo, Num_Days_Promo, #Quarter_Low_Price,
         Num_Holidays, Num_Business_Days, Num_School_Holidays, Easter, #Unit_Price,
         Lag_Class_Sales, Lag_Item_Sales, Lag_Price_Promo,
         Winter, Spring, Summer, Autumn) %>% 
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
  arrange(AICc) 

most_accurate(full_model_results_df, 20, "Sum_Quantity ~ ")

winner <- full_model_results_df[4,1:length(variable_names)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[winner]) ,collapse=" + ")))

fit.trial8 <- tslm(winner_script, data=item_ts_train)

checkresiduals(fit.trial8)

new_data <- item_df_test#item_df %>% filter(Period>'2019-01-01' & Period <'2019-04-01')
fc.trial8 <- forecast (fit.trial8, newdata = new_data)

autoplot(fc.trial8) +
  autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.trial8,x=item_ts_test[,1])


winners_trial8 <- variable_names[winner]




# TRAINING MODEL WITH "SPREAD" PROMO REGRESSORS
#create variable name vector
variable_names <- item_df %>%
  select(c(columns_match(item_df,"Promo_"))) %>% 
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
  arrange(CV)

most_accurate(full_model_results_df3,10, "Sum_Quantity ~ ")

#saving the winner model
winner <- full_model_results_df3[8,1:length(variable_names)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[winner]) ,collapse=" + ")))

fit.trial9 <- tslm(winner_script, data=item_ts_train)

checkresiduals(fit.trial9)

new_data <- item_df_test#item_df %>% filter(Period>'2019-01-01' & Period <'2019-04-01')
fc.trial9 <- forecast (fit.trial9, newdata = new_data, h=4)

autoplot(fc.trial9) +
  autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.trial9, x=item_ts_test[,1])

#Adding Lagged Class Sales improves the forecast accuracy (MASE) by 2 points

#Let´s try to see if there is a better model than the one that is ranked first

most_accurate(full_model_results_df3,50,"Sum_Quantity ~ ")

winners_trial9 <- variable_names[winner]

#It looks like number of holidays improves accuracy slightly
#If the final purpose were to get monthly predictions, a good idea would be to add information with num of working days

#It would be useful to find a possible explanation to why only certain retail chains are relevant to model
#A suspicion is that there are 2 factors:
# --total sales during promotional periods
# --retail chains like Billa & Kaufland might be stretching Promo Price Periods to get overall lower purchase costs 


# TRAINING MODEL WITH "SPREAD" PROMO & PRICE PROMO AND BASIC REGRESSORS
#create variable name vector
variable_names <- c(winners_trial8, winners_trial9)

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)

#passing the boolean dataframe to the variable name vector to create a dataframe with the code for all possible combinations
model_list <- apply(combo_df, 1, function(x) as.formula(
  paste("Sum_Quantity ~ ",
        paste(c( variable_names[x]) ,collapse=" + "))))

#using the code stored in previous dataframe to pass it to a CV(tslm()) function and storing result in df
model_results_df4 <- lapply(model_list,
                            function(x) CV(tslm(x, data=item_ts_train))) %>%
  as.data.frame() %>%
  t() %>% 
  as_tibble()

#saving index for mutating in next lines
rankcv_index <- length(combo_df) + length(model_results_df4) +1

#binding previous dataframe with boolean dataframe, and sorting by best score
full_model_results_df4 <- cbind (combo_df, model_results_df4) %>% 
  mutate(RankCV = rank(CV), RankAIC = rank(AIC), RankAICc = rank(AICc), RankBIC = rank(BIC)) %>% 
  mutate(Mean_Rank = rowMeans(.[, rankcv_index:(rankcv_index+3)])) %>%
  #mutate(Mean_Rank = rowMeans(.[,23:26])) %>% 
  arrange(CV,AICc)

most_accurate(full_model_results_df4,15,"Sum_Quantity ~ " )

#saving the winner model
winner <- full_model_results_df4[7,1:length(variable_names)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[winner]) ,collapse=" + ")))

fit.trial11 <- tslm(winner_script, data=item_ts_train)

checkresiduals(fit.trial11)

new_data <- item_df_test#item_df %>% filter(Period>'2019-01-01' & Period <'2019-04-01')
fc.trial11 <- forecast (fit.trial11, newdata = new_data, h=4)

autoplot(fc.trial11) +
  autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.trial11, x=item_ts_test[,1])

winners_trial11 <- variable_names[winner]
#Basic Regressors don´t improve the model at all. However, there is a clue: spread unit price could be really interesting
#A problem to overcome is what to do with NAs in unit price that are explained by still unlisted customers




#TRYING A MODEL WITH ONLY SPREAD PRICES
variable_names <- columns_match(item_df,"Unit_Price_")

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)

#passing the boolean dataframe to the variable name vector to create a dataframe with the code for all possible combinations
model_list <- apply(combo_df, 1, function(x) as.formula(
  paste("Sum_Quantity ~ ",
        paste(c( variable_names[x]) ,collapse=" + "))))

#using the code stored in previous dataframe to pass it to a CV(tslm()) function and storing result in df
model_results_df5 <- lapply(model_list,
                            function(x) CV(tslm(x, data=item_ts_train))) %>%
  as.data.frame() %>%
  t() %>% 
  as_tibble()

#saving index for mutating in next lines
rankcv_index <- length(combo_df) + length(model_results_df5) +1

#binding previous dataframe with boolean dataframe, and sorting by best score
full_model_results_df5 <- cbind (combo_df, model_results_df5) %>% 
  mutate(RankCV = rank(CV), RankAIC = rank(AIC), RankAICc = rank(AICc), RankBIC = rank(BIC)) %>% 
  mutate(Mean_Rank = rowMeans(.[, rankcv_index:(rankcv_index+3)])) %>%
  arrange(CV)

most_accurate(full_model_results_df5, 10, "Sum_Quantity ~ ")

#saving the winner model
winner <- full_model_results_df5[7,1:length(variable_names)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[winner]) ,collapse=" + ")))

fit.trial12 <- tslm(winner_script, data=item_ts_train)

checkresiduals(fit.trial12)

new_data <- item_df_test#item_df %>% filter(Period>'2019-01-01' & Period <'2019-04-01')
fc.trial12 <- forecast (fit.trial12, newdata = new_data, h=4)

autoplot(fc.trial12) +
  autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.trial12, x=item_ts_test[,1])


winners_trial12 <- variable_names[winner]
#SURPRISINGLY THE ACCURACY OF THE MODEL BASED EXCLUSIVELY ON PRICE PERFORMS REALLY WELL!!!!


#TRYING A MODEL WITH WINNER SPREAD PRICES AND SPREAD PROMO & PROMO PRICE
variable_names <- c(winners_trial11, winners_trial12)

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)

#passing the boolean dataframe to the variable name vector to create a dataframe with the code for all possible combinations
model_list <- apply(combo_df, 1, function(x) as.formula(
  paste("Sum_Quantity ~ ",
        paste(c( variable_names[x]) ,collapse=" + "))))

#using the code stored in previous dataframe to pass it to a CV(tslm()) function and storing result in df
model_results_df6 <- lapply(model_list,
                            function(x) CV(tslm(x, data=item_ts_train))) %>%
  as.data.frame() %>%
  t() %>% 
  as_tibble()

#saving index for mutating in next lines
rankcv_index <- length(combo_df) + length(model_results_df6) +1

#binding previous dataframe with boolean dataframe, and sorting by best score
full_model_results_df6 <- cbind (combo_df, model_results_df6) %>% 
  mutate(RankCV = rank(CV), RankAIC = rank(AIC), RankAICc = rank(AICc), RankBIC = rank(BIC)) %>% 
  mutate(Mean_Rank = rowMeans(.[, rankcv_index:(rankcv_index+3)])) %>%
  #mutate(Mean_Rank = rowMeans(.[,23:26])) %>% 
  arrange(CV)

most_accurate(full_model_results_df6, 30, "Sum_Quantity ~ ")

#saving the winner model
winner <- full_model_results_df6[11,1:length(variable_names)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[winner]) ,collapse=" + ")))

fit.trial13 <- tslm(winner_script, data=item_ts_train)

checkresiduals(fit.trial13)

new_data <- item_df_test#item_df %>% filter(Period>'2019-01-01' & Period <'2019-04-01')
fc.trial13 <- forecast (fit.trial13, newdata = new_data, h=4)

autoplot(fc.trial13) +
  autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.trial13, x=item_ts_test[,1])

winners_trial13 <- variable_names[winner]



autoplot(item_ts_train[,'Sum_Quantity'], series="Data") +
  autolayer(fitted(fit.trial8), series="Fitted") +
  xlab("Period") + ylab("") +
  guides(colour=guide_legend(title=" "))



#BECAUSE IT LOOKS LIKE SPREAD PROMO DAYS DOES NOT HELP TOO MUCH, LET´S FIND A MODEL EXCLUDING ONLY THOSE
variable_names <- item_df %>%
  select(-c(columns_match(item_df,"Days_") ),
         -c("Quarter_Low_Price", "Unit_Price", "Period",
            "Sum_Quantity", "Class_Sales", "Unit_Price_CB00"),
         -c(columns_match(item_df, "MT00" ) ),
         -c(columns_match(item_df, "CP00" ) ) ) %>% 
  colnames()

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)

#passing the boolean dataframe to the variable name vector to create a dataframe with the code for all possible combinations
model_list <- apply(combo_df, 1, function(x) as.formula(
  paste("Sum_Quantity ~ ",
        paste(c( variable_names[x]) ,collapse=" + "))))

#using the code stored in previous dataframe to pass it to a CV(tslm()) function and storing result in df
model_results_df7 <- lapply(model_list,
                            function(x) CV(tslm(x, data=item_ts_train))) %>%
  as.data.frame() %>%
  t() %>% 
  as_tibble()

model_results_df8 <- lapply(model_list,
                            function(x) CV(tslm(x, data=item_ts))) %>%
  as.data.frame() %>%
  t() %>% 
  as_tibble()

#saving index for mutating in next lines
rankcv_index <- length(combo_df) + length(model_results_df7) +1

#saving index for mutating in next lines
rankcv_index <- length(combo_df) + length(model_results_df8) +1

#binding previous dataframe with boolean dataframe, and sorting by best score
full_model_results_df7 <- cbind (combo_df, model_results_df7) %>% 
  mutate(RankCV = rank(CV), RankAIC = rank(AIC), RankAICc = rank(AICc), RankBIC = rank(BIC)) %>% 
  mutate(Mean_Rank = rowMeans(.[, rankcv_index:(rankcv_index+3)])) %>%
  arrange(AICc)

full_model_results_df8 <- cbind (combo_df, model_results_df8) %>% 
  mutate(RankCV = rank(CV), RankAIC = rank(AIC), RankAICc = rank(AICc)) %>% 
  mutate(Mean_Rank = rowMeans(.[, rankcv_index:(rankcv_index+2)])) %>%
  arrange(AICc)

most_accurate(full_model_results_df7, 50, "Sum_Quantity ~ ")
most_accurate(full_model_results_df8, 50, "Sum_Quantity ~ ")

#saving the winner model
winner <- full_model_results_df7[10,1:length(variable_names)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[winner]) ,collapse=" + ")))

fit.trial14 <- tslm(winner_script, data=item_ts_train)

checkresiduals(fit.trial14)

new_data <- item_df_test#item_df %>% filter(Period>'2019-01-01' & Period <'2019-04-01')
fc.trial14 <- forecast (fit.trial14, newdata = new_data, h=4)

autoplot(fc.trial14) +
  autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.trial14, x=item_ts_test[,1])

winners_trial14 <- variable_names[winner]



autoplot(item_ts_train[,'Sum_Quantity'], series="Data") +
  autolayer(fitted(fit.trial14), series="Fitted") +
  xlab("Period") + ylab("") +
  guides(colour=guide_legend(title=" "))




#NON LINEAR REGRESSION --------------

#Up to noW, data has not been transformed.
#Lets try a linear regression in log-log , log-linear and linear-log with previous winner and see what happens

#Log-log: Because there are zeros in predictors, we must use a log+1 transformation


fit.trial15 <- tslm(winner_script, data= item_ts_train %>% log1p() )

checkresiduals(fit.trial15)

new_data <- log1p(item_ts_test) %>% as.data.frame()
fc.trial15 <- forecast (fit.trial15, newdata = new_data)


#backtransforming values
fc.trial15$mean <- fc.trial15$mean %>% expm1() 
fc.trial15$lower <- fc.trial15$lower %>% expm1()
fc.trial15$upper <- fc.trial15$upper %>% expm1()
fc.trial15$x <- fc.trial15$x %>% expm1()
fc.trial15$fitted <- fc.trial15$fitted %>% expm1()
fc.trial15$model$model <- fc.trial15$model$model %>% expm1()

accuracy(fc.trial15, item_ts_test[,1])

autoplot(fc.trial15) +
  autolayer(item_ts_test[,1] , series="Actual")

#TREATING OUTLIERS--------------------------------------------------------------------------------------

#Using a defined function to take outliers from more than 15 months ago and moderate them
#It looks like it works!! For the moment in this ts, it looks like the best results are given by range 97,3 
item_ts_train_outlier <- moderate_outliers(item_df,percentile_lo = 3, percentile_hi=97) %>% 
  filter(year(Period)<2019) %>% 
  select(-Period) %>% 
  ts(start=c(2015,1), frequency = 12)

#plotting the changes
item_ts_train_outlier[,1] %>% autoplot()+
  autolayer(item_ts[,1],series="Actual")

#trying with previous result, it improves both train and test accuracy
tslm(winner_script, data= item_ts_train_outlier) %>% forecast(new_data) %>%# autoplot() + autolayer(item_ts_test[,1],series="Actual")
  accuracy(x=item_ts_test[,1])


#LOOKING FOR A MORE ROBUST APPROACH THAN PLAIN TRAIN TEST -----------------------------------------------------------------


#Finding out if previous winner models are good models according to tsCV (using implemented MASE accuracy measure)
#We´ll check multiple initial points so we find out how that parameter changes results in start tsCV

#First defining a function to be passed to tsCV
my_tslm <- function(y, h, xreg)
{
  if(nrow(xreg) < length(y) + h) {
    stop("Not enough xreg data for forecasting") }
  X <- xreg[seq_along(y),] %>% ts(start=c(2015,1), frequency = 12)
  fit <- tslm(y ~ X)
  X <- xreg[length(y)+seq(h), , drop=FALSE]
  colnames(X) <- colnames(xreg)
  forecast(fit, newdata=X)
}


#Now preparing for a loop that will go through all possible initial points for all winner models
h <- 4  
winners_list <- list(winners_trial8, winners_trial12, winners_trial13, winners_trial14)
result_list <- list()

#Creating as many empty vectors as there are in winner_list for storing results
for (i in length(winners_list)){
  result_list[[i]]<- vector()
}

for (i in (1:51)) { #loop for number of possible tscvs
  for (j in (1:length(winners_list))) { #loop for number of winnner models checked
    pred <- item_ts_outlier[, colnames(item_ts_outlier) %in% winners_list[[j]]] # TSCV requires passing separately values of predictors 
    naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=i) #storing naive component of MASE
    mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial = i) %>% 
      get_mase_cv(naive = naive_error, h=h)
    result_list[[j]] <- append(result_list[[j]],mase) 
  } 
}

#Plotting results
#One interesting thing that is seen here is the more "robustness" of models
#Also, It looks best to trust more models that perform best with recent values, rather than with the whole series
do.call(cbind, result_list) %>%
  as.data.frame() %>%
  slice(43:47) %>% 
  ts() %>%
  autoplot()


# NEXT STEPS- TRY FINDING BEST MODELS USING TSCV AND SEE DIFFERENCE WITH PREVIOUS RESULTS
# procedure: from top 500 models in full model results,
# get a ranking of accuracy and compare to the original ranking in full model results


#Now let´s see if there is much difference between what AICc points to and tsCV with ALL data
#Preparing a loop that goes through top N models chosen by AICc, and trying tsCV for them
#Then comparing scores of both approaches
h<-4
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=(51-5))
result_vector <- c()
for (i in 1:500) {
  model_index <- which(full_model_results_df7[i,1:(ncol(full_model_results_df7)-8)] == TRUE)
  colnames(full_model_results_df7)[model_index]
  pred <- item_ts[,colnames(full_model_results_df7)[model_index]]
  cv_mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial = 1) %>% 
    get_mase_cv(naive = naive_error, h=h)
  result_vector <- append(result_vector,cv_mase)
}
as.data.frame(result_vector) %>% 
  mutate(Index = row.names(.)) %>% 
  mutate(RankMASE = rank(result_vector)) %>%
  cbind (full_model_results_df7[1:100,]) %>% View()

most_accurate(full_model_results_df7 %>% arrange(RankAICc), 10, "Sum_Quantity ~ ")

full_model_results_df7[c(41,456,1446,2151),]

#CONCLUSIONS: 
#tsCV is pretty slow to compute, so it is better to find a different strategy.
#On the other hand, for monthly data there aren´t many observations, so it looks like we can´t afford training leaving data out (classic train-test approach)
#So perhaps training with ALL data, and then using tsCV to find a model that doesn´t overfit would be interesting (not too orthodox) 
#looks like AICc is better than using MASE TSCV (pretty unreliable) or CV statistic

#Strategy:
#1. Use AICc for selecting top 200 combinations + Heatmap with ALL data
#2. Use tsCV MASE to find the combination that overfits less


item_df %>% 
  filter(Period == '2016-01-01') %>% View()
  group_by(Period) %>% 
  tally() %>% 
  filter(n >1)
  

month_school_holidays_df %>% 
  group_by(Period) %>% 
  tally() %>% 
  filter(n >1)
  



full_model_results_df7[1:(ncol(full_model_results_df7)-10)] %>%
  mutate_all(as.numeric) %>% 
  slice(1:nrow(.)*0.1) %>%
  colSums()





#------------------------------------------------------------------------------------------------

