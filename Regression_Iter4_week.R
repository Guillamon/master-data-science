#install.packages("DataCombine")
#install.packages("TSA")
#install.packages("seasonal")
#install.packages("cowplot")
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
library (data.table)


# INITIALIZING VARIABLES, DECLARING FUNCTIONS, AND READING FILES --------------------------------------------------

period <- "week"


#function that creates a dataframe with all periods necessary for tseries, using first and last observations
create_period_df <- function(dataframe,period_column= Period, period="month") {
  first_observation <- dataframe$Period %>% min() %>% floor_date(period) %>% as_date()
  last_observation <-  dataframe$Period %>% max() %>% floor_date(period) %>% as_date()
  
  c1 <- first_observation
  period_df <- data.frame(Period = c1)
  
  n_periods <- time_length (interval (first_observation, last_observation), unit= period)
  for (n in 2: (n_periods+1)) {
    period_df [n,"Period"] <- (first_observation + weeks(n-1))
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
most_accurate <- function(models_dframe, train_ts=item_ts_train, test_ts=item_ts_test, n_trials, target_string)
  
{  winner_index <- 0

new_data <- test_ts %>% as.data.frame()
candidate_accuracy <- 1
for (i in (1:n_trials)) {
  

  
  candidate <- models_dframe[i,1:(length(variable_names))] %>%
    t() %>%
    as.vector
  
  candidate_script <- as.formula(paste(target_string, paste(c( variable_names[candidate]) ,collapse=" + ")))
  
  fit <- tslm(candidate_script, data=train_ts)
  
  fc <- forecast (fit, newdata = new_data)
  
  if (accuracy(fc,x=test_ts[,1])[2,6] < candidate_accuracy) {
    candidate_accuracy <- accuracy(fc,x=test_ts[,1])[2,6]
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

#function that creates a lagged column with the specified lags
create_lag_weeks <- function(dframe,lag_vector,suffix = "_Item_Sales", period_col=Period, value_col="Sum_Quantity") {
  df_lags <- dframe %>% 
    mutate(Period= Period + weeks(lag_vector[1])) %>% 
    rename(!!paste0("Lag_", lag_vector[1], suffix) := value_col)
  
  if (length(lag_vector)>1) {
    for (i in (2:(length(lag_vector)-1) )) {
      df_lags <- dframe %>%
        mutate(Period= Period + weeks(lag_vector[i])) %>% 
        rename(!!paste0("Lag_", lag_vector[i], suffix) := value_col) %>% 
        left_join(df_lags)
    } 
  }
  return(df_lags)
}

# function that takes a ts object with target and predictor columns, 
# and runs a tscv while removing one predictor at a time, to see if accuracy improves by doing it
# it removes one predictor per iteration while accuracy of any model improves moving benchmark
remove_predictors <- function (ts_object, target_col_index=1, benchmark=1, n_iters=40, h=4, init=initial, naive=naive_error)  {
  
  target <- ts_object [,target_col_index]
  ts_object <- ts_object [,-target_col_index]
  indexes_vec <- seq(1,ncol(ts_object))
  indexes_removed <- c()
  removed <- c()
  
  for (i in (1:n_iters)) {
    print(i)
    result_vector <- c()
    j_vector <- c()
    
    for (j in indexes_vec) {
      
      pred <- ts_object[,-c(indexes_removed,j)]
      mase <- tsCV(target, my_tslm, xreg=pred, h=h, initial = initial ) %>%
        get_mase_cv(naive = naive_error, h=h)
      result_vector <- append(result_vector, mase)
      j_vector <- append(j_vector, j)
      
    }
    #print (result_vector)
    if (any(result_vector <= benchmark)) {
      benchmark <- min(result_vector)
      print(paste0("benchmark: ", benchmark))
      
      
      min_result <- min(result_vector) 
      index_removable <- j_vector[which(result_vector == min_result)][1]
      
      
      #print(indexes_vec[which(result_vector == min(result_vector))])
      #index_includable <- indexes_vec[which(result_vector == min(result_vector))] %>% base::sample(size = 1)
      removable <- colnames(ts_object)[index_removable]
      print (paste0("adding: ",removable))
      removed <- append(removed, removable)
      #winners_vector <- append(winners_vector, includable)
      indexes_removed <- append(indexes_removed, index_removable)
      indexes_vec <- indexes_vec[-which(indexes_vec == index_removable)]
      print(paste0("Removed: ", removed))
      
      
    } else {
      return (ts_object[,-c(indexes_removed)] %>% colnames())
      break}
  }
  return(ts_object[,-c(indexes_removed)] %>% colnames())
}


# function that takes a ts object with target and predictor columns, 
# and runs a tscv while adding one predictor at a time, to see if accuracy improves by doing it
# it adds one predictor per iteration while accuracy of any model improves moving benchmark
add_predictors <- function (ts_object, winners_vector, target_col_index=1, benchmark=1, n_iters=40, h=4, init=initial, naive=naive_error)  {
  
  target <- ts_object [,target_col_index]
  ts_object <- ts_object [,-target_col_index]
  indexes_added <- which (colnames(ts_object) %in% winners_vector)
  indexes_vec <- seq (1,ncol(ts_object))
  indexes_vec <- indexes_vec [-indexes_added]
  added <- c()
  
  for (i in (1:n_iters)) {
    print(i)
    result_vector <- c()
    j_vector <- c()
    
    for (j in indexes_vec) {
      
      pred <- ts_object[,c(indexes_added,j)]
      mase <- tsCV(target, my_tslm, xreg=pred, h=h, initial = initial ) %>%
        get_mase_cv(naive = naive_error, h=h)
      result_vector <- append(result_vector, mase)
      j_vector <- append(j_vector, j)
      
    }
    #print (result_vector)
    if (any(result_vector <= benchmark)) {
      benchmark <- min(result_vector)
      print(paste0("benchmark: ", benchmark))
      set.seed(2)
      
      
      min_result <- min(result_vector) 
      index_includable <- j_vector[which(result_vector == min_result)][1]
      
      
      #print(indexes_vec[which(result_vector == min(result_vector))])
      #index_includable <- indexes_vec[which(result_vector == min(result_vector))] %>% base::sample(size = 1)
      includable <- colnames(ts_object)[index_includable]
      print (paste0("adding: ",includable))
      added <- append(added, includable)
      winners_vector <- append(winners_vector, includable)
      indexes_added <- append(indexes_added, index_includable)
      indexes_vec <- indexes_vec[-which(indexes_vec == index_includable)]
      print(paste0("Added: ",added))
      
      
    } else {
      return(winners_vector)
      break}
  }
  return(winners_vector)
}

#PREPARING DATAFRAME WITH FEATURES THAT WILL BE USED FOR TRYING MODELS--------------------------------------------------

item <- top_items$`Item No_`[2]

#filtering sales dataframe for given item
filtered_sales <- sales_df_item %>%
  filter (Period >= '2014-10-01' ) %>% 
  filter(`Item No_` == item) %>% 
  select(-`Item No_`)

#filtering sales price dataframe for given item, and grouping by date to get sums of promo days, and a mean of price
filtered_sales_price <- full_joined_sales_price %>% 
  filter(No_ == item) %>% # View()
  group_by(Period) %>% 
  #summarise(Num_Days_Price_Promo = sum(Num_Days_Price_Promo))%>% View()
  summarise(Num_Days_Price_Promo = sum(Num_Days_Price_Promo), 
              Num_Days_Promo = sum(Num_Days_Promo),
              Quarter_Low_Price = sum(Quarter_Low_Price),
              Unit_Price =  mean(Unit_Price)) %>%
  ungroup()


#creating a dataframe with all periods in timespan for given item
period_df <- create_period_df(filtered_sales, period = period)


#Creating a dataframe with the months of the seasons
Summer <- c(7,8,9)
Autumn <- c(10,11,12)
Winter <- c(1,2,3)
Spring <- c(4,5,6)



seasons_df <- data.frame(Summer,Autumn,Winter,Spring) %>% gather(key="Season", value="Month")
seasons_df$Season <- factor(seasons_df$Season, levels= c("Winter","Spring","Summer","Autumn"))



#Creating a dataframe with Bank Holidays
week_bank_holidays_df <- bank_holidays_df %>% 
  mutate(Period = floor_date(start, period) %>%  as_date()) %>% #Creating period
  group_by(Period) %>%
  summarise(Easter = sum(Easter), Num_Holidays= n()) %>% 
  ungroup() %>% 
  mutate(Easter = ifelse (Easter>0,1,0)) %>% 
  right_join(period_df) %>% 
  replace(is.na(.),0) %>%
  #Now adding features: weeks after/before holiday
  #This part of the code was taken from Sven Hohenstein´s reply in this stackoverflow´s question:
  #https://stackoverflow.com/questions/26553638/calculate-elapsed-time-since-last-event
  mutate(Week = as.numeric(row.names(.))) %>% 
  mutate(tmpG = cumsum(c(FALSE, as.logical(diff(Num_Holidays))))) %>%
  mutate(tmp_a = c(0, diff(Week)) * !Num_Holidays,
         tmp_b = c(diff(Week), 0) * !Num_Holidays) %>%
  group_by(tmpG) %>%
  mutate(Weeks_After_Hol = cumsum(tmp_a),
         Weeks_Before_Hol = rev(cumsum(rev(tmp_b)))) %>%
  ungroup() %>%
  select(-c(tmp_a, tmp_b, tmpG, Week))
  


#Creating a dataframe with school_holidays
week_school_holidays_df <- school_cal_df %>% 
  expand_time_int(start_col="Start", end_col="End", line_col="Line", n_days_col="Num_Days", period=period) %>% 
  mutate(Period = floor_date(Start, period) %>%  as_date()) %>%
  mutate(Period = Period + weeks(Line-1)) %>% 
  arrange(Period) %>% 
  #select(Period,Num_Days) %>%
  group_by(Period) %>% 
  summarise(Num_Days = sum(Num_Days)) %>% 
  ungroup() %>% 
  rename(Num_School_Holidays = Num_Days) %>%
  right_join(period_df) %>% 
  replace(is.na(.),0) %>%
  #Now adding features: weeks after/before holiday
  #This part of the code was taken from Sven Hohenstein´s reply in this stackoverflow´s question:
  #https://stackoverflow.com/questions/26553638/calculate-elapsed-time-since-last-event
  mutate(Week = as.numeric(row.names(.))) %>% 
  mutate(tmpG = cumsum(c(FALSE, as.logical(diff(Num_School_Holidays))))) %>%
  mutate(tmp_a = c(0, diff(Week)) * !Num_School_Holidays,
         tmp_b = c(diff(Week), 0) * !Num_School_Holidays) %>%
  group_by(tmpG) %>%
  mutate(Weeks_After_Sch_Hol = cumsum(tmp_a),
         Weeks_Before_Sch_Hol = rev(cumsum(rev(tmp_b)))) %>%
  ungroup() %>%
  select(-c(tmp_a, tmp_b, tmpG, Week))


#creating a dframe with sales of the class
#excludes sales of the forecast item
class_sales_df <- sales_df_item %>%
  filter (Period >= '2014-10-01') %>% #doing this for later using lagged values
  left_join(class_key_df, by =c("Item No_"="No_")) %>%
  inner_join(top_items) %>%
  drop_na() %>%
  filter(`Item No_` != item) %>% #getting rid of item sales
  group_by(Period, Class) %>% 
  summarise(Class_Sales = sum(Sum_Quantity)) %>%
  filter(Class == class_key_df[ match(item,class_key_df$No_) ,"Class"]) %>% 
  select(-Class) %>% 
  ungroup()

  
#creating a df for lagged item sales
lagged_item_sales <- filtered_sales %>%
  create_period_df(period = period) %>%
  left_join(filtered_sales) %>% 
  replace(is.na(.),0) %>%
  select(Period,Sum_Quantity) %>% 
  create_lag_weeks(c(4,6,8,9,17,18,24),value_col = "Sum_Quantity")


#creating a df for lagged price promo days (1&8 months lag)
lagged_price_promo_df <- filtered_sales_price %>%
  select(Period, Num_Days_Price_Promo) %>%
  create_period_df(period = period) %>%
  left_join(filtered_sales_price) %>% 
  select(Period, Num_Days_Price_Promo) %>%
  create_lag_weeks(c(1,2,3,4,8),suffix = "_Price_Promo", value_col = "Num_Days_Price_Promo")

#creating a df for lagged promo days (1&8 months lag)
lagged_promo_df <- filtered_sales_price %>%
  select(Period, Num_Days_Promo) %>%
  create_period_df(period = period) %>%
  left_join(filtered_sales_price) %>% 
  select(Period, Num_Days_Promo) %>%
  create_lag_weeks(c(1,2,3,4,8),suffix = "_Promo", value_col = "Num_Days_Promo")

lagged_bank_hol_df <- week_bank_holidays_df %>% 
  select(Period, Num_Holidays) %>% 
  create_lag_weeks(c(1,2,3,4,6,20,21),suffix = "_Holidays", value_col = "Num_Holidays")

lagged_school_hol_df <- week_school_holidays_df %>% 
  select(Period, Num_School_Holidays) %>% 
  create_lag_weeks(c(1,2,3,4),suffix = "_School_Holidays", value_col = "Num_School_Holidays")

#spreading promo days by customer
promo_spread <- full_joined_sales_price %>% 
  filter(No_ == item) %>%
  group_by(Period, Customer_Code) %>%
  summarise(Num_Days_Promo = sum(Num_Days_Promo))%>% #Doing this because, unlike the case of month, it is possible that two promotions coincide on the same week at the same customer
  ungroup() %>% 
  #select(Period, Customer_Code, Num_Days_Promo) %>% View()
  spread(Customer_Code,Num_Days_Promo) %>% 
  rename_at(vars(-Period), funs(paste0("Days_Promo_", .))) #%>%
  #replace(is.na(.), 0)

#spreading price promo days by customer
price_promo_spread <- full_joined_sales_price %>% 
  filter(No_ == item) %>%
  group_by(Period, Customer_Code) %>%
  summarise(Num_Days_Price_Promo = sum(Num_Days_Price_Promo))%>%
  #select(Period, Customer_Code, Num_Days_Price_Promo) %>%
  spread(Customer_Code,Num_Days_Price_Promo) %>%
  rename_at(vars(-Period), funs(paste0("Days_Price_Promo_", .))) #%>%
  #replace(is.na(.), 0)

#spreading unit price by customer
unit_price_spread <- full_joined_sales_price %>% 
  filter(No_ == item) %>%
  filter(year(Period) >= 2015) %>%
  group_by(Period, Customer_Code) %>%
  summarise(Unit_Price = min(Unit_Price))%>% #keeping only the minimum price if 2 promos coincide on the same week at the same customer
  #select(Period, Customer_Code, Unit_Price) %>% 
  spread(Customer_Code, Unit_Price) %>% 
  rename_at(vars(-Period), funs(paste0("Unit_Price_", .))) %>%
  replace(is.na(.), 10)


#creating a dataframe with all features
item_df <- period_df %>% 
  mutate(Month=month(Period)) %>% 
  left_join(filtered_sales, by = c("Period")) %>% 
  left_join(filtered_sales_price, by = c("Period")) %>%
  left_join(week_bank_holidays_df, by = c("Period")) %>%
  left_join(week_school_holidays_df, by = c("Period")) %>%
  left_join(promo_spread, by = c("Period")) %>%
  left_join(price_promo_spread, by = c("Period")) %>%
  left_join(unit_price_spread, by = c("Period")) %>%
  left_join(lagged_class_df, by = c("Period")) %>%
  left_join(lagged_item_sales, by = c("Period")) %>%
  left_join(lagged_price_promo_df, by = c("Period")) %>%
  left_join(lagged_promo_df, by = c("Period")) %>%
  left_join(lagged_bank_hol_df, by = c("Period")) %>% 
  left_join(lagged_school_hol_df, by = c("Period")) %>% 
  left_join(seasons_df) %>%
  replace(is.na(.), 0) %>% #fill nas with zeros
  mutate(Winter = as.numeric(Season=="Winter")) %>% 
  mutate(Summer = as.numeric(Season=="Summer")) %>%
  mutate(Spring = as.numeric(Season=="Spring")) %>%
  mutate(Autumn = as.numeric(Season=="Autumn")) %>% 
  mutate(Num_Business_Days = 7-Num_Holidays) %>% 
  select(-c("Month","Season"))
  

#saving a vector with names of columns that have all zeros
all_zero <- names( item_df [apply(item_df, MARGIN = 2, FUN = function(x) sum(x==0)/nrow(item_df) ==1)] )

#getting rid of columns that have all zeros
item_df <- item_df %>%
  select(-all_zero)

ts(df$Sum_Quantity, 
   freq=365.25/7, 
   start=decimal_date(ymd("2015-01-04")))

#creating a timeseries object with data
item_ts <- item_df %>% 
  filter( Period >='2016-01-01') %>%
  filter(Period < '2019-04-01') %>%
  select(-Period) %>%
  ts()
  #ts(start = c(2015,1), frequency = 12)

#creating a train data set that is a timeseries object
item_ts_train <- item_df %>% 
  filter( Period >='2016-01-01') %>% 
  filter( Period <'2019-01-01') %>% 
  select(-Period) %>%
  ts()
  #ts(start = c(2015,1), frequency = 52)


#creating a timeseries test data set for plotting (autoplot requires it)
item_ts_test <- item_df %>%
  filter( Period >= '2019-01-01' & Period < '2019-02-01') %>% 
  #filter( Period < '2019-04-01') %>% 
  select(-Period) %>%
  ts(start=158)
  #ts(start = c(2019,1), frequency = 12)



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

#First find out if there is any extra week in dataset:
#2017 has 53 weeks
period_df %>% mutate(Row = row.names(.)) %>% 
  group_by(year(Period)) %>% 
  tally()

#If it were necessary to remove a week from the dataset... (not necessary in regression since i am not going to use weekly seasonality)
#an approach would be to find an apparently irrelevant week, distant from important dates, that has a big standard deviation
#And remove it the values in 2017 fall in the middle
item_df %>%
  filter(year(Period) >2015) %>% 
  select(Period,Sum_Quantity) %>%
  mutate(Week = week(Period)) %>%
  group_by(Week) %>% 
  summarise(STDev = sd(Sum_Quantity)) %>% 
  arrange(desc(STDev)) %>%
  ungroup() %>% 
  filter(STDev <= quantile(STDev, 0.1) | 
           STDev >= quantile(STDev, 0.8))


#EXPLORING BEHAVIOUR AND RELATIONSHIPS OF REGRESSAND AND POSSIBLE REGRESSORS-------------------------

#Histogram of Sales Quantity
item_df %>% 
  ggplot( aes(x= Sum_Quantity))+
  geom_histogram(bins=10, binwidth = 300)

#Checking if log(x+1) is any good
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
gglagplot(item_ts[,1])

#Lag 1&2 show high autocorrelation, Lag16 could be significant too
ggAcf(item_ts_target,lag.max = 60)
# PACF shows interesting spikes at lags 1,2, 14, 17
ggPacf(item_ts_target,lag.max = 60)
#Box.test(item_ts[,1], lag=52, type = "Ljung")


#LOOKING FOR SEASONALITY

#Difficult to see anything in this plot, except for a couple of peaks and troughs that are common for most years
#They are irregularly distributed, so there seems to be no weekly seasonality
item_df %>% 
  select(Period, Sum_Quantity) %>%
  mutate(Year=year(Period)) %>%
  mutate(Week = week(Period)) %>% 
  select(-Period) %>% 
  spread(Year,Sum_Quantity,sep='_') %>%
  replace(is.na(.), 0) %>% 
  ggplot() +
    geom_line(aes(x = Week, y = Year_2015, color= "#fae205")) +
    geom_line(aes(x = Week, y = Year_2016, color= "#ff8c18")) +
    geom_line(aes(x = Week, y = Year_2017, color= "#ff0759")) +
    geom_line(aes(x = Week, y = Year_2018, color= "#ca0092")) +
    geom_line(aes(x = Week, y = Year_2019, color= "#141fb5")) +
    scale_color_discrete(name = "Series", labels = c(2019, 2018, 2015, 2016, 2017)) + 
    xlab('WEEK') +
    ylab('SALES') +
    geom_vline(data = trial, aes(xintercept = Week), color="dark grey", size = 0.2)


#From the boxplot it looks like there are specific weeks with little variance, 
#they don´t exactly match with patterns seen before, but most are close
item_df %>%
  filter(year(Period) >2015) %>% 
  select(Period,Sum_Quantity) %>%
  mutate(Week = lubridate::week(Period)) %>%
  ggplot(aes(x=Week, y=Sum_Quantity, fill=Week, group=Week)) +
  geom_boxplot()

#Let´s take a look at weeks sorted by variance
item_df %>%
  filter(year(Period) >2015) %>% 
  select(Period,Sum_Quantity) %>%
  mutate(Week = lubridate::week(Period)) %>%
  group_by(Week) %>% 
  summarise(VAR = var(Sum_Quantity)) %>% 
  arrange(VAR)

  
  
#LETS LOOK AT THE INFLUENCE OF HOLIDAYS AND CHANGE OF SEASON IN THE TS()

#Dataframe with weeks in which there have been holidays at least three years 
holidays <- item_df %>% 
  mutate(Week = week(Period)) %>%
  #filter(Week == 18) %>%
  #arrange(year(Period)) 
  group_by(Week) %>% 
  summarise (Num_Holidays = sum(Num_Holidays)) %>% 
  filter (Num_Holidays > 2) 


#If there is a pattern here it is very difficult to see
item_df %>% 
  select(Period, Sum_Quantity) %>%
  mutate(Year=year(Period)) %>%
  mutate(Week = week(Period)) %>% 
  select(-Period) %>% 
  spread(Year,Sum_Quantity,sep='_') %>%
  replace(is.na(.), 0) %>% 
  ggplot() +
  geom_line(aes(x = Week, y = Year_2015, color= "#fae205")) +
  geom_line(aes(x = Week, y = Year_2016, color= "#ff8c18")) +
  geom_line(aes(x = Week, y = Year_2017, color= "#ff0759")) +
  geom_line(aes(x = Week, y = Year_2018, color= "#ca0092")) +
  geom_line(aes(x = Week, y = Year_2019, color= "#141fb5")) +
  scale_color_discrete(name = "Series", labels = c(2019, 2018, 2015, 2016, 2017)) + 
  xlab('WEEK') +
  ylab('SALES') +
  geom_vline(data = holidays, aes(xintercept = Week), color="dark grey", size = 0.2)


#Better to see holidays year by year
#And generally the weeks after and before holidays there are peaks, while weeks with holidays account for troughs
year_colors <- c("#fae205", "#ff8c18", "#ff0759", "#ca0092", "#141fb5")

for (y in 1:5) {
  holidays <- item_df %>% 
    mutate(Week = week(Period)) %>%
    filter(year(Period) == years[y]) %>% 
    filter (Num_Holidays > 0)
  #arrange(year(Period))
  
  plot <- item_df %>% 
    select(Period, Sum_Quantity) %>%
    mutate(Year=year(Period)) %>%
    mutate(Week = week(Period)) %>% 
    select(-Period) %>% 
    spread(Year,Sum_Quantity,sep='_') %>%
    replace(is.na(.), 0) %>% 
    ggplot() +
    geom_line(aes(x = Week, y = get( paste0("Year_", years[y] ))), color= year_colors[y]) +
    geom_vline(data = holidays, aes(xintercept = Week), color="dark grey", size = 0.2)+
    xlab('WEEK') +
    ylab('SALES')
    
  print(plot)
}


#It does look like there is influence 
for (y in 1:5) {
  holidays <- school_cal_df %>% 
  mutate(Start_Week = week(Start), End_Week = week(End)) %>% 
  filter(year(Start)== years[y] | year(End) == years[y]) %>% 
  mutate (Start_Week = ifelse(year(Start) < years[y], 0, Start_Week)) %>% 
  mutate (End_Week = ifelse(year(End) > years[y], 53, End_Week))# %>% 
  
  plot <- item_df %>% 
  select(Period, Sum_Quantity) %>%
  mutate(Year=year(Period)) %>%
  mutate(Week = week(Period)) %>% 
  select(-Period) %>% 
  spread(Year,Sum_Quantity,sep='_') %>%
  replace(is.na(.), 0) %>% 
  ggplot() +
  geom_line(aes(x = Week, y = get( paste0("Year_", years[y] ))), color= year_colors[y]) +
  geom_rect(holidays, mapping =aes(xmin = Start_Week, xmax = End_Week, ymin = -Inf, ymax = +Inf), fill = 'yellow', alpha = 0.2) +
  xlab('WEEK') +
  ylab('SALES')
print(plot)
}

  
item_df %>% 
  select(Period, Sum_Quantity) %>%
  mutate(Year=year(Period)) %>%
  mutate(Week = week(Period)) %>% 
  select(-Period) %>%
  filter(Year == 2018) %>% 
  #spread(Year,Sum_Quantity,sep='_') %>%
  #replace(is.na(.), 0) %>% 
  ggplot() +
  geom_line(aes(x = Week, y = Sum_Quantity, color= year_colors[y])) +
  geom_rect(holidays, mapping =aes(xmin = Start_Week, xmax = End_Week, ymin = -Inf, ymax = +Inf), fill = 'yellow', alpha = 0.2) +
  xlab('WEEK') +
  ylab('SALES')


#filtered_sales %>%
#  filter(year(Period) > 2015) %>% 
#  mutate(Month = month(Period, label =TRUE)) %>%
#  ggplot(aes(x=Sum_Quantity)) + 
#  geom_histogram(binwidth = 30) +
#  facet_wrap(~Month )



# Another approach to finding seasonality: Fourier transformations

# Periodogram returns a list of frequencies at which the observations were vectorized, and the power of these
pg_list <- item_df %>% select(Sum_Quantity) %>% periodogram()

# Creating a dataframe with the results from previous step
pg_df <- data.frame(Frequency=pg_list$freq, Power=pg_list$spec) %>%
  mutate(Days = 1/Frequency) %>% #transforming frequency to unit days
  arrange(desc(Power))

# Checking top 3 power frequencies
# Seems like top scorer is 54 days by far
pg_df %>% head(10)

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


for (y in 1:5) {
plot <- item_df %>%
  mutate(Week = week(Period)) %>%
  filter(year(Period) == years[y]) %>% 
  rename(Promo = Num_Days_Promo,
         Price_Promo = Num_Days_Price_Promo,
         Sales = Sum_Quantity) %>%
  select (Promo, Price_Promo, Sales, Week) %>% 
  gather("Key", "Value", 1:3 ) %>% 
  ggplot() +
    geom_line (aes(x=Week, y=Value), color=year_colors[y], size =1.5) +
    facet_grid(rows = vars(Key), scales = "free") 
  print (plot)
}
  #gather ("Type", "Num_Days", 3:4) %>% 




#let´s see how the number of promo days per week has evolved along years
item_df %>%
  mutate(Year = year(Period)) %>% 
  mutate(Week = week(Period)) %>%
  rename(Promo = Num_Days_Promo,
         Price_Promo = Num_Days_Price_Promo) %>% 
  gather ("Type", "Num_Days", 3:4) %>%
  filter(Type=="Promo") %>% 
  ggplot(aes(x=Week, y=Num_Days)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") +
  scale_x_discrete(limits=seq(1,12)) +
  facet_grid(Year ~ .)



item_df %>%
  select(Num_Days_Price_Promo) %>% 
  ggAcf()
item_df %>%
  select(Num_Days_Price_Promo) %>% 
  ggPacf()



#LOOKING AT CORRELATION OF LAGGED HOLIDAYS AND PROMOTIONS WITH TARGET VARIABLE
#Price Promo
#ca 0.36 at lags 1 and 8
vec <- c()
for( i in (1:25)) {
  trial <- item_df %>%
    select(Period, Num_Days_Price_Promo) %>%
    mutate(Period= Period + weeks(+i)) %>% 
    rename(Lag_Price_Promo = Num_Days_Price_Promo) %>% 
    right_join(item_df %>% select(Sum_Quantity, Period)) %>% 
    drop_na()
  
  correlation <- cor(trial[,"Lag_Price_Promo"],trial[,"Sum_Quantity"])
  
  vec <- append(vec, correlation)
}

vec %>%
  as.data.frame() %>% 
  ggplot() +
  geom_bar(aes (x= seq(1:25), y=.), stat ="identity")


#Promo
#Similar but lower results at same lags as price promo
vec <- c()
for( i in (1:15)) {
  trial <- item_df %>%
    select(Period, Num_Days_Promo) %>%
    mutate(Period= Period + weeks(+i)) %>% 
    rename(Lag_Promo = Num_Days_Promo) %>% 
    right_join(item_df %>% select(Sum_Quantity, Period)) %>% 
    drop_na()
  
  correlation <- cor(trial[,"Lag_Promo"],trial[,"Sum_Quantity"])
  
  vec <- append(vec, correlation)
}

vec %>%
  as.data.frame() %>% 
  ggplot() +
  geom_bar(aes (x= seq(1:15), y=.), stat ="identity")


#Number of School Holidays
#Spike at lag 4, but only just above 0.2
vec <- c()
for( i in (1:25)) {
  trial <- item_df %>%
    select(Period, Num_School_Holidays) %>%
    mutate(Period= Period + weeks(+i)) %>% 
    right_join(item_df %>% select(Sum_Quantity, Period)) %>% 
    drop_na()
  
  correlation <- cor(trial[,"Num_School_Holidays"],trial[,"Sum_Quantity"])
  
  vec <- append(vec, correlation)
}

vec %>%
  as.data.frame() %>% 
  ggplot() +
  geom_bar(aes (x= seq(1:25), y=.), stat ="identity")


#Holidays
#Nothing significant here
vec <- c()
for( i in (1:25)) {
  trial <- item_df %>%
    select(Period, Num_Holidays) %>%
    mutate(Period= Period + weeks(+i)) %>% 
    #rename(Lag_Price_Promo = Num_Days_Price_Promo) %>% 
    right_join(item_df %>% select(Sum_Quantity, Period)) %>% 
    drop_na()
  
  correlation <- cor(trial[,"Num_Holidays"],trial[,"Sum_Quantity"])
  
  vec <- append(vec, correlation)
}

vec %>%
  as.data.frame() %>% 
  ggplot() +
  geom_bar(aes (x= seq(1:25), y=.), stat ="identity")



#ITEM SALES
#Lag 3 and 9
vec <- c()
for( i in (1:40)) {
  trial <- item_df %>%
    select(Period, Sum_Quantity) %>%
    mutate(Period= Period + weeks(+i)) %>% 
    rename(Lag_Sales = Sum_Quantity) %>% 
    right_join(item_df %>% select(Sum_Quantity, Period)) %>% 
    drop_na()
  
  correlation <- cor(trial[,"Lag_Sales"],trial[,"Sum_Quantity"])
  
  vec <- append(vec, correlation)
}

vec %>%
  as.data.frame() %>% 
  ggplot() +
  geom_bar(aes (x= seq(1:40), y=.), stat ="identity")


ggAcf(item_df$Sum_Quantity)

#Let´s take a look at autocorrelation in class sales- this could make a good predictor
# Lag 3 is top, followed by 9, 6, 1, and 2
ggAcf(class_sales_df$Class_Sales)

#Looks like lag 3 and 9 are going to give best results
vec <- c()
for( i in (1:40)) {
  trial <- class_sales_df %>%
    select(Period, Class_Sales) %>%
    mutate(Period= Period + weeks(+i)) %>% 
    #rename(Lag_Sales = Sum_Quantity) %>% 
    right_join(item_df %>% select(Sum_Quantity, Period)) %>% 
    drop_na()
  
  correlation <- cor(trial[,"Class_Sales"],trial[,"Sum_Quantity"])
  
  vec <- append(vec, correlation)
}

vec %>%
  as.data.frame() %>% 
  ggplot() +
  geom_bar(aes (x= seq(1:40), y=.), stat ="identity")

item_ts [,c("Sum_Quantity","Class_Sales")] %>% cor()



#Looks like lag 3 and 9 are going to give best results
vec <- c()
for( i in (1:40)) {
  trial <- item_df %>%
    select(Period, Unit_Price_KF00) %>%
    mutate(Period= Period + weeks(+i)) %>% 
    #rename(Lag_Sales = Sum_Quantity) %>% 
    right_join(item_df %>% select(Sum_Quantity, Period)) %>% 
    drop_na()
  
  correlation <- cor(trial[,"Unit_Price_KF00"],trial[,"Sum_Quantity"])
  
  vec <- append(vec, correlation)
}

vec %>%
  as.data.frame() %>% 
  ggplot() +
  geom_bar(aes (x= seq(1:40), y=.), stat ="identity")

added_pred

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




#Nothing too interesting seen at customer sales
customer_sales_df %>%
  select(-Period) %>% ggpairs()



#FINDING BEST MODELS WITH BASIC TRAIN-TEST ---------------------------------------------

#TRAINING MODEL WITH BASIC REGRESSORS
#create variable name vector
variable_names <- item_df %>% 
  select(Num_Days_Price_Promo, Num_Days_Promo, #Quarter_Low_Price,
         Num_Holidays, Num_School_Holidays, 
         Weeks_After_Hol, Weeks_Before_Hol,
         Lag_3_Class_Sales, Lag_9_Class_Sales, Lag_3_Item_Sales, Lag_9_Item_Sales,
         Lag_1_Price_Promo, Lag_8_Price_Promo,
         Winter, Spring, Summer, Autumn) %>% 
  colnames()


#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)

#passing the boolean dataframe to the variable name vector to create a dataframe with the code for all possible combinations
model_list <- apply(combo_df, 1, function(x) as.formula(
  paste("Sum_Quantity ~ ",
        paste(c( variable_names[x]) ,collapse=" + "))))

#using the code stored in previous dataframe to pass it to a CV(tslm()) function and storing result in df
model_results_df_week <- lapply(model_list,
                           function(x) CV(tslm(x, data=item_ts_train))) %>%
  as.data.frame() %>%
  t() %>% 
  as_tibble()

#saving index for mutating in next lines
rankcv_index <- length(combo_df) + length(model_results_df_week) +1

#binding previous dataframe with boolean dataframe, and sorting by best score
full_model_results_df_week <- cbind (combo_df, model_results_df_week) %>% 
  mutate(RankCV = rank(CV), RankAIC = rank(AIC), RankAICc = rank(AICc), RankBIC = rank(BIC)) %>% 
  mutate(Mean_Rank = rowMeans(.[, rankcv_index:(rankcv_index+3)])) %>% 
  arrange(AICc)

full_model_results_df_week %>% 
  slice(1:(nrow(.)*0.01)) %>%
  select(1:(ncol(.)-10)) %>% 
  colSums() %>%
  sort(decreasing = TRUE) %>% 
  `/`(nrow(full_model_results_df_week)*0.01)

full_model_results_df_week %>% 
  slice((nrow(.)*0.99):nrow(.)) %>%
  select(1:(ncol(.)-10)) %>% 
  colSums() %>%
  sort(decreasing = TRUE) %>% 
  `/`(nrow(full_model_results_df_week) - nrow(full_model_results_df_week)*0.99)
  

most_accurate(full_model_results_df_week, train_ts = item_ts_train, test_ts = item_ts_test, n_trials=100, target_string ="Sum_Quantity ~ ")

winner <- full_model_results_df_week[48,1:length(variable_names)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[winner]) ,collapse=" + ")))

fit.trial1_week <- tslm(winner_script, data=item_ts_train)

checkresiduals(fit.trial1_week)

new_data <- item_ts_test %>% as.data.frame()#item_df %>% filter(Period>'2019-01-01' & Period <'2019-04-01')
fc.trial1_week <- forecast (fit.trial1_week, newdata = new_data)

autoplot(fc.trial1_week) +
  autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.trial1_week,x=item_ts_test[,1])


winners_trial1_week <- variable_names[winner]


#TRYING A MODEL WITH ONLY SPREAD PRICES
#Most models show very significant residual autocorrelation at lags 3 and 9, so probably adding lagged unit price
#No model i valid by itself
variable_names <- columns_match(item_df,"Unit_Price_")

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)

#passing the boolean dataframe to the variable name vector to create a dataframe with the code for all possible combinations
model_list <- apply(combo_df, 1, function(x) as.formula(
  paste("Sum_Quantity ~ ",
        paste(c( variable_names[x]) ,collapse=" + "))))

#using the code stored in previous dataframe to pass it to a CV(tslm()) function and storing result in df
model_results_df2_week <- lapply(model_list,
                            function(x) CV(tslm(x, data=item_ts_train))) %>%
  as.data.frame() %>%
  t() %>% 
  as_tibble()

#saving index for mutating in next lines
rankcv_index <- length(combo_df) + length(model_results_df2_week) +1

#binding previous dataframe with boolean dataframe, and sorting by best score
full_model_results_df2_week <- cbind (combo_df, model_results_df2_week) %>% 
  mutate(RankCV = rank(CV), RankAIC = rank(AIC), RankAICc = rank(AICc), RankBIC = rank(BIC)) %>% 
  mutate(Mean_Rank = rowMeans(.[, rankcv_index:(rankcv_index+3)])) %>%
  arrange(CV)

most_accurate(full_model_results_df2_week, n_trials=50 ,target_string ="Sum_Quantity ~ ")

winner <- full_model_results_df2_week[1,1:length(variable_names)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c( variable_names[winner]) ,collapse=" + ")))

fit.trial2_week <- tslm(winner_script, data=item_ts_train)

checkresiduals(fit.trial2_week)

new_data <- item_ts_test %>% as.data.frame()
fc.trial2_week <- forecast (fit.trial2_week, newdata = new_data)

autoplot(fc.trial2_week) +
  autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.trial2_week, x=item_ts_test[,1])

winners_trial2_week <- variable_names[winner]

    


#NON LINEAR REGRESSION ------------------------------------------------------------------------------------------

#Up to noW, data has not been transformed.

BoxCox.lambda(item_df_clean[,2])

#It looks like lambda 0.5 is suitable for this time series. This is equivalent to a square root
BoxCox(item_df_clean[,2], 0.5) %>%
  as.data.frame() %>% 
  ggplot() +
    geom_line(aes(x=seq(1:(nrow(item_df_clean))), y=.), color = "dark blue") 



fit.trial3_week <- tslm(winner_script, data= item_ts_train %>% sqrt() )

checkresiduals(fit.trial3_week)

new_data <- sqrt(item_ts_test) %>% as.data.frame()
fc.trial3_week <- forecast (fit.trial3_week, newdata = new_data)


#backtransforming values
fc.trial3_week$mean <- fc.trial3_week$mean %>% square() 
fc.trial3_week$lower <- fc.trial3_week$lower %>% square()
fc.trial3_week$upper <- fc.trial3_week$upper %>% square()
fc.trial3_week$x <- fc.trial3_week$x %>% square()
fc.trial3_week$fitted <- fc.trial3_week$fitted %>% square()
fc.trial3_week$model$model <- fc.trial3_week$model$model %>% square()

accuracy(fc.trial3_week, item_ts_test[,1])

autoplot(fc.trial3_week) +
  autolayer(item_ts_test[,1] , series="Actual")

#TREATING OUTLIERS--------------------------------------------------------------------------------------

#Using a defined function to take outliers from more than 15 months ago and moderate them
#It looks like it works!! For the moment in this ts, it looks like the best results are given by range 97,3 
item_df_clean <- moderate_outliers(item_df,percentile_hi = 98, percentile_lo = 2)

item_ts_clean <- item_df_clean %>% ts()

item_ts_train_clean <- item_df_clean %>% 
  filter( Period <'2019-01-01') %>% 
  select(-Period) %>%
  ts()

#creating a timeseries test data set for plotting (autoplot requires it)
item_ts_test_clean <- item_df_clean %>% 
  filter( Period >= '2019-01-01' & Period < '2019-02-01') %>% 
  #filter( Period < '2019-04-01') %>% 
  select(-Period) %>%
  ts(start=223)


#plotting the changes
ggplot() +
  geom_line(data = item_df, aes(x=Period, y=Sum_Quantity), color = "magenta") +
  geom_line(data = item_df_clean, aes(x=Period, y=Sum_Quantity), color = "dark blue")


new_data <- item_ts_test_clean %>% as.data.frame()
#trying with previous result, it improves both train and test accuracy
tslm(winner_script, data= item_ts_train_clean) %>% forecast(new_data) %>%# autoplot() + autolayer(item_ts_test[,1],series="Actual")
  accuracy(x=item_ts_test_clean[,1])

fit.trial1_week %>% checkresiduals()




#LOOKING FOR A MORE ROBUST APPROACH THAN PLAIN TRAIN TEST -----------------------------------------------------------------


#Finding out if previous winner models are good models according to tsCV (using implemented MASE accuracy measure)
#We´ll check multiple initial points so we find out how that parameter changes results in start tsCV

#First defining a function to be passed to tsCV
my_tslm <- function(y, h, xreg)
{
  if(nrow(xreg) < length(y) + h) {
    stop("Not enough xreg data for forecasting") }
  X <- xreg[seq_along(y),] %>% ts()
  fit <- tslm(y ~ X)
  X <- xreg[length(y)+seq(h), , drop=FALSE]
  colnames(X) <- colnames(xreg)
  forecast(fit, newdata=X)
}


#Now preparing for a loop that will go through all possible initial points for all winner models
h <- 4  
winners_list <- list(winners_trial1_week)
result_list <- list()

#Creating as many empty vectors as there are in winner_list for storing results
for (i in length(winners_list)){
  result_list[[i]]<- vector()
}

for (i in (100:nrow(item_ts))) { #loop for number of possible tscvs
  for (j in (1:length(winners_list))) { #loop for number of winnner models checked
    pred <- item_ts[, colnames(item_ts) %in% winners_list[[j]]] # TSCV requires passing separately values of predictors 
    naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=i) #storing naive component of MASE
    mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial = i ) %>%
      get_mase_cv(naive = naive_error, h=h)
    result_list[[j]] <- append(result_list[[j]],mase) 
  } 
}


#Plotting results
#One interesting thing that is seen here is the more "robustness" of models
#Also, It looks best to trust more models that perform best with recent values, rather than with the whole series
do.call(cbind, result_list) %>%
  as.data.frame() %>%
  #slice(43:47) %>% 
  ts() %>%
  autoplot()


# NEXT STEPS- TRY FINDING BEST MODELS USING TSCV AND SEE DIFFERENCE WITH PREVIOUS RESULTS
# procedure: from top 500 models in full model results,
# get a ranking of accuracy and compare to the original ranking in full model results


#Now let´s see if there is much difference between what AICc points to and tsCV with ALL data
#Preparing a loop that goes through top N models chosen by AICc, and trying tsCV for them
#Then comparing scores of both approaches

h<-4
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=round(nrow(item_ts)*0.90))
result_vector <- c()
for (i in 1:100) {
  model_index <- which(full_model_results_df_week[i,1:(ncol(full_model_results_df_week)-8)] == TRUE)
  colnames(full_model_results_df_week)[model_index]
  pred <- item_ts[,colnames(full_model_results_df_week)[model_index]]
  cv_mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial=round(nrow(item_ts)*0.95)) %>% 
    get_mase_cv(naive = naive_error, h=h)
  result_vector <- append(result_vector,cv_mase)
}


as.data.frame(result_vector) %>% 
  mutate(RankMASE = rank(result_vector)) %>%
  cbind (combo_df[1:100,]) %>% View()

most_accurate(full_model_results_df7 %>% arrange(RankAICc), 10, "Sum_Quantity ~ ")

full_model_results_df7[c(41,456,1446,2151),]


tsCV(item_ts[,1], naive, h=h, initial=round(nrow(item_ts)*0.95)) %>% 
  get_mase_cv(naive = naive_error, h=h)

#CONCLUSIONS: 
#tsCV is pretty slow to compute, so it is better to find a different strategy.
#On the other hand, for monthly data there aren´t many observations, so it looks like we can´t afford training leaving data out (classic train-test approach)
#So perhaps training with ALL data, and then using tsCV to find a model that doesn´t overfit would be interesting (not too orthodox) 
#looks like AICc is better than using MASE TSCV (pretty unreliable) or CV statistic

#Strategy:
#1. Use AICc for selecting top 200 combinations + Heatmap with ALL data
#2. Use tsCV MASE to find the combination that overfits less


#create variable name vector
#I am also sure i want lagged values of the predictor itself

#storing steps back for tscv, four months: initial plan is to retrain every four months
steps_back <- 16

fixed_predictors <- item_df %>%
  select(Lag_3_Item_Sales, Lag_9_Item_Sales) %>% 
  colnames()

variable_names <- item_df %>% 
  select(Lag_9_Class_Sales, Lag_3_Class_Sales,
         Lag_1_Price_Promo, Lag_8_Price_Promo) %>% 
  colnames()


#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)
fixed_pred_df <- data.frame(matrix(TRUE, nrow = nrow(combo_df), ncol = length(fixed_predictors))) #%>%
colnames(fixed_pred_df) <- fixed_predictors
combo_df <- cbind (fixed_pred_df, combo_df)


h<-4 #number of step forecasts
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial) #saving scaling component for MASE
result_vector <- c()

#loop that does a TSCV and finds and stores MASE score for all models passed 
for (i in 1:nrow(combo_df)) {
  model_index <- which(combo_df[i,] == TRUE)
  pred <- item_ts[,colnames(combo_df)[model_index]]
  cv_mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial=initial) %>% 
    get_mase_cv(naive = naive_error, h=h)
  result_vector <- append(result_vector,cv_mase)
}

#storing results in a df for inspection
ts_cv_results_df0 <- as.data.frame(result_vector) %>%
  rename(MASE = result_vector) %>% 
  mutate(RankMASE = rank(MASE)) %>%
  cbind (combo_df) %>%
  arrange(RankMASE)

#In case there is a tie in ranking, save a vector with number of predictors in each model
#So next step will be to choose the model with least predictors
first_rank <- ts_cv_results_df %>% 
  filter(RankMASE == max(RankMASE)) %>%
  select(3:ncol(.)) %>%
  rowSums()

winner <- ts_cv_results_df[which(first_rank == min(first_rank)),3:ncol(ts_cv_results_df0)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c(colnames(combo_df)[winner]) ,collapse=" + ")))

fit.trial0_week <- tslm(winner_script, data=item_ts)

checkresiduals(fit.trial0_week)

winners_trial0_week <- colnames(combo_df)[winner]








h<-4 #number of step forecasts
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial)
pred <- item_ts[,c("Lag_3_Item_Sales","Lag_9_Item_Sales")]
cv_mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial=initial) %>% 
  get_mase_cv(naive = naive_error, h=h)
cv_mase

item_ts[,1] %>% ggAcf()

#fixed_predictors <- item_df %>%
#  select(Lag_3_Item_Sales, Lag_9_Item_Sales) %>% 
#  colnames()

fixed_predictors <- winners_trial0_week


variable_names <- item_df %>% 
  select(Num_Days_Price_Promo, Num_Days_Promo,
         Num_Holidays, Num_School_Holidays, 
         Weeks_After_Hol, Weeks_Before_Hol,
         Weeks_After_Sch_Hol, Weeks_Before_Sch_Hol,
         Winter, Spring, Summer, Autumn) %>% 
  colnames()


#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)
fixed_pred_df <- data.frame(matrix(TRUE, nrow = nrow(combo_df), ncol = length(fixed_predictors))) #%>%
colnames(fixed_pred_df) <- fixed_predictors
combo_df <- cbind (fixed_pred_df, combo_df)

h<-4 #number of step forecasts
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial) #saving scaling component for MASE
result_vector <- c()

#loop that does a TSCV and finds and stores MASE score for all models passed 
for (i in 1:nrow(combo_df)) {
  model_index <- which(combo_df[i,] == TRUE)
  pred <- item_ts[,colnames(combo_df)[model_index]]
  cv_mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial=initial) %>% 
    get_mase_cv(naive = naive_error, h=h)
  result_vector <- append(result_vector,cv_mase)
}

#storing results in a df for inspection
ts_cv_results_df <- as.data.frame(result_vector) %>%
  rename(MASE = result_vector) %>% 
  mutate(RankMASE = rank(MASE)) %>%
  cbind (combo_df) %>%
  arrange(RankMASE)

#In case there is a tie in ranking, save a vector with number of predictors in each model
#So next step will be to choose the model with least predictors
first_rank <- ts_cv_results_df %>% 
  filter(RankMASE == max(RankMASE)) %>%
  select(3:ncol(.)) %>%
  rowSums()

winner <- ts_cv_results_df[which(first_rank == min(first_rank)),3:ncol(ts_cv_results_df)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c(colnames(combo_df)[winner]) ,collapse=" + ")))

fit.trial1_week <- tslm(winner_script, data=item_ts)

checkresiduals(fit.trial1_week)

winners_trial1_week <- colnames(combo_df)[winner]






#create variable name vector
fixed_predictors <- winners_trial1_week
variable_names <- item_df %>% 
  columns_match("Promo_")

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)
fixed_pred_df <- data.frame(matrix(TRUE, nrow = nrow(combo_df), ncol = length(fixed_predictors)))
colnames(fixed_pred_df) <- fixed_predictors
combo_df <- cbind (fixed_pred_df, combo_df)


h<-4 #number of step forecasts
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial) #saving scaling component for MASE
result_vector <- c()

#loop that does a TSCV and finds and stores MASE score for all models passed 
for (i in 1:nrow(combo_df)) {
  model_index <- which(combo_df[i,] == TRUE)
  pred <- item_ts[,colnames(combo_df)[model_index]]
  cv_mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial=initial) %>% 
    get_mase_cv(naive = naive_error, h=h)
  result_vector <- append(result_vector,cv_mase)
}

#Saving the results in a dataframe for inspection
ts_cv_results_df2 <- as.data.frame(result_vector) %>%
  rename(MASE = result_vector) %>% 
  mutate(RankMASE = rank(MASE)) %>%
  cbind (combo_df) %>%
  arrange(RankMASE)

#In case there is a tie in ranking, save a vector with number of predictors in each model
#So next step will be to choose the model with least predictors
first_rank <- ts_cv_results_df2 %>% 
  filter(RankMASE == max(RankMASE)) %>%
  select(3:ncol(.)) %>%
  rowSums()

winner <- ts_cv_results_df2[which(first_rank == min(first_rank)),3:ncol(ts_cv_results_df2)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c(colnames(combo_df)[winner]) ,collapse=" + ")))

fit.trial2_week <- tslm(winner_script, data=item_ts)

checkresiduals(fit.trial2_week)

winners_trial2_week <- colnames(combo_df)[winner]





#ADDING PREDICTORS WITH SPREAD PROMO & PROMO PRICE DAYS
#create variable name vector
fixed_predictors <- winners_trial2_week
variable_names <- item_df %>% 
  columns_match("Unit_Price_")

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)
fixed_pred_df <- data.frame(matrix(TRUE, nrow = nrow(combo_df), ncol = length(fixed_predictors)))
colnames(fixed_pred_df) <- fixed_predictors
combo_df <- cbind (fixed_pred_df, combo_df)


h<-4 #number of step forecasts
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial) #saving scaling component for MASE
result_vector <- c()


#loop that does a TSCV and finds and stores MASE score for all models passed 
for (i in 1:nrow(combo_df)) {
  model_index <- which(combo_df[i,] == TRUE)
  pred <- item_ts[,colnames(combo_df)[model_index]]
  cv_mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial=initial) %>% 
    get_mase_cv(naive = naive_error, h=h)
  result_vector <- append(result_vector,cv_mase)
}

#Saving the results in a dataframe for inspection
ts_cv_results_df3 <- as.data.frame(result_vector) %>%
  rename(MASE = result_vector) %>% 
  mutate(RankMASE = rank(MASE)) %>%
  cbind (combo_df) %>%
  arrange(RankMASE)

#In case there is a tie in ranking, save a vector with number of predictors in each model
#So next step will be to choose the model with least predictors
first_rank <- ts_cv_results_df3 %>% 
  filter(RankMASE == max(RankMASE)) %>%
  select(3:ncol(.)) %>%
  rowSums()

winner <- ts_cv_results_df3[which(first_rank == min(first_rank)),3:ncol(ts_cv_results_df3)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c(colnames(combo_df)[winner]) ,collapse=" + ")))

fit.trial3_week <- tslm(winner_script, data=item_ts)

checkresiduals(fit.trial3_week)

winners_trial3_week <- colnames(combo_df)[winner]







#create variable name vector
fixed_predictors <- item_df %>%
  select(Lag_9_Item_Sales, Lag_3_Item_Sales) %>% 
  colnames

variable_names <- winners_trial1_week[which(!winners_trial1_week %in% fixed_predictors)]

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)
fixed_pred_df <- data.frame(matrix(TRUE, nrow = nrow(combo_df), ncol = length(fixed_predictors)))
colnames(fixed_pred_df) <- fixed_predictors
combo_df <- cbind (fixed_pred_df, combo_df)

h<-4 #number of step forecasts
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial) #saving scaling component for MASE
result_vector <- c()

#loop that does a TSCV and finds and stores MASE score for all models passed 
for (i in 1:nrow(combo_df)) {
  model_index <- which(combo_df[i,] == TRUE)
  pred <- item_ts[,colnames(combo_df)[model_index]]
  cv_mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial=initial) %>% 
    get_mase_cv(naive = naive_error, h=h)
  result_vector <- append(result_vector,cv_mase)
}

#Saving the results in a dataframe for inspection
ts_cv_results_df4 <- as.data.frame(result_vector) %>%
  rename(MASE = result_vector) %>% 
  mutate(RankMASE = rank(MASE)) %>%
  cbind (combo_df) %>%
  arrange(RankMASE) %>% 
  filter(Lag_3_Item_Sales == TRUE & Lag_9_Item_Sales == TRUE)

#In case there is a tie in ranking, save a vector with number of predictors in each model
#So next step will be to choose the model with least predictors
first_rank <- ts_cv_results_df4 %>% 
  filter(RankMASE == max(RankMASE)) %>%
  select(3:ncol(.)) %>%
  rowSums()

winner <- ts_cv_results_df4[which(first_rank == min(first_rank)),3:ncol(ts_cv_results_df4)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c(colnames(combo_df)[winner]) ,collapse=" + ")))

fit.trial4_week <- tslm(winner_script, data=item_ts)

checkresiduals(fit.trial4_week)

winners_trial4_week <- colnames(combo_df)[winner]





fixed_predictors <- winners_trial4_week
variable_names <- winners_trial2_week[which(str_detect(winners_trial2_week , "Promo_"))]

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)
fixed_pred_df <- data.frame(matrix(TRUE, nrow = nrow(combo_df), ncol = length(fixed_predictors)))
colnames(fixed_pred_df) <- fixed_predictors
combo_df <- cbind (fixed_pred_df, combo_df)

h<-4 #number of step forecasts
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial) #saving scaling component for MASE
result_vector <- c()

#loop that does a TSCV and finds and stores MASE score for all models passed 
for (i in 1:nrow(combo_df)) {
  model_index <- which(combo_df[i,] == TRUE)
  pred <- item_ts[,colnames(combo_df)[model_index]]
  cv_mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial=initial) %>% 
    get_mase_cv(naive = naive_error, h=h)
  result_vector <- append(result_vector,cv_mase)
}

#Saving the results in a dataframe for inspection
ts_cv_results_df5 <- as.data.frame(result_vector) %>%
  rename(MASE = result_vector) %>% 
  mutate(RankMASE = rank(MASE)) %>%
  cbind (combo_df) %>%
  arrange(RankMASE)

#In case there is a tie in ranking, save a vector with number of predictors in each model
#So next step will be to choose the model with least predictors
first_rank <- ts_cv_results_df5 %>% 
  filter(RankMASE == max(RankMASE)) %>%
  select(3:ncol(.)) %>%
  rowSums()

winner <- ts_cv_results_df5[which(first_rank == min(first_rank)),3:ncol(ts_cv_results_df5)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c(colnames(combo_df)[winner]) ,collapse=" + ")))

fit.trial5_week <- tslm(winner_script, data=item_ts)

checkresiduals(fit.trial5_week)

winners_trial5_week <- colnames(combo_df)[winner]







fixed_predictors <- winners_trial5_week
variable_names <- winners_trial3_week[which(str_detect(winners_trial3_week , "Unit_Price_"))]

#create a boolean dataframe with all possible combinations of variables
combo_df <- create_combination_df(variable_names)
fixed_pred_df <- data.frame(matrix(TRUE, nrow = nrow(combo_df), ncol = length(fixed_predictors)))
colnames(fixed_pred_df) <- fixed_predictors
combo_df <- cbind (fixed_pred_df, combo_df)

h<-4 #number of step forecasts
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial) #saving scaling component for MASE
result_vector <- c()

#loop that does a TSCV and finds and stores MASE score for all models passed 
for (i in 1:nrow(combo_df)) {
  model_index <- which(combo_df[i,] == TRUE)
  pred <- item_ts[,colnames(combo_df)[model_index]]
  cv_mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial=initial) %>% 
    get_mase_cv(naive = naive_error, h=h)
  result_vector <- append(result_vector,cv_mase)
}

#Saving the results in a dataframe for inspection
ts_cv_results_df6 <- as.data.frame(result_vector) %>%
  rename(MASE = result_vector) %>% 
  mutate(RankMASE = rank(MASE)) %>%
  cbind (combo_df) %>%
  arrange(RankMASE)

#In case there is a tie in ranking, save a vector with number of predictors in each model
#So next step will be to choose the model with least predictors
first_rank <- ts_cv_results_df6 %>% 
  filter(RankMASE == max(RankMASE)) %>%
  select(3:ncol(.)) %>%
  rowSums()

winner <- ts_cv_results_df6[which(first_rank == min(first_rank)),3:ncol(ts_cv_results_df6)] %>%
  t() %>%
  as.vector

winner_script <- as.formula(paste("Sum_Quantity ~ ", paste(c(colnames(combo_df)[winner]) ,collapse=" + ")))

fit.trial6_week <- tslm(winner_script, data=item_ts)

checkresiduals(fit.trial6_week)

winners_trial6_week <- colnames(combo_df)[winner]





final_winners <- append(winners_trial6_week,c("Lag_3_Item_Sales", "Lag_9_Item_Sales"))
h<-4 #number of step forecasts
initial <- round(nrow(item_ts)-steps_back)
pred <- item_ts[,final_winners]
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial)
tsCV(item_ts[,1], my_tslm, xreg =pred,  h=h, initial=initial) %>% 
  get_mase_cv(naive = naive_error, h=h)


create_combination_df(final_winners)




#CHECKING HOW TSCV results behave for all previous models

h <- 4  
winners_list <- list(winners_trial4_week, winners_trial5_week, winners_trial6_week)
result_list <- list()

#Creating as many empty vectors as there are in winner_list for storing results
for (i in length(winners_list)){
  result_list[[i]]<- vector()
}


for (i in (100:(nrow(item_ts)-h-1))) { #loop for number of possible tscvs
  for (j in (1:length(winners_list))) { #loop for number of winnner models checked
    pred <- item_ts[, colnames(item_ts) %in% winners_list[[j]]] # TSCV requires passing separately values of predictors 
    naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=i) #storing naive component of MASE
    mase <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial = i ) %>%
      get_mase_cv(naive = naive_error, h=h)
    result_list[[j]] <- append(result_list[[j]],mase) 
  } 
}


do.call(cbind, result_list) %>%
  as.data.frame() %>%
  rename(trial_4= V1, trial_5=V2, trial_6 =V3) %>% 
  ts() %>%
  autoplot()

#PLOTTING MULTIPLE FORECASTS FROM THE MODEL
plot_list <- list()
for (i in (1:8)) {
  item_ts_train <- item_ts[1:(nrow(item_ts)-i*4),] %>% ts()
  item_ts_test <- item_ts[(end(item_ts_train)[1]+1): (end(item_ts_train)[1]+4),] %>%  ts(start=end(item_ts_train)[1]+1)
  
  fit.loop <- tslm(winner_script, data=item_ts_train)
  new_data <- item_ts_test %>% as.data.frame()
  fc.loop <- forecast (fit.loop, newdata = new_data)
  
  plot <- autoplot(fc.loop) +
    autolayer(item_ts_test[,1] , series="Actual")
  
  plot_list[[i]] <- plot
  print(accuracy(fc.loop, new_data))
}
#putting all listed plots in a grid
plot_list <- rev(plot_list)

#First install Image Magick
#https://imagemagick.org/script/download.php
png(file="fc_plot%02d.png", width=1250, height=750)
for (n in (1:length(plot_list))){
  print(plot_list[[n]])
}
dev.off()

#This command will work for Windows
shell("magick convert -delay 150 fc_plot*.png fc_plot.gif")
file.remove(list.files(pattern="fc_plot...png"))


#MASE when data is transformed with square root is significantly worse, so it can be discarded
h<-4 #number of step forecasts
initial <- round(nrow(item_ts)-steps_back)
pred <- item_ts[,winners_trial6_week] %>% sqrt()
naive_error <- get_naive_cv_comp(item_ts[,1] %>% sqrt(), h=h, initial=initial)
tsCV(item_ts[,1] %>% sqrt(), my_tslm, xreg=pred, h=h, initial=initial) %>% 
  get_mase_cv(naive = naive_error, h=h)




#WHEN TO CUT THE DATA
h=4
min <- 10
result_vector <- c()
for (y in 2015:2016) {
  for (m in 1:12) {
  #for (w in 1:51) {
   item_ts2 <- item_df %>% 
      filter(Period >= paste0(y, "-", m, "-","1")) %>%
      filter(Period < '2019-04-01') %>% 
      #filter (week(Period) >= w) %>% 
      select(-Period) %>%
      ts()
   initial <- round(nrow(item_ts2)-steps_back)
    pred <- item_ts2[,winners_trial6_week]
    naive_error <- get_naive_cv_comp(item_ts2[,1], h=h, initial=initial)
    candidate <- tsCV(item_ts2[,1], my_tslm, xreg=pred, h=h, initial=initial) %>% 
      get_mase_cv(naive = naive_error, h=h)
    
    result_vector <- append(result_vector, candidate)
    
    if (candidate < min) {
      min <- candidate
      candidate_vec <- c(m,y,min)
    }
  }
}

candidate_vec
result_vector %>% ts(start = c(2015,1), frequency = 12) %>% autoplot()


item_ts2 <- item_df %>% 
  filter(Period >= paste0(2016, "-", 9, "-","1")) %>%
  #filter (week(Period) >= w) %>% 
  select(-Period) %>%
  ts()


h<-4 #number of step forecasts
initial <- round(nrow(item_ts2)-steps_back)
pred <- item_ts2[,winners_trial6_week]
naive_error <- get_naive_cv_comp(item_ts2[,1], h=h, initial=initial)
tsCV(item_ts2[,1], my_tslm, xreg=pred, h=h, initial=initial) %>% 
  get_mase_cv(naive = naive_error, h=h)


plot_list <- list()
for (i in (1:8)) {
  item_ts_train <- item_ts2[1:(nrow(item_ts2)-i*4),] %>% ts()
  item_ts_test <- item_ts2[(end(item_ts_train)[1]+1): (end(item_ts_train)[1]+4),] %>%
    ts(start=end(item_ts_train)[1]+1)
  
  fit.loop <- tslm(winner_script, data=item_ts_train)
  new_data <- item_ts_test %>% as.data.frame()
  fc.loop <- forecast (fit.loop, newdata = new_data)
  
  plot <- autoplot(fc.loop) +
    autolayer(item_ts_test[,1] , series="Actual")
  
  plot_list[[i]] <- plot
  print(accuracy(fc.loop, new_data))
}

plot_list <- rev(plot_list)

#First install Image Magick
#https://imagemagick.org/script/download.php
png(file="fc_plot%02d.png", width=1250, height=750)
for (n in (1:length(plot_list))){
  print(plot_list[[n]])
}
dev.off()

#This command will work for Windows
shell("magick convert -delay 150 fc_plot*.png fc_plot2.gif")
file.remove(list.files(pattern="fc_plot...png"))




#CHECKING HOW TSCV results behaves for all previous models
h <- 4  
winners_list <- list(winners_trial1_week, winners_trial2_week, winners_trial3_week)
result_list <- list()

#Creating as many empty vectors as there are in winner_list for storing results
for (i in length(winners_list)){
  result_list[[i]]<- vector()
}


for (i in (100:(nrow(item_ts2)-h-1))) { #loop for number of possible tscvs
  for (j in (1:length(winners_list))) { #loop for number of winnner models checked
    pred <- item_ts2[, colnames(item_ts2) %in% winners_list[[j]]] # TSCV requires passing separately values of predictors 
    naive_error <- get_naive_cv_comp(item_ts2[,1], h=h, initial=i) #storing naive component of MASE
    mase <- tsCV(item_ts2[,1], my_tslm, xreg=pred, h=h, initial = i ) %>%
      get_mase_cv(naive = naive_error, h=h)
    result_list[[j]] <- append(result_list[[j]],mase) 
  } 
}


do.call(cbind, result_list) %>%
  as.data.frame() %>%
  rename(trial_1= V1, trial_2=V2, trial_3 =V3) %>% 
  ts() %>%
  autoplot()




#TRYING ENTIRELY DIFFERENT APPROACH---------------------------------------------------------------------
#Iterating add and removal of predictors, and checking of residuals 

h=4
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial)



added_pred <- add_predictors(item_ts, c("Lag_4_Item_Sales"))
remaining_pred <- remove_predictors(item_ts[,c("Sum_Quantity",added_pred)])

y <- item_ts[,1]
X <- item_ts[,remaining_pred]
trial <- tslm(y~X, data=item_ts)
checkresiduals(trial)

manual_pred <- append(remaining_pred, "Lag_9_Item_Sales")
added_pred <- add_predictors(item_ts, manual_pred)
remaining_pred <- remove_predictors(item_ts[,c("Sum_Quantity",added_pred)])

y <- item_ts[,1]
X <- item_ts[,remaining_pred]
trial <- tslm(y~X, data=item_ts)
checkresiduals(trial)

manual_pred <-  append(remaining_pred, c("Lag_3_Price_Promo", "Lag_3_Promo"))
added_pred <- add_predictors(item_ts, manual_pred)
remaining_pred <- remove_predictors(item_ts[,c("Sum_Quantity",added_pred)])

y <- item_ts[,1]
X <- item_ts[,remaining_pred]
trial <- tslm(y~X, data=item_ts)
checkresiduals(trial)


final_winners <- remaining_pred


#WHEN TO CUT THE DATA
h=4
min <- 10
result_vector <- c()
for (y in 2015:2016) {
  for (m in 1:12) {
    #for (w in 1:51) {
    item_ts2 <- item_df %>% 
      filter(Period >= paste0(y, "-", m, "-","1")) %>%
      filter(Period < "2019-04-01") %>% 
      #filter (week(Period) >= w) %>% 
      select(-Period) %>%
      ts()
    initial <- round(nrow(item_ts2)-steps_back)
    pred <- item_ts2[,final_winners]
    naive_error <- get_naive_cv_comp(item_ts2[,1], h=h, initial=initial)
    candidate <- tsCV(item_ts2[,1], my_tslm, xreg=pred, h=h, initial=initial) %>% 
      get_mase_cv(naive = naive_error, h=h)
    
    result_vector <- append(result_vector, candidate)
    
    if (candidate < min) {
      min <- candidate
      candidate_vec <- c(m,y,min)
    }
  }
}

candidate_vec
result_vector %>% ts(start = c(2015,1), frequency = 12) %>% autoplot()

new_period <- paste0(candidate_vec[2], "-", candidate_vec[1], "-","1")

cut_vector <- new_period

item_ts2 <- item_df %>% 
  filter(Period >= new_period) %>%
  filter (Period < '2019-04-01') %>% 
  select(-Period) %>%
  ts()


h <- 4
initial <- round(nrow(item_ts2)-steps_back)
naive_error <- get_naive_cv_comp(item_ts2[,1], h=h, initial=initial)
pred <- item_ts2[,final_winners]
tsCV(item_ts2[,1], my_tslm, xreg=pred, h=h, initial = initial ) %>%
  get_mase_cv(naive = naive_error, h=h)


y <- item_ts2[,1]
X <- item_ts2[,final_winners]


fit.winners1 <- tslm(y~X, data=item_ts2)
checkresiduals(fit.winners1)

new_data <- item_df %>%
  select(-Sum_Quantity) %>%
  filter (Period >= "2019-04-01") %>% 
  filter (Period <= "2019-04-30")

fc.winners1 <- forecast (fit.winners1, newdata = new_data) 



#PLOTTING MULTIPLE FORECASTS FROM THE MODEL
plot_list <- list()
for (i in (1:12)) {
  item_ts_train <- item_ts2[1:(nrow(item_ts2)-i*4),] %>% ts()
  item_ts_test <- item_ts2[(end(item_ts_train)[1]+1): (end(item_ts_train)[1]+4),] %>%  ts(start=end(item_ts_train)[1]+1)
  
  
  fit.loop <- tslm(item_ts_train[,1]~ item_ts_train[,final_winners], data=item_ts_train)
  new_data <- item_ts_test %>% as.data.frame()
  fc.loop <- forecast (fit.loop, newdata = new_data)
  
  plot <- autoplot(fc.loop) +
    autolayer(item_ts_test[,1] , series="Actual")
  
  plot_list[[i]] <- plot
  #print(accuracy(fc.loop, item_ts_test))
}
#putting all listed plots in a grid
plot_list <- rev(plot_list)

#First install Image Magick
#https://imagemagick.org/script/download.php
png(file="fc_plot%02d.png", width=1250, height=750)
for (n in (1:length(plot_list))){
  print(plot_list[[n]])
}
dev.off()

#This command will work for Windows
shell("magick convert -delay 150 fc_plot*.png fc_plot3.gif")
file.remove(list.files(pattern="fc_plot...png"))



