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
library (data.table)
library (urca)

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

#function that creates a dataframe with all periods necessary for tseries, using first and last observations
create_period_df_month <- function(dataframe,period_column= Period, period="month", months_ahead=1) {
  first_observation <- dataframe$Period %>% min() %>% floor_date(period) %>% as_date()
  last_observation <-  dataframe$Period %>% max() %>% floor_date(period) %>% as_date() %m+% months(months_ahead)
  
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
                              months_excluded =15,
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
              round() >months_excluded )
  
  
  #create a dataframe with maximum and minimum values per year within specified percentile range
  min_max_df <- dframe %>%
    select(!!period_name,!!target_name) %>%
    mutate (Percentile = ntile(dframe[,2], 100)) %>%
    filter (!Percentile <= percentile_lo & !Percentile >= percentile_hi) %>% 
    group_by(Year = year(get(period_name))) %>%
    summarise(Max = max(get(target_name)), Min = min(get(target_name)))
  
  print(min_max_df)
  
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


#Defining a function that runs tslm that is to be passed to tsCV
#Only works with more than one predictos
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

#Only works with one predictor
my_tslm2 <- function(y, h, xreg)
{
  if(nrow(xreg) < length(y) + h) {
    stop("Not enough xreg data for forecasting") }
  X <- xreg[seq_along(y),]
  fit <- tslm(y ~ X)
  X <- xreg[length(y)+seq(h),]
  forecast(fit, newdata=X)
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
create_lag_months <- function(dframe,lag_vector,suffix = "_Item_Sales", period_col=Period, value_col="Sum_Quantity") {
  df_lags <- dframe %>% 
    mutate(Period= Period %m+% months(lag_vector[1])) %>% 
    rename(!!paste0("Lag_", lag_vector[1], suffix) := value_col)
  
  if (length(lag_vector)>1) {
    for (i in (2:(length(lag_vector)) )) {
      df_lags <- dframe %>%
        mutate(Period= Period %m+% months(lag_vector[i])) %>% 
        rename(!!paste0("Lag_", lag_vector[i], suffix) := value_col) %>% 
        left_join(df_lags)
    } 
  }
  return(df_lags)
}

#function that creates a lagged column with the specified lags
create_lead_months <- function(dframe,lead_vector,suffix = "_Item_Sales", period_col=Period, value_col="Sum_Quantity") {
  df_leads <- dframe %>% 
    mutate(Period= Period %m+% months(-lead_vector[1])) %>% 
    rename(!!paste0("Lead_", lead_vector[1], suffix) := value_col)
  
  if (length(lead_vector)>1) {
    for (i in (2:(length(lead_vector)) )) {
      df_leads <- dframe %>%
        mutate(Period= Period %m+% months(-lead_vector[i])) %>% 
        rename(!!paste0("Lead_", lead_vector[i], suffix) := value_col) %>% 
        left_join(df_leads)
    } 
  }
  return(df_leads)
}

# function that takes a ts object with target and predictor columns, 
# and runs a tscv while removing one predictor at a time, to see if accuracy improves by doing it
# it removes one predictor per iteration while accuracy of any model improves moving benchmark
remove_predictors <- function (ts_object, target_col_index=1, benchmark=1, n_iters=40, h=4, init=initial, naive=naive_error, fcfunction=my_tslm)  {
  
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
      mase <- tsCV(target, fcfunction, xreg=pred, h=h, initial = initial ) %>%
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
      print (paste0("removing: ",removable))
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
add_predictors <- function (ts_object, winners_vector, target_col_index=1, benchmark=1, n_iters=40, h=4, init=initial, naive=naive_error, fcfunction = my_tslm)  {
  
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
      mase <- tsCV(target, fcfunction, xreg=pred, h=h, initial = initial ) %>%
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
      print("No option improved benchmark in this iteration")
      return(winners_vector)
      break}
  }
  return(winners_vector)
}

#PREPARING DATAFRAME WITH FEATURES THAT WILL BE USED FOR TRYING MODELS--------------------------------------------------

#In this iteration we will only use item R240271 
#filtering sales dataframe for given item
filtered_sales <- sales_df_item %>%
  #filter (year(Period) >= 2015 ) %>% 
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
  mutate(Easter = ifelse (Easter>0,1,0)) %>% 
  right_join(period_df) %>% 
  replace(is.na(.),0)

#Creating a dataframe with school_holidays
month_school_holidays_df <- school_cal_df %>% 
  expand_time_int(start_col="Start", end_col="End", line_col="Line", n_days_col="Num_Days", period=period) %>% 
  mutate(Period = floor_date(Start, period) %>%  as_date()) %>%
  mutate(Period = Period %m+% months(Line-1)) %>% 
  arrange(Period) %>% 
  #select(Period,Num_Days) %>%
  group_by(Period) %>% 
  summarise(Num_School_Holidays = sum(Num_Days)) %>% 
  ungroup() %>% 
  right_join(period_df) %>% 
  replace(is.na(.),0)


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
lagged_item_sales <- filtered_sales %>%
  create_period_df(period = period) %>%
  left_join(filtered_sales) %>% 
  replace(is.na(.),0) %>%
  select(Period,Sum_Quantity) %>% 
  create_lag_months(seq(1,12),value_col = "Sum_Quantity")



#creating a df for lagged price promo days (1&8 months lag)
lagged_price_promo_df <- filtered_sales_price %>%
  select(Period, Num_Days_Price_Promo) %>%
  create_period_df(period = period) %>%
  left_join(filtered_sales_price) %>% 
  select(Period, Num_Days_Price_Promo) %>%
  create_lag_months(seq(1,12),suffix = "_Price_Promo", value_col = "Num_Days_Price_Promo")

#creating a df for lagged promo days (1&8 months lag)
lagged_promo_df <- filtered_sales_price %>%
  select(Period, Num_Days_Promo) %>%
  create_period_df(period = period) %>%
  left_join(filtered_sales_price) %>% 
  select(Period, Num_Days_Promo) %>%
  create_lag_months(seq(1,12),suffix = "_Promo", value_col = "Num_Days_Promo")

#creating a df for lagged bank holidays
lagged_bank_hol_df <- month_bank_holidays_df %>% 
  create_period_df(period = period) %>%
  left_join(month_bank_holidays_df) %>%
  select(Period, Num_Holidays) %>%
  replace(is.na(.),0) %>% 
  create_lag_months(seq(1,3),suffix = "_Holidays", value_col = "Num_Holidays")

#creating a df for lagged school holidays
lagged_school_hol_df <- month_school_holidays_df %>% 
  select(Period, Num_School_Holidays) %>%
  create_period_df(period = period) %>%
  left_join(month_school_holidays_df) %>%
  replace(is.na(.),0) %>% 
  create_lag_months(seq(1,6),suffix = "_School_Holidays", value_col = "Num_School_Holidays")

#creating a df for lead bank holidays
lead_bank_hol_df <- month_bank_holidays_df %>% 
  create_period_df(period = period) %>%
  left_join(month_bank_holidays_df) %>%
  select(Period, Num_Holidays) %>%
  replace(is.na(.),0) %>% 
  create_lead_months(seq(1,6),suffix = "_Holidays", value_col = "Num_Holidays")

#creating a df for lead school holidays
lead_school_hol_df <- month_school_holidays_df %>% 
  create_period_df(period = period) %>%
  left_join(month_school_holidays_df) %>%
  select(Period, Num_School_Holidays) %>% 
  replace(is.na(.),0) %>% 
  create_lead_months(seq(1,6),suffix = "_School_Holidays", value_col = "Num_School_Holidays")

#creating a df for lagged price promo days (2 months lag)
#lagged_price_promo_df <- filtered_sales_price %>%
#  select(Period, Num_Days_Price_Promo) %>%
#  mutate( Period= Period %m+% months(+2)) %>% 
#  rename(Lag_Price_Promo = Num_Days_Price_Promo) %>% View()


#spreading promo days by customer
promo_spread <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>%
  select(Period, Customer_Code, Num_Days_Promo) %>%
  spread(Customer_Code,Num_Days_Promo) %>% 
  rename_at(vars(-Period), funs(paste0("Days_Promo_", .))) #%>%
#replace(is.na(.), 0)

#spreading price promo days by customer
price_promo_spread <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>%
  select(Period, Customer_Code, Num_Days_Price_Promo) %>%
  spread(Customer_Code,Num_Days_Price_Promo) %>%
  rename_at(vars(-Period), funs(paste0("Days_Price_Promo_", .))) #%>%
#replace(is.na(.), 0)

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
  #left_join(class_sales_df, by = c("Period")) %>%
  left_join(promo_spread, by = c("Period")) %>%
  left_join(price_promo_spread, by = c("Period")) %>%
  left_join(unit_price_spread, by = c("Period")) %>%
  #left_join(lagged_class_df, by = c("Period")) %>%
  left_join(lagged_item_sales, by = c("Period")) %>%
  left_join(lagged_price_promo_df, by = c("Period")) %>%
  left_join(lagged_promo_df, by = c("Period")) %>%
  left_join(lagged_bank_hol_df, by = c("Period")) %>%
  left_join(lagged_school_hol_df, by = c("Period")) %>% 
  left_join(lead_bank_hol_df, by = c("Period")) %>% 
  left_join(lead_school_hol_df, by = c("Period")) %>% 
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
  filter (Period >= '2015-09-01') %>% 
  filter(Period < "2019-04-01") %>% 
  #filter( Period <='2019-01-01') %>% 
  select(-Period) %>% 
  ts(start = c(2015,9), frequency = 12)

#creating a train data set that is a timeseries object
item_ts_train <- item_df %>% 
  filter (Period >= '2015-09-01') %>% 
  filter( Period <as_date('2019-02-01')) %>% 
  select(-Period) %>% 
  ts(start = c(2015,9), frequency = 12)


#creating a timeseries test data set for plotting (autoplot requires it)
item_ts_test <- item_df %>% 
  filter( Period >= '2019-02-01') %>% 
  #filter( Period < '2019-04-01') %>% 
  select(-Period) %>% 
  ts(start = c(2019,1), frequency = 12)








# MULTIPLE LINEAR REGRESSION -----------------------------------------------------------------
# Procedure:
# Using a hybrid stepwise regression for selecting predictors
# -- starting by adding predictors
# -- then removing them
# -- checking remaining model´s residuals
# -- iterating if not satisfied
# -- looking when to cut the observations
# All this is done using timeseries crossvalidation to train models,
# -- and MASE as a metric for measuring accuracy


#First defining a function to be passed to tsCV
my_tslm <- function(y, h, xreg)
{
  if(nrow(xreg) < length(y) + h) {
    stop("Not enough xreg data for forecasting") }
  X <- xreg[seq_along(y),] %>% ts(start=c(2015,9), frequency = 12)
  fit <- tslm(y ~ X)
  X <- xreg[length(y)+seq(h), , drop=FALSE]
  colnames(X) <- colnames(xreg)
  forecast(fit, newdata=X)
}


steps_back <- 12 #This will define when tscv should start: n observations before most recent observations
h=1 
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial)

# Adding predictors. Has to start with one chosen by me due to issues with timeseries objects in my_tslm
added_pred <- add_predictors(item_ts, c("Lead_1_Holidays"), benchmark=1, h=1)
# Removing predictors from previous result
remaining_pred <- remove_predictors(item_ts[,c("Sum_Quantity",added_pred)], benchmark=1, h=1)


# Checking if residuals from the remaining model look satisfying
y <- item_ts[,1]
X <- item_ts[,remaining_pred]
trial <- tslm(y~X)
checkresiduals(trial)


# Just in case, checking residuals from only adding predictors
y <- item_ts[,1]
X <- item_ts[,added_pred]
trial <- tslm(y~X)
checkresiduals(trial)


#Because residuals look reasonable, and accuracy from remaining model is ok we choose it as a final winner
final_winners <- remaining_pred


# Looking at when to cut the data
# The loop progressively removes starting observations, 
# -- and tries the accuracy of the model training with that data, saving results into a vector

h=1
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

#Checking winner month and accuracy
candidate_vec
#Plotting results to see what has happened in the loop
result_vector %>% ts(start = c(2015,1), frequency = 12) %>% autoplot()


new_period <- paste0(candidate_vec[2], "-", candidate_vec[1], "-","1")

item_ts2 <- item_df %>% 
  filter(Period >= new_period) %>%
  filter (Period < '2019-04-01') %>% 
  select(-Period) %>%
  ts(start=(rev(candidate_vec[1:2])), frequency = 12)


h <- 1
initial <- round(nrow(item_ts2)-steps_back)
naive_error <- get_naive_cv_comp(item_ts2[,1], h=h, initial=initial)
pred <- item_ts2[,final_winners]
tsCV(item_ts2[,1], my_tslm, xreg=pred, h=h, initial = initial ) %>%
  get_mase_cv(naive = naive_error, h=h)


y <- item_ts2[,1]
X <- item_ts2[,final_winners]


fit.winners0 <- tslm(y~X, data=item_ts2)
checkresiduals(fit.winners0)

new_data <- item_df %>%
  select(-Sum_Quantity) %>%
  filter (Period >= "2019-04-01") %>% 
  filter (Period <= "2019-04-30")

fc.winners0 <- forecast (fit.winners0, newdata = new_data) 

#accuracy measured by MASE doesn´t look great,
#but in this case this happens because a Naive forecast would have nailed it
fc.winners0 %>% accuracy

#Plotting forecast. Autoplot won´t work with this forecast because it is one step
past_values <- data.frame(Period=time(fc.winners0$x)%>% as_date, Past=fc.winners0$x)

forecast_values <- fc.winners0 %>% as.data.frame() %>% cbind(Period=as_date("2019-04-01"))

gg_df <- past_values %>% full_join(forecast_values, by = c("Period")) %>% 
  left_join(item_df[,1:2], by = c("Period")) %>% 
  arrange(Period)


gg_df[1:(nrow(gg_df)-2),"Sum_Quantity"] <- NA
gg_df[(nrow(gg_df)-1),3:ncol(gg_df)] <- gg_df[(nrow(gg_df)-1),"Past"]


colnames(gg_df) <- colnames(gg_df) %>% str_replace(" ","_")


gg_df %>%
  ggplot(aes(x=Period)) +
  geom_ribbon(fill="navy", alpha=0.2, aes(ymin = Lo_80, ymax = Hi_80)) + 
  geom_ribbon(fill="navy", alpha=0.2, aes(ymin = Lo_95, ymax = Hi_95)) +
  geom_line(aes(y=Sum_Quantity, colour ="Actual"), size = 1.5, show.legend = TRUE,alpha=0.8) +
  geom_line(aes(y=Past, colour="Past"),  size = 1.5, show.legend = TRUE) +
  geom_line(aes(y=Point_Forecast, colour = "Forecast"), size = 1.5, show.legend = TRUE, alpha=0.8) +
  #my_theme +
  scale_x_date (date_breaks = "6 months") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  scale_color_manual(values=summertime3[c(5,3,1)])
  





# EXPONENTIAL MODELS ---------------------------------------------------
#Approach: 
# -- Exploring model forecasts using a simple holdout. Drawing no conclusions, just exploring
# -- After ARIMA models have been explored, then comparing models using TSCV and MASE metric, under same conditions as multiple linear regression


#creating a train data set that is a timeseries object
item_ts_train <- item_df %>% 
  filter (Period >= '2015-09-01') %>% 
  filter( Period <= '2019-02-01') %>% 
  select(-Period) %>% 
  ts(start = c(2015,9), frequency = 12)

#creating a train data set that is a timeseries object
item_ts_test <- item_df %>% 
  filter (Period > '2019-02-01') %>% 
  filter( Period < '2019-05-01') %>% 
  select(-Period) %>% 
  ts(start = c(2019,3), frequency = 12)


h=2 #just using two windows to be able to plot easily... the end of this part is understand the models
#naïve forecast of the seasonally adjusted data and a seasonal naïve forecast of the seasonal component, 
#- after an STL decomposition of the data
fit.loess <- item_ts_train[, 1] %>%
  stl(t.window=5, s.window=5, robust=TRUE)

#Checking residuals
fit.loess$time.series[,"remainder"] %>% ggAcf()
fit.loess$time.series[,"remainder"] %>% autoplot()

loess %>% seasadj() %>% naive() %>%
  autoplot()

fc.loess <- fit.loess %>%
  forecast(method="naive", h=h) 

fc.loess %>% 
  accuracy(x=item_ts_test[,1])

fc.loess %>%
  autoplot() + autolayer(item_ts_test[,1])


#Simple Exponential Smoothing

fc.ses <- ses(item_ts_train[,1], h=h)

checkresiduals(fc.ses)

fc.ses %>% 
  accuracy (item_ts_test[,1])

fc.ses %>% 
  autoplot() + autolayer(item_ts_test[,1])



#Holt
fc.holt <- holt(item_ts_train[,1], h=h)

checkresiduals(fc.holt)

fc.holt %>% 
  accuracy(item_ts_test[,1])

fc.holt %>% 
  autoplot() + autolayer(item_ts_test[,1])



#Holt-Winters
#multiplicative works better
fc.holwinters1 <- hw(item_ts_train[,1],seasonal="multiplicative", h=h)

checkresiduals(fc.holwinters1)

fc.holwinters1  %>% 
  accuracy(item_ts_test[,1])

fc.holwinters1 %>% 
  autoplot() + autolayer(item_ts_test[,1])



#Using ETS function (Exponential smoothing state space model)
fit.ets <- ets(item_ts_train[,1])

checkresiduals(fit.ets)

summary(fit.ets)

forecast(fit.ets, h=h) %>% autoplot() + autolayer(item_ts_test[,1] , series="Actual")
accuracy(forecast(fit.ets, h=h), x=item_ts_test[,1])


# ARIMA MODELS ---------------------------------------------------
# Approach
# -- Explore time series to prepare both seasonal and non seasonal Arima model manually
# -- Use auto arima function to produce seasonal and non seasonal Arima models
# -- Use a simple holdout test for exploration
# -- When suitable Arima models are found, then comparing them using TSCV and MASE

#Is transformation necessary? (it looks like not)
BoxCox.lambda(item_ts_train[,1]) 

item_ts_train[,1] %>% BoxCox(0.9)%>% autoplot()

#checking how BoxCox stabilizes change in variance along years
#the lambda that sets a straight line is 0.35
# however, this is a multiplicative tseries, so I would expect that transformation won´t help unless it´s mild
item_df %>% 
  select(Period,Sum_Quantity) %>%
  mutate(Sum_Quantity = BoxCox(Sum_Quantity,0.35)) %>% 
  group_by(year(Period)) %>% 
  summarise(Var = var(Sum_Quantity)) %>% 
  ungroup() %>% 
  select(Var) %>%
  ts(start=c(2015,1), frequency = 1) %>% 
  autoplot()


#Unit Root test to find out if differenced data is still not stationary (null hypothesis), 
#and thus needs further differencing
#p-value is just above 0.05, so data IS STILL NOT stationary and may require further differencing

item_ts[,1] %>% 
  diff() %>%
  ur.kpss() %>% 
  summary()

item_ts[,1] %>% 
  diff() %>%
  diff() %>%
  ur.kpss()%>%
  summary()

#However function ndiffs diagnoses only one differencing for non seasonal, and 0 for seasonal
ndiffs(item_ts[,1])
nsdiffs(item_ts[,1])


#Checking Autocorrelation and Partial Autocorrelation to determine where the ARIMA model is (p,d,q)
# With only one differentiation, neither ACF is exponentially decaying, 
# - but PACF does have a spike at lag 1 (although it is ), so it is not conclusive
item_ts[,1] %>% 
  diff() %>%
  #diff() %>%
  ggAcf(lag.max=40)

item_ts[,1] %>% 
  diff() %>%
  #diff() %>%
  ggPacf(lag.max=40)

# It is more or less the same case with differentiation of order 2, 
# but there are important spikes after lag 1 in PACF
item_ts[,1] %>% 
  diff() %>%
  diff() %>%
  ggAcf(lag.max=40)

item_ts[,1] %>% 
  diff() %>%
  diff() %>%
  ggPacf(lag.max=40)

#From previous exploration, we could conclude that 1,1,1 or 1,2,1 would be appropiate
fit.manual_arima <- Arima(item_ts_train[,1], order=c(1,2,1))

summary(fit.manual_arima)

checkresiduals(fit.manual_arima)

fc.manual_arima <- fit.manual_arima %>% forecast(h=h)

fc.manual_arima %>% autoplot() +autolayer(item_ts_test[,1])

fc.manual_arima %>% accuracy(item_ts_test[,1], series="Actual")


# Non-seasonal Arima found by auto.arima has worse AICc (and all other metrics), but its accuracy is MUCH better
fit.auto_arima <- auto.arima(item_ts_train[,1] ,stepwise = FALSE, approximation=FALSE, seasonal= FALSE)

summary(fit.auto_arima)

checkresiduals(fit.auto_arima)

fc.auto_arima <- forecast (fit.auto_arima, h=h) 

fc.auto_arima %>% autoplot() + autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.auto_arima , x=item_ts_test[,1])


#Seasonal Arima

#Trying seasonal differencing
#The ts needs one seasonal differencing, and a second normal differencing to be stationary
item_ts[,1] %>% diff(lag=12) %>% ggtsdisplay(lag.max = 25)
item_ts[,1] %>% diff(lag=12) %>% diff() %>% ggtsdisplay(lag.max = 25)

#Looking at ACF and PACF of seasonally differenced ts 
#Lag 1 has a significant spikes in Lag 1 and 2,
#so that means add a non seasonal MA component of order 1 (PACF shows spikeonly in lag 1) or 2
item_ts[,1] %>% diff(lag=12) %>% diff() %>% ggtsdisplay(lag.max = 25)


#after much iterating, 
#fit.sarima <- item_ts_train[,1] %>% Arima(order=c(1,1,2), seasonal=c(0,1,0)) 
fit.sarima <- item_ts_train[,1] %>% Arima(order=c(0,1,2), seasonal=c(0,1,0)) 

summary(fit.sarima)

checkresiduals(fit.sarima)

fc.sarima <- forecast (fit.sarima, h=h) 

fc.sarima %>% autoplot() + autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.sarima, x=item_ts_test[,1])


#It certainly looks like seasonal auto.arima performs best of all
fit.auto_sarima <- auto.arima(item_ts_train[,1] ,stepwise = FALSE, approximation=FALSE, seasonal= TRUE)

summary(fit.auto_sarima)

checkresiduals(fit.auto_sarima)

fc.auto_sarima <- forecast (fit.auto_sarima, h=h) 

fc.auto_sarima %>% autoplot() + autolayer(item_ts_test[,1] , series="Actual") + theme(axis.title.x = element_blank(),
                                                                                      axis.title.y = element_blank())
accuracy(fc.auto_sarima , x=item_ts_test[,1])



# TIME SERIES CROSS VALIDATION
#finding out which arima is best using time series cross validation
#because I am not sure when to start cross validation best, we´ll make a loop that will try a few possibilities
#auto_arima <- c()
#manual_arima <- c()
#auto_sarima<- c()
#manual_sarima <- c()
h <- 1
steps_back <- 12
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial)

#THIS IS AUTO ARIMA
my_arima <- function(x, h){ x %>%  Arima(order=c(0,1,1)) %>% forecast(h=h)}
auto_arima <- tsCV(item_ts[,1], my_arima, h=h, initial=initial) %>% #  square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h, naive = naive_error)
#a1 <- append(a1,trial)

#THIS IS MANUAL ARIMA
my_arima <- function(x, h){ x %>%  Arima(order=c(1,2,1)) %>% forecast(h=h)}
manual_arima <- tsCV(item_ts[,1], my_arima, h=h, initial=initial) %>% #  square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h, naive = naive_error)
#a2 <- append(a2,trial)

#THIS IS AUTO SEASONAL ARIMA
my_sarima <-function(x, h){ x %>%  Arima(order=c(1,1,0), seasonal=c(0,0,1)) %>% forecast(h=h)}
auto_sarima <- tsCV(item_ts[,1], my_sarima, h=h, initial=initial) %>% # square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h, naive = naive_error)
#sa1 <- append(sa1,trial)

#THIS IS MANUAL SEASONAL ARIMA
my_sarima <-function(x, h){ x %>%  Arima(order=c(0,1,2), seasonal=c(0,1,0)) %>% forecast(h=h)}
manual_sarima <- tsCV(item_ts[,1], my_sarima, h=h, initial=initial) %>%#  square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h)
#sa2 <- append(sa2,trial)


#Saving exponential models TSCV MASE too
my_holt <- function (x,h) {x %>% holt() %>% forecast(h=h)}
holt_model <- tsCV(item_ts[,1], my_holt, h=h, initial=initial) %>%#  square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h)

my_hw <- function (x,h) {x %>% hw(seasonal = "additive") %>% forecast(h=h)}
holt_winters <- tsCV(item_ts[,1], my_hw, h=h, initial=initial) %>%#  square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h)

my_ets <- function (x,h) {x %>% ets() %>% forecast(h=h)}
ets_model <- tsCV(item_ts[,1], my_ets, h=h, initial=initial) %>%#  square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h)


pred <- item_ts[,final_winners]
regression_model <- tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial = initial ) %>%
  get_mase_cv(naive = naive_error, h=h)


model_comparison <- cbind(auto_arima, manual_arima, auto_sarima,manual_sarima, holt_model, holt_winters, ets_model,regression_model) 


model_comparison

#CONCLUSION: MULTIPLE LINEAR REGRESSION IS MUCH MORE ACCURATE THAN ANY OF THE OTHER MODELS TRIED