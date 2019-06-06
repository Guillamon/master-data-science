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
#library (DataCombine)
library (stringr)
library (TSA)
library (seasonal)
#library (data.table)


# INITIALIZING VARIABLES, DECLARING FUNCTIONS, AND READING FILES --------------------------------------------------

#Sys.setlocale("LC_ALL","English")

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
    for (i in (2:(length(lag_vector)) )) {
      df_lags <- dframe %>%
        mutate(Period= Period + weeks(lag_vector[i])) %>% 
        rename(!!paste0("Lag_", lag_vector[i], suffix) := value_col) %>% 
        left_join(df_lags)
    } 
  }
  return(df_lags)
}

item_df %>% select(Period, Num_Days_Promo) %>% create_lag_weeks(c(0,1,2,3,4),value_col = "Num_Days_Promo") %>% View()

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

#Next Item: 2nd--------------------------------------------------

item <- top_items$`Item No_`[3]


#In this iteration we will only use item R240271 
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


#creating a dataframe with customer sales for top items (just for exploration)
customer_sales_df <- joined_sales %>%
  inner_join(top_items) %>% 
  mutate(Period = floor_date(`Shipment Date`,period) %>% as_date()) %>%
  group_by(Period, Customer_Code) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>%
  ungroup() %>% 
  spread(Customer_Code, Sum_Quantity) %>% 
  replace(is.na(.), 0)


#EXPLORING BEHAVIOUR AND RELATIONSHIPS OF REGRESSAND AND POSSIBLE REGRESSORS

#Histogram of Sales Quantity
item_df %>% 
  ggplot( aes(x= Sum_Quantity))+
  geom_histogram(bins=10, binwidth = 300)


#CHECKING AUTOCORRELATION
ggAcf(item_ts_target,lag.max = 60)
ggPacf(item_ts_target,lag.max = 60)
item_lags <- c(4,6,7,9,10,14,16,17,18)





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

price_promo_lags <- c(seq(1,10),18)


#Promo
#Similar but lower results at same lags as price promo
vec <- c()
for( i in (1:20)) {
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
  geom_bar(aes (x= seq(1:20), y=.), stat ="identity")

promo_lags <- c(seq(1,10),18)


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

school_holiday_lags <- c(1,2,3,4,7,10,12,18,24)



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


bank_holiday_lags <- c(1,2,3,4,6,12,18)


#Class Sales
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

class_lags <- c(6,9,18)



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




#creating a df for lagged item sales
lagged_item_sales <- item_df %>%
  select(Period,Sum_Quantity) %>% 
  create_lag_weeks(item_lags, value_col = "Sum_Quantity") 


#creating a df for lagged price promo days (1&8 months lag)
lagged_price_promo_df <- item_df %>%
  select(Period,Num_Days_Price_Promo) %>%  
  create_lag_weeks(price_promo_lags, suffix = "_Price_Promo", value_col = "Num_Days_Price_Promo")

#creating a df for lagged promo days 
lagged_promo_df <- item_df %>%
  select(Period, Num_Days_Promo) %>%
  create_lag_weeks(promo_lags, suffix = "_Promo", value_col = "Num_Days_Promo")

#creating a df for lagged bank holidays
lagged_bank_hol_df <- week_bank_holidays_df %>% 
  select(Period, Num_Holidays) %>% 
  create_lag_weeks(bank_holiday_lags, suffix = "_Holidays", value_col = "Num_Holidays")

#creating a df for lagged school holidays
lagged_school_hol_df <- week_school_holidays_df %>% 
  select(Period, Num_School_Holidays) %>% 
  create_lag_weeks(school_holiday_lags, suffix = "_School_Holidays", value_col = "Num_School_Holidays")



#creating a dataframe with all features
item_df <- item_df %>%
  left_join(lagged_class_df, by = c("Period")) %>%
  left_join(lagged_item_sales, by = c("Period")) %>%
  left_join(lagged_price_promo_df, by = c("Period")) %>%
  left_join(lagged_promo_df, by = c("Period")) %>%
  left_join(lagged_bank_hol_df, by = c("Period")) %>% 
  left_join(lagged_school_hol_df, by = c("Period")) %>% 
  drop_na()

#doing this so moderate outliers function can work (after drop_na rownames do not begin by 1)
rownames(item_df) <- NULL


#creating a timeseries object with data
item_ts <- item_df %>% 
  filter( Period >='2016-01-01') %>%
  filter(Period < '2019-04-01') %>%
  select(-Period) %>%
  ts()



#Iterating add and removal of predictors, and checking of residuals 
steps_back <- 16
h=4
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial)
added_pred <- add_predictors(item_ts, c("Num_Business_Days"))
remaining_pred <- remove_predictors(item_ts[,c("Sum_Quantity",added_pred)])

y <- item_ts[,1]
X <- item_ts[,remaining_pred]
trial <- tslm(y~X, data=item_ts)
checkresiduals(trial)


#manual_pred <- append(remaining_pred, "Lag_18_Price_Promo")

final_winners <- remaining_pred



#WHEN TO CUT THE DATA
h=4
min <- 10
result_vector <- c()
for (y in 2015:2017) {
  for (m in 1:12) {
    #for (w in 1:51) {
    item_ts2 <- item_df %>% 
      filter(Period >= paste0(y, "-", m, "-","1")) %>%
      filter(Period < '2019-04-01') %>% 
      #filter (week(Period) >= w) %>% 
      select(-Period) %>%
      ts()
    
    initial <- round(nrow(item_ts2)-steps_back)
    pred <- item_ts2[,remaining_pred]
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


fit.winners2 <- tslm(y~X)
checkresiduals(fit.winners2)

new_data <- item_df %>%
  #select(-Sum_Quantity) %>% 
  filter (Period >="2019-04-01") %>% 
  filter (Period <= "2019-04-30")
new_data <- new_data[final_winners]

fc.winners2 <- forecast (fit.winners2, newdata = new_data)




#PLOTTING MULTIPLE FORECASTS FROM THE MODEL
plot_list <- list()
for (i in (1:8)) {
  item_ts_train <- item_ts2[1:(nrow(item_ts2)-i*4),] %>% ts()
  item_ts_test <- item_ts2[(end(item_ts_train)[1]+1): (end(item_ts_train)[1]+4),] %>%  ts(start=end(item_ts_train)[1]+1)
  
  
  fit.loop <- tslm(item_ts_train[,1]~ item_ts_train[,remaining_pred], data=item_ts_train)
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
shell("magick convert -delay 150 fc_plot*.png fc_plot4.gif")
file.remove(list.files(pattern="fc_plot...png"))






#Next Item : 3rd-----------------------------------------------------------------------------

item <- top_items$`Item No_`[4]


#filtering sales dataframe for given item
filtered_sales <- sales_df_item %>%
  filter (Period >= '2014-10-01' ) %>% 
  filter(`Item No_` == item) %>% 
  select(-`Item No_`)

filtered_sales[,-1] %>% 
  ts() %>% 
  autoplot()

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

#creating a timeseries object with data
item_ts <- item_df %>% 
  filter( Period >='2016-01-01') %>%
  filter(Period < '2019-04-01') %>%
  select(-Period) %>%
  ts()


#Histogram of Sales Quantity
item_df %>% 
  ggplot( aes(x= Sum_Quantity))+
  geom_histogram(bins=10, binwidth = 300)


#CHECKING AUTOCORRELATION
ggAcf(item_ts_target,lag.max = 60)
ggPacf(item_ts_target,lag.max = 60)
item_lags <- c(4,6,14,16,18)




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

price_promo_lags <- c(1,2,3,6,7,8,9,10,18)


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

promo_lags <- c(1,2,7,8,9,10)


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

school_holiday_lags <- c(seq(1,10),18)



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


bank_holiday_lags <- c(seq(1,10),17)


#Class Sales
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

class_lags <- c(4,12,18)



#creating a df for lagged item sales
lagged_item_sales <- item_df %>%
  select(Period,Sum_Quantity) %>% 
  create_lag_weeks(item_lags, value_col = "Sum_Quantity") 


#creating a df for lagged price promo days (1&8 months lag)
lagged_price_promo_df <- item_df %>%
  select(Period,Num_Days_Price_Promo) %>%  
  create_lag_weeks(price_promo_lags, suffix = "_Price_Promo", value_col = "Num_Days_Price_Promo")

#creating a df for lagged promo days 
lagged_promo_df <- item_df %>%
  select(Period, Num_Days_Promo) %>%
  create_lag_weeks(promo_lags, suffix = "_Promo", value_col = "Num_Days_Promo")

#creating a df for lagged bank holidays
lagged_bank_hol_df <- week_bank_holidays_df %>% 
  select(Period, Num_Holidays) %>% 
  create_lag_weeks(bank_holiday_lags, suffix = "_Holidays", value_col = "Num_Holidays")

#creating a df for lagged school holidays
lagged_school_hol_df <- week_school_holidays_df %>% 
  select(Period, Num_School_Holidays) %>% 
  create_lag_weeks(school_holiday_lags, suffix = "_School_Holidays", value_col = "Num_School_Holidays")



#creating a dataframe with all features
item_df <- item_df %>%
  left_join(lagged_class_df, by = c("Period")) %>%
  left_join(lagged_item_sales, by = c("Period")) %>%
  left_join(lagged_price_promo_df, by = c("Period")) %>%
  left_join(lagged_promo_df, by = c("Period")) %>%
  left_join(lagged_bank_hol_df, by = c("Period")) %>% 
  left_join(lagged_school_hol_df, by = c("Period")) %>% 
  drop_na() 

rownames(item_df) <- NULL

#creating a timeseries object with data
item_ts <- item_df %>% 
  filter( Period >='2016-01-01') %>%
  filter(Period < '2019-04-01') %>%
  select(-Period) %>%
  ts()




item_ts[,1] %>% ggAcf()


#Iterating add and removal of predictors, and checking of residuals 
steps_back <- 16
h=4
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial)
added_pred <- add_predictors(item_ts, c("Num_Business_Days"))
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
for (y in 2015:2017) {
  for (m in 1:12) {
    #for (w in 1:51) {
    item_ts2 <- item_df %>% 
      filter(Period >= paste0(y, "-", m, "-","1")) %>%
      filter(Period < '2019-04-01') %>% 
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



y <- item_ts[,1]
X <- item_ts[,final_winners]


fit.winners3 <- tslm(y~X)
checkresiduals(fit.winners3)

new_data <- item_df %>%
  select(-Sum_Quantity) %>% 
  filter( Period >='2019-04-01') %>%
  filter(Period <= '2019-04-30')

fc.winners3 <- forecast (fit.winners3, newdata = new_data) 




#PLOTTING MULTIPLE FORECASTS FROM THE MODEL
plot_list <- list()
for (i in (1:8)) {
  item_ts_train <- item_ts[1:(nrow(item_ts)-i*4),] %>% ts()
  item_ts_test <- item_ts[(end(item_ts_train)[1]+1): (end(item_ts_train)[1]+4),] %>%  ts(start=end(item_ts_train)[1]+1)
  
  
  fit.loop <- tslm(item_ts_train[,1]~ item_ts_train[,remaining_pred], data=item_ts_train)
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
shell("magick convert -delay 150 fc_plot*.png fc_plot5.gif")
file.remove(list.files(pattern="fc_plot...png"))




#Next Item: 4th-----------------------------------------------------------------------------

item <- top_items$`Item No_`[5]
item_vector <-append(item_vector,item) 

#filtering sales dataframe for given item
filtered_sales <- sales_df_item %>%
  filter (Period >= '2014-10-01' ) %>% 
  filter(`Item No_` == item) %>% 
  select(-`Item No_`)

filtered_sales[,-1] %>% 
  ts() %>% 
  autoplot()

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

#creating a timeseries object with data
item_ts <- item_df %>% 
  filter( Period >='2016-01-01') %>%
  filter(Period < '2019-04-01') %>%
  select(-Period) %>%
  ts()


#Histogram of Sales Quantity
item_df %>% 
  ggplot( aes(x= Sum_Quantity))+
  geom_histogram(bins=10, binwidth = 300)


#CHECKING AUTOCORRELATION
ggAcf(item_ts_target,lag.max = 60)
ggPacf(item_ts_target,lag.max = 60)
item_lags <- c(4,5,6,9,14,16,18)




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

price_promo_lags <- c(1,2,3,6,7,8,9,10,11,18)


#Promo
#Similar but lower results at same lags as price promo
vec <- c()
for( i in (1:20)) {
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
  geom_bar(aes (x= seq(1:20), y=.), stat ="identity")

promo_lags <- c(1,2,6,7,8,9,10,18)


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

school_holiday_lags <- c(seq(1,10),14)



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


bank_holiday_lags <- c(seq(1,10),6,9)


#Class Sales
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

class_lags <- c(4,6,9,10,12)



#creating a df for lagged item sales
lagged_item_sales <- item_df %>%
  select(Period,Sum_Quantity) %>% 
  create_lag_weeks(item_lags, value_col = "Sum_Quantity") 


#creating a df for lagged price promo days (1&8 months lag)
lagged_price_promo_df <- item_df %>%
  select(Period,Num_Days_Price_Promo) %>%  
  create_lag_weeks(price_promo_lags, suffix = "_Price_Promo", value_col = "Num_Days_Price_Promo")

#creating a df for lagged promo days 
lagged_promo_df <- item_df %>%
  select(Period, Num_Days_Promo) %>%
  create_lag_weeks(promo_lags, suffix = "_Promo", value_col = "Num_Days_Promo")

#creating a df for lagged bank holidays
lagged_bank_hol_df <- week_bank_holidays_df %>% 
  select(Period, Num_Holidays) %>% 
  create_lag_weeks(bank_holiday_lags, suffix = "_Holidays", value_col = "Num_Holidays")

#creating a df for lagged school holidays
lagged_school_hol_df <- week_school_holidays_df %>% 
  select(Period, Num_School_Holidays) %>% 
  create_lag_weeks(school_holiday_lags, suffix = "_School_Holidays", value_col = "Num_School_Holidays")




#creating a dataframe with all features
item_df <- item_df %>%
  left_join(lagged_class_df, by = c("Period")) %>%
  left_join(lagged_item_sales, by = c("Period")) %>%
  left_join(lagged_price_promo_df, by = c("Period")) %>%
  left_join(lagged_promo_df, by = c("Period")) %>%
  left_join(lagged_bank_hol_df, by = c("Period")) %>% 
  left_join(lagged_school_hol_df, by = c("Period")) %>% 
  drop_na() 

rownames(item_df) <- NULL

#creating a timeseries object with data
item_ts <- item_df %>% 
  filter( Period >='2016-01-01') %>%
  filter(Period < '2019-04-01') %>%
  select(-Period) %>%
  ts()




#Iterating add and removal of predictors, and checking of residuals 
steps_back <- 16
h=4
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial)
added_pred <- add_predictors(item_ts, c("Num_Business_Days"))
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
for (y in 2015:2017) {
  for (m in 1:12) {
    #for (w in 1:51) {
    item_ts2 <- item_df %>% 
      filter(Period >= paste0(y, "-", m, "-","1")) %>%
      filter(Period < '2019-04-01') %>% 
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







y <- item_ts[,1]
X <- item_ts[,final_winners]

fit.winners4 <- tslm(y~X)
checkresiduals(fit.winners4)

new_data <- item_df %>%
  select(-Sum_Quantity) %>% 
  filter( Period >='2019-04-01') %>%
  filter(Period <= '2019-04-30')

fc.winners4 <- forecast (fit.winners4, newdata = new_data) 




#PLOTTING MULTIPLE FORECASTS FROM THE MODEL
plot_list <- list()
for (i in (1:8)) {
  item_ts_train <- item_ts[1:(nrow(item_ts)-i*4),] %>% ts()
  item_ts_test <- item_ts[(end(item_ts_train)[1]+1): (end(item_ts_train)[1]+4),] %>%  ts(start=end(item_ts_train)[1]+1)
  
  
  fit.loop <- tslm(item_ts_train[,1]~ item_ts_train[,remaining_pred], data=item_ts_train)
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
shell("magick convert -delay 150 fc_plot*.png fc_plot6.gif")
file.remove(list.files(pattern="fc_plot...png"))

#Next Item: 5th-----------------------------------------------------------------------------
  
item <- top_items$`Item No_`[6]

#filtering sales dataframe for given item
filtered_sales <- sales_df_item %>%
  filter (Period >= '2014-10-01' ) %>% 
  filter(`Item No_` == item) %>% 
  select(-`Item No_`)

filtered_sales[,-1] %>% 
  ts() %>% 
  autoplot()

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

#creating a timeseries object with data
item_ts <- item_df %>% 
  filter( Period >='2016-01-01') %>%
  filter(Period < '2019-04-01') %>%
  select(-Period) %>%
  ts()


#Histogram of Sales Quantity
item_df %>% 
  ggplot( aes(x= Sum_Quantity))+
  geom_histogram(bins=10, binwidth = 300)


#CHECKING AUTOCORRELATION
ggAcf(item_ts_target,lag.max = 60)
ggPacf(item_ts_target,lag.max = 60)
item_lags <- c(4,5,6,14,16)



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

price_promo_lags <- c(1,seq(5,14))


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

promo_lags <- c(1,2,3,10,11,14)


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

school_holiday_lags <- c(seq(1,10),14)



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


bank_holiday_lags <- c(seq(1,10))


#Class Sales
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

class_lags <- c(4,8,11)



#creating a df for lagged item sales
lagged_item_sales <- item_df %>%
  select(Period,Sum_Quantity) %>% 
  create_lag_weeks(item_lags, value_col = "Sum_Quantity") 


#creating a df for lagged price promo days (1&8 months lag)
lagged_price_promo_df <- item_df %>%
  select(Period,Num_Days_Price_Promo) %>%  
  create_lag_weeks(price_promo_lags, suffix = "_Price_Promo", value_col = "Num_Days_Price_Promo")

#creating a df for lagged promo days 
lagged_promo_df <- item_df %>%
  select(Period, Num_Days_Promo) %>%
  create_lag_weeks(promo_lags, suffix = "_Promo", value_col = "Num_Days_Promo")

#creating a df for lagged bank holidays
lagged_bank_hol_df <- week_bank_holidays_df %>% 
  select(Period, Num_Holidays) %>% 
  create_lag_weeks(bank_holiday_lags, suffix = "_Holidays", value_col = "Num_Holidays")

#creating a df for lagged school holidays
lagged_school_hol_df <- week_school_holidays_df %>% 
  select(Period, Num_School_Holidays) %>% 
  create_lag_weeks(school_holiday_lags, suffix = "_School_Holidays", value_col = "Num_School_Holidays")



#creating a dataframe with all features
item_df <- item_df %>%
  left_join(lagged_class_df, by = c("Period")) %>%
  left_join(lagged_item_sales, by = c("Period")) %>%
  left_join(lagged_price_promo_df, by = c("Period")) %>%
  left_join(lagged_promo_df, by = c("Period")) %>%
  left_join(lagged_bank_hol_df, by = c("Period")) %>% 
  left_join(lagged_school_hol_df, by = c("Period")) %>% 
  drop_na()

rownames(item_df) <- NULL

#creating a timeseries object with data
item_ts <- item_df %>% 
  filter( Period >='2016-01-01') %>%
  filter(Period < '2019-04-01') %>%
  select(-Period) %>%
  ts()



#Iterating add and removal of predictors, and checking of residuals 
steps_back <- 8
h=4
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial)
added_pred <- add_predictors(item_ts, c("Weeks_After_Hol"))
remaining_pred <- remove_predictors(item_ts[,c("Sum_Quantity",added_pred)])

y <- item_ts[,1]
X <- item_ts[,remaining_pred]
trial <- tslm(y~X, data=item_ts)
checkresiduals(trial)

  
final_winners <- remaining_pred

h=4
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial)
pred <- item_ts[,final_winners]
tsCV(item_ts[,1], my_tslm, xreg=pred, h=h, initial = initial ) %>%
  get_mase_cv(naive = naive_error, h=h)



y <- item_ts[,1]
X <- item_ts[,final_winners]

fit.winners5 <- tslm(y~X)
checkresiduals(fit.winners5)

new_data <- item_df %>%
  select(-Sum_Quantity) %>% 
  filter( Period >='2019-04-01') %>%
  filter(Period <= '2019-04-30')

fc.winners5 <- forecast (fit.winners5, newdata = new_data) 




#PLOTTING MULTIPLE FORECASTS FROM THE MODEL
plot_list <- list()
for (i in (1:4)) {
  item_ts_train <- item_ts[1:(nrow(item_ts)-i*4),] %>% ts()
  item_ts_test <- item_ts[(end(item_ts_train)[1]+1): (end(item_ts_train)[1]+4),] %>%  ts(start=end(item_ts_train)[1]+1)
  
  
  fit.loop <- tslm(item_ts_train[,1]~ item_ts_train[,remaining_pred], data=item_ts_train)
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
shell("magick convert -delay 150 fc_plot*.png fc_plot7.gif")
file.remove(list.files(pattern="fc_plot...png"))











