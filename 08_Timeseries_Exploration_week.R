
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



#vectorized function for extracting weekdays
nweekdays <- Vectorize(function(date1, date2) 
  sum(!wday(seq(date1, date2, "days")) %in% c(6,1)))



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

#creating a timeseries object with data
item_ts <- item_df %>% 
  filter( Period >='2016-01-01') %>%
  filter(Period < '2019-04-01') %>%
  select(-Period) %>%
  ts()





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
  drop_na() %>% 
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




#Lag 1&2 show high autocorrelation, Lag16 could be significant too
ggAcf(item_ts[,1],lag.max = 60)
# PACF shows interesting spikes at lags 1,2, 14, 17
ggPacf(item_ts[,1],lag.max = 60)
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
  geom_line(aes(x = Week, y = Year_2015, color= "2015")) +
  geom_line(aes(x = Week, y = Year_2016, color= "2016")) +
  geom_line(aes(x = Week, y = Year_2017, color= "2017")) +
  geom_line(aes(x = Week, y = Year_2018, color= "2018")) +
  geom_line(aes(x = Week, y = Year_2019, color= "2019")) +
  xlab('WEEK') +
  ylab('SALES')


#From the boxplot it looks like there are specific weeks with little variance, 
#they don´t exactly match with patterns seen before, but most are close
item_df %>%
  filter(year(Period) >2015) %>% 
  select(Period,Sum_Quantity) %>%
  mutate(Week = lubridate::week(Period)) %>%
  ggplot(aes(x=Week, y=Sum_Quantity, group=Week)) +
  geom_boxplot(fill= huey[1])

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
  geom_line(aes(x = Week, y = Year_2015, color= "2015")) +
  geom_line(aes(x = Week, y = Year_2016, color= "2016")) +
  geom_line(aes(x = Week, y = Year_2017, color= "2017")) +
  geom_line(aes(x = Week, y = Year_2018, color= "2018")) +
  geom_line(aes(x = Week, y = Year_2019, color= "2019")) +
  xlab('WEEK') +
  ylab('SALES') +
  geom_vline(data = holidays, aes(xintercept = Week), color="dark grey", size = 0.2)


#Better to see holidays year by year
#And generally the weeks after and before holidays there are peaks, while weeks with holidays account for troughs
year_colors <- c("#fae205", "#ff8c18", "#ff0759", "#ca0092", "#141fb5")
years <- seq(2015,2019)

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
    ylab('SALES') +
    ggtitle(years[y])
  
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
    ylab('SALES') +
    ggtitle(years[y])
  print(plot)
}





# Another approach to finding seasonality: Fourier transformations

# Periodogram returns a list of frequencies at which the observations were vectorized, and the power of these
pg_list <- item_df %>% select(Sum_Quantity) %>% periodogram()

# Creating a dataframe with the results from previous step
pg_df <- data.frame(Frequency=pg_list$freq, Power=pg_list$spec) %>%
  mutate(Days = 1/Frequency) %>% #transforming frequency to unit days
  arrange(desc(Power))

# Checking top 3 power frequencies
# Seems like top scorer is 243 Days days by far, but this really doesn´t make much sense
pg_df %>% head(10)








#LET´S TAKE A LOOK AT WHAT PROBABLY CAUSES IT: PROMOTIONS

#Peaks in number of promotional days usually explain peaks in sales, however, this does not always happen

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
    geom_line (aes(x=Week, y=Value), color=year_colors[y], size =1) +
    facet_grid(rows = vars(Key), scales = "free")  +
    ggtitle(years[y])
  print (plot)
  
}





# Let´s see how the number of promo days per week has evolved along years
# Number of promotional days has been shifting a bit along years

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

