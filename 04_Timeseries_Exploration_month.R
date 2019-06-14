
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


# Function that creates a dataframe with all periods necessary for tseries, using first and last observations
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

# Function that creates a dataframe with all periods necessary for tseries, using first and last observations
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



# Function that creates a lagged column with the specified lags
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

# Function that creates a lagged column with the specified lags
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

# Vectorized function for extracting weekdays
nweekdays <- Vectorize(function(date1, date2) 
  sum(!wday(seq(date1, date2, "days")) %in% c(6,1)))

# PREPARING DATAFRAME WITH FEATURES THAT WILL BE USED FOR MODELS--------------------------------------------------

# Filtering sales dataframe for given item
# Using second item in list, because first is a seasonal product
filtered_sales <- sales_df_item %>%
  filter(`Item No_` == top_items$`Item No_`[2]) %>% 
  select(-`Item No_`)

# Filtering sales price dataframe for given item, and grouping by date to get sums of promo days, and a mean of price
filtered_sales_price <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>%
  group_by(Period) %>% 
  summarise(Num_Days_Price_Promo = sum(Num_Days_Price_Promo), 
            Num_Days_Promo = sum(Num_Days_Promo),
            Quarter_Low_Price = sum(Quarter_Low_Price),
            Unit_Price =  mean(Unit_Price)) %>%
  ungroup()


# Creating a dataframe with all periods in timespan for given item
period_df <- create_period_df(filtered_sales)


# Creating a dataframe with the months of the seasons
Summer <- c(7,8,9)
Autumn <- c(10,11,12)
Winter <- c(1,2,3)
Spring <- c(4,5,6)

seasons_df <- data.frame(Summer,Autumn,Winter,Spring) %>% gather(key="Season", value="Month")
seasons_df$Season <- factor(seasons_df$Season, levels= c("Winter","Spring","Summer","Autumn"))

# Creating a dataframe with sales grouped by seasons, to be used in exploration only
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

# Creating a dataframe with Bank Holidays
month_bank_holidays_df <- bank_holidays_df %>% 
  mutate(Period = floor_date(start, period) %>%  as_date()) %>% #Creating period
  group_by(Period) %>%
  summarise(Easter = sum(Easter), Num_Holidays= n()) %>% 
  ungroup() %>% 
  mutate(Easter = ifelse (Easter>0,1,0)) %>% 
  right_join(period_df) %>% 
  replace(is.na(.),0)

# Creating a dataframe with school_holidays
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


# Creating a dframe with sales of the class
# excludes sales of the forecast item
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

# Creating a df for lagged class sales
lagged_class_df <- class_sales_df %>%
  mutate(Period= Period %m+% months(+1)) %>% 
  rename(Lag_Class_Sales = Class_Sales)


# Creating a df for lagged item sales
lagged_item_sales <- filtered_sales %>%
  create_period_df(period = period) %>%
  left_join(filtered_sales) %>% 
  replace(is.na(.),0) %>%
  select(Period,Sum_Quantity) %>% 
  create_lag_months(seq(1,12),value_col = "Sum_Quantity")


# Creating a df for lagged price promo days
lagged_price_promo_df <- filtered_sales_price %>%
  select(Period, Num_Days_Price_Promo) %>%
  create_period_df(period = period) %>%
  left_join(filtered_sales_price) %>% 
  select(Period, Num_Days_Price_Promo) %>%
  create_lag_months(seq(1,12),suffix = "_Price_Promo", value_col = "Num_Days_Price_Promo")

# Creating a df for lagged promo days
lagged_promo_df <- filtered_sales_price %>%
  select(Period, Num_Days_Promo) %>%
  create_period_df(period = period) %>%
  left_join(filtered_sales_price) %>% 
  select(Period, Num_Days_Promo) %>%
  create_lag_months(seq(1,12),suffix = "_Promo", value_col = "Num_Days_Promo")

# Creating a df for lagged bank holidays
lagged_bank_hol_df <- month_bank_holidays_df %>% 
  create_period_df(period = period) %>%
  left_join(month_bank_holidays_df) %>%
  select(Period, Num_Holidays) %>%
  replace(is.na(.),0) %>% 
  create_lag_months(seq(1,3),suffix = "_Holidays", value_col = "Num_Holidays")

# Creating a df for lagged school holidays
lagged_school_hol_df <- month_school_holidays_df %>% 
  select(Period, Num_School_Holidays) %>%
  create_period_df(period = period) %>%
  left_join(month_school_holidays_df) %>%
  replace(is.na(.),0) %>% 
  create_lag_months(seq(1,6),suffix = "_School_Holidays", value_col = "Num_School_Holidays")

# Creating a df for lead bank holidays
lead_bank_hol_df <- month_bank_holidays_df %>% 
  create_period_df(period = period) %>%
  left_join(month_bank_holidays_df) %>%
  select(Period, Num_Holidays) %>%
  replace(is.na(.),0) %>% 
  create_lead_months(seq(1,6),suffix = "_Holidays", value_col = "Num_Holidays")

# Creating a df for lead school holidays
lead_school_hol_df <- month_school_holidays_df %>% 
  create_period_df(period = period) %>%
  left_join(month_school_holidays_df) %>%
  select(Period, Num_School_Holidays) %>% 
  replace(is.na(.),0) %>% 
  create_lead_months(seq(1,6),suffix = "_School_Holidays", value_col = "Num_School_Holidays")


# Spreading promo days by customer
promo_spread <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>%
  select(Period, Customer_Code, Num_Days_Promo) %>%
  spread(Customer_Code,Num_Days_Promo) %>% 
  rename_at(vars(-Period), funs(paste0("Days_Promo_", .))) #%>%
#replace(is.na(.), 0)

# Spreading price promo days by customer
price_promo_spread <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>%
  select(Period, Customer_Code, Num_Days_Price_Promo) %>%
  spread(Customer_Code,Num_Days_Price_Promo) %>%
  rename_at(vars(-Period), funs(paste0("Days_Price_Promo_", .))) #%>%
#replace(is.na(.), 0)

# Spreading unit price by customer
unit_price_spread <- full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>%
  filter(year(Period) >= 2015) %>% 
  select(Period, Customer_Code, Unit_Price) %>% 
  spread(Customer_Code, Unit_Price) %>% 
  rename_at(vars(-Period), funs(paste0("Unit_Price_", .))) %>%
  replace(is.na(.), 10)

# Creating a dataframe with all features
item_df <- period_df %>% 
  filter(Period < as_date('2019-05-01')) %>% 
  mutate(Month=month(Period)) %>% 
  left_join(filtered_sales, by = c("Period")) %>% 
  left_join(filtered_sales_price, by = c("Period")) %>%
  left_join(month_bank_holidays_df, by = c("Period")) %>%
  left_join(month_school_holidays_df, by = c("Period")) %>%
  left_join(promo_spread, by = c("Period")) %>%
  left_join(price_promo_spread, by = c("Period")) %>%
  left_join(unit_price_spread, by = c("Period")) %>%
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
  mutate(Num_Business_Days = nweekdays(Period, Period %m+% months(+1)-1) -Num_Holidays) %>% #Creating business days feature
  select(-c("Month","Season"))



# Saving a vector with names of columns that have all zeros
all_zero <- names( item_df [apply(item_df, MARGIN = 2, FUN = function(x) sum(x==0)/nrow(item_df) ==1)] )

# Getting rid of columns that have all zeros
item_df <- item_df %>%
  select(-all_zero)

# Creating a timeseries object with data
item_ts <- item_df %>%
  filter (Period >= '2015-09-01') %>% 
  filter(Period < "2019-04-01") %>% 
  #filter( Period <='2019-01-01') %>% 
  select(-Period) %>% 
  ts(start = c(2015,9), frequency = 12)


# Spreading sales df by item 
# (this will not be included as feature- just for exploration)
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

# Plotting a scatterplot matrix to find correlation between engineered features
# USE ONLY WHEN  NUM OF FEATURES IS LOWER THAN 20 
#item_df %>% 
#  select(-Period) %>%
#  ggpairs()

#CHECKING AUTOCORRELATION
# Plot doesn´t show anything interesting
gglagplot(item_ts[,1])

# Lag 2 shows high correlation, could make up for a good predictor
ggAcf(item_ts[,1])



# LOOKING FOR SEASONALITY----------------------------------------------------------

# Seems like there is no monthly seasonality
ggseasonplot(item_ts[,"Sum_Quantity"], polar = TRUE)
ggsubseriesplot(item_ts[,"Sum_Quantity"])

# From previous plots it looks like there could be quarterly seasonality that follows natural seasons
# Let´s plot sales by quarters:
season_sales %>% 
  select (Sum_Quantity) %>% 
  ts(start = c(2015,2), frequency = 4) %>% 
  ggseasonplot(polar = TRUE)


season_sales %>% 
  select (Sum_Quantity) %>% 
  ts(start = c(2015,2), frequency = 4) %>% 
  autoplot(series="Quarterly") + 
  autolayer(item_ts[,1], series="Monthly")


# Let´s take a look at a boxplot of seasons
# From the boxplot it looks like grouping seasons by months works doesn´t work too badly
# Any way This boxplot doesn´t help much because it has so little points
season_sales %>%
  filter(Year < 2019) %>% 
  ggplot(aes(x=Season, y=Sum_Quantity, fill=Season)) + 
  geom_boxplot()

# Let´s try a scatterplot better
# Nothing really conclusive can be said yet, except that summer is certainly a peak season
season_sales %>% 
  filter(Year < 2019) %>% 
  ggplot(aes(x=Season, y=Sum_Quantity,label=Year, color=Year)) + 
  geom_point()+
  geom_text(aes(label=Year),hjust=1, vjust=-0.7)


# Let´s take a look at a boxplot of months
# March has a great dispersion, but April does not, so it doesn´t look like an Easter effect
Sys.setlocale("LC_ALL","English")
filtered_sales %>%
  filter(year(Period) > 2015) %>% 
  mutate(Month = lubridate::month(Period, label =TRUE)) %>%
  ggplot(aes(x=Month, y=Sum_Quantity, fill=Month)) +
  geom_boxplot()

# Same, scatterplot for months
# this just leaves clear that the previous boxplot gives no conclusive insights-
#- except that autumn looks like the most coherent season, and that months do have some difference in levels,
#- but not the same ones among years.
# So, creating a feature for month could help a bit, but it doesn´t look like too much -
#- here treating outliers would be a good idea
filtered_sales %>%
  filter(year(Period) > 2016) %>%
  mutate(Month = lubridate::month(Period, label =TRUE)) %>%
  ggplot(aes(x=Month, y=Sum_Quantity, color= year(Period))) + 
  geom_point(aes(size=0.2))

# Histogram of sales per month
# Doesn´t help much
filtered_sales %>%
  filter(year(Period) > 2015) %>% 
  mutate(Month = month(Period)) %>%
  ggplot(aes(x=Sum_Quantity)) + 
  geom_histogram(binwidth = 1000) +
  facet_wrap(~Month )

# A smoothed timeseries with a moving average of order 4 shows that there is certain seasonality
# Not too orthodox
ma(item_ts[,1], 4) %>% autoplot(series="Monthly Smoothed",size=2) +
  autolayer(item_ts[,1], series="Monthly")

# Plotting, monthly sales, smoothed monthly, and quarter on same plot
season_sales %>% 
  select (Sum_Quantity) %>% 
  ts(start = c(2015,2), frequency = 4) %>% 
  autoplot(series="Quarterly") + 
  autolayer(item_ts[,1], series="Monthly") +
  autolayer(ma(item_ts[,1], 4), series="Monthly Smoothed", size=2)

season_sales %>% 
  select(Sum_Quantity) %>% 
  ts(start= c(2015,1), frequency = 4) %>% 
  ggseasonplot(polar=TRUE)

season_sales %>% 
  select(Sum_Quantity) %>% 
  ts(start= c(2015,1), frequency = 4) %>% 
  ggseasonplot()

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

# All three follow a similar pattern, although in 2018, class sales seems to lead a month or two, 
#- and thus, brand sales has a rounder peak in 2018
# Also it does look like the market has adjusted a bit,
#- and we could expect that peaks and troughs look more stable in the future
# Bear in mind that neither brand or class sales have forecast item in their data!
# From class sales, we can see that the brand pattern is not extensible to all items, 
#- so we should be careful when forecasting other items
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
# Seems like top scorer is 60 days by far
pg_df %>% head(5)

# And plotting 60 days in a polar seasonal plot
# Perhaps this makes more sense when looking at weekly data
item_df %>%
  mutate(Bimonthly = lubridate::floor_date(Period, "2 months")) %>%
  group_by(Bimonthly) %>%
  summarise(Sum_Quantity = sum(Sum_Quantity)) %>%
  select(-Bimonthly) %>%
  ts(start = c(2015,1), frequency = 6) %>%
  ggseasonplot(polar=TRUE)

# When plotted in a line chart it is not too clear there is bimonthly seasonality
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

#creating a faceted gantt chart for both promo and promo price
#First important conclusion: features related to promotions and customers CB00, and MT00 are useless
#CB00 and BL00 are promotionally very active, features giving info related to this could be very helpful
rbind(promo_df, price_promo_df) %>%    
  ggplot(aes(x=Date, y=Customer_Code, color=Customer_Code, group=`Sales Code`)) +
  geom_line(size = 10) +
  labs(x="", y=NULL, title="Promotion Timeline") +
  facet_grid(Type ~ .) +
  scale_color_manual(values=huey)

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


# NEXT STEP, LET´S LOOK FOR RELATIONS AMONG ITEMS -------------------------------------------------
# Because there are so many, it will be more practical to use ggpairs
spread_sales_df %>% 
  select(-Period) %>%
  ggpairs()

# Lagged_trial with items-- j
# Just trying to see if lagged+/-1 target item has any relation with other items
# No strong correlation
spread_sales_df %>% select(Period, R240271) %>% #taking only period and item
  mutate( Period= Period %m+% months(+1)) %>% #shifting period back a month
  right_join(spread_sales_df, by= c("Period")) %>% #joining with original dataframe
  select(-Period) %>% 
  ggpairs()

ggAcf(item_ts[,1])

# Finding items that correlate most with other items
round(spread_sales_df[,2:ncol(spread_sales_df)] %>% cor, 2) %>% colMeans()

# Finding correlation among sales of  item classes
# Surprise: the sales of the class of the item (excluding its own sales) has a high correlation to its sales
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

# Let´s take a look at autocorrelation in class sales- this could make a good predictor
ggAcf(class_sales_df$Class_Sales)
ggPacf(class_sales_df$Class_Sales)


# Finding correlation in a class... most have a pretty clear positive correlation, except for those that sell few items
# Strong class are clearly 750 and 250
# Most probably letting the model see the sales of the class will help improving it
# Remember to filter out the sales of the item from class sales variable
sales_df_item %>% 
  left_join(class_key_df, by =c("Item No_"="No_")) %>%
  inner_join(top_items) %>%
  filter(Class == "750ML") %>% 
  select(-c(Class, sum_sales)) %>%
  spread(`Item No_`,Sum_Quantity) %>% select (-Period) %>%  
  replace(is.na(.), 0) %>%
  ggpairs()


#NEXT STEP CHECKING RELATION BETWEEN SALES TO RETAIL CHAINS... ------------------------------
item_df %>% 
  select(Sum_Quantity, colnames(item_df) [str_detect (colnames(item_df), "Unit_Price")]) %>% 
  ggpairs


#It looks like DL, CP, CB and TR sales have a solid relation them (a threshold of ca.0.7)
#On the other hand, BL, KF, MT have no relation among them or others
#Here it would be really interesting to see if there is any relation between lagged values
customer_sales_df %>%
  select(-Period) %>% ggpairs()

