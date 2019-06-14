
library (dplyr)
library (tidyr)
library (lubridate)
library (tcR)
library (stringr)
library (stringi)
library (varhandle)
library (data.table)
library (ggplot2)

# DECLARING FUNCTIONS --------------------------------------------------

# Defining "Global" Variable
period <- "month"

# Defining a function for extracting starting date (only available day&month) from description"
date_cleaner <- function(x) {
  rev_x <- reverse.string(x)
  rev_x_split <- str_split(rev_x,"")[[1]]
  
  char_count <- 0
  dot_count <- 0
  dot_index <- 0
  for (char in rev_x_split) {
    char_count <- char_count + 1
    if (char == ".") {
      dot_count <- dot_count + 1
      dot_index <- char_count
    }
    if (dot_count == 4 | char_count > 18) {break}
  }
  init_date_index <- (nchar(x) - dot_index)-1
  return (substr(x,init_date_index,init_date_index + 4))
}

# Defining a function for extracting substring from description.Admits vector of substrings
extract_n_element <- function(string,index=1,sep=" ") {
  element <- strsplit(string,sep)[[1]] [index]
  return (element)
}

# Defining a function to expand the rows of a df by the number of periods in a time interval
# STEPS
# 1. Get number of days: difftime(`Ending Date`,`Starting Date`)
# 2. Get ceiling date for Starting date : ceiling_date(`Starting Date`)
# 3.a IF ceiling date - starting date > number of days then:
#       Fill "number of promo days" with "number of days"
#       Break loop 
# 3.b ELSE: 
#       Fill "number of promo days" with (ceiling date - starting date)
#       Set starting date as ceiling date
#       Restart loop
#Number of days - (ceiling date - starting date)
expand_time_int <- function(df, start_col, end_col, line_col, n_days_col, period= "month") {
  init_last_row <- nrow(df)
  new_row_count <- 0
  for (row in 1:init_last_row) {
    
    starting_date <- df[row,start_col]
    ending_date <- df[row,end_col]
    original_starting_date <- df[row,start_col]
    line_count <- 0
    
    repeat {
      num_days <- difftime(ending_date,starting_date) +1
      end_period <- ceiling_date(starting_date,period)
      diff_end_period <- difftime(end_period,starting_date)
      promo_days <- 0
      
      if (diff_end_period >= num_days) {
        promo_days <- num_days
        line_count <- line_count + 1
        
        if (line_count == 1) { #checking if there is only one period unit in promo
          df[row, line_col] <- line_count
          df[row, n_days_col] <- promo_days
        } else {
          new_row_count <- new_row_count + 1
          df[init_last_row + new_row_count,] <- df[row,] #Creating new empty row
          df[init_last_row + new_row_count, line_col] = line_count
          df[init_last_row + new_row_count, n_days_col] = promo_days
        }
        break # end loop if first condition statement is true
        
        
      } else {
        promo_days <- diff_end_period
        line_count <- line_count + 1
        starting_date <- end_period #changing starting date for next loop
        
        if (line_count == 1) { #checking if it is the original row that needs including promo days
          df[row, line_col] <- line_count
          df[row, n_days_col] <- promo_days
        } else {
          new_row_count <- new_row_count + 1
          df[init_last_row + new_row_count,] <- df[row,] #Creating new empty row
          df[init_last_row + new_row_count, line_col] <- line_count
          df[init_last_row + new_row_count, n_days_col] <- promo_days
          
        }
      }
    }
  }
  return (df)
}   



#LOADING DATA --------------------------------------------------------------------------------------------------

#Reading csv that contains info about promotion campaigns 
query_campaign_copy <- data.table::fread("Data_Files\\Promo_Campaigns.csv", sep=";",stringsAsFactors = FALSE) %>%
  as.data.frame() %>% 
  mutate(`Starting Date`= as_date(`Starting Date`),
         `Ending Date`= as_date(`Ending Date`),
         `Last Date Modified`= as.POSIXct(`Last Date Modified`))

#Table with information about the customers to whom the items were shipped to 
query_ssl_copy <- data.table::fread("Data_Files\\Sales_Line.csv", sep=";",stringsAsFactors = FALSE) %>%
  as.data.frame() %>% 
  mutate(`Shipment Date`= as.POSIXct(`Shipment Date`))

query_sales_price_copy <- data.table::fread("Data_Files\\Sales_Price.csv", sep=";",stringsAsFactors = FALSE) %>%
  as.data.frame() %>% 
  mutate(`Starting Date`= as.POSIXct(`Starting Date`))

#CLEANING QUERY_CAMPAIGN ---------------------------------------------------------------------------------------

#THIS STEP EXTRACTS STARTING PROMO DATE AND CUSTOMER FROM  CAMPAIGN DESCRIPTION
# For some reason, the custom functions do not always return the same output using lapply or mutate in dplyr, or rowwise, 
# so I will just use a simple loop for each row, which works fine
# Second command in for loop checks checks if first element in description is numeric, this is done because input pattern changes
campaign_df <- query_campaign_copy %>% 
  mutate(Starting_Date_Promo = NA) %>% 
  mutate(Customer_Extr = NA) 



for (row in 1:nrow(campaign_df)) {
  campaign_df[row,"Starting_Date_Promo"] <- date_cleaner(campaign_df[row,"Description"])
  campaign_df[row,"Customer_Extr"] <- ifelse (!check.numeric(substr(extract_n_element(campaign_df[row,"Description"]),2,2)),
                                                extract_n_element(campaign_df[row,"Description"]),
                                                extract_n_element(campaign_df[row,"Description"],index=2))
}

# there are a few cases that haven´t returned a date, simply because they don´t exist in description,
# so we´ll have to handle that later



# now adding year to the date we have extracted and formatting to date ymd
campaign_df <- campaign_df %>%
  mutate(Starting_Date_Promo = gsub("\\.", "-", Starting_Date_Promo)) %>% 
  mutate(Starting_Date_Promo = paste(Starting_Date_Promo, year(`Starting Date`), sep="-")) %>% 
  mutate(Starting_Date_Promo = dmy(Starting_Date_Promo)) %>% 
  rename(Customer_Code = Customer_Extr)


# now checking that dates are originally OK (starting date of promo prices should be previous to starting date of promos),
# and handling them if not
# also doing some final changes before next steps
campaign_df <- campaign_df %>%
  mutate(Check = campaign_df$`Starting Date` <= campaign_df$Starting_Date_Promo) %>% #finding out if promo price starts before promo (normal situation)
  mutate(Starting_Date_Promo = ifelse(Check == 'FALSE' & month(Starting_Date_Promo) == 1, #if not promo price is after and promo happens in january
                                      Starting_Date_Promo +years(1),
                                      Starting_Date_Promo)) %>%
  mutate (Starting_Date_Promo = as_date(Starting_Date_Promo)) %>%  #making sure dates are in date format
  mutate (`Starting Date` = as_date(`Starting Date`)) %>% 
  mutate (`Ending Date` = as_date(`Ending Date`)) %>%
  select (-Check)
  
campaign_df <- campaign_df %>%   
  rename (Starting_Date_Price_Promo = `Starting Date`) %>% 
  rename (Ending_Date_Price_Promo = `Ending Date`) %>% 
  mutate (Line_Price_Promo = 0) %>%                       #including column for "expanding" promos later
  mutate (Num_Days_Price_Promo = 0) %>%                   #including column for "expanding" promos later
  mutate (Period_Price_Promo = Starting_Date_Price_Promo) #including column for "expanding" promos later

campaign_df_2 <- campaign_df %>% 
  select(No_,Customer_Code, Starting_Date_Promo, Ending_Date_Price_Promo) %>% 
  mutate (Line_Promo = 0) %>%                       #including column for "expanding" promos later
  mutate (Num_Days_Promo = 0) %>%                   #including column for "expanding" promos later
  mutate (Period_Promo = Starting_Date_Promo) %>%        #including column for "expanding" promos later
  mutate(Starting_Date_Promo = as_date(Starting_Date_Promo)) %>% 
  filter(!is.na(Starting_Date_Promo)) #in case there is any row where Date extraction failed (i.e. some DNC promos)


#saving variables for expanding df by promotion price interval
line_col <- "Line_Price_Promo"
num_days_col <- "Num_Days_Price_Promo"
starting_date_col <- "Starting_Date_Price_Promo"
ending_date_col <- "Ending_Date_Price_Promo"

#expanding campaign_df (promotion price intervals)
campaign_df <- expand_time_int(campaign_df, starting_date_col, ending_date_col, line_col, num_days_col, period) %>%
  arrange(No_,desc(Starting_Date_Price_Promo)) %>% #arranging to make inspection easier
  mutate(Period_Price_Promo = Starting_Date_Price_Promo %m+% months(Line_Price_Promo-1)) %>% #creating column with date of the promo´s row
  mutate(Period = floor_date(Period_Price_Promo,period)) %>% #Creating a period column for later joins
  mutate(Period = as_date(Period)) %>% 
  select(Period, everything())

#repeating previous expansion steps for campaign df (promotion intervals)
line_col <- "Line_Promo"
num_days_col <- "Num_Days_Promo"
starting_date_col <- "Starting_Date_Promo"
ending_date_col <- "Ending_Date_Price_Promo"

#expanding campaign_df_2 (promotion intervals)
campaign_df_2 <- expand_time_int(campaign_df_2, starting_date_col, ending_date_col, line_col, num_days_col, period) %>% 
  arrange(No_,desc(Starting_Date_Promo)) %>%
  mutate(Period_Promo = Starting_Date_Promo %m+% months(Line_Promo-1)) %>%
  mutate(Period = floor_date(Period_Promo,period)) %>%
  mutate(Period = as_date(Period)) %>% 
  select(Period, everything()) %>% 
  select(-Period_Promo) #unnecessary column



# JOINING EXPANDED PROMOTION PRICE AND PROMOTION DFs--------------------------------------------------



campaign_df <- campaign_df %>% 
  left_join(campaign_df_2, by=c("No_", "Period","Starting_Date_Promo", "Ending_Date_Price_Promo", "Customer_Code")) 


# CLEANING QUERY_SALES_PRICE AND JOINING WITH FULLY CLEANED CAMPAIGN DF----------------------------------------------------



# Changing SALES TYPE variable to verbose, and filtering --------------------------------------------------------
sales_price_df <- query_sales_price_copy %>% 
  mutate(`Sales Type` = ifelse(`Sales Type` == 3, "Promotion", `Sales Type`)) %>% 
  mutate(`Sales Type` = ifelse(`Sales Type` == 0, "Customer", `Sales Type`)) %>%
  mutate(`Sales Type` = ifelse(`Sales Type` == 1, "Customer Group", `Sales Type`)) %>% 
  filter(`Sales Type` == "Promotion") 

#joining sales_price with campaign
joined_sales_price <- sales_price_df %>% 
  left_join(campaign_df ,by= c("Sales Code" = "No_")) %>%
  inner_join(top_items) %>% #inner join with top item selection
  select(-sum_sales) %>% 
  rename(Unit_Price_Set = `Unit Price`) %>% # renaming to avoid confusion with next df join
  filter(`Starting Date` >= '2015-01-01')


#- SOLVING "STRUCTURAL" PROBLEMS IN DATA!!: 

# Base price is sometimes not available, --------------------------------------------------------------------
# so it will be necessary to fill empty spaces between promotional prices using another source:
# sales shipment line: where we see the actual sales price of every operation 
#the approach is to group by period and get max price for it, thus getting the base price when there are no promotions
ssl_df <- query_ssl_copy %>% 
  filter(!stri_startswith_fixed(`Customer Price Group` ,"mystery")) %>% # only interested in sales in slovakia
  filter(stri_startswith_fixed(`No_` ,"R24")) %>% # keeping only the brand (it has distinct prefix)
  filter(!`Customer Price Group` %in% c("GASTRO")) %>% # only interested in retail chains, not HORECA
  filter(!`Customer Price Group` == "ZAKL") %>% # excluding "deprecated" customers
  left_join(customer_key_df, by = c("Sell-to Customer No_" = "Original_Code")) %>%  
  filter(!is.na(Customer_Code)) %>% #removing nas from Customer Code (Customers that are not in list, and thus, are not clients)
  group_by(Period = floor_date(`Shipment Date`,period), Customer_Code, No_) %>%
  summarise(Unit_Price_Sold = max(`Unit Price`)) %>% 
  ungroup() %>% 
  mutate(Period = as_date(Period))

# full join of sales line and sales price, using period as key
full_joined_sales_price <- ssl_df %>% #3808
  full_join(joined_sales_price, by = c("Period" = "Period",
                                       "Customer_Code" = "Customer_Code",
                                       "No_" = "Item No_"))

# Cleaning duplicated promo rows:
# Because some customers have multiple rows, there are campaign duplicates after join
# (example: cba may have promo prices for different units for the same item at the same month)
# approach is to identify duplicates, and and keep only those rows that have most promo days
duplicated_promo <- full_joined_sales_price %>%
  group_by(Period,No_,Customer_Code) %>%
  filter(n()>1) %>% #keeps duplicated rows, taking in account only groupby
  ungroup() 

# saving vector of column names to rearrange them for rbind later
rearrange_columns <- colnames(full_joined_sales_price) 

# creating a dataframe with only unique promos (taking max num of promo days)
unique_promo <- duplicated_promo %>%
  group_by(Period,No_,Customer_Code) %>%
  summarise(Num_Days_Price_Promo = max(Num_Days_Price_Promo)) %>%
  ungroup() %>% 
  inner_join(duplicated_promo) %>% 
  select(one_of(rearrange_columns))

# taking out all duplicated promo rows, to perform full join with previously saved df
full_joined_sales_price <- full_joined_sales_price %>% #3611
  group_by(Period,No_,Customer_Code) %>%
  filter(!n()>1) %>% #keeps duplicated rows, taking in account only groupby
  ungroup() 

# stacking full joined (without all duplicates) and unique #3708
full_joined_sales_price <- rbind(full_joined_sales_price,unique_promo)
  


# PREPARING FEATURES -------------------------------------------------------------------------------------

full_joined_sales_price <- full_joined_sales_price %>% #3706
  mutate(Unit_Price = ifelse (is.na(Unit_Price_Set),
                              Unit_Price_Sold,
                              Unit_Price_Set)) %>%
  mutate(Num_Days_Price_Promo = ifelse(is.na(Num_Days_Price_Promo), #replacing NAs with 0s (...) 
                                       0,Num_Days_Price_Promo)) %>%
  mutate(Num_Days_Promo = ifelse(is.na(Num_Days_Promo),
                                 0,Num_Days_Promo)) %>%
  mutate(Line_Price_Promo = ifelse(is.na(Line_Price_Promo),
                                       0,Line_Price_Promo)) %>%
  mutate(Line_Promo = ifelse(is.na(Line_Promo),
                                   0,Line_Promo)) %>% # (...) replacing NAs with 0s
  mutate(Quarter_Low_Price = grepl("DNC",Description)) %>%  # setting Quarter_Low_Price feature
  mutate(Quarter_Low_Price = ifelse(Quarter_Low_Price == "TRUE", 1, 0)) %>% 
  select(Period, No_, Customer_Code, Unit_Price,  #just reordering columns
         Quarter_Low_Price, Num_Days_Price_Promo, Line_Price_Promo, 
         Num_Days_Promo, Line_Promo) %>% 
  arrange(No_, Customer_Code, Period) %>% 
  distinct()




#full_joined_sales_price %>% group_by(No_) %>% summarise(Init_Date = min(Period)) %>% View()

#NEXT STEPS:
#-----DONE! check if there is any other unit other than KS in Limenita ()
#-----DONE! to build timeseries, it will be necessary to find the latest non-promotional price for the customer, 
#           and make sure the ts has it when there is no promotion
#-----DONE!create a procedure that extracts the months (or weeks) in a timespan, and the number of promotion price days for that month
#-----DONE!Once promotion periods are "spread"try a full join with ssl_df in order to get non promotional prices for the remaining months
#-----DONE!fill empty values in Salesperson Code
#-----DONE!Get rid of promotional prices from ssl_df (use df of promotional prices w/ customer and period)
#-----DONE!Get rid of duplicate promotions due to several operators for one customer (i.e, CBA,COOP,TERNO)
#-----DONE!Clean price feature --> make two columns, one: baseline price, one promo price
#-----                    --> with baseline price put NAs where price = to promo price(same row)
#-----                    --> then replace NAs with previous value (use library zoo, function na.locf(object, fromlast=TRUE))
#-----DONE!Study if it is worth to input DNC as a feature-- it really is not a promotion, just low prices
#-- Find out WHY there were duplicates in full_joined sales price
#-----DONE! Find out if there is a better source to get prices (or dates for them) than ssl (it might be possible that shipment date is not best choice)


#GRAPHS--------------------------------------------------------------------------------------------------

#Exploring prices for different items

full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[2]) %>% 
  ggplot (aes(x=Period,y=Unit_Price)) +
    geom_line(aes(colour = Customer_Code)) +
    facet_wrap(~ Customer_Code, scales = 'free_y', ncol = 1)

full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[3]) %>% 
  ggplot (aes(x=Period,y=Unit_Price)) +
  geom_line(aes(colour = Customer_Code)) +
  facet_wrap(~ Customer_Code, scales = 'free_y', ncol = 1)

full_joined_sales_price %>% 
  filter(No_ == top_items$`Item No_`[4]) %>% 
  ggplot (aes(x=Period,y=Unit_Price)) +
  geom_line(aes(colour = Customer_Code)) +
  facet_wrap(~ Customer_Code, scales = 'free_y', ncol = 1)

