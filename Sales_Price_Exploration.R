#install.packages("tcR")
library(dplyr)
library (tidyr)
library (lubridate)
library(tcR)
library (stringr)
library (stringi)
library (ggplot2)

# defining a function that will be used to extract starting date (only available day&month) from description"
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

# for some reason, the function does not always return the same output using lapply or mutate in dplyr, 
# so I will just use a simple loop for each row, which works fine
campaign_df <- query_campaign %>% 
  mutate(Promo_Starting_Date = Description)

for (row in 1:nrow(campaign_df)) {
  campaign_df[row,"Promo_Starting_Date"] <- date_cleaner(campaign_df[row,"Promo_Starting_Date"])
}
# there are a few cases that haven´t returned a date, simply beacause they don´t exist in description,
# so we´ll have to handle that later

# now adding year to the date we have extracted and formatting to date ymd
campaign_df <- campaign_df %>%
  mutate(Year = year(campaign_df$`Starting Date`)) %>% 
  mutate(Promo_Starting_Date = gsub("\\.", "-", Promo_Starting_Date)) %>% 
  mutate(Promo_Starting_Date = paste(Promo_Starting_Date, Year, sep="-")) %>% 
  mutate(Promo_Starting_Date = dmy(Promo_Starting_Date))


# now checking that dates are originally OK (starting date of promo prices should be previous to starting date of promos),
# and handling them if not
# also doing some final changes before next steps
campaign_df <- campaign_df %>%
  mutate(Check = campaign_df$`Starting Date` <= campaign_df$Promo_Starting_Date) %>% 
  mutate(Promo_Starting_Date = ifelse(Check == 'FALSE' & month(Promo_Starting_Date) == 1,
                                      Promo_Starting_Date +years(1),Promo_Starting_Date)) %>%
  mutate (Promo_Starting_Date = as_date(Promo_Starting_Date)) %>%  #making sure dates are in date format
  mutate (`Starting Date` = as_date(`Starting Date`)) %>% 
  mutate (`Ending Date` = as_date(`Ending Date`)) %>%
  select (-Check) %>% 
  rename ( Price_Promo_Starting_Date = `Starting Date`) %>% 
  rename ( Price_Promo_Ending_Date = `Ending Date`) %>% 
  mutate (Line = 0) %>%                             #including column for "spreading" promos later
  mutate (Num_Promo_Days = 0) %>%                   #including column for "spreading" promos later
  mutate (Period_Date = Price_Promo_Starting_Date)  #including column for "spreading" promos later


# CREATING A ROW PER PERIOD (MONTH INITIALLY) FOR EVERY PROMOTION, AND FINDING NUMBER OF PROMO DAYS IN EACH PERIOD
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

campaign_df_copy <- campaign_df

init_last_row <- nrow(campaign_df)
new_row_count <- 0
for (row in 1:init_last_row) {
  
  starting_date <- campaign_df[row,"Price_Promo_Starting_Date"]
  ending_date <- campaign_df[row,"Price_Promo_Ending_Date"]
  original_starting_date <- campaign_df[row,"Price_Promo_Starting_Date"]
  line_count <- 0
  
  repeat {
    num_days <- difftime(ending_date,starting_date) +1
    end_period <- ceiling_date(starting_date,"month")
    diff_end_period <- difftime(end_period,starting_date)
    promo_days <- 0
    
    if (diff_end_period >= num_days) {
      promo_days <- num_days
      line_count <- line_count + 1
      
      if (line_count == 1) { #checking if there is only one period unit in promo
        campaign_df[row, "Line"] = line_count
        campaign_df[row, "Num_Promo_Days"] = promo_days
      } else {
        new_row_count <- new_row_count + 1
        campaign_df[init_last_row + new_row_count,] <- campaign_df[row,] #Creating new empty row
        campaign_df[init_last_row + new_row_count, "Line"] = line_count
        campaign_df[init_last_row + new_row_count, "Num_Promo_Days"] = promo_days
      }
      break # end of check if repeat condition
      
      
    } else {
      promo_days <- diff_end_period
      line_count <- line_count + 1
      starting_date <- end_period #changing starting date for next loop
      
      if (line_count == 1) { #checking if it is the original row that needs including promo days
        campaign_df[row, "Line"] = line_count
        campaign_df[row, "Num_Promo_Days"] = promo_days
      } else {
        new_row_count <- new_row_count + 1
        campaign_df[init_last_row + new_row_count,] <- campaign_df[row,] #Creating new empty row
        campaign_df[init_last_row + new_row_count, "Line"] = line_count
        campaign_df[init_last_row + new_row_count, "Num_Promo_Days"] = promo_days
        
      }
    }
  }
}

#final changes on data frame
campaign_df <- campaign_df %>% 
  arrange(No_,desc(Price_Promo_Starting_Date)) %>%
  mutate(Period_Date = Price_Promo_Starting_Date %m+% months(Line-1)) %>% #creating column with date of the promo´s row
  mutate(Month = month(Period_Date))  


#------------------------------------------------------------------------------------------------  


# Now preparing to join sales price table with campaign
sales_price_df <- query_sales_price %>% 
  mutate(`Sales Type` = ifelse(`Sales Type` == 3, "Promotion", `Sales Type`)) %>% 
  mutate(`Sales Type` = ifelse(`Sales Type` == 0, "Customer", `Sales Type`)) %>%
  mutate(`Sales Type` = ifelse(`Sales Type` == 1, "Customer Group", `Sales Type`)) 

joined_sales_price <- sales_price_df %>% 
  left_join(campaign_df ,by= c("Sales Code" = "No_")) %>%
  inner_join(top_items) %>%
  select(-sum_sales) %>% 
  mutate(Year = year(`Starting Date`)) %>% 
  mutate(Month = month(`Starting Date`)) %>%
  unite(Year_Month,Year,Month, sep="-") %>%
  rename(Unit_Price_Set = `Unit Price`) %>% 
  #inner join with top item selection
  filter(`Starting Date` >= '2015-01-01')# %>% 
filter(stri_startswith_fixed(`Item No_`,"R240271")) %>% 
  #filter(`Salesperson Code` == "CBA" | `Sales Code` %in% c('CBA'))
  filter(`Salesperson Code` == "BILLA" | `Sales Code` %in% c('7000','9000','BILLA'))
#filter(`Sales Type` %in% c('Customer', 'Customer Group'))

#--------------------------------------------------------------------------------------------------------------------------
#There is a problem: the base price is sometimes not available, 
#so it will be necessary to fill empty spaces between promotional prices using some other source

#Exploring other sources to see prices
#this source has the unit price for every single sale that was made
#the idea is to group by period and get max price for it, thus getting the base price when there are no promotions
ssl_df <- query_ssl %>% 
  filter(!stri_startswith_fixed(`Customer Price Group` ,"MEDIST")) %>%
  filter(!`Customer Price Group` %in% c("GASTRO")) %>%
  filter(stri_startswith_fixed(`No_` ,"R24")) %>%
  group_by(Year = year(`Shipment Date`), Month = month(`Shipment Date`), `Customer Price Group`, No_) %>% 
  summarise(Unit_Price_Sold = max(`Unit Price`)) %>% 
  ungroup() %>%
  unite(Year_Month,Year,Month, sep="-") #%>% 
#filter(stri_startswith_fixed(`No_`,"R240371"))# %>%   
#filter(`Customer Price Group`== 'BILLA')


# full join of sales line and sales price, using period as key
full_joined_sales_price <- ssl_df %>% 
  full_join(joined_sales_price, by = c("Year_Month" = "Year_Month", 
                                       "Customer Price Group" = "Sales Code",
                                       "No_" = "Item No_")) %>% 
  mutate(Unit_Price_Set = ifelse(is.na(Unit_Price_Set),Unit_Price_Sold, Unit_Price_Set)) %>%
  filter(stri_startswith_fixed(`No_`,"R240271")) 

full_joined_sales_price %>% group_by(No_) %>% tally() %>% View

#PLOT--------------------------------------------------------------------------------------------------
ggplot (data=joined_sales_price, aes(x=`Starting Date`)) +
  geom_point(aes(y=`Unit Price`, colour = "red"))

ggplot (data=joined_sales_price, aes(x=`Starting Date`,y=`Unit Price`, colour=`Sales Type`)) +
  geom_line()

ggplot (data=ssl_df, aes(x=`Shipment Date`,y=Unit_Price, colour=`Customer Price Group`)) +
  geom_line()

ggplot (data=full_joined_sales_price, aes(x=`Year_Month`,y=Unit_Price_Set, colour=`Customer Price Group`)) +
  geom_point()


#NEXT STEPS:
#-----DONE! check if there is any other unit other than KS in Limenita ()
#-----DONE! to build timeseries, it will be necessary to find the latest non-promotional price for the customer, 
#     and make sure the ts has it when there is no promotion
#-----DONE!create a procedure that extracts the months (or weeks) in a timespan, and the number of promotion price days for that month
# Once promotion periods are "spread"try a full join with ssl_df in order to get non promotional prices for the remaining months









