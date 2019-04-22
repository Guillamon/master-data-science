#install.packages("tcR")
library(dplyr)
library (lubridate)
library(tcR)
library (stringr)
library (ggplot2)

#defining a function that will be used to extract starting date (only available day&month) from description"
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

#for some reason, the function does not always return the same output using lapply or mutate in dplyr, 
#so I will just use a simple loop for each row, which works fine
campaign_df <- query_campaign %>% 
  mutate(Promo_Starting_Date = Description)

for (row in 1:nrow(campaign_df)) {
  campaign_df[row,"Promo_Starting_Date"] <- date_cleaner(campaign_df[row,"Promo_Starting_Date"])
  }
#there are a few cases that haven´t returned a date, simply beacause they don´t exist in description,
#so we´ll have to handle that later

# now adding year to the date we have extracted and formatting to date ymd
campaign_df <- campaign_df %>%
  mutate(Year = year(campaign_df$`Starting Date`)) %>% 
  mutate(Promo_Starting_Date = gsub("\\.", "-", Promo_Starting_Date)) %>% 
  mutate(Promo_Starting_Date = paste(Promo_Starting_Date, Year, sep="-")) %>% 
  mutate(Promo_Starting_Date = dmy(Promo_Starting_Date))



# now checking that dates are OK (starting date of promo prices should be previous to starting date of promos),
# and handling them if not
# also doing some final changes before next steps
campaign_df <- campaign_df %>%
  mutate(Check = campaign_df$`Starting Date` <= campaign_df$Promo_Starting_Date) %>% 
  mutate(Promo_Starting_Date = ifelse(Check == 'FALSE' & month(Promo_Starting_Date) == 1,
                                      Promo_Starting_Date +years(1),Promo_Starting_Date)) %>%
  mutate (Promo_Starting_Date = as_date(Promo_Starting_Date)) %>% 
  mutate (`Starting Date` = as_date(`Starting Date`)) %>% 
  mutate (`Ending Date` = as_date(`Ending Date`)) %>% 
  select (-Check) %>% 
  rename ( Price_Promo_Starting_Date = `Starting Date`) %>% 
  rename ( Price_Promo_Ending_Date = `Ending Date`)

#Now preparing to join sales price table with campaign
sales_price_df <- query_sales_price %>% 
  mutate(`Sales Type` = ifelse(`Sales Type` == 3, "Promotion", `Sales Type`))
  #mutate(`Sales Type` = ifelse(`Sales Type` == 0, "General", `Sales Type`)) %>%
  #mutate(`Sales Type` = ifelse(`Sales Type` == 1, "Special", `Sales Type`)) 

joined_sales_price <- sales_price_df %>% 
  left_join(campaign_df ,by= c("Sales Code" = "No_")) %>% 
  filter(`Item No_` == 'R240271') %>%
  filter(`Sales Type` == "Promotion")
  #filter(`Salesperson Code` = "BILLA" & )



ggplot (data=joined_sales_price, aes(x=`Starting Date`)) +
  geom_point(aes(y=`Unit Price`, colour = "red"))



  
