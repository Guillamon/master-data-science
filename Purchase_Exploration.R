# NOTE: The dataframes that are used here, are loaded into the environment, eventually they will be uploaded to repo
# list of loaded dataframes:
#   - query_all - a table with historic purchases for all items
#   - query_names - a table with names of all items
#   - query_item - a table with historic purchase costs for an item
#   - query_exw - a table with vendors of an item. A supplier may have several codes
# The first 2 queries are used to explore purchase frequency and and the last 2 are for purchase costs

library(dplyr)
library(lubridate)
library(ggplot2)
#install.packages("ggrepel")
library(ggrepel)
library("RColorBrewer")

#EXPLORING PURCHASE FREQUENCY ----------------------------------------------------------------------------------- 

filtered_date_query <- query_all %>%
  filter( `Posting Date`> '2015/01/01') %>% 
  filter(!`Item Ledger Entry Type` == 1) %>% #Source Type != Sale
  filter(!`Source Type` == 1) %>% #Source Type != Customer
  filter(!`Gen_ Bus_ Posting Group` %in% c("VYSTAVA","OBCH_VZOR", "INVEN_PREZ",
                                           "INVEN_MANZ", "PRESUN_PRI", "POCSTAV" , 
                                           "REKLAMPRED","MEZIP.MCZ", "MEZIP.MHU", 
                                           "PRESUN-VYD" )) %>%  #cleaning all rows that don´t have to do with product orders
  filter(`Document Type` == 6) #Document Type = Purchase Invoice


# Getting latest order date per item
max_date <- filtered_date_query %>% 
  group_by( `Item No_`) %>% 
  summarise(Last_Order = max(`Posting Date`))
# Getting first order date per item
min_date <- filtered_date_query %>% 
  group_by( `Item No_`) %>% 
  summarise(First_Order = min(`Posting Date`))
# Joining First and Last and computing difference
max_min_date <- max_date %>%  
  full_join(min_date, by = c("Item No_" = "Item No_")) %>% 
  mutate(Range = difftime(Last_Order,First_Order, units = "days"))


# Getting a count of purchases per item
purchase_count <- filtered_date_query %>%
  select (`Item No_`,`Posting Date`) %>%
  left_join (max_min_date) %>% 
  filter (Last_Order > '2017-01-01') %>% 
  group_by(`Item No_`,`Posting Date`) %>% 
  summarise(Purchase_Count = n()) %>%    # This gives the number of times an order is changed
  ungroup() %>% 
  group_by(`Item No_`) %>% 
  summarise(Purchase_Count = n()) %>% 
  arrange(desc(Purchase_Count))

#Just getting a table with number of changes per order to see if it makes sense
filtered_date_query %>%
  #filter(`Document No_` == "NF5190376") %>%  
  filter (`Item No_` == "R261020") %>%
  left_join (max_min_date) %>% 
  filter (Last_Order > '2017-01-01') %>%
  group_by(`Document No_`,`Posting Date`) %>% 
  summarise(Num_Order_Changes = n()) %>% # This gives the number of times an order is changed
  ungroup() %>% View()

filtered_date_query %>%
  filter(`Document No_` == "ID-DOP18032") %>% 
  filter (`Item No_` == "R261020") %>% View()

# Getting an index of Items whose orders require changes (No Purchases/ No of Changes per Order)
# the index is turned around (1-i) to make it understandable
painfulness <- filtered_date_query %>%
  select (`Item No_`,`Posting Date`) %>%
  left_join (max_min_date) %>% 
  filter (Last_Order > '2017-01-01') %>% 
  group_by(`Item No_`) %>% 
  summarise(Num_Order_Changes = n()) %>% # This gives the number of times an order is changed
  ungroup() %>% 
  left_join(purchase_count) %>% # joining with purchase_count to perform operation
  mutate(Painfulness_Abs = 1-(Purchase_Count/Num_Order_Changes)) %>% #filter(Purchase_Count >= 50) %>% 
  arrange(desc(Painfulness_Abs))


# Creating function that finds category of a product, to be passed to dataframe
category_func <- function(item) {
  cat <- substr(item,0,1)  
  if (cat == "A"){
    return ("Ambient")
    
  } else if (cat == "R") {
    return ("Refrigerated")
    
  } else if (cat == "F") {
    return ("Frozen")
    
  } else  {
    return ("Irrelevant")
  } 
}


# Creating a table with all relevant information to analyse how much workload items have
# Relative Painfulness takes (No Purchases/ No of Changes per Order) and divides it by Frequency of orders
# Also includes a column with a % of Changes from Purchases

purchase_frequency <- max_min_date %>% 
  full_join(purchase_count) %>% 
  mutate (Frequency = Range/Purchase_Count) %>%
  filter (Frequency > 0) %>% 
  filter (Purchase_Count > 10) %>% 
  left_join(query_names, by= c("Item No_" = "No_")) %>% # just adding names of items to dataframe
  select (`Item No_`,Description, First_Order, Last_Order, Purchase_Count, Frequency) %>% # changing order of columns in dataframe
  left_join(painfulness) %>% 
  mutate(Painfulness_Rel = (Painfulness_Abs/as.numeric(Frequency))*100) %>% # as.numeric is necessary because of time object
  mutate(Changes_Percent = Purchase_Count/Num_Order_Changes) %>%
  rowwise() %>% # this is necessary for the next mutate function to be applied row by row
  mutate (Category = category_func(`Item No_`)) %>% # assigning Category to each item
  arrange(asc(Frequency))

purchase_frequency %>%left_join(query_names, by= c("Item No_" = "No_")) %>% View()


# EXPLORING ITEM UNIT COSTS ---------------------------------------------------------------------------------------

library(dplyr)

# Getting a vector of unique exworks vendors, will be used as filter:
exw_vendors <- as.vector(query_exw[,1])

# Getting a table of exworks unit costs
exw_cost_table <- query_item %>% filter(!`Item Ledger Entry Type` == 1) %>% 
  filter(!`Source Type` == 1) %>% 
  filter(!`Gen_ Bus_ Posting Group` %in% c("VYSTAVA","OBCH_VZOR", "INVEN_PREZ",
                                           "INVEN_MANZ", "PRESUN-PRI", "POCSTAV" , 
                                           "REKLAMPRED","MEZIP.MCZ", "MEZIP.MHU", 
                                           "PRESUN-VYD", "REPRE", "INVEN_MANK", "INVEN_PREB")) %>% 
  filter(!`Document Type` == 5) %>% 
  filter(!`Source Code` %in% c("DENPREC", "UPRSKLADU")) %>%
  filter(`Source No_` %in% exw_vendors) %>% 
  select (`Item Ledger Entry No_`, `Posting Date`, `Cost per Unit`) %>% 
  group_by(`Item Ledger Entry No_`) %>%
  summarise(Exworks_Cost = sum(`Cost per Unit`))



# Getting a table of transport costs
transport_cost_table <- query_item %>% filter(!`Item Ledger Entry Type` == 1) %>% 
  filter(!`Source Type` == 1) %>% 
  filter(!`Gen_ Bus_ Posting Group` %in% c("VYSTAVA","OBCH_VZOR", "INVEN_PREZ",
                                           "INVEN_MANZ", "PRESUN-PRI", "POCSTAV" , 
                                           "REKLAMPRED","MEZIP.MCZ", "MEZIP.MHU", 
                                           "PRESUN-VYD", "REPRE", "INVEN_MANK", "INVEN_PREB")) %>% 
  filter(!`Document Type` == 5) %>% 
  filter(!`Source Code` %in% c("DENPREC", "UPRSKLADU")) %>%
  filter(!`Source No_` %in% exw_vendors) %>% 
  group_by(`Item Ledger Entry No_`) %>%
  summarise(Transport_Cost = sum(`Cost per Unit`))

#summarise (count = n()) %>% 
#filter (count > 1)

# Getting a table where exworks and transport costs appear summed per Item Ledger Entry No
# Filter could be simpler, but until structure is not fully understood better this way
unit_cost_table <- query_item %>% filter(!`Item Ledger Entry Type` == 1) %>% 
  filter(!`Source Type` == 1) %>% 
  filter(!`Gen_ Bus_ Posting Group` %in% c("VYSTAVA","OBCH_VZOR", "INVEN_PREZ",
                                           "INVEN_MANZ", "PRESUN-PRI", "POCSTAV" , 
                                           "REKLAMPRED","MEZIP.MCZ", "MEZIP.MHU", 
                                           "PRESUN-VYD", "REPRE", "INVEN_MANK", "INVEN_PREB")) %>% 
  filter(!`Document Type` == 5) %>% 
  filter(!`Source Code` %in% c("DENPREC", "UPRSKLADU")) %>%
  mutate(Unit_Cost = `Cost Amount (Actual)`/`Valued Quantity`) %>%
  group_by(`Item Ledger Entry No_`) %>%
  summarise(Unit_Cost_Ivanka = sum(`Unit_Cost`))


# Getting a table where every Item Ledger Entry No gets a single date
date_table <- query_item %>% filter(!`Item Ledger Entry Type` == 1) %>% 
  filter(!`Source Type` == 1) %>% 
  filter(!`Gen_ Bus_ Posting Group` %in% c("VYSTAVA","OBCH_VZOR", "INVEN_PREZ",
                                           "INVEN_MANZ", "PRESUN_PRI", "POCSTAV" , 
                                           "REKLAMPRED","MEZIP.MCZ", "MEZIP.MHU", 
                                           "PRESUN-VYD" )) %>% 
  filter(`Document Type` == 6) %>% 
  select(`Item Ledger Entry No_`, `Posting Date`)



# Joining unit cost table and date table to get time series
unit_cost_date_table <- date_table %>% right_join(unit_cost_table)

unit_cost_exw_table <- unit_cost_date_table %>%  right_join(exw_cost_table) #%>%  rename( Exw_Cost = `Cost per Unit`) 

full_cost_table <- unit_cost_exw_table %>% left_join(transport_cost_table) #%>% tidyr::drop_na()


# GRAPHS -------------------------------------------------------------------------

ggplot () + geom_point(data=full_table, aes(x=`Posting Date` , y=Unit_Cost_Ivanka, color = "red"))

ggplot (data=full_cost_table, aes(x=`Posting Date`)) +
  geom_point(aes(y=Unit_Cost_Ivanka, colour = "red"))+ 
  geom_point(aes(y=Exworks_Cost),colour = "light blue")+ 
  geom_point(aes(y=Transport_Cost),colour = "navy blue")

ggplot (data=full_cost_table, aes(x=`Posting Date`)) + geom_point(data=full_cost_table, aes(y=Unit_Cost_Ivanka))+ 
  geom_point(data=full_cost_table, aes(x= `Posting Date` , y=Exworks_Cost))+ 
  geom_point(data=full_cost_table, aes(x= `Posting Date` , y=Transport_Cost)) +
  scale_colour_continuous("Pattern") 

ggplot (data=purchase_frequency, aes(x= as.numeric(Frequency), y=Painfulness_Rel))

ggplot (data=purchase_frequency, aes(x= as.numeric(Frequency), y=Painfulness_Rel)) + 
  geom_point(aes(size = Purchase_Count, color = Category)) +
  geom_label_repel(label.size = 0.15, aes( label=ifelse(Painfulness_Rel>2,Description,'') ) ) + 
  scale_x_continuous(breaks=c(0, 5, 10, 15, 20,25,30, 40, 50, 75, 100)) +
  scale_color_brewer(palette="Dark2") +
  xlab("Purchase Frequency (days)") + 
  ylab("Painfulness Index (0-10)") +
  guides(fill=guide_legend(title="New Legend Title"))