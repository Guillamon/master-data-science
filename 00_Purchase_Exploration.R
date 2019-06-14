
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)


#READING CSVS ----------------------------------------------------------------------------------- 
query_all_copy <- data.table::fread("Data_Files\\Purchase_Line.csv", sep=";",stringsAsFactors = FALSE) %>%
  as.data.frame() %>% 
  mutate(`Posting Date`= as.POSIXct(`Posting Date`))


query_agg_sales_copy <- data.table::fread("Data_Files\\Sales_Brands.csv", sep=";",stringsAsFactors = FALSE) %>%
  as.data.frame()

#EXPLORING PURCHASE FREQUENCY ----------------------------------------------------------------------------------- 

summertime3 <- c("#236e96", "#15b2d3", "#ffd700", "#f3872f", "#ff598f")
huey <- c("#72d6c9","#ffc785", "#df7599", "#7189bf", "#145374", "#ff8260", "#007880", "#b42757")

filtered_date_query <- query_all_copy %>%
  filter( `Posting Date`> '2018/01/01') %>% 
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
  ungroup()




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


#Getting a dataframe with top brands for plotting
top_brands <- query_agg_sales_copy %>% 
  rename(Brand = `Shortcut Dimension 2 Code`, Sum = SUM) %>%
  group_by(Brand) %>% 
  summarise(Sum = sum(Sum)) %>% 
  arrange(desc(Sum)) %>% 
  ungroup %>% 
  head(5) %>% 
  as.data.frame() %>% 
  mutate(Top_Brands = Brand) %>%
  mutate(Brand = replace(Brand, Brand=="", "BLANK")) %>%
  select(-Sum)



#Creating a table with purchase frequency of every item
purchase_frequency <- max_min_date %>% 
  full_join(purchase_count) %>% 
  mutate (Frequency = Range/Purchase_Count) %>%
  filter (Frequency > 0) %>% 
  filter (Purchase_Count > 10) %>% 
  select (`Item No_`, First_Order, Last_Order, Purchase_Count, Frequency) %>% # changing order of columns in dataframe
  rowwise() %>% # this is necessary for the next mutate function to be applied row by row
  mutate (Category = category_func(`Item No_`)) %>% # assigning Category to each item
  left_join(query_agg_sales_copy,by = c("Item No_" = "No_")) %>%
  rename(Brand = `Shortcut Dimension 2 Code`, Sum = SUM) %>% 
  left_join(top_brands, by =c("Brand")) %>%
  mutate(Brand = replace(Brand, Brand=="", "BLANK")) %>%
  mutate(Top_Brands = replace(Top_Brands, which(is.na(Top_Brands)), "OTHERS")) %>%
  arrange(Frequency)







# GRAPHS -------------------------------------------------------------------------

#This graph shows item revenues vs purchase frequency
#Colour scale is category (ambient, refrigerated, frozen)
ggplot (data=purchase_frequency, aes(x= as.numeric(Frequency), y=Sum)) + 
  geom_point(aes(size = Purchase_Count, color = Category), alpha=0.7 ) +
  scale_x_continuous(breaks=c(0, 10, 20, 30, 40, 50, 75, 100)) +
  scale_color_manual(values=huey)+
  xlab("Purchase Frequency (days)") + 
  ylab("Item Revenues Since 2018") +
  guides(fill=guide_legend(title="New Legend Title"))


#This shows revenues per brand, arranged and filtered to top 8. 
#This is done because I am now interested in checking previous graph, but with scale color brand
query_agg_sales_copy %>% 
  rename(Brand = `Shortcut Dimension 2 Code`, Sum = SUM) %>%
  group_by(Brand) %>% 
  summarise(Sum = sum(Sum)) %>% 
  arrange(desc(Sum)) %>% 
  mutate(Brand = replace(Brand, Brand=="", "BLANK")) %>% 
  head(8) %>% 
  ggplot () +
    #scale_x_discrete()+
    geom_bar(stat="identity", aes(x=reorder(factor(Brand),sort(Sum)), y=Sum, fill=Brand), alpha =0.7)+
    scale_fill_manual(values = huey) +
    xlab(element_blank()) +
    ylab("Brand Revenues Since 2018") +
    theme(axis.text.x = element_blank())


#First graph, but with top 5 brands as scale color 
ggplot (data=purchase_frequency, aes(x= as.numeric(Frequency), y=Sum)) + 
  geom_point(aes(size = Purchase_Count, color = Top_Brands), alpha=0.5 ) +
  #geom_label_repel(label.size = 0.15, aes( label=ifelse(SUM>250000,Description,'') ) ) + 
  scale_x_continuous(breaks=c(0, 10, 20, 30, 40, 50, 75, 100)) +
  #scale_color_brewer(palette="Dark2") +
  xlab("Purchase Frequency (days)") + 
  ylab("Item Revenues Since 2018") +
  guides(fill=guide_legend(title="New Legend Title"))


