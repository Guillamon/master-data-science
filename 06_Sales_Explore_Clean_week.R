

library (dplyr)
library (lubridate)
library(tidyr)
library (ggplot2)


# LOADING DATA----------------------------------------------

#Transforming dates from characters to posixt (somehow as_date causes errors here) 
query_ssh_copy <- data.table::fread("Data_Files\\Sales_Header.csv", sep=";",stringsAsFactors = FALSE) %>%
  as.data.frame() %>% 
  mutate(`Shipment Date`= as.POSIXct(`Shipment Date`),
         `Document Date` = as.POSIXct(`Document Date`),
         `Order Date` = as.POSIXct(`Order Date`))

#Not transforming dates here because they won´t be used
query_ile_copy <- data.table::fread("Data_Files\\Item_Ledger_Entry.csv", sep=";",stringsAsFactors = FALSE) %>%
  as.data.frame()

#The weights will be used to transform sales from units to litres
query_weights <- data.table::fread("Data_Files\\Item_Weights.csv", sep=";",stringsAsFactors = FALSE) %>%
  as.data.frame()


#reading a csv for parsing customer codes
#Contains only customers that purchase any item of the target brand in 2019. This is done to reduce size of csv
customer_key_df <- as.data.frame(data.table::fread("Data_Files\\Customer_Key.csv", sep=";")) %>% 
  mutate(Original_Code = as.character(Original_Code))# #changing Original_Code from numeric to character

#reading a csv for parsing item classes
class_key_df <- as.data.frame(data.table::fread("Data_Files\\Class_Key.csv", sep=";"))


#setting period variable
period <- "week"



#CLEANING DATA -----------------------------------------------------------------------------------------

# First joining both item ledger and sales shipment tables, and filtering the result
# Second, right join with customer key, to get only customers that interest us
joined_sales <- query_ile_copy %>% 
  left_join(query_ssh_copy, by= c("Document No_" = "No_"),suffix = c("_ile", "_ssh"))%>% 
  filter(Correction_ile == 0) %>%   #not interested in corrections for the moment, may affect date
  filter (Quantity <0) %>%    #not interested in positive sales entries (returns, corrections)
  filter(`Gen_ Bus_ Posting Group` == "TUZEMSKO")%>%    #leaving out sales to other countries, only want inland
  filter (`Customer Price Group` != 'GASTRO') %>%   #leaving out horeca clients for the moment too
  right_join(customer_key_df, by = c("Bill-to Customer No_" = "Original_Code")) %>%
  select (Customer_Code, everything()) %>% 
  right_join(query_weights, by =c("Item No_" = "No_")) %>% #joining with df that contains volume for each item
  mutate(Quantity = Quantity *-1) %>% #database registers units shipped as negative, so transforming this
  mutate(Quantity = Quantity * `Net Weight`) %>%  #getting sales in litres
  left_join(class_key_df, by= c("Item No_" = "No_"))



# Creating this for plotting
sales_df_top_items <- sales_df_item %>% 
  inner_join(top_items[1:7,1])

# Preparing a table that will have monthly / weekly sales of items as rows, and every item as a column
sales_df_item <- joined_sales %>%
  mutate(Period = (floor_date(`Shipment Date`,period)+1) %>% as_date()) %>% 
  group_by(Period, `Item No_`) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>%
  ungroup()


# Preparing a table that will have weekly sales of brands as rows, and every brand as a column
sales_df_brand <- joined_sales %>%   
  mutate(Period = (floor_date(`Shipment Date`,period)+1) %>% as_date()) %>% 
  group_by(Period) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>% 
  ungroup()


# Preparing a table that will have weekly sales of brands as rows, and every brand as a column
sales_df_class <- joined_sales %>%   
  mutate(Period = (floor_date(`Shipment Date`,period)+1) %>% as_date()) %>% 
  group_by(Period, Class) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>% 
  ungroup()


sales_df_customer <- joined_sales %>%   
  mutate(Period = (floor_date(`Shipment Date`,period)+1) %>% as_date()) %>% 
  group_by(Period, Customer_Code) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>%
  ungroup() %>% 
  drop_na()


#GRAPHS------------------------------------------------------------------


# chart for brand
ggplot(data = sales_df_brand , aes(x=Period,y=Sum_Quantity)) +
  geom_line(color= huey[6], size = 1)+
  ylab("Litres Sold")+
  xlab(element_blank())

#stacked charts for class sales
ggplot(sales_df_class, aes(x = Period, y = Sum_Quantity)) + 
  geom_line(aes(color= Class), size = 0.8) + 
  facet_wrap(~ Class, scales = 'free_y', ncol = 1)  +
  ylab("Litres Sold")+
  xlab(element_blank())+
  theme(legend.position = "none")  +
  scale_color_manual(values=huey)



#line chart for bestseller items
ggplot(sales_df_class, aes(y = Sum_Quantity)) +
  geom_bar(stat="identity", aes(x = Class, fill=Class), alpha=0.7) +
  scale_fill_manual(values = huey) +
  xlab(element_blank()) +
  ylab("Litres Sold since 2014") +
  theme(legend.position = "none")


#stacked charts for bestseller items
ggplot(sales_df_top_items, aes(x = Period, y = Sum_Quantity)) + 
  geom_line(aes(color=`Item No_`), size = 0.8) + 
  facet_wrap(~ `Item No_`, scales = 'free_y', ncol = 1)  +
  ylab("Litres Sold")+
  xlab(element_blank())+
  theme(legend.position = "none") +
  scale_color_manual(values=huey)


#line chart for bestseller items
ggplot(sales_df_top_items, aes(y = Sum_Quantity)) +
  geom_bar(stat="identity", aes(x = reorder(factor(`Item No_`),sort(Sum_Quantity)), fill=`Item No_`), alpha=0.7) +
  scale_fill_manual(values = huey) +
  xlab(element_blank()) +
  ylab("Litres Sold since 2014")




#stacked charts for customer sales
ggplot(sales_df_customer, aes(x = Period, y = Sum_Quantity)) + 
  geom_line(aes(color= Customer_Code), size = 0.8) + 
  facet_wrap(~ Customer_Code, scales = 'free_y', ncol = 1)  +
  ylab("Litres Sold")+
  xlab(element_blank())+
  theme(legend.position = "none")  +
  scale_color_manual(values=huey)


#LINE chart for class sales
ggplot(sales_df_customer, aes(x = Period, y = Sum_Quantity)) + 
  geom_line(aes(color= Customer_Code), size = 0.8) + 
  #facet_wrap(~ Customer_Code, scales = 'free_y', ncol = 1)  +
  ylab("Litres Sold")+
  xlab(element_blank())+
  #theme(legend.key.height = c(1,2))  +
  scale_color_manual(values=huey)


#BAR chart for bestseller items
ggplot(sales_df_customer, aes(y = Sum_Quantity)) +
  geom_bar(stat="identity", aes(x = reorder(factor(Customer_Code),sort(Sum_Quantity)), fill=Customer_Code), alpha=0.7) +
  scale_fill_manual(values = huey) +
  xlab(element_blank()) +
  ylab("Litres Sold since 2014")



