# NOTE: The dataframes that are used here, are loaded into the environment, eventually they will be uploaded to repo
# list of loaded dataframes:
#   - query_ile : a table with item sales
#   - query_ssh : a table with information about the customers to whom the items were shipped to 
# The merged queries produce a complete table including data of what items where sold, and who they were sold to


library (dplyr)
library (lubridate)
library(tidyr)
library(stringi)
library (ggplot2)
library(reshape2)

customer_key_df <- as.data.frame(data.table::fread("Customer_Key.csv", sep=";")) %>% 
  mutate(Original_Code = as.character(Original_Code)) %>%  #changing Original_Code from numeric to character
  select(-Campaign_Name)

#setting period variable
period <- "month"

# cleaning weird characters from the dataframe (BETTER FIND OTHER METHOD)
#col_index <- grep("Global.Dimension.2.Code", colnames(query_ile))
#query_ile[,col_index]<-query_ile[,col_index] %>% lapply(stri_trans_general,"Latin-ASCII")

# first joining both item ledger and sales shipment tables, and filtering the result
# second, right join with customer key, to get only customers that interest us
joined_sales <- query_ile %>% 
  left_join(query_ssh, by= c("Document No_" = "No_"),suffix = c("_ile", "_ssh"))%>% 
  filter(Correction_ile == 0) %>%   #not interested in corrections for the moment, may affect date
  filter (Quantity <0) %>%    #not interested in positive sales entries (returns, corrections)
  filter(`Gen_ Bus_ Posting Group` == "TUZEMSKO")%>%    #leaving out sales to other countries, only want inland
  filter (`Customer Price Group` != 'GASTRO') %>%   #leaving out horeca clients for the moment too
  right_join(customer_key_df, by = c("Bill-to Customer No_" = "Original_Code")) %>%
  select (Customer_Code,`Bill-to Name`, everything()) %>% 
  right_join(query_weights, by =c("Item No_" = "No_")) %>% #joining with df that contains volume for each item
  mutate(Quantity = Quantity *-1) %>% #database registers units shipped as negative
  mutate(Quantity = Quantity * `Net Weight`) #getting sales in litres

  

#checking that we are left only with customers that interest us
joined_sales %>% group_by(`Customer Price Group`) %>%  tally()



#-------------------------------------------------------------------------------------------------------------------
#En principio nos vamos a quedar con el order date, mejor que shipment date

#preparing a table that will have monthly / weekly sales of items as rows, and every item as a column
sales_df_item <- joined_sales %>%
  mutate(init_period_date = floor_date(`Shipment Date`,period) %>% as_date()) %>% 
  group_by(init_period_date, `Item No_`) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>%
  ungroup()


#creating a new dataframe with our best_sellers by sum - 
#useful to get rid of promotional products
#ALTERNATIVE: (creating a new dataframe with our best_sellers by mean -useful to show products that at some moment have had great sales)
top_items <- sales_df_item %>% 
  filter (year(init_period_date) > 2017) %>%
  group_by(`Item No_`) %>% 
  summarise(sum_sales = sum(Sum_Quantity)) %>%
  ungroup() %>%
  arrange(desc(sum_sales)) #%>% 
  #filter(sum_sales > 13000)


#--------------------------------------------------------------------------------------------------------------------

#preparing a table that will have weekly sales of brands as rows, and every brand as a column
sales_df_brand <- joined_sales %>%   
  mutate(init_period_date = floor_date(`Order Date`,period) %>% as_date()) %>% 
  group_by(init_period_date, `Global Dimension 2 Code`) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>% 
  ungroup()


#-----------------------------------------------------------------------------------------------------------------

# charts for brand
ggplot(data = sales_df_brand , aes(x=init_period_date,y=Sum_Quantity)) +
  geom_line(aes(color= "dark orange"))


#stacked charts for bestseller items
ggplot(sales_df_top_items, aes(x = init_period_date, y = Sum_Quantity)) + 
  geom_line(aes(color=`Item No_`)) + 
  facet_wrap(~ `Item No_`, scales = 'free_y', ncol = 1)


#EXPORT CHARTS TO PNG --------------------------------------------------------------------------------


# 1. open the file
png("limenita_month.png", width = 2700, height = 1800)
# 2. Create the plot
ggplot(data = sales_df_brand , aes(x=init_period_date,y=Sum_Quantity)) +
  geom_line(aes(color= "dark orange", size=1)) +
  theme(legend.position="none",
        axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"),
        strip.text.x = element_text(size = 30, colour = "black"))
# 3. Close the file
dev.off()



sales_df_top_items <- sales_df_item %>% 
  inner_join(head(top_items[21:30,]))


# 1. open the file
png("21-30_limenita_month.png", width = 2700, height = 1800)
# 2. Create the plot
ggplot(sales_df_top_items, aes(x = init_period_date, y = Sum_Quantity)) + 
  geom_line(aes(color=`Item No_`),size=2) + 
  facet_wrap(~ `Item No_`, scales = 'free_y', ncol = 1) +
  theme(legend.position="none",
        axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"),
        strip.text.x = element_text(size = 30, colour = "black"))
# 3. Close the file
dev.off()



