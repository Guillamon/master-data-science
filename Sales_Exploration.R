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

# cleaning weird characters from the dataframe
col_index <- grep("Global.Dimension.2.Code", colnames(query_ile))
query_ile[,col_index]<-query_ile[,col_index] %>% lapply(stri_trans_general,"Latin-ASCII")

# joining both tables, and filtering the result
joined_sales <- query_ile %>% 
  left_join(query_ssh, by= c("Document No_" = "No_"),suffix = c("_ile", "_ssh"))%>% 
  filter(Correction_ile == 0) %>%   #not interested in corrections for the moment, may affect date
  filter (Quantity <0) %>%    #not interested in positive sales entries (returns, corrections)
  filter(`Gen_ Bus_ Posting Group` == "TUZEMSKO")%>%    #leaving out sales to other countries, only want inland
  filter (`Customer Price Group` != 'GASTRO')   #leaving out horeca clients for the moment too

#checking that we are left only with customers that interest us
joined_sales %>% group_by(`Customer Price Group`) %>%  tally()


#-------------------------------------------------------------------------------------------------------------------
#En principio nos vamos a quedar con el order date, mejor que shipment date

#preparing a table that will have weekly sales of items as rows, and every item as a column
sales_df_item <- joined_sales %>%
  filter(`Global Dimension 2 Code` != "CALVO") %>%
  mutate(init_week_day = floor_date(`Order Date`,"week")) %>% 
  group_by(init_week_day, `Item No_`) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>% 
  ungroup()

#getting positive values
sales_df_item$Sum_Quantity <- sales_df_item$Sum_Quantity*(-1)

#spreading initial table to get final intended result
sales_df_item <- sales_df_item %>% spread(`Item No_`,Sum_Quantity)

#filling in NAs with zeros
sales_df_item[is.na(sales_df_item)] <- 0

#adding a column with week no_, might be useful when sorting  
sales_df_item <- sales_df_item%>% mutate (week_num = as.numeric(rownames(sales_df_item)))


#--------------------------------------------------------------------------------------------------------------------

#preparing a table that will have weekly sales of brands as rows, and every brand as a column
sales_df_brand <- joined_sales %>%   
  mutate(init_week_day = floor_date(`Order Date`,"week")) %>% 
  group_by(init_week_day, `Global Dimension 2 Code`) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>% 
  ungroup()


sales_df_brand$Sum_Quantity <- sales_df_brand$Sum_Quantity*(-1)
sales_df_brand <- sales_df_brand %>% 
  spread(`Global Dimension 2 Code`,Sum_Quantity) 

#filling in NAs with zeros
sales_df_brand[is.na(sales_df_brand)] <- 0
#-----------------------------------------------------------------------------------------------------------------

#preparing a df for a stacked ts chart for brands
sales_df_brand_2 <- joined_sales %>%   
  mutate(init_week_day = floor_date(`Order Date`,"week")) %>% 
  group_by(init_week_day, `Global Dimension 2 Code`) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>% 
  ungroup()

sales_df_brand_2$Sum_Quantity <- sales_df_brand_2$Sum_Quantity*(-1)

#making a df for a stacked ts chart for items
sales_df_item_2 <- joined_sales %>%   
  filter(`Global Dimension 2 Code` != "CALVO") %>%
  mutate(init_week_day = floor_date(`Order Date`,"week")) %>% 
  group_by(init_week_day, `Item No_`) %>% 
  summarise(Sum_Quantity = sum(Quantity)) %>%  
  ungroup()

sales_df_item_2$Sum_Quantity <- sales_df_item_2$Sum_Quantity*(-1)


#sales_df_item_2 %>% count(`Item No_`) %>% View()

#creating a new dataframe with our best_sellers by mean - 
#useful to show products that at some moment have had great sales
#top_items <- sales_df_item_2 %>% 
#filter (year(init_week_day) > 2017) %>% 
#group_by(`Item No_`) %>% 
#summarise(mean_sales = mean(Sum_Quantity)) %>% 
#arrange(desc(mean_sales)) %>% 
#ungroup() %>% 
#head(5)

#creating a new dataframe with our best_sellers by sum - 
#useful to get rid of promotional products
top_items <- sales_df_item_2 %>% 
  filter (year(init_week_day) > 2017) %>%
  filter(`Item No_`!='R241381') %>% 
  filter(`Item No_`!='R240681') %>% 
  group_by(`Item No_`) %>% 
  summarise(sum_sales = sum(Sum_Quantity)) %>% 
  arrange(desc(sum_sales)) %>% 
  ungroup() %>% 
  filter(sum_sales >5000)
#%>% 
#head(5)

#innner join of our best sellers with timeseries df
sales_df_top_items <- sales_df_item_2 %>% inner_join(top_items)


#-----------------------------------------------------------------------------------------------------------------



# charts for brands
ggplot(data = sales_df_brand , aes(x=init_week_day,y=CALVO)) +
  geom_line()
ggplot(data = sales_df_brand , aes(x=init_week_day,y=LIMENA)) +
  geom_line()

# charts for items
ggplot(data = sales_df_item , aes(x=init_week_day,y=A020120)) +
  geom_line()
ggplot(data = sales_df_item , aes(x=init_week_day,y=R242031)) +
  geom_line()

#stacked charts for brands
ggplot(sales_df_brand_2, aes(x = init_week_day, y = Sum_Quantity)) + 
  geom_line(aes(color=`Global Dimension 2 Code`)) + 
  facet_wrap(~ `Global Dimension 2 Code`, scales = 'free_y', ncol = 1)

#stacked charts for bestseller items
ggplot(sales_df_top_items, aes(x = init_week_day, y = Sum_Quantity)) + 
  geom_line(aes(color=`Item No_`)) + 
  facet_wrap(~ `Item No_`, scales = 'free_y', ncol = 1)


#------------------------------------------------------------------------------------------------------------


# 1. open the file
png("brands.png", width = 2700, height = 1800)
# 2. Create the plot
ggplot(sales_df_brand_2, aes(x = init_week_day, y = Sum_Quantity)) + 
  geom_line(aes(color=`Global Dimension 2 Code`),size=2) + 
  facet_wrap(~ `Global Dimension 2 Code`, scales = 'free_y', ncol = 1)+
  theme(legend.position="none",
        axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"),
        strip.text.x = element_text(size = 30, colour = "black"))
# 3. Close the file
dev.off()


# 1. open the file
png("top_limenita.png", width = 2700, height = 1800)
# 2. Create the plot
ggplot(sales_df_top_items, aes(x = init_week_day, y = Sum_Quantity)) + 
  geom_line(aes(color=`Item No_`),size=2) + 
  facet_wrap(~ `Item No_`, scales = 'free_y', ncol = 1) +
  theme(legend.position="none",
        axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"),
        strip.text.x = element_text(size = 30, colour = "black"))
# 3. Close the file
dev.off()
