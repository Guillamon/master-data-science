#USE THIS ONLY WHEN THE REST OF THE REPO IS LOADED TO THE ENVIRONMENT


#EXPORT CHARTS TO PNG --------------------------------------------------------------------------------
#Uses objects loaded in Sales_Exploration.R


# 1. open the file- will be saved to current working directory (where RProj is saved)
png("brand_month.png", width = 2700, height = 1800)
# 2. Create the plot
ggplot(data = sales_df_brand , aes(x=Period,y=Sum_Quantity)) +
  geom_line(aes(color= "dark orange", size=1)) +
  theme(legend.position="none",
        axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"),
        strip.text.x = element_text(size = 30, colour = "black"))+
  ylab("Litres Sold")+
  xlab(element_blank())
# 3. Close the file
dev.off()



# 1. open the file
png("item_month.png", width = 2700, height = 1800)
# 2. Create the plot
ggplot(sales_df_top_items, aes(x = Period, y = Sum_Quantity)) + 
  geom_line(aes(color=`Item No_`),size=2) + 
  facet_wrap(~ `Item No_`, scales = 'free_y', ncol = 1) +
  theme(legend.position="none",
        axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"),
        strip.text.x = element_text(size = 30, colour = "black"))+
  ylab("Litres Sold")+
  xlab(element_blank())
# 3. Close the file
dev.off()




# 1. open the file
png("master-data-science\\Charts\\Purchase1.png", width = 1000, height = 500)
# 2. Create the plot
ggplot (data=purchase_frequency, aes(x= as.numeric(Frequency), y=Sum)) + 
  geom_point(aes(size = Purchase_Count, color = Category), alpha=0.7 ) +
  scale_x_continuous(breaks=c(0, 10, 20, 30, 40, 50, 75, 100)) +
  scale_color_manual(values=huey)+
  xlab("Purchase Frequency (days)") + 
  ylab("Item Revenues Since 2018") +
  guides(fill=guide_legend(title="New Legend Title"))+
  ggtitle("CATEGORIES")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=25),
        #strip.text.x = element_text(size = 30, colour = "black"),
        title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18)) +
  guides(colour = guide_legend(override.aes = list(size=10)))
# 3. Close the file
dev.off()


# 1. open the file
png("master-data-science\\Charts\\Purchase2.png", width = 1000, height = 500)
# 2. Create the plot
ggplot (data=purchase_frequency, aes(x= as.numeric(Frequency), y=Sum)) + 
  geom_point(aes(size = Purchase_Count, color = Top_Brands), alpha=0.5 ) +
  #geom_label_repel(label.size = 0.15, aes( label=ifelse(SUM>250000,Description,'') ) ) + 
  scale_x_continuous(breaks=c(0, 10, 20, 30, 40, 50, 75, 100)) +
  #scale_color_brewer(palette="Dark2") +
  xlab("Purchase Frequency (days)") + 
  ylab("Item Revenues Since 2018") +
  labs(color = "Brands")+
  ggtitle("TOP BRANDS")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=25),
        #strip.text.x = element_text(size = 30, colour = "black"),
        title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18)) +
  guides(colour = guide_legend(override.aes = list(size=10)))
  
# 3. Close the file
dev.off()


# 1. open the file
png("master-data-science\\Charts\\Purchase3.png", width = 600, height = 350)
# 2. Create the plot
ggplot (data=purchase_frequency, aes(x= as.numeric(Frequency), y=Sum)) + 
  geom_point(aes(size = Purchase_Count, color = Top_Brands), alpha=0.5 ) +
  #geom_label_repel(label.size = 0.15, aes( label=ifelse(SUM>250000,Description,'') ) ) + 
  scale_x_continuous(breaks=c(0, 10, 20, 30, 40, 50, 75, 100)) +
  #scale_color_brewer(palette="Dark2") +
  xlab("Purchase Frequency (days)") + 
  ylab("Item Revenues Since 2018") +
  labs(color = "Brands")+
  ggtitle("TOP BRANDS")+
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=12),
        #strip.text.x = element_text(size = 30, colour = "black"),
        title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9)) +
  guides(colour = guide_legend(override.aes = list(size=5)))

# 3. Close the file
dev.off()



brand_monthly <- sales_df_brand
brand_weekly <- sales_df_brand

p1 <- ggplot(data = brand_monthly , aes(x=Period,y=Sum_Quantity)) +
  geom_line(color= huey[6], size=1.5)+
  ylab("Litres Sold")+
  xlab(element_blank())+
  theme(axis.text.x = element_text(size=20),
        axis.title=element_text(size=25),
        #strip.text.x = element_text(size = 30, colour = "black"),
        title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))


p2 <- ggplot(data = brand_weekly , aes(x=Period,y=Sum_Quantity)) +
  geom_line(color= huey[6], size=1.5)+
  ylab("Litres Sold")+
  xlab(element_blank())+
  theme(axis.text.x = element_text(size=20),
    axis.title=element_text(size=25),
    #strip.text.x = element_text(size = 30, colour = "black"),
    title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5))

# 1. open the file
png("master-data-science\\Charts\\Weekly_vs_Monthly.png", width = 1000, height = 650)

# 2. Create the plot

cowplot::plot_grid(p1, p2, align="v", nrow = 2)

# 3. Close the file
dev.off()






# 1. open the file
png("master-data-science\\Charts\\Brand_Weekly.png", width = 700, height = 400)

# 2. Create the plot

ggplot(data = brand_weekly , aes(x=Period,y=Sum_Quantity)) +
  geom_line(color= huey[3], size=1.5)+
  ylab("Litres Sold")+
  xlab(element_blank())+
  theme(axis.text.x = element_text(size=20),
        axis.title=element_text(size=25),
        #strip.text.x = element_text(size = 30, colour = "black"),
        title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))

# 3. Close the file
dev.off()




# 1. open the file
png("master-data-science\\Charts\\Top_Items_Weekly.png", width = 1000, height = 700)

# 2. Create the plot
ggplot(sales_df_item %>% 
         inner_join(top_items[1:5,1]), aes(x = Period, y = Sum_Quantity)) + 
  geom_line(aes(color=`Item No_`), size = 1) + 
  facet_wrap(~ `Item No_`, scales = 'free_y', ncol = 1)  +
  ylab("Litres Sold")+
  xlab(element_blank())+
  theme(legend.position = "none") +
  scale_color_manual(values=huey) +
  theme(axis.text.x = element_text(size=20),
        axis.title=element_text(size=25),
        #strip.text.x = element_text(size = 30, colour = "black"),
        title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))
# 3. Close the file
dev.off()



# 1. open the file
png("master-data-science\\Charts\\Target_Item_Monthly.png", width = 700, height = 400)

# 2. Create the plot
ggplot(sales_df_item %>% 
         inner_join(top_items[2,1]), aes(x = Period, y = Sum_Quantity)) + 
  geom_line(aes(color=`Item No_`), size = 1.5) + 
  facet_wrap(~ `Item No_`, scales = 'free_y', ncol = 1)  +
  ylab("Litres Sold")+
  xlab(element_blank())+
  theme(legend.position = "none") +
  scale_color_manual(values=huey) +
  theme(axis.text.x = element_text(size=20),
        axis.title=element_text(size=25),
        strip.text.x = element_blank(),
        title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))
# 3. Close the file
dev.off()

# 1. open the file
png("master-data-science\\Charts\\Polar_Season_Plot.png", width = 700, height = 500)

ggseasonplot(item_ts[,"Sum_Quantity"], polar = TRUE) +
  geom_line(size=1.5)+
  ggtitle(element_blank())+
  scale_color_manual(values = huey)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.position = "none") +
  guides(colour = guide_legend(override.aes = list(size=5)))

# 3. Close the file
dev.off()
  

# 1. open the file
png("master-data-science\\Charts\\Subseries_Plot.png", width = 700, height = 400)

# 2. write png
ggsubseriesplot(item_ts[,"Sum_Quantity"]) +
  geom_line(size=1.5, colour= huey[1])+
  #scale_color_manual(values = huey) +
  ggtitle("SUBSERIES PLOT")+
  theme(title = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        #title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_blank()) +
  scale_color_manual(values = huey)

# 3. Close the file
dev.off()



# 1. open the file
png("master-data-science\\Charts\\Season_Plot.png", width = 700, height = 500)

# 2. write png
ggseasonplot(item_ts[,"Sum_Quantity"]) +
  geom_line(size=1.5)+
  ggtitle(element_blank())+
  scale_color_manual(values = huey)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=5)))

# 3. Close the file
dev.off()



# 1. open the file
png("master-data-science\\Charts\\Monthly_Smoothed.png", width = 800, height = 500)

ma(item_ts[,1], 4) %>% autoplot(series="Smoothed Sales",size=3) +
  autolayer(item_ts[,1], series="Monthly Sales", size=1.5) +
  scale_color_manual(values = huey)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 20),
        legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=5)))
  

# 3. Close the file
dev.off()



# 1. open the file
png("master-data-science\\Charts\\Smoothed_Comparison.png", width = 800, height = 500)

smoothed_comparison %>% 
  autoplot(facets =TRUE, color = huey[6], size = 1.5)+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 15),
        axis.title=element_blank())


# 3. Close the file
dev.off()



# 1. open the file
png("master-data-science\\Charts\\X11.png", width = 800, height = 500)

x11 %>% autoplot(color = huey[], size =1.5)+
  ggtitle("X11") +
  theme(axis.text.x = element_text(size = 20),
        #axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 15),
        axis.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size = 20))

# 3. Close the file
dev.off()

# 1. open the file
png("master-data-science\\Charts\\SEATS.png", width = 800, height = 500)

seats$data %>% 
  autoplot(facets=TRUE) +
  ggtitle("SEATS") +
  theme(axis.text.x = element_text(size = 20),
        #axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 15),
        axis.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size = 20))

# 3. Close the file
dev.off()

# 1. open the file
png("master-data-science\\Charts\\LOESS.png", width = 800, height = 500)

loess %>% 
  autoplot() +
  ggtitle("LOESS") +
  theme(axis.text.x = element_text(size = 20),
        #axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 15),
        axis.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size = 20))
# 3. Close the file
dev.off()


# 1. open the file
png("Charts\\Promo_Timeline.png", width = 1000, height = 800)

rbind(promo_df, price_promo_df) %>%    
  ggplot(aes(x=Date, y=Customer_Code, color=Customer_Code, group=`Sales Code`)) +
  geom_line(size = 10) +
  labs(x="", y=NULL, title="Promotion Timeline") +
  facet_grid(Type ~ .) +
  scale_color_manual(values=huey) +
  ggtitle(element_blank())+
  ylab("Customer")+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size = 25),
        legend.position = "none")

# 3. Close the file
dev.off()




# 1. open the file
png("Charts\\Promo_Evolution_Monthly.png", width = 900, height = 600)

item_df %>%
  mutate(Year = year(Period)) %>%
  filter(Year > 2014) %>% 
  mutate(Month = lubridate::month(Period,label=TRUE)) %>%
  rename(Promo = Num_Days_Promo,
         Price_Promo = Num_Days_Price_Promo) %>% 
  gather ("Type", "Num_Days", 3:4) %>%
  filter(Type=="Promo") %>% 
  ggplot(aes(x=Month, y=Num_Days, fill=as.factor(Month))) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position="none") +
  #scale_x_discrete(limits=seq(1,12)) +
  facet_grid(Year ~ .) +
  ggtitle(element_blank())+
  ylab("Number of Days")+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size = 25),
        legend.position = "none")
  




# 3. Close the file
dev.off()






predictors_0 <- fc.winners0$model$coefficients %>% names() %>% str_remove_all("X")
predictors_0 <- predictors_0[-1]


# 1. open the file
png("master-data-science\\Charts\\Forecast_Month.png", width = 700, height = 500)

gg_df %>%
  ggplot(aes(x=Period)) +
  geom_ribbon(fill="navy", alpha=0.2, aes(ymin = Lo_80, ymax = Hi_80)) + 
  geom_ribbon(fill="navy", alpha=0.2, aes(ymin = Lo_95, ymax = Hi_95)) +
  geom_line(aes(y=Sum_Quantity, colour ="Actual"), size = 3, show.legend = TRUE,alpha=0.8) +
  geom_line(aes(y=Past, colour="Past"),  size = 1.5, show.legend = TRUE) +
  geom_line(aes(y=Point_Forecast, colour = "Forecast"), size = 1.5, show.legend = TRUE, alpha=0.8) +
  #my_theme +
  scale_x_date (date_breaks = "6 months") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  scale_color_manual(values=summertime3[c(5,3,1)]) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20),
        axis.title=element_blank(),
        legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=5)))

# 3. Close the file
dev.off()








fc.trial <- hw(item_ts[,1],seasonal="multiplicative", h=1)

#Plotting forecast. Autoplot won´t work with this forecast because it is one step
past_values <- data.frame(Period=time(fc.trial$x)%>% as_date, Past=fc.trial$x)

forecast_values <- fc.trial %>% as.data.frame() %>% cbind(Period=as_date("2019-04-01"))

gg_df <- past_values %>% full_join(forecast_values, by = c("Period")) %>% 
  left_join(item_df[,1:2], by = c("Period")) %>% 
  arrange(Period)


gg_df[1:(nrow(gg_df)-2),"Sum_Quantity"] <- NA
gg_df[(nrow(gg_df)-1),3:ncol(gg_df)] <- gg_df[(nrow(gg_df)-1),"Past"]


colnames(gg_df) <- colnames(gg_df) %>% str_replace(" ","_")



# 1. open the file
png("master-data-science\\Charts\\Forecast_HW_Month.png", width = 700, height = 500)

gg_df %>%
  ggplot(aes(x=Period)) +
  geom_ribbon(fill="navy", alpha=0.2, aes(ymin = Lo_80, ymax = Hi_80)) + 
  geom_ribbon(fill="navy", alpha=0.2, aes(ymin = Lo_95, ymax = Hi_95)) +
  geom_line(aes(y=Sum_Quantity, colour ="Actual"), size = 3, show.legend = TRUE,alpha=0.8) +
  geom_line(aes(y=Past, colour="Past"),  size = 3, show.legend = TRUE) +
  geom_line(aes(y=Point_Forecast, colour = "Forecast"), size = 3, show.legend = TRUE, alpha=0.8) +
  #my_theme +
  #scale_x_date (date_breaks = "6 months") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  scale_color_manual(values=summertime3[c(5,3,1)]) +
  ggtitle("HOLT WINTERS")+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20),
        axis.title=element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        title = element_text(size=40)) +
  guides(colour = guide_legend(override.aes = list(size=20)))

# 3. Close the file
dev.off()




#Using ETS function (Exponential smoothing state space model)
fit.trial <- ets(item_ts[,1])

fc.trial <- forecast(fit.trial, h=1) 


#Plotting forecast. Autoplot won´t work with this forecast because it is one step
past_values <- data.frame(Period=time(fc.trial$x)%>% as_date, Past=fc.trial$x)

forecast_values <- fc.trial %>% as.data.frame() %>% cbind(Period=as_date("2019-04-01"))

gg_df <- past_values %>% full_join(forecast_values, by = c("Period")) %>% 
  left_join(item_df[,1:2], by = c("Period")) %>% 
  arrange(Period)


gg_df[1:(nrow(gg_df)-2),"Sum_Quantity"] <- NA
gg_df[(nrow(gg_df)-1),3:ncol(gg_df)] <- gg_df[(nrow(gg_df)-1),"Past"]


colnames(gg_df) <- colnames(gg_df) %>% str_replace(" ","_")



# 1. open the file
png("master-data-science\\Charts\\Forecast_ETS_Month.png", width = 700, height = 500)

gg_df %>%
  ggplot(aes(x=Period)) +
  geom_ribbon(fill="navy", alpha=0.2, aes(ymin = Lo_80, ymax = Hi_80)) + 
  geom_ribbon(fill="navy", alpha=0.2, aes(ymin = Lo_95, ymax = Hi_95)) +
  geom_line(aes(y=Sum_Quantity, colour ="Actual"), size = 3, show.legend = TRUE,alpha=0.8) +
  geom_line(aes(y=Past, colour="Past"),  size = 3, show.legend = TRUE) +
  geom_line(aes(y=Point_Forecast, colour = "Forecast"), size = 3, show.legend = TRUE, alpha=0.8) +
  #my_theme +
  #scale_x_date (date_breaks = "6 months") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  scale_color_manual(values=summertime3[c(5,3,1)]) +
  ggtitle("ETS")+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20),
        axis.title=element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        title = element_text(size=40)) +
  guides(colour = guide_legend(override.aes = list(size=20)))
# 3. Close the file
dev.off()





#MANUAL SARIMA

fit.trial <- item_ts[,1] %>% Arima(order=c(0,1,2), seasonal=c(0,1,0)) 


fc.trial <- forecast (fit.trial, h=1) 

#Plotting forecast. Autoplot won´t work with this forecast because it is one step
past_values <- data.frame(Period=time(fc.trial$x)%>% as_date, Past=fc.trial$x)

forecast_values <- fc.trial %>% as.data.frame() %>% cbind(Period=as_date("2019-04-01"))

gg_df <- past_values %>% full_join(forecast_values, by = c("Period")) %>% 
  left_join(item_df[,1:2], by = c("Period")) %>% 
  arrange(Period)


gg_df[1:(nrow(gg_df)-2),"Sum_Quantity"] <- NA
gg_df[(nrow(gg_df)-1),3:ncol(gg_df)] <- gg_df[(nrow(gg_df)-1),"Past"]


colnames(gg_df) <- colnames(gg_df) %>% str_replace(" ","_")



# 1. open the file
png("master-data-science\\Charts\\Forecast_Manual_Sarima_Month.png", width = 700, height = 500)

gg_df %>%
  ggplot(aes(x=Period)) +
  geom_ribbon(fill="navy", alpha=0.2, aes(ymin = Lo_80, ymax = Hi_80)) + 
  geom_ribbon(fill="navy", alpha=0.2, aes(ymin = Lo_95, ymax = Hi_95)) +
  geom_line(aes(y=Sum_Quantity, colour ="Actual"), size = 3, show.legend = TRUE,alpha=0.8) +
  geom_line(aes(y=Past, colour="Past"),  size = 3, show.legend = TRUE) +
  geom_line(aes(y=Point_Forecast, colour = "Forecast"), size = 3, show.legend = TRUE, alpha=0.8) +
  #my_theme +
  #scale_x_date (date_breaks = "6 months") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  scale_color_manual(values=summertime3[c(5,3,1)]) +
  ggtitle("MANUAL SEASONAL ARIMA")+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20),
        axis.title=element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        title = element_text(size=40)) +
  guides(colour = guide_legend(override.aes = list(size=20)))

# 3. Close the file
dev.off()










#MANUAL SARIMA

fit.trial <- auto.arima(item_ts[,1] ,stepwise = FALSE, approximation=FALSE, seasonal= TRUE)

fc.trial <- forecast (fit.trial, h=1) 

#Plotting forecast. Autoplot won´t work with this forecast because it is one step
past_values <- data.frame(Period=time(fc.trial$x)%>% as_date, Past=fc.trial$x)

forecast_values <- fc.trial %>% as.data.frame() %>% cbind(Period=as_date("2019-04-01"))

gg_df <- past_values %>% full_join(forecast_values, by = c("Period")) %>% 
  left_join(item_df[,1:2], by = c("Period")) %>% 
  arrange(Period)


gg_df[1:(nrow(gg_df)-2),"Sum_Quantity"] <- NA
gg_df[(nrow(gg_df)-1),3:ncol(gg_df)] <- gg_df[(nrow(gg_df)-1),"Past"]


colnames(gg_df) <- colnames(gg_df) %>% str_replace(" ","_")



# 1. open the file
png("master-data-science\\Charts\\Forecast_Auto_Sarima_Month.png", width = 900, height = 500)

gg_df %>%
  ggplot(aes(x=Period)) +
  geom_ribbon(fill="navy", alpha=0.2, aes(ymin = Lo_80, ymax = Hi_80)) + 
  geom_ribbon(fill="navy", alpha=0.2, aes(ymin = Lo_95, ymax = Hi_95)) +
  geom_line(aes(y=Sum_Quantity, colour ="Actual"), size = 3, show.legend = TRUE,alpha=0.8) +
  geom_line(aes(y=Past, colour="Past"),  size = 3, show.legend = TRUE) +
  geom_line(aes(y=Point_Forecast, colour = "Forecast"), size = 3, show.legend = TRUE, alpha=0.8) +
  #my_theme +
  #scale_x_date (date_breaks = "6 months") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  scale_color_manual(values=summertime3[c(5,3,1)]) +
  ggtitle("AUTO SEASONAL ARIMA")+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20),
        axis.title=element_blank(),
        legend.text = element_text(size = 40),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size=40)) +
  guides(colour = guide_legend(override.aes = list(size=20)))

# 3. Close the file
dev.off()






#PLOTTING MULTIPLE FORECASTS FROM THE MODEL




plot_list <- list()


for (y in 1:5) {
  holidays <- item_df %>% 
    mutate(Week = week(Period)) %>%
    filter(year(Period) == years[y]) %>% 
    filter (Num_Holidays > 0)
  #arrange(year(Period))
  
  Week <- seq(1,53) %>% as.data.frame()
  
  gg <- item_df %>% 
    select(Period, Sum_Quantity) %>%
    mutate(Year=year(Period)) %>%
    mutate(Week = week(Period)) %>% 
    filter(Year == years[y]) %>% 
    right_join (Week, by = c("Week" = ".")) 
  #spread(Year,Sum_Quantity,sep='_') %>%
  #replace(is.na(.), 0) #%>% %>% 
  
  
  plot <-  ggplot(data = gg) +
    geom_line(aes(x = Week, y = Sum_Quantity), color= huey[4], size = 2) +
    geom_vline(data = holidays, aes(xintercept = Week), color="magenta", size = 0.2)+
    xlab('WEEK') +
    ylab('') +
    ggtitle(years[y]) +
    theme(axis.text.x = element_text(size=15),
          axis.title.x = element_text(size=20),
          axis.title.y = element_blank(),
          title = element_text(size = 30),
          plot.title = element_text(hjust = 0.5))
  
  print(plot)
  plot_list[[y]] <- plot
}


#First install Image Magick
#https://imagemagick.org/script/download.php
png(file="holiday%02d.png", width=1250, height=750)
for (n in (1:length(plot_list))){
  print(plot_list[[n]])
}
dev.off()

#This command will work for Windows
shell("magick convert -delay 700 -loop 3 holiday*.png master-data-science\\Charts\\Bank_Holiday_Plot.gif")
file.remove(list.files(pattern="holiday...png"))








plot_list <- list()

for (y in 1:5) {
  holidays <- school_cal_df %>% 
    mutate(Start_Week = week(Start), End_Week = week(End)) %>% 
    filter(year(Start)== years[y] | year(End) == years[y]) %>% 
    mutate (Start_Week = ifelse(year(Start) < years[y], 0, Start_Week)) %>% 
    mutate (End_Week = ifelse(year(End) > years[y], 53, End_Week))# %>% 
  
  Week <- seq(1,53) %>% as.data.frame()
  
  gg <- item_df %>% 
    select(Period, Sum_Quantity) %>%
    mutate(Year=year(Period)) %>%
    mutate(Week = week(Period)) %>% 
    filter(Year == years[y]) %>% 
    right_join (Week, by = c("Week" = ".")) 
  #spread(Year,Sum_Quantity,sep='_') %>%
  #replace(is.na(.), 0) #%>% %>% 
  
  
  plot <-  ggplot(data = gg) +
    geom_line(aes(x = Week, y = Sum_Quantity), color= huey[4], size= 2) +
    geom_rect(holidays, mapping =aes(xmin = Start_Week, xmax = End_Week, ymin = -Inf, ymax = +Inf), fill = "magenta", alpha = 0.15) +
    xlab('WEEK') +
    ylab('SALES') +
    ggtitle(years[y])+
    theme(axis.text.x = element_text(size=15),
          axis.title.x = element_text(size=20),
          axis.title.y = element_blank(),
          title = element_text(size = 30),
          plot.title = element_text(hjust = 0.5))
  
  #print(plot)
  plot_list[[y]] <- plot
}


#First install Image Magick
#https://imagemagick.org/script/download.php
png(file="holiday%02d.png", width=1250, height=750)
for (n in (1:length(plot_list))){
  print(plot_list[[n]])
}
dev.off()

#This command will work for Windows
shell("magick convert -delay 700 -loop 3 holiday*.png master-data-science\\Charts\\School_Holiday_Plot.gif")
file.remove(list.files(pattern="holiday...png"))


























#PLOTTING MULTIPLE FORECASTS FROM THE MODEL
plot_list <- list()

for (i in (1:12)) {
  item_ts_train <- item_ts2[1:(nrow(item_ts2)-i),] %>% ts(start= c(2015,9),frequency = 12)
  item_ts_test <- item_ts2[(nrow(item_ts2)-i+1),]
  
  
  test_start <- paste(end(item_ts_train)[1],end(item_ts_train)[2],1, sep = "-") %>% as_date() %m+% months(+1)

  
  fit.loop <- tslm(item_ts_train[,1]~ item_ts_train[,final_winners], data=item_ts_train)
  new_data <- item_ts_test %>% t() %>%  as.data.frame() #%>% 
  fc.loop <- forecast (fit.loop, newdata = new_data)
  
  
  
  #Plotting forecast. Autoplot won´t work with this forecast because it is one step
  past_values <- data.frame(Period=time(fc.loop$x)%>% as_date, Past=fc.loop$x)
  
  forecast_values <- fc.loop %>% as.data.frame() %>% cbind(Period=test_start)
  
  gg_df <- past_values %>% full_join(forecast_values, by = c("Period")) %>% 
    left_join(item_df[,1:2], by = c("Period")) %>% 
    arrange(Period)
  
  
  gg_df[1:(nrow(gg_df)-2),"Sum_Quantity"] <- NA
  gg_df[(nrow(gg_df)-1),3:ncol(gg_df)] <- gg_df[(nrow(gg_df)-1),"Past"]
  
  
  colnames(gg_df) <- colnames(gg_df) %>% str_replace(" ","_")
  
  
  plot <- gg_df %>%
    ggplot(aes(x=Period)) +

    geom_ribbon(alpha=0.2, fill ="navy", aes(ymin = Lo_95, ymax = Hi_95)) +
    geom_ribbon(alpha=0.2, fill ="navy", aes(ymin = Lo_80, ymax = Hi_80)) + 
    geom_line(aes(y=Sum_Quantity, colour ="Actual"), size = 1.5, show.legend = TRUE ,alpha=0.8) +
    geom_line(aes(y=Past, colour="Past"),  size = 1.5, show.legend = TRUE) +
    geom_line(aes(y=Point_Forecast, colour = "Forecast"), size = 1.5, show.legend = TRUE, alpha=0.8) +
    #my_theme +
    scale_x_date (date_breaks = "6 months") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size=20),
          legend.title = element_blank(),
          legend.text = element_text(size=20)) +
    scale_color_manual(values=summertime3[c(5,3,1)]) +
    #scale_fill_manual(values=huey[c(4,6)]) +
    guides(colour = guide_legend(override.aes = list(size=10)))
  
  
  print(plot)
  
  plot_list[[i]] <- plot
  #print(accuracy(fc.loop, item_ts_test))
}
#putting all listed plots in a grid
plot_list <- rev(plot_list)

#First install Image Magick
#https://imagemagick.org/script/download.php
png(file="fc_plot%02d.png", width=1250, height=750)
for (n in (1:length(plot_list))){
  print(plot_list[[n]])
}
dev.off()



#This command will work for Windows
shell("magick convert -delay 300 -loop 3 fc_plot*.png Charts\\Forecast_Month.gif")
file.remove(list.files(pattern="fc_plot...png"))









#PLOTTING MULTIPLE FORECASTS FROM THE MODEL
plot_list <- list()
for (i in (1:8)) {
  item_ts_train <- item_ts2[1:(nrow(item_ts2)-i*4),] %>% ts()
  item_ts_test <- item_ts2[(end(item_ts_train)[1]+1): (end(item_ts_train)[1]+4),] %>%  ts(start=end(item_ts_train)[1]+1)
  
  
  fit.loop <- tslm(item_ts_train[,1]~ item_ts_train[,final_winners], data=item_ts_train)
  new_data <- item_ts_test %>% as.data.frame()
  fc.loop <- forecast (fit.loop, newdata = new_data)
  
  
  #Plotting forecast. Autoplot won´t work with this forecast because it is one step
  past_values <- data.frame(Row=time(fc.loop$x) %>% as.integer(), Past=fc.loop$x)
  
  forecast_values <- fc.loop %>% as.data.frame() %>% mutate(Row= rownames(.) %>% as.integer())
  
  actual_values <- item_ts_test[,1] %>% as.data.frame()
  
  actual_values <- cbind(actual_values, Row=forecast_values$Row)
  

  gg_df <- past_values %>% full_join(forecast_values, by = c("Row")) %>% 
    left_join(actual_values, by = c("Row")) %>% 
    rename(Actual= x) %>% 
    arrange(Row)
  

  colnames(gg_df) <- colnames(gg_df) %>% str_replace(" ","_")
  
  
  plot <- gg_df %>%
    ggplot(aes(x=Row)) +
    
    geom_ribbon(alpha=0.2, fill ="navy", aes(ymin = Lo_95, ymax = Hi_95)) +
    geom_ribbon(alpha=0.2, fill ="navy", aes(ymin = Lo_80, ymax = Hi_80)) + 
    geom_line(aes(y=Actual, colour ="Actual"), size = 1.5, show.legend = TRUE ,alpha=0.8) +
    geom_line(aes(y=Past, colour="Past"),  size = 1.5, show.legend = TRUE) +
    geom_line(aes(y=Point_Forecast, colour = "Forecast"), size = 1.5, show.legend = TRUE, alpha=0.8) +
    #my_theme +
    #scale_x_date (date_breaks = "6 months") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size=20),
          legend.title = element_blank(),
          legend.text = element_text(size=20)) +
    scale_color_manual(values=summertime3[c(5,3,1)]) +
    #scale_fill_manual(values=huey[c(4,6)]) +
    guides(colour = guide_legend(override.aes = list(size=10)))
  

  
  plot_list[[i]] <- plot
  #print(accuracy(fc.loop, item_ts_test))
}
#putting all listed plots in a grid
plot_list <- rev(plot_list)

#First install Image Magick
#https://imagemagick.org/script/download.php
png(file="fc_plot%02d.png", width=1250, height=750)
for (n in (1:length(plot_list))){
  print(plot_list[[n]])
}
dev.off()

#This command will work for Windows
shell("magick convert -delay 300 -loop 3 fc_plot*.png Charts\\Forecast_Week_Item5.gif")
file.remove(list.files(pattern="fc_plot...png"))


predictors_1 <- fc.winners1$model$coefficients %>% names() %>% str_remove_all("X")
predictors_1 <- predictors_1[-1]