#Shiny
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("ggthemes")
library(shiny)
library(shinydashboard)
library (tidyverse)
library (lubridate)
Sys.setlocale("LC_ALL","English")

#--------------------------------------------------------------------------------------------------------

#https://colorhunt.co/
create_period_df_week <- function(dataframe,period_column= Period, period="week", weeks_ahead=0) {
  first_observation <- dataframe$Period %>% min() %>% floor_date(period) %>% as_date()
  last_observation <-  dataframe$Period %>% max() %>% floor_date(period) %>% as_date() + weeks(weeks_ahead)
  
  c1 <- first_observation
  period_df <- data.frame(Period = c1)
  
  n_periods <- time_length (interval (first_observation, last_observation), unit= period)
  for (n in 2: (n_periods+1)) {
    period_df [n,"Period"] <- (first_observation + weeks(n-1)) #+ weeks(weeks_ahead))
  }
  period_df <- period_df %>% 
    mutate(Period = as_date(Period))
  return (period_df)
}


#Storing vector with forecasted items
item_vector <- top_items$`Item No_`[2:6]

#creating a list with dataframes that contain past and forecasted values for all items
gg_list <- list()
for(i in 1:5) {
  past_values <- get(paste0("fc.winners",i))$x %>%
    as.data.frame() %>%
    mutate(Row = as.numeric(rownames(.))) %>% 
    rename (Past = x)
  
  forecast_values <- get(paste0("fc.winners",i)) %>% 
    as.data.frame() %>% 
    mutate(Row= as.numeric(rownames(.)))
  
  colnames(forecast_values) <- colnames(forecast_values) %>% str_replace(" ","_")
  
  #name <- paste("gg_item",i,sep="_")
  
  gg_df <- sales_df_item %>%
    #filter(Period > '2016-01-01') %>% 
    filter(Period < "2019-04-01") %>% 
    filter(`Item No_` == item_vector[i]) %>% 
    select(-`Item No_`) %>% 
    create_period_df_week(period = period, weeks_ahead = 4) %>%
    mutate(Row = as.numeric(row.names(.)))
  
  gg_df <- gg_df[((nrow(gg_df)-nrow(past_values))-3):nrow(gg_df),]
  rownames(gg_df) <- NULL
  
  gg_df <- gg_df %>% 
    mutate(Row = as.numeric(row.names(.))) %>% 
    left_join(past_values, by=c("Row")) %>% 
    left_join(forecast_values, by=c("Row"))
  
  
  gg_list[[i]] <- gg_df 
  
}




#USING THIS TO EXPERIMENT

n_yline <- 500

my_theme <- theme( panel.background = element_blank(),
                   panel.grid.major = element_line(size = 0.3, linetype = 'solid',
                                                   colour = "#e5e6eb"),
                   axis.title.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.title.y=element_blank(),
                   axis.ticks.y=element_blank())
                   #axis.text.y=element_blank())

p <- gg_list[[1]] %>%
  ggplot(aes(x=Period)) +
  geom_ribbon(fill="#0953aa", alpha=0.25, aes(ymin = Lo_80, ymax = Hi_80)) + 
  geom_ribbon(fill="#00a5cf", alpha=0.25, aes(ymin = Lo_95, ymax = Hi_95)) +
  geom_line(aes(y=Point_Forecast), colour = "#ff0097", size = 0.8) +
  geom_line(aes(y=Past), colour = "#0c457d", size = 0.8) +
  my_theme +
  scale_x_date (date_breaks = "3 months") +
  scale_y_continuous(breaks = seq(0, 6000, n_yline))#+
  
i <- geom_rect(school_holidays_df,
               mapping =aes(xmin = Start_Week,
                            xmax = End_Week,
                            ymin = -Inf, ymax = +Inf),
               inherit.aes = FALSE,
               fill = 'yellow', alpha = 0.1)
l <- geom_vline(data = bank_holidays, aes(xintercept = Period), color="dark grey", size = 0.2)

trial <- NA
if (is.na(trial)){
  p+i
  
} else if (all(trial == "Bank Holiday")){
      p + l
  
} else if(all(trial == c("Bank Holiday","School Holiday"))) {
      p + i + l
    } else if (is.na(trial)) {p}




all(trial ==c(TRUE,TRUE))

  

summertime3 <- c("#236e96", "#15b2d3", "#ffd700", "#f3872f", "#ff598f")
huey <- c("#72d6c9","#ffc785", "#df7599", "#7189bf", "#145374", "#ff8260", "#007880", "#b42757")


promo_df %>%
  filter(Date > '2016-01-01') %>% 
  filter(Date < '2019-05-01') %>% 
  ggplot(aes(x=Date, y=Customer_Code, color=Customer_Code, group=`Sales Code`)) +
  geom_line(size = 10) +
  labs(x="", y=NULL, title="Promotion Timeline") +
  scale_colour_manual(values = huey) +
  my_theme +
  theme(legend.position="bottom") +
  theme(legend.key = element_rect(fill = NA, colour = NA, size = 0.1)) +
  scale_x_date (date_breaks = "3 months") +
  scale_y_discrete(breaks = NULL)

cowplot::plot_grid(p1,p2, align = "v", nrow=2)





#creating a dataframe with promo price dates to plot a gantt chart
price_promo_df <- joined_sales_price %>%
  select(`Sales Code`, `Item No_`, Customer_Code, Starting_Date_Price_Promo, Ending_Date_Price_Promo) %>% 
  rename(Start = Starting_Date_Price_Promo,
         End = Ending_Date_Price_Promo) %>% 
  gather("State", "Date", 4:5) %>%
  drop_na() %>% 
  mutate (Type = "Price Promo")

#creating a dataframe with promo dates to plot a gantt chart
promo_df <- joined_sales_price %>%
  select(`Sales Code`,`Item No_`, Customer_Code, Starting_Date_Promo, Ending_Date_Price_Promo) %>% 
  rename(Start = Starting_Date_Promo,
         End = Ending_Date_Price_Promo) %>% 
  gather("State", "Date", 4:5) %>%
  drop_na() %>% 
  mutate (Type = "Promo") 

joined_promos_df <- rbind(promo_df,price_promo_df)


customers_distinct <- joined_sales_price %>%
  filter(`Item No_`== top_items$`Item No_`[2]) %>%

  select(Customer_Code) %>% 
  distinct() %>% 
  t() %>% 
  as.vector()


school_holidays <- school_cal_df %>% 
  mutate(Start_Week = floor_date(Start,period), End_Week = floor_date(End,period)) %>% 
  select(Start_Week, End_Week)

bank_holidays <- bank_holidays_df %>% 
  mutate(Period = floor_date(start, period) %>%  as_date()) %>% #Creating period
  group_by(Period) %>%
  summarise(Easter = sum(Easter), Num_Holidays= n()) %>% 
  ungroup()

geom_rect(school_holidays, mapping =aes(xmin = Start_Week, xmax = End_Week, ymin = -Inf, ymax = +Inf), fill = 'yellow', alpha = 0.2)

#------------------------------------------------------------------------------------------------------

#Defining what shall appear in the sidebar. To be passed to ui()
sidebar <- dashboardSidebar(
  
  # Create a selection box for item
  selectInput(inputId = "item",
              label = "Item",
              choices = item_vector,
              selected = "R240271"),

  
  # Create a slider for years
  sliderInput(inputId = "year_range",
              label ="Year",
              min = min(year(joined_sales_price$Period)),
              max = max(year(joined_sales_price$Period)),
              value = c(2016, max(year(joined_sales_price$Period))),
              step =1),

  
  # Create a checkbox for customers
  checkboxGroupInput(inputId = "holidays",
                     label = "Show Holidays",
                     choices = c("Bank Holidays", "School Holidays"),
                     selected = NULL),
  
  # Create a radio button for promo or price_promo
  radioButtons(inputId = "promo_type",
               label = "Promo Type",
               choices = c("Promo","Price Promo"),
               #choiceNames = c("Promotions", "Promotion Prices"),
               selected = c("Promo")),
  
  # Create a checkbox for customers
  checkboxGroupInput(inputId = "customer",
                     label = "Customer Code",
                     choices = customers_distinct,
                     selected = customers_distinct)
  
)


#defining what shall appear in body. To be passed to ui()
body <- dashboardBody(
  
  plotOutput("sales",height="800px"),
  plotOutput("gannt_promo")

)


#setting up ui with previous building blocks
ui <- fluidPage(
  dashboardPage( header = dashboardHeader(),
                 sidebar = sidebar,
                 body = body
  )
)


#setting up server function
server <- function(input, output) {
  
  #Reactive expression that will produce sales line chart
  SalesInputDF <- reactive({
    
    sales_df <- gg_list[[which(item_vector == input$item)]] %>%
      filter (year(Period) >= input$year_range[1] & year(Period) <= input$year_range[2] )
    
    sales_df
  })
  
  #Reactive expression that will produce filtered school holidays df
  SchoolInputDF <- reactive({
    
    react_sales_df <- SalesInputDF()
    
    school_holidays %>% 
      filter(Start_Week >= min(react_sales_df$Period)) %>% 
      filter(End_Week >= min(react_sales_df$Period)) %>% 
      filter(Start_Week <= max(react_sales_df$Period))# %>% 
      #filter(End_Week <= max(react_sales_df$Period)+ weeks(input$weeks_ahead))
    
  })
  
  #Reactive expression that will produce filtered school holidays df
  BankInputDF <- reactive({
    
    react_sales_df <- SalesInputDF()
    
    bank_holidays %>% 
      filter(Period >= min(react_sales_df$Period)) %>% 
      filter(Period <= max(react_sales_df$Period))
    
  })
  
  #Output sales line chart
  #output$sales <- renderPlot({
  #  react_sales_df <- SalesInputDF() 
  #  react_sales_df %>%    
  #    ggplot(aes(x=Period, y=Sum_Quantity))+geom_line(colour = "dark blue") +
  #    labs(x="", y=NULL, title="Item_Sales")
    
  #})
  
  output$sales <- renderPlot({
    
    react_sales_df <- SalesInputDF()
    react_school_holidays <- SchoolInputDF()
    react_bank_holidays <- BankInputDF()
    
    p <- react_sales_df %>% 
      #forecast_values %>%   
      #as.data.frame() %>% 
      ggplot(aes(x=Period)) +
      #ggplot(aes(x=seq(201,204))) +
      geom_ribbon(fill="#0953aa", alpha=0.25, aes(ymin = Lo_80, ymax = Hi_80)) + 
      geom_ribbon(fill="#00a5cf", alpha=0.25, aes(ymin = Lo_95, ymax = Hi_95)) +
      geom_line(aes(y=Point_Forecast), colour = "#ff0097", size = 0.8) +
      geom_line(aes(y=Past), colour = "#0c457d", size = 0.8) +
      my_theme +
      scale_x_date (date_breaks = "2 months", date_labels = "%Y %b")
      #scale_y_continuous(breaks = seq(0, 6000, n_yline))
      
    i <- geom_rect(react_school_holidays,
                mapping =aes(xmin = Start_Week,
                             xmax = End_Week,
                             ymin = -Inf, ymax = +Inf),
                inherit.aes = FALSE,
                fill = 'yellow', alpha = 0.1)
    
    l <- geom_vline(data = react_bank_holidays, aes(xintercept = Period), color="#ff6337", size = 0.4, alpha=0.5)
    
    if (is.null(input$holidays)){
      p
    } else if (all(input$holidays == "Bank Holidays")){
      p + l
      
    } else if (all(input$holidays == "School Holidays")) {
      p + i
    } else { p + i + l}
    
  })
  
  #Reactive expression that will produce promotion gannt chart
  GanntInputDF <- reactive({
    
    react_sales_df <- SalesInputDF()
    
    gannt_df <- joined_promos_df %>%
      filter(`Item No_`== input$item) %>% 
      #filter (year(Date) >= input$year_range[1] & year(Date) <= input$year_range[2]) %>% 
      filter(Date >= min(react_sales_df$Period) - weeks(12)) %>% 
      filter(Date <= max(react_sales_df$Period)) %>% 
      filter(Type == input$promo_type) %>% 
      filter(Customer_Code %in% input$customer)
    
    gannt_df
  })
  
  #Output promotion Gannt Chart
  output$gannt_promo <- renderPlot({
    react_gannt_df <- GanntInputDF()
    react_sales_df <- SalesInputDF() %>% 
      rename(Date=Period)
    
    react_sales_df %>%    
      ggplot(aes(x=(Date))) +
      geom_line(data=react_gannt_df,aes(y=Customer_Code, color=Customer_Code, group=`Sales Code`), size = 12) +
      scale_colour_manual(values = huey) +
      my_theme +
      theme(legend.position="bottom") +
      theme(legend.key = element_rect(fill = NA, colour = NA, size = 0.1))+
      #theme(text = element_text(size=8)) +
      scale_x_date (date_breaks = "8 weeks", date_labels = "%Y %b")+
                    #limits =as.Date(c("2016-02-01","2019-05-01")) ) +
      
      #labs(x=NULL) + #title="Promotion Timeline")+
      #labs(x="", y=NULL, title="Promotion Timeline") +
      scale_y_discrete(breaks = NULL)

    
  })
}
shinyApp(ui, server)


#---------------------------------------------


#trial <- full_join (gg_df,promo_df, by= c("Period" = "Date")) %>% 
#  ggplot(aes(x=Period,y=value,ymin=0,ymax=value)) + facet_grid(variable~., scales='free')


x <- seq(1992, 2002, by=2)
d1 <- data.frame(x=x, y=rnorm(length(x)))
xy <- expand.grid(x=x, y=x)
d2 <- data.frame(x=xy$x, y=xy$y, z= jitter(xy$x + xy$y))

d1$panel <- "a"
d2$panel <- "b"
d1$z <- d1$x

d <- rbind(d1, d2)

p <- ggplot(data = d, mapping = aes(x = x, y = y)) + 
  facet_grid(panel~., scale="free") + 
  geom_line(data = d1, stat = "identity") + 
  geom_tile(data=d2, mapping=aes(colour=z, fill=z), stat = "identity")
p

joined_promos_df

trial1 <- gg_list[[1]] %>%
  select(-Row) %>% 
  rename(Date = Period) %>% 
  mutate(`Item No_` = item_vector[1]) %>% 
  mutate(Customer_Code = NA) %>% 
  mutate(Type= NA) %>% 
  mutate(`Sales Code`=NA) %>%
  mutate(Type = NA) %>% 
  mutate(State = NA) %>%
  mutate(Panel = "Item Sales") %>% 
  select(`Sales Code`, `Item No_`, Customer_Code, State, Date, Type, Panel, everything())# %>% 
  

trial2 <- joined_promos_df %>% 
  mutate (Past =NA) %>% 
  mutate(Point_Forecast = NA) %>% 
  mutate(Lo_80 = NA) %>% 
  mutate(Hi_80 = NA) %>% 
  mutate(Lo_95 = NA) %>% 
  mutate(Hi_95 = NA) %>%
  mutate(Panel = "Promotions") %>% 
  select(`Sales Code`, `Item No_`, Customer_Code, State, Date, Type, Panel, everything())

rbind(trial1,trial2) %>%
  ggplot(aes(x = Date)) +
  facet_grid(Panel~., scale="free") + 
  geom_line(data = trial1, aes(y=Past), colour = "#0c457d", size = 0.8, stat = "identity") +
  geom_ribbon(fill="#0953aa", alpha=0.25, aes(ymin = Lo_80, ymax = Hi_80)) + 
  geom_ribbon(fill="#00a5cf", alpha=0.25, aes(ymin = Lo_95, ymax = Hi_95)) +
  geom_line(aes(y=Point_Forecast), colour = "#ff0097", size = 0.8) +
  my_theme +
  #l +
  i+
  geom_line(data = trial2, aes(y=Customer_Code, color=Customer_Code, group=`Sales Code`),size = 10,stat = "identity")+
  scale_y_discrete(breaks = NULL)+
  scale_colour_manual(values = huey)+
  my_theme +
  theme(legend.position="bottom") +
  theme(legend.key = element_rect(fill = NA, colour = NA, size = 0.1)) +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())
  #scale_x_date (date_breaks = "3 months") +
  #scale_y_discrete(breaks = NULL)
  

  
  geom_tile(data=d2, mapping=aes(colour=z, fill=z), stat = "identity")


