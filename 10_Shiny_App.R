
library(shiny)
library(shinydashboard)
library (tidyverse)
library (lubridate)
Sys.setlocale("LC_ALL","English")

#--------------------------------------------------------------------------------------------------------

#Weeks ahead parameter added
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


#Saving colour palette 
summertime3 <- c("#236e96", "#15b2d3", "#ffd700", "#f3872f", "#ff598f")
huey <- c("#72d6c9","#ffc785", "#df7599", "#7189bf", "#145374", "#ff8260", "#007880", "#b42757")

#Saving theme for plots
my_theme <- theme( panel.background = element_blank(),
                   panel.grid.major = element_line(size = 0.3, linetype = 'solid',
                                                   colour = "#e5e6eb"),
                   axis.title.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.title.y=element_blank(),
                   axis.ticks.y=element_blank())


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
  
  fitted_values <- get(paste0("fc.winners",i))$fitted %>%
    as.data.frame() %>%
    mutate(Row = as.numeric(rownames(.))) %>% 
    rename (Fitted = x)
  
  colnames(forecast_values) <- colnames(forecast_values) %>% str_replace(" ","_")
  
  customer_sales_df <- joined_sales %>%
    filter(`Item No_` == item_vector[i]) %>% 
    mutate(Period = floor_date(`Shipment Date`,period) %>% as_date()) %>%
    group_by(Period, Customer_Code, `Item No_`) %>% 
    summarise(Sum_Quantity = sum(Quantity)) %>%
    ungroup()
  
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
    left_join(forecast_values, by=c("Row")) %>%
    left_join(fitted_values, by=c("Row")) %>%
    #left_join(customer_sales_df, by=c("Period")) %>% 
    mutate(`Item No_` = item_vector[i])
  
  
  gg_list[[i]] <- gg_df
  rm(gg_df)
}

#creating a dataframe with previosuly generated list, to be used for line & ribbon plots
gg_sales_df <- bind_rows(gg_list) %>%
  select(-Row) %>% 
  rename(Date=Period) %>%
  mutate(Panel = "Item Sales")




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

#joining previous dfs
gg_promos_df <- rbind(promo_df,price_promo_df) %>%
  filter(`Item No_` %in% item_vector) %>% 
  mutate(Panel = "Promotions")

#creating a vector of distinct customers for select input box
customers_distinct <- joined_sales_price %>%
  filter(`Item No_`== top_items$`Item No_`[2]) %>%
  select(Customer_Code) %>% 
  distinct() %>%
  #slice(-which(.$Customer_Code=="CP00")) %>% #removing this customer
  t() %>%
  as.vector()

#Transforming school holidays df for plotting
school_holidays <- school_cal_df %>% 
  mutate(Start_Week = floor_date(Start,period), End_Week = floor_date(End,period)) %>% 
  select(Start_Week, End_Week)

#Transforming school holidays df for plotting
bank_holidays <- bank_holidays_df %>% 
  mutate(Period = floor_date(start, period) %>%  as_date()) %>% #Creating period
  group_by(Period) %>%
  summarise(Easter = sum(Easter), Num_Holidays= n()) %>% 
  ungroup()


#BUILDING SHINY APP------------------------------------------------------------------------------------------------------

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
  checkboxGroupInput(inputId = "plot",
                     label = "Show Plot",
                     choices = c("Item Sales", "Promo Timeline"),
                     selected = "Item Sales"),
  
  #Checkbox to show fitted values
  checkboxGroupInput(inputId = "model",
                label = "Show Fitted Values",
                choices = c("Forecasted Values", "Fitted Values" ),
                selected = "Forecasted Values"),
  
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
  
  plotOutput("sales",height="650px")
  
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
    
    sales_df <-gg_sales_df %>%
      filter (year(Date) >= input$year_range[1] & year(Date) <= input$year_range[2] ) %>%
      filter(`Item No_`== input$item)
    
    sales_df
  })
  
  #Reactive expression that will produce filtered school holidays df
  SchoolInputDF <- reactive({
    
    react_sales_df <- SalesInputDF()
    
    school_holidays %>% 
      filter(Start_Week >= min(react_sales_df$Date)) %>% 
      filter(End_Week >= min(react_sales_df$Date)) %>% 
      filter(Start_Week <= max(react_sales_df$Date))# %>% 
    #filter(End_Week <= max(react_sales_df$Period)+ weeks(input$weeks_ahead))
    
  })
  
  #Reactive expression that will produce filtered school holidays df
  BankInputDF <- reactive({
    
    react_sales_df <- SalesInputDF()
    
    bank_holidays %>% 
      filter(Period >= min(react_sales_df$Date)) %>% 
      filter(Period <= max(react_sales_df$Date))
    
  })
  
  #Reactive expression that will produce promotion gannt chart
  GanntInputDF <- reactive({
    
    react_sales_df <- SalesInputDF()
    
    gannt_df <- gg_promos_df %>%
      filter(`Item No_`== input$item) %>% 
      filter(Date >= min(react_sales_df$Date) - weeks(12)) %>% 
      filter(Date <= max(react_sales_df$Date)) %>% 
      filter(Type == input$promo_type) %>% 
      filter(Customer_Code %in% input$customer)
    
    gannt_df
  })
  
  output$sales <- renderPlot({
    
    react_sales_df <- SalesInputDF()
    react_gannt_df <- GanntInputDF()
    
    react_school_holidays <- SchoolInputDF()
    react_bank_holidays <- BankInputDF()
    
    #EVALUATING "SHOW PLOT" INPUT
    if (is.null(input$plot)) {
      
      #returns nothing 
       
    } else if (all(input$plot == c("Item Sales", "Promo Timeline"))) {
      
      #returns a faceted plot
      p <- rbind(react_sales_df %>% select(Date, Panel),
                 react_gannt_df%>% select(Date, Panel)) %>% #Use a dummy dataframe for facets
        ggplot(aes(x = Date)) +
        scale_x_date (date_breaks = "2 months", date_labels = "%Y %b") +
        facet_grid(Panel~., scale="free") +
        theme(strip.background = element_blank(),
              strip.text.y = element_blank()) +
        geom_line(data = react_sales_df, aes(y=Past), colour = "#0c457d", size = 0.8, stat = "identity") +

        { 
          if (any(input$model == "Forecasted Values")) geom_ribbon(data = react_sales_df, fill="#0953aa", alpha=0.25, aes(ymin = Lo_80, ymax = Hi_80))
        } + 
        { 
          if (any(input$model == "Forecasted Values")) geom_ribbon(data = react_sales_df, fill="#00a5cf", alpha=0.25, aes(ymin = Lo_95, ymax = Hi_95))
        } +
        { 
          if (any(input$model == "Forecasted Values")) geom_line(data = react_sales_df, aes(y=Point_Forecast), colour = "#ff0097", size = 0.8)
        } +
        { 
          if (any(input$model == "Fitted Values")) geom_line(data = react_sales_df,aes(y=Fitted), colour = "black", size = 0.8, alpha = 0.3)
        } +
        
        geom_line(data = react_gannt_df, aes(y=Customer_Code, color=Customer_Code, group=`Sales Code`),size = 10,stat = "identity")+
        scale_y_discrete(breaks = NULL)+
        scale_colour_manual(values = huey)+
        theme(legend.position="bottom",
              legend.key = element_rect(fill = NA, colour = NA, size = 0.1),
              legend.title = element_blank()) +
        my_theme  
      
      
      
    } else if (input$plot == "Item Sales") {
      
      p <- react_sales_df %>% 
        ggplot(aes(x=Date)) +
        geom_line(aes(y=Past), colour = "#0c457d", size = 0.8) + 
        { 
          if (any(input$model == "Forecasted Values")) geom_ribbon(data = react_sales_df, fill="#0953aa", alpha=0.25, aes(ymin = Lo_80, ymax = Hi_80))
        } + 
        { 
          if (any(input$model == "Forecasted Values")) geom_ribbon(data = react_sales_df, fill="#00a5cf", alpha=0.25, aes(ymin = Lo_95, ymax = Hi_95))
        } +
        { 
          if (any(input$model == "Forecasted Values")) geom_line(data = react_sales_df, aes(y=Point_Forecast), colour = "#ff0097", size = 0.8)
        } +
        {
          if(any(input$model == "Fitted Values")) geom_line(aes(y=Fitted), colour = "black", size = 0.8, alpha = 0.3)
        } +
        #geom_line(aes(y=Fitted), colour = "black", size = 0.8, alpha = 0.3) +
        my_theme +
        scale_x_date (date_breaks = "2 months", date_labels = "%Y %b")
      
      } else if (input$plot == "Promo Timeline") {
        
        p <- react_gannt_df %>% ggplot(aes(x=(Date))) +
          geom_line(aes(y=Customer_Code, color=Customer_Code, group=`Sales Code`), size = 12) +
          scale_colour_manual(values = huey) +
          my_theme +
          theme(legend.position="bottom",
                legend.key = element_rect(fill = NA, colour = NA, size = 0.1),
                legend.title = element_blank())+
          scale_x_date (date_breaks = "2 months", date_labels = "%Y %b")+
          scale_y_discrete(breaks = NULL)
        
      } 
      

    
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

}
shinyApp(ui, server)
