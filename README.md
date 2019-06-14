# FORECASTS OF ITEM SALES FOR ORDER PLACEMENT
Project developed for a food and beverage distributor in Central Europe.
The sections in this readme are:
1. OBJECTIVES
2. CONTEXT
3. DATA
4. METHODOLOGY
5. CONCLUSIONS
6. INSTRUCTIONS FOR RUNNING CODE

For a more detailed story of the development of the project, read the [Report](https://github.com/Guillamon/master-data-science/blob/master/Report.md) in the repository 

## 1. OBJECTIVES
The purpose of the project is to improve the ordering system of the company, which is currently largely dependent on a single person and unstructured manual tasks.
In this sense, this project aims to achieve:
+ Help understand what influences demand of items and how
+ Use this knowledge to produce short term forecasts that can assist the task of order placement
+ Make relevant information available and easy to understand

## 2. CONTEXT
The company manages a stock with more than 1000 items. For practical reasons **the scope of the project is reduced to exploring and forecasting a subset of items belonging to a single brand**. The main **restrictions** that affect this brand are listed here:
+ Refrigerated products, with a **shelf life of only 60 days**
+ Orders placed on a **weekly** basis, and thus weekly incoming stock too
+ Lead time from placed order to delivery to customer of 3 to 4 weeks
+ Only a handful of customers: retail chains
+ New items launched every year
+ The brand belongs to a relatively new category that is still in growth stage in the beverage market. The brand actually kick-started the category in the market in 2014
+ The brand has several classes or families: 1 litre, 750ml, Smoothies, etc.

## 3. DATA
The company uses and ERP sytem, that runs on a Microsoft SQL Server as a database. The core of the data is obtained from there, and is saved in the folder "Data_Files". A brief description of the files is given:

+ **Item_Ledger_Entry.csv**: a register of outbound items and details about them
+ **Sales_Header.csv**: a register of sales with details about customers to whom items were sold
+ **Sales_Line.csv**: a register of sales with details about each operation
+ **Sales_Price.csv**: a register of prices set for each item and customer
+ **Promo_Campaigns.csv**: a register of each promotional campaign
+ **Customer_Key.csv** and **Class_Key.csv**: key tables used to input additional information to previous tables
+ **Item_Weights.csv**: a list of weights (or volume rather) of each item
+ **Purchase_Line.csv**: a register of purchases of items
+ **Sales_Brands.csv**: a sum of revenues produced by each brand since 2018

Other sources of data were used to get holiday calendars, but are not saved to the repository because they are obtained directly from their urls

## 4. METHODOLOGY
A preparatory cleaning and exploration of data has been necessary, in order to get it ready for deeper exploration and modelling.

**Monthly and weekly data has been used for exploration and forecasting**, which has divided the focus of the project. The reason for this has been the peculiarity of weekly data: the number of weeks in a year is variable, which affects seasonality. Also, the bulk of exploration and forecasting has been done with the data of the best-seller item, based on the hypothesis that general insights would be extendable to most items belonging to the brand.

When exploring, the main focus has been understanding how demand behaves and what could cause it. Once insights were obtained, modelling was attempted.

For the **training of models, time-series cross validation** (or forecasting on a rolling window) has been used, which favours more robust models than using a simple hold-out test. The metric that has been chosen to measure accuracy is **Mean Absolute Scaled Errors (MASE)**. This metric scales errors by using a benchmark model, in this case a naive forecast.

With **monthly data**, forecasts were of one month, and they were produced by:
+ Multiple Linear Regression
+ Exponential methods (Loess, Holt, Holt-Winters, ETS)
+ Arima models (both seasonal and non-seasonal)

It is worth mentioning that with multiple linear regression, the **predictors used can be grouped as**:
+ Number of holidays and business days in a month
+ Dummy variables of the four seasons
+ Unit Price of the Item for each customer
+ Number of days with promotions and promotion prices, both aggregated and for each customer
+ Lagged values of item sales, and of other predictors such as number of promotion days or sales of the class the item belongs to (excluding itself)
+ Lead values of holidays

With more than 50 predictors, the **approach used for selecting them was a hybrid stepwise regression**. That is, in an iterative process, adding predictors that increase accuracy the most. Then the process is inverted, by removing predictors and finding subsets that maximise accuracy. Finally, residuals of the remaining model are checked, and if they are not satisfying, predictors can be added or removed manually, and stepwise regression is reiterated.

When **weekly data** was approached, only multiple linear regression was attempted, due to issues with seasonality, and good performance with monthly data. The focus here shifted, and was to produce forecasts for more items. A total of five items were forecasted. The procedure

Finally, with the results of weekly foreacasts, a simple dashboard has been developed in Shiny. It allows to explore visualizations of relevant information used for forecasting.

## 5. CONCLUSIONS
**Mean Absolute Scaled Error (MASE)** was very useful in the sense that it allowed to have a "universal" notion of how the models were performing across time series. It would be helpful though to **explore the possibility of adjusting it so that it penalises positive and large errors**. This is due to business logic: it is crucial that there is always enough stock for customers to be serviced, so overestimating demand (negative error) is welcome; but it is undesirable to be over-stocked and generate waste.

**Multiple linear regression worked much better than expected**. The convenience of it is that it **works well with weekly data**, and models can learn surprisingly well from relatively short time series. This is very useful because new items are launched regularly, so having more than 200 observations is rarely the case. Also, the **approach used here seems extensible to other families of products**, as long as they are heavily dependant on promotions (very likely) and influenced by holidays. The **downside** to the approach is that the models produced are too complex (a range of 15 to 25 predictors per model), and would probably require re-evaluating frequently (every 4 months would be prudent). This also results in a loss in interpretability. 

In the future **exploring other models would be a healthy exercise.** More advanced models such as Vector Autoregression, TBATS or Random Forest have proved useful in business forecasting. Particularly ensembling models would really enrichen forecasts. Using neural networks seems tempting to boost accuracy, but unpractical due to insufficient observations, and too many item forecasts to train.

The **dashboard could still dig deeper**, showing more dimensions and visualizations such as item sales per customer, unit price per customer, or level of stock. Shiny proved to be a useful and flexible framework to continue building this.

As a final thought, **multiple linear regression was not accurate enough to provide automated orders**, however it does seem like a useful tool as support for taking decisions, especially if the data that feeds is it is made available and understandable to the decision-maker. This is the **building block to start testing the use of forecasts in the company**.

## 6. INSTRUCTIONS FOR RUNNING CODE
The entire code has been developed with R, the advantages of it being the useof the tidyverse collection, and the forecast package.
A Windows 10 OS was used, so make sure you run it there.
To get the code running, **install the following**:
1. **Anaconda**. You can download it [here](https://www.anaconda.com/distribution/)
2. **R**
3. **RStudio** (make sure it is at least version 3.4.2)
4. This is accesory: you can install **Image Magick** for Windows to produce GIFs from charts. You can download it [here](https://imagemagick.org/script/download.php)

For a detailed step by step of installation go [here](https://www.guru99.com/download-install-r-rstudio.html#8)

Now download or clone this repository on your local disk. 
From RStudio, open the RPROJ file master-data-science.proj. Always run the code from there to make sure paths to directories are correct.

When running the code **remember this**:
+ Pay special attention to running the script Install_Packages.R correctly
+ Have an internet connection when running Holidays.csv, since it imports data directly from urls
+ Once you have opened the project, run the scripts in the order that is assigned to their file names
+ Make sure that *Data*, *Values*, and *Functions* that result from running each script are saved to the environment to move on to the next one
+ The scripts with suffix "week" overwrite saved *Data*, *Values* and *Functions* from the scripts with suffix "month"

Final remarks regarding the **Dashboard**:
+ To access it, execute the script Shiny App. It has not been hosted on a server online, so this is the only way to access it
+ It has only one page, where all toggling options appear on the sidebar
+ You can see a small demo [here](https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Shiny_Demo.gif)
