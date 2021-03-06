# FORECASTS OF ITEM SALES FOR ORDER PLACEMENT
## OR THE DISMAL DIVE INTO THE FUTURE OF BEHEMOTH


### Chapter I: A  Case For The Flawless Monocle

The sniff of this case first came to me when I was fresh newcomer at my current company not many months ago. Despite being a humble **food & beverage distributor** in Central Europe, the first thing that called my attention was the suspiciously **broad range of products, brands -and therefore suppliers** a small company like mine could handle.

I knew I was on track when I found that many **operations were still heavily dependent on physical paperwork, and what is worse: Excel!**.

One of the most chaotic, obscure and time-consuming operations was **order placement**, where you could be buried in heaps of Excel files and bible-long email loops with erratic pieces of important information flying about everywhere.
Certainly if there was something that could crunch the efficiency of operations it was addressing this.

### Chapter II: The  Purple Purpose

But what could I do in this dirty job with my humble and newly acquired Data Scientist skills? I decided I was going to try two things:

+ Help understand what influences demand of items and how
+ Use this knowledge to attempt to forecast short term demand of items
+ Make relevant information available and easy to understand

With a scope of hundreds of different items I had to begin somewhere, and after some questioning and querying, I came across this chart that help me get started.

<h5 align="center">THE LEFT SIDE OF THE LONG TAIL IS THE TARGET </h5> 

&nbsp;

<p float = "left" align = "center" >
  <img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Purchase1.png width="430" />
  <img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Purchase2.png width="430" /> 
</p>

&nbsp;

Yes, it was definitely the left side of the long tail that I had to go after. It boiled down to either TheDaughterofTime, or PresumedInnocent. A little more investigation made me decide: I would begin by exploring the latter, a brand that had the following restrictions:

+ Refrigerated products, with a **shelf life of only 60 days**
+ Orders placed on a **weekly** basis, and thus weekly incoming stock too
+ Lead time from placed order to delivery to customer (remember this is a distributor) of 3 to 4 weeks
+ Only a handful of customers: retail chains
+ New items launched every year
+ The brand belongs to a relatively new category that is still in growth stage in the beverage market. The brand actually kick-started the category in the market in 2014
+ The brand has several classes or families: 1 litre, 750ml, Smoothies, etc.

Here is the weekly sales of the chosen brand.

<h5 align="center"> BRAND´S WEEKLY SALES LOOK KIND OF FUZZY</h5> 

<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Brand_Weekly.png width="500" height = "300" /> 
</p>

&nbsp;

And This is what sales of the top 5 items looked liked:

<h5 align="center">ITEM SALES DON´T SEEM TOO HOMOGENEOUS, SCARY? </h5> 

<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Top_Items_Weekly.png width=700 height = 500>
</p>

&nbsp;

At that moment I was new to Time Series, but this **kind of resembled white noise.**
It had been a brave move, but could it have been foolish too? 

### Chapter 3: The  Misty Data Of Jericho

Luckily, my company used an ERP sytem, that ran on a **Microsoft SQL Server** as a database. The core of the data I would use came from here. I **connected the database to RStudio** and queried from there, loading the data to the environment. However, if any poor soul ever needs to run my code, the data is uploaded in my **Git repo as CSVs**.
Incidentally, other sources of data were used to get holiday calendars, for which ics files were parsed and pdfs scraped.

Of course, data was anonimized to keep curious cats from fishing around.

To get the data ready, the biggest job was to dive in the database with no guide, and make sure the extraction and merging of tables was consistent. When cleaning it, you may have hinted, I resolved to use R - I couldn´t miss out on **dplyr and the tidyverse package** in general. Some important **decisions** I made when cleaning were:

+ Several relevant dates were registered for sales operations. Finally, it was the **date of shipment that was chosen as observation date**
+ Exclude all sales to customers that no longer purchase an item
+ Exclude sales to customers that are not retail chains (account)
+ Keep only sales at the main market (the company operates in several countries)
+ Consider the sales price of an item during a given period as its promotional price if it has it
+ In sales prices, **NAs occured when a customer had not yet listed a product**. Instead of replacing them with zeros, the adjustment was to set a fixed high value:10 euros -prices are in the range of 0.50- 1.50 euros
+ Some retail chains are composed of autonomous business units, that may negotiate and purchase as one, or independently. The first choice was to **consider all business units as one customer**. This implied another issue: a different sales price may be assigned at the same moment. The choice was to keep always the lowest price set
+ It is worth saying that the register of promotion campaigns was pretty messy, so a few small decisions had to be made, like leaving  out campaigns with no ending date (not too many cases)

I was now ready to get my hands truly dirty.

### The Phantom Methodology Of Parade

I finally had the data I craved for, but what could I do with it? I was blessed to use *Forecasting: Principles and Practice, by Rob J Hyndman and George Athanasopoulos* as a guide. This led me to choose **R for this phase** of my case too, since the book led me to the **forecast package** developed by Hyndman, and time series objects.

My first task was to make my mind up as to what was the final purpose of the forecast. It would be **to provide a guide, or support in the weekly human decision of how much of each item should be ordered** (altough I secretly desired ultimate automation). To this end, the **forecast period had to match the lead time in the decision-makers´mind: 3 to 4 weeks**.

Also I could not forget, that perhaps even more important than the forecasts was the provision of relevant information for the task of ordering. Any insight would be precious.

When I had chosen a brand to investigate, I had made an assumption: business conditions would be common to the items belonging to it. It was convenient to be consistent with it, so **all my attempts at forecasting would be made only with the top-seller item**. If I found anything interesting, I would then extend it to more items to test my hypothesis.

As to what **frequency of observations** I was going to use, it was pretty obvious that the case´s restrictions called for weekly data. However I had read that weekly data could be tricky due to variable number of weeks along years, so **I gave it a first try with monthly data**. This would allow me to use Exponential Methods and ARIMA Models.

This is what the monthly sales of the top-seller item looked like:

<h5 align="center">IT´S YOU AND ME ALONE NOW</h5> 

<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Target_Item_Monthly.png width=500 height = 300>
</p>

&nbsp;

Trend was crystal clear, and cycles were not an issue. My main concern was to find patterns of seasonality (beyond weekly of course), but it didn´t look good at first sight. There appeared to be slightly regular peaks and troughs

<h5 align="center">ANY SEASONALITY IN THESE SEASONAL PLOTS? </h5> 

<p float = "left" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Polar_Season_Plot.png height="300">
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Season_Plot.png height="300" >
&nbsp;

It wasn´t orthodox, but I decided to see if a moving average could show me something that I was missing. And four windows told me it was possible that there was quarterly seasonality. Although it wasn´t too clear that a pattern was picking up.

<h5 align="center">AN UNORTHODOX USE OF MOVING AVERAGE </h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Monthly_Smoothed.png width=500 height = 300>
</p>
&nbsp;

This same pattern showed up when looking at the sales of the item´s class (750 ml), and the brand too.

<h5 align="center">LIKE FATHER, LIKE SON</h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Smoothed_Comparison.png width=500 height = 300>
</p>
&nbsp;

And finally, using more advanced methods, X11 and Loess decomposition showed the expected seasonality. Seats however was unable to do so, and to me this just could mean that quarter seasonality did not look rock solid. What could be causing this? 


<h5 align="center"> ADVANCED DECOMPOSITION OF THE CORPSE</h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/X11.png width=500 height = 300>
</p>
&nbsp;

<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/LOESS.png width=500 height = 300>
</p>
&nbsp;

<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/SEATS.png width=500 height = 300>
</p>
&nbsp;

The main suspect behind the irregular behaviour of the time series was of course, **promotions**. I had to drill a hole here. The first question was to be made was **how active was customers´promotional activity?**. This timeline said "enough":

<h5 align="center"> PROMOTIONS: REGULARLY IRREGULAR</h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Promo_Timeline.png width=600 height = 400>
</p>

*Note: Customers have to buy stock before promotions, so that is why promotion prices have a longer duration*

&nbsp;

The sum of the number of promotional days in each month also revealed something interesting: patterns were shifting, in a way that reminded a lot of the shifting of peaks and troughs in our time series

<h5 align="center"> SHIFTY PROMOTIONS </h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Promo_Evolution_Monthly.png width=600>
</p>

&nbsp;

I now had enough information to attempt my first approach to forecasting: a **Multiple Linear Regression**.
This had the advantage of being simple, interpretable, and fast to compute. Also I had **information of promotions** with one month in advance, which would also make it feasible.

I tried many predictors, and spent some time engineering new features reiteratively. These are the predictors that worked best:

+ Number of Holidays and Business Days
+ Dummy season variables (I decided not to use a Fourier series because my approach only added four predictors)
+ Number of Days with Promotions and Promotion Prices, aggregated and for each customer too 
+ Unit Price of the Item for each Customer
+ Lagged Values of the item sales, of the sales of all items of the same class (excluding the target item of course), of promotions, and of holidays

However my main concerns were others...

First of all, finding a method to get to a model with reasonable accuracy. Although I tried it, I **couldn´t rely on a simple holdout sample** for two reasons: with monthly data I had at best 50 solid observations (the brand started in 2014), so I really couldn´t afford leaving observations out. Plus, this metohod would leave me stark naked if the model was just really good on the tested period

**Time series cross validation (TSCV)**, also known as forecasting on a rolling window, **had to be my approach**. This method runs several tests, where it has trained a model only with observations that ocurred *before* the test. Of course, this meant slower computing too, but the main advantage was that it gives a better idea of the robustness of a model. After a little exploring, I concluded the TSCV would begin twelve months before the last observation.

My other concern was the **metric I should use for accuracy**. In the end I was inclined to use **Mean Absolute Scaled Errors, MASE**. This takes the Mean Absolute Error, and scales it by using the MAE of a benchmark method: Naive Forecasting. A 1 states that a model is just as bad a Naive forecast, and a 0.5 is two times better. The great advantage of the metric clearly was that I could use it to compare different models and forecasts of other time series.

The greatest of all my worries was however, **finding a method fast and reliable enough to test combinations of predictors to come up with a decent model**. My problem was that with more than 40 predictors all possible combinations were countless, so trying all was NOT an option. To make a long story short the best way I found relied on randomness and bruteforce, just my style! 

This method was **Hybrid Stepwise Regression**. I defined a function that added one predictor at a time. The one that increased accuracy the most was chosen. This process was iterated until accuracy would improve no longer. Then I would replicate the process removing predictors. I would check the residuals of the resulting model, and if they didn´t satisfy me, I would simply add a few predictors more or less randomly and reiterate. Surprisingly this would give good results.

As a final step, I wanted to know **when should I cut my data**, since I didn´t want to train with too long or too short time series. With the model I found, I performed **multiple forecasts, each beginning at a different month**. The month that returned best accuracy would be when my time series should start.

Also, to mention a fiasco: I **handling outliers by moderating them**, that is changing their values to the minimum or maximum value of a n-tile. It worked in the first iterations, when accuracy was still low. Later, when accuracy improved the method just worsened forecasts. I also tried **Box-Cox tranformations**, and regardless the lambda I used, the same thing happened.

<h5 align="center"> TESTS OF THE WINNER MODEL, PRETTY SATISFYING </h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Forecast_Month.gif width=600>
</p>

&nbsp;

Regression had worked pretty well, and now what could **Exponential Methods and Arima** do for me?

To make it brief, I first explored Loess, Holt, Holt Winters and ETS (all implemented wonderfully by forecast package). These weren´t looking too impressive so I quicly hopped on to what I really was looking forward to- Arima. Because I wasn´t too sure about the existence of seasonality, I tried both normal and seasonal Arima. And I was feeling brave so I read a bit about the topic, dissected my time series in a dark corner, and attempted to produce my models manually before resourcing Auto Arima function... I never managed to outperform it.

Finally I ran a timeseries cross validation for all the models I had produced in my monthly-data-exploration. **Non of the exponential or Arima models stood the comparison to multiple linear regression**. Actually, those weren´t even much better than a Naive forecast.


<h5 align="center">THESE DON´T LOOK TOO BAD HERE, BUT TSCV DOESN´T THINK SO </h5>
<p float = "left" align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Forecast_ETS_Month.png height=120>
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Forecast_HW_Month.png height=120>
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Forecast_Manual_Sarima_Month.png height=120>
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Forecast_Auto_Sarima_Month.png height=120>
</p>

&nbsp;

It was now **time to approach weekly data**. I was **only going to try a multiple linear regression**. The model had performed pretty well with monthly data, plus time was pressing, and Arima had not been much better. Also, the latter implied dealing with the issue of variable number of weeks in a year, and I was not convinced by workarounds I had read about.

Before going straight to it, I dug a little deeper, and explored the weekly data time series. Now I was **really interested in how demand behaved in relation to both bank holidays and school holidays**.

<h5 align="center"> BANK HOLIDAYS </h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Bank_Holiday_Plot.gif width=600>
</p>

<h5 align="center"> SCHOOL HOLIDAYS </h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/School_Holiday_Plot.gif width=600>
</p>

&nbsp;

It looked like I **could expect peaks before and after bank holidays, and troughs during them**. A similar pattern could be seen for school holidays, only that, because their duration is longer, buyer activity would simply moderate. Further from that, there didn´t seem to be weekly seasonality, only certain weeks at which activity tended to lower or intensify- in most cases due to holidays.

Back to the **multiple linear regression**, I recycled the good old same predictors I had used for monthly data, but added weeks after and before holidays, and was **careful not to use lagged sales values inferior to four weeks** (my forecast window).

And although my time series was really messy, regression did an unexpectedly decent job. Here you can see a few of the tests performed by TSCV with the winner model:

<h5 align="center"> WEEKLY DATA FORECAST TESTS, NOT THAT SCARY ANYMORE </h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Forecast_Week_Item1.gif width=600>
</p>

&nbsp;

I had to take a deep breath, it was now time for the ultimate test: I had to **try forecasting sales of other items of the same brand**. 

So I tried the **same approach for multiple linear regression with four more items**... and although some time series only had observations from 2018, they still performed surprisingly well:

<h5 align="center"> 2nd ITEM: WINNER MODEL TESTS </h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Forecast_Week_Item2.gif width=600>
</p>

&nbsp;

<h5 align="center"> 3rd ITEM: WINNER MODEL TESTS  </h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Forecast_Week_Item3.gif width=600>
</p>

&nbsp;

<h5 align="center"> 4th ITEM: WINNER MODEL TESTS  </h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Forecast_Week_Item4.gif width=600>
</p>

&nbsp;

<h5 align="center"> 5th ITEM: WINNER MODEL TESTS  </h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Forecast_Week_Item5.gif width=600>
</p>

&nbsp;

I had now something solid enough to go a step further, I had not forgotten my other dark purpose. The case was calling for a Dashboard. I rolled my sleeves up and quickly built an **interactive Shiny app**, where the end user could basically explore the time series, check the forecast of an item, and see at the same time when promotions and holidays had happened or would happen. Was that not making relevant information available? Of course I was still leaving important data out, but this was more than enough to start testing it at my company... 

<h5 align="center"> A SHORT DEMO FOR A SIMPLE APP <br/> (mouse on the sidebar!) </h5>
<p align = "center" >
<img src=https://raw.githubusercontent.com/Guillamon/master-data-science/master/Charts/Shiny_Demo.gif width=900>
</p>


### THE SILK LESSONS OF THE TIGER

The fury of the case was over already, it was time to sit down and think what had I learnt.

In the first place, **Mean Absolute Scaled Error (MASE)** was very useful in the sense that it allowed to have a "universal" notion of how the models were performing across time series. It would be helpful though to **explore the possibility of adjusting it so that it penalises positive and large errors**. This is due to business logic: it is crucial that there is always enough stock for customers to be serviced, so overestimating demand (negative error) is welcome; but it is undesirable to be over-stocked and generate waste. 

**Multiple linear regression worked much better than expected**. The convenience of it is that it **works well with weekly data**, and models can learn surprisingly well from relatively short time series. This is very useful because new items are launched regularly, so having more than 200 observations is rarely the case. Also, the **approach used here seems extensible to other families of products**, as long as they are heavily dependant on promotions (very likely) and influenced by holidays. The **downside** to the approach is that the models produced are too complex (a range of 15 to 25 predictors per model), and would probably require re-evaluating frequently (every 4 months would be prudent). This also results in a loss in interpretability. 

I was very surprised at the fact that what really boosted the accuracy of linear regression was **including lagged values**, not only of the series itself, but of any other value. It would have been worth trying other predictors, in particular sales at each customer, and fourier series for seasonality.

In the future **exploring other models would be a healthy exercise.** More advanced models such as Vector Autoregression, TBATS or Random Forest have proved useful in business forecasting. Particularly ensembling models would really enrichen forecasts. Using neural networks seems tempting to boost accuracy, but unpractical due to insufficient observations, and too many item forecasts to train.

The **dashboard could still dig deeper**, showing more dimensions and visualizations such as item sales per customer, unit price per customer, or level of stock. Shiny proved to be a useful and flexible framework to continue building this.

As a final thought,**multiple linear regression was not accurate enough to provide automated orders**, however it does seem like a useful tool as **support for taking decisions**, especially if the data that feeds is it is made available and understandable to the decision-maker. This is the building block to start testing the use of forecasts in the company.

All this came to me sitting at my favourite spot in my dirty old couch. I was exhausted and felt my old bones aching for rest. But my guts were pretty talkative that night, and though this time I tried not to, I couldn´t help listening to them. Deep inside me I knew the truth: the case would never be over...





*Chapter titles are courtesy of Mystery Title Generator: http://www.starmanseries.com/toolkit/titles_mystery.html*
