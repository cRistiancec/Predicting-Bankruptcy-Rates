# Time Series analysis
Use ARIMA and vector autoregressive models to predict bankruptcy rates in Canada

See discussion PDF in `final.pdf`



#Problem Description

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; According to the Office of the Superintendent of Bankruptcy in Canada, "The consumer insolvency rate is defined as the number of consumer insolvencies per thousand residents aged 18 years or above." Bankruptcy rates have gained prevalence and importance sine the 2008 economic downturn since most of the issues arising in that time came from unpaid mortgages and the bundling of sub-prime loans that were unpaid. Being able to accurately predict economic indicators is paramount to understand and prepare for many macro- and micro-economic events. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; In the following sections, we will utilize bankruptcy rates from the country of Canada from January 1987 to December 2010 to forecast this same economic indicator into the future. Figure 1 below showcases said economic indicator through time. We can see that the indicator is volatile, has a clear upward trend, has some spikes (likely due to external economic factors), and could have some seasonality. 




&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Given that most of the bankruptcy filings are caused by economic downturns, it is no surprise that there is a visual rising trend in the data, and it is also not surprising that the largest bankruptcy rates are seen at the end of 2008 and beginning of 2009. Also, some of the largest overall increases in bankruptcy rates are seen around 1990, 1995, and 2008, all years of very bad economic performance. 


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Knowing that there is an intricate relationship between the indicator we will be trying to predict and several other economic factors, it could be important to analyze other economic indicators. Some of those are given in the train data. These include Unemployment Rate, Population, and the Housing Price Index. 


#Methods to Solve Problem

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The main idea behind the approach to solve the problem of forecasting bankruptcy rates into the future is to exploit the fact that the bankruptcy rate tomorrow is very likely to be similar to that of today. Similarly, we can exploit the fact that the rate today is likely to be very similar to that of yesterday. Furthermore, we can exploit other relationships in the data to gain insights into the future like the fact that bankruptcy rates in July tend to go up or that every January sees a decline in bankruptcy rates. We will now use plots and other metrics to see which are these kinds of relationships in the data and once we extract them, use them to predict into the future. 


## External Regressors Approach

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; One approach is to use other data that highly correlates to bankruptcy rates as external regressors. For example, if we know from economic theory that bankruptcy rates spike after people have a hard time paying off their debt, we can use other indicators like the House Price Index to tell us whether the bankruptcy rates are likely to go up or down when the House Price Index moves up or down. We will further explore these external regressors in the Final Method section to see if they should be included in a model to predict future bankruptcy rates. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;It is important to remember that when adding external regressors, we want to keep the number of variables as small as possible to keep the complexity of the model down and the number of parameters to estimate small as well. Below are some of the potential variables we could use to help with the forecasting of bankruptcy rates. 




&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Given that the regressors in Figure 2 look like thet might move in unison with bankruptcy rates , we might want to include these in the models we test. If we decide to go this "multivariate" route, we can choose one of two approaches. The first approach works if we believe that the other variables we believe are correlated with bankruptcy rates are not affected by bankruptcy rates, but they **do** affect bankruptcy rates (i.e exogenous). That method is ARIMAX, which is an extension of AR, MA, and ARMA models whose intuition is described in the following section.  

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Another possible approach to model and forecast bankruptcy rates is we treat the external regressors (variables such as Unemployment Rate or Population) as variables that are affected by bankruptcy rates and bankruptcy rates are also affected by these external regressors (i.e endogenous variables). This method is called vector auto-regression. For both of these multivariate approaches we need to make sure that certain assumptions about the data or the modeling results (residuals) are met. The results and discussion of these assumptions will be covered in the Technical Appendix section of this report. 

## One variable/univariate approaches

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The main approaches to model time series data are AR, MA, and its combination ARMA models. The intuition behind MA models is that the time series is related to an error term and the error term one lag before that. For AR models, the intuition is that there is a dependence between an observation of the data and that same data point p steps before it. ARMA models are a combination of those two approaches. 


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; A completely different approach is EX¡exponential Smoothing. In exponential smoothing we predict data points in the future through a set of recursive equations that do not require any distributional assumptions. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; To solve the bankruptcy rates problem, we have employed every one of these different approaches and the following sections introduce the best model and why we think it is so. 

### Stationary Data

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Stationary in a time series happens when the average values, or the overall variability of the time series are not changing over time. This basically means that we do not see an evident upward or downward trend, that we do not observe seasonality (spikes every July for example), and that we also do not observe large chunks of high volatility in the data. If we have a stationary time series (which the bankruptcy rates data does not look stationary), we can employ several methods which include AR, MA, ARMA or single exponential smoothing. 


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Stationary in a time series model is important since we are looking for some aspect of the data to **not** depend on time. In order to build a model with any kind of accuracy, we require an assumption that something does not vary with time. The Box-Jenkins methods for modeling time series belong to a class of models that adequately describe a stationary time series. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; To illustrate stationarity and how we deal with it in our final model (described in the following section), below is an example of the raw time series of bankruptcy rates and what it looks like after one round of ordinary differencing. 



#Final Method Chosen 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; After looking closely at the available methods to solve this problem, it is clear that the best modeling approach will be to include an external regressor that helps predict movements in bankruptcy rates. Looking back at Figure 2, it looks like unemployment rate can help explain the big dips and large spikes in bankruptcy rates. For example, right after 1990 there is a very significant spike in the unemployment rate. This is followed very closely by a very large spike in the bankruptcy rate starting at the same exact time period. This makes sense from an economic stand point because many of the bankruptcies re cause by people who are unable to pay credit cards or medical bills, so it makes sense that when there is a significant increase in the unemployment rate, bankruptcies will spike since people who are unemployed are less likely to be able to pay their debt. even though it is not a perfect correlation between bankruptcy rates and unemployment rates this will be the best predictor to help account for large spikes and significant decline in the bankruptcy rates. 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The final model is an ARIMAX model which uses unemployment rate as an external regressor, does ordinary differencing on the log of bankruptcy rates, and uses 2 lags for the error and 5 lags for the inter-dependency of bankruptcy rates today and bankruptcy rates in the past. The final model also uses the log of the bankruptcy data to model the changes in rates from one month to the next as opposed to the raw rates. 


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Using this model to forecast into the future, the results look promising, as showcased in Figure 5. 


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Given that we have monthly data from 1987-2010, to find an optimal model we split said data into two chunks, one from 1987-2008 and the other from 2009-2010. This will help us validate how the model is performing in terms of forecasting values into the "future", while still being able to find the error between what the model predicts and observed data. This model is doing a very good job of predicting into the future since when we predict the years 2009 and 2010, the model is on average missing the true rates by only **0.0059**.


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Using the fact that we have those two years' data to validate the forecasts of our model, we tried all different kinds of models described in the "Methods to Solve Problem" section above. Fitting different models, we were able to compare 1. whether the models satisfied the error assumptions we imposed of the errors not depending on time or on each other and 2. the predictive power and prediction accuracy of each model by way of root mean squared error. 

