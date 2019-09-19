NBCU Case Study
================
Wanlin Ji
5/16/2017

Hello, I am your data scientist Eric today, and I would like to walk you through my analysis on analyzing panel measurement for this TV show. First, let's start by loading the data and looking for any missing value still remaining.

0. Test the integrity of data
-----------------------------

``` r
# Alert: Running this chunk would risk deleting all the previous data in RStudio environment.
library(mosaic)
library(lattice)
rm(list = ls())
setwd('/Users/jiwanlin/Desktop') # Set this url to new working directory if needed.
mxm <- read.csv('mxm.csv')
tele <- read.csv('seasonal_telecast_ratings.csv')
sapply(mxm, function(x) sum(is.na(x)))
```

    ##                    X              Network                 Date 
    ##                    0                    0                    0 
    ##                 Time              Program               Length 
    ##                    0                    0                    0 
    ##               Rating Minute_In_Commercial      Total_Loss_perc 
    ##                    0                    0                    0

``` r
sapply(tele, function(x) sum(is.na(x)))
```

    ##                  X          show_name            network 
    ##                  0                  0                  0 
    ##              genre          time_slot program_start_time 
    ##                  0                  0                  0 
    ##   program_duration       show_premier        show_repeat 
    ##                  0                  0                  0 
    ##             rating 
    ##                  0

Thanks to our diligent engineers, there is no missing values left and no need to impute the values. Now we can move on to do some exploratory analysis next step.

1. Exploratory analysis
-----------------------

``` r
summary(mxm)
```

    ##        X          Network            Date            Time     
    ##  Min.   :   0.0   DKN:2088   2015-09-21:  60   10:01 PM:  24  
    ##  1st Qu.: 521.8              2015-09-28:  60   10:02 PM:  24  
    ##  Median :1043.5              2015-10-05:  60   10:03 PM:  24  
    ##  Mean   :1043.5              2015-10-12:  60   10:04 PM:  24  
    ##  3rd Qu.:1565.2              2015-10-19:  60   10:05 PM:  24  
    ##  Max.   :2087.0              2015-10-26:  60   10:06 PM:  24  
    ##                              (Other)   :1728   (Other) :1944  
    ##         Program         Length          Rating      Minute_In_Commercial
    ##  BLACK_S_BOT:2088   Min.   :59.00   Min.   :0.955   Min.   :0.0000      
    ##                     1st Qu.:59.00   1st Qu.:1.372   1st Qu.:0.0000      
    ##                     Median :60.00   Median :1.552   Median :0.0000      
    ##                     Mean   :59.66   Mean   :1.706   Mean   :0.3472      
    ##                     3rd Qu.:60.00   3rd Qu.:1.964   3rd Qu.:1.0000      
    ##                     Max.   :60.00   Max.   :3.831   Max.   :1.0000      
    ##                                                                         
    ##  Total_Loss_perc 
    ##  Min.   : 0.000  
    ##  1st Qu.: 1.776  
    ##  Median : 2.548  
    ##  Mean   : 3.417  
    ##  3rd Qu.: 3.824  
    ##  Max.   :29.634  
    ## 

``` r
summary(tele)
```

    ##        X                            show_name    network    
    ##  Min.   :   0   LOWEST COMMON DENOMINATOR:  97   DKN : 869  
    ##  1st Qu.:1149   BACKYARD LOT             :  93   JGBC:1413  
    ##  Median :2298   SING YOUR SOCKS OFF      :  88   JGN :1226  
    ##  Mean   :2298   YOU'RE FIRED             :  87   PNN :1090  
    ##  3rd Qu.:3448   CRIME PAYS               :  78              
    ##  Max.   :4597   BLACK IN AMERICA         :  73              
    ##                 (Other)                  :4082              
    ##        genre                    time_slot              program_start_time
    ##  Animation: 237   2015-09-01 20:00:00:   4   2015-09-01 20:00:00:   4    
    ##  Comedy   :1309   2015-09-02 20:00:00:   4   2015-09-02 20:00:00:   4    
    ##  Drama    :2038   2015-09-02 21:00:00:   4   2015-09-03 20:00:00:   4    
    ##  Other    :1014   2015-09-03 20:00:00:   4   2015-09-04 20:00:00:   4    
    ##                   2015-09-04 20:00:00:   4   2015-09-07 20:00:00:   4    
    ##                   2015-09-07 20:00:00:   4   2015-09-08 20:00:00:   4    
    ##                   (Other)            :4574   (Other)            :4574    
    ##  program_duration  show_premier      show_repeat         rating        
    ##  Min.   :  240    Min.   :0.00000   Min.   :0.0000   Min.   :0.001711  
    ##  1st Qu.: 1800    1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.009592  
    ##  Median : 3600    Median :0.00000   Median :0.0000   Median :0.015021  
    ##  Mean   : 3239    Mean   :0.08286   Mean   :0.3084   Mean   :0.017389  
    ##  3rd Qu.: 3600    3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:0.023137  
    ##  Max.   :10800    Max.   :1.00000   Max.   :1.0000   Max.   :0.063658  
    ## 

From the summary, we found there are 2088 observations in our measurement, all on the network of DKN and for this program. Since the rating and total\_loss\_percent are two most variable indicators, these two variables are our focus for analysis. And from real-setting, the ratings and total loss percentages also make sense for predictive or inference purposes.

Let's take a closer look at their patterns of distribution.

``` r
favstats(~ Rating, data=mxm)
```

    ##        min       Q1   median       Q3      max     mean        sd    n
    ##  0.9550059 1.372028 1.552339 1.963901 3.831234 1.706287 0.4694263 2088
    ##  missing
    ##        0

``` r
# Test for density curve
hist(mxm$Rating, freq=F,  xlab = "Rating", col="cyan", main="The rating distribution curve")
rug(jitter(mxm$Rating))  
lines(density(mxm$Rating), col="purple", lwd=2)
```

![](NBCU_files/figure-markdown_github/unnamed-chunk-3-1.png)

From the graph, we found the rating is pretty right skewed distributed, looks closer to a Possion distribution and far from normal distribution. If I were the producer, I would love to see it left skewed, for sure. It turns out there are some potential outliers outside the right side of 1.5 IQR, indicating some of the series moments can be very popular.

``` r
favstats(~ Total_Loss_perc, data=mxm)
```

    ##  min       Q1   median      Q3      max     mean       sd    n missing
    ##    0 1.776482 2.548101 3.82366 29.63402 3.416686 3.147592 2088       0

``` r
# Test for density curve
hist(mxm$Total_Loss_perc, freq=F,  xlab = "Loss percentage for TV series", col="cyan", main="The Loss percentage distribution curve")
rug(jitter(mxm$Total_Loss_perc))
lines(density(mxm$Total_Loss_perc), col="purple", lwd=2)
```

![](NBCU_files/figure-markdown_github/unnamed-chunk-4-1.png)

The loss percentage distribution is even more right skewed. Most of the times the loss proportion is less than 30, but very rarely we can have some moments when around 30 percent of audience are leaving. That is horrible, maybe we will want to specify these outliers and find out the reason later, but let's move on the exploration.

To discover more insights, we need to take a look at how it varies with time and other explanatory variables. Here we take a look at its overall pattern across the overall time.

``` r
ggplot(mxm, aes(x = X, y = Total_Loss_perc)) + geom_area(colour = "black", 
    fill = "blue", alpha = 0.2)
```

![](NBCU_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
ggplot(mxm, aes(x = X, y = Rating)) + geom_area(colour = "black", 
    fill = "blue", alpha = 0.2)
```

![](NBCU_files/figure-markdown_github/unnamed-chunk-5-2.png)

Surprise! We found that the loss percentage is clearly repeating its pattern divided by a black line. And if we recall the loss percentage is 0 in the very last minute of every episode, this black line shows that the pattern is largely repeating itself by episode. For the rating, it is deceasing with time and also shows some patterns. But it is less stable, with a downward tendency.

From the explanatory perspective, loss percentages are time-dependent across three seasons, indicating it is influenced by some time-related factors regularly. Based on that we can decide to use time-series analysis if we want to make some predictions on how a single variable may change in future.

And we carry out some exploratory analysis on the first season and first episode below to take a closer look on the tendency, figuring out if there is decline even before the finale and its possible influencing factors.

``` r
# For the first season
mxm_test <- subset(mxm, Date == "2016-05-23", 
select=c(X, Rating, Total_Loss_perc))

tail(mxm_test, 7)
```

    ##         X   Rating Total_Loss_perc
    ## 1362 1361 1.440756        2.741919
    ## 1363 1362 1.449238        2.177074
    ## 1364 1363 1.446222        1.769702
    ## 1365 1364 1.461675        1.098317
    ## 1366 1365 1.483708        1.165550
    ## 1367 1366 1.521850        2.520446
    ## 1368 1367 1.599028        0.000000

``` r
mxm1 <- subset(mxm,  X <= 1367)

ggplot(mxm1, aes(x = X, y = Total_Loss_perc)) + geom_area(colour = "black", fill = "blue", alpha = 0.2) 
```

![](NBCU_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
ggplot(mxm1, aes(x = X, y = Rating)) + geom_area(colour = "black", fill = "blue", alpha = 0.2)
```

![](NBCU_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
# For the first episode
mxm11 <- subset(mxm1, Date == "2015-09-21")

ggplot(mxm11, aes(x=X)) + 
#            geom_point(aes(y = Total_Loss_perc), ) + 
            geom_line(aes(y = Total_Loss_perc, color="Audience Loss Percentage")) +
#            geom_point(aes(y = Rating)) + 
            geom_line(aes(y = Rating, color="Rating"))+
  geom_point(aes(y = Minute_In_Commercial, color="Ad")) 
```

![](NBCU_files/figure-markdown_github/unnamed-chunk-6-3.png)

By comparing the three major variable, commercial, rating and total loss percentage, we found that the scope can really mislead us. The ratings are not so dramatic as we saw previously. And the pulses responses as well as time-dependence for both the two variables are very clear in this graph. Until now, we have got some solid recognition of our data at hand.

Now let's consider some of the hypothesis from Mr. A as well as our team member. They are paying attention to the rating and loss percentage variations. We may need to pick up a dependent variable that could solve our puzzle. From my perspective, the industry are more concerned with the number of audience, so we should think of the total loss percentage as our primary target here. Note I don't have the number of viewers who join the watching during the show, so it is impossible to analyze the real number of audience, but we can still aim to manage the viewers loss according to the insight from marketing that it is always more expensive to earn a new customer than to prevent losing an old customer. Same go with viewers.

Next we want to consider other factors that could have an effect on the total loss percentage to find the primary drive behind it. We want to assume the rating stands for the quality of narratives, then we can see the total loss percentage an explanatory variable for analyzing total loss percentage. This seems plausible explanation. But we also may pay attention to the pulse-responses that comes with the total loss percentage, which is largely corresponding to Minute\_In\_Commercial.

However, we might want to think deeper as how a viewer could decide to leave (behavior) based on the rating (belief) and other factors. And a feature that it could have is the accumulated effect independent variables may have on dependent variables. In fact, it would be better to obtain individual level data and build a Bayesian model towards the decision making process, but now we want to focus on analyzing the relationship across the time. And considering the fact that I have no idea how the rating is construct as well as the pulses, we may be curious about how the three variables are influencing each other. It all leads us to the Multivariate time-series model, where we can analyze different relationships between sequences.

2. Multivariate time-series model
---------------------------------

For the rating, it is decreasing with time. For the total loss percentage, it is repeating itself on a episode basis. To carry out analysis, we need to make sure the sequences are stationary.

### 2.1 Unit Root Test

``` r
library(tseries)
```

    ## 
    ## Attaching package: 'tseries'

    ## The following object is masked from 'package:mosaic':
    ## 
    ##     value

``` r
adf.test(mxm$Rating)
```

    ## Warning in adf.test(mxm$Rating): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  mxm$Rating
    ## Dickey-Fuller = -6.9171, Lag order = 12, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
adf.test(mxm$Minute_In_Commercial)
```

    ## Warning in adf.test(mxm$Minute_In_Commercial): p-value smaller than printed
    ## p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  mxm$Minute_In_Commercial
    ## Dickey-Fuller = -13.53, Lag order = 12, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
adf.test(mxm$Total_Loss_perc)
```

    ## Warning in adf.test(mxm$Total_Loss_perc): p-value smaller than printed p-
    ## value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  mxm$Total_Loss_perc
    ## Dickey-Fuller = -12.75, Lag order = 12, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
pp.test(mxm$Rating)
```

    ## Warning in pp.test(mxm$Rating): p-value smaller than printed p-value

    ## 
    ##  Phillips-Perron Unit Root Test
    ## 
    ## data:  mxm$Rating
    ## Dickey-Fuller Z(alpha) = -238.09, Truncation lag parameter = 8,
    ## p-value = 0.01
    ## alternative hypothesis: stationary

``` r
pp.test(mxm$Minute_In_Commercial)
```

    ## Warning in pp.test(mxm$Minute_In_Commercial): p-value smaller than printed
    ## p-value

    ## 
    ##  Phillips-Perron Unit Root Test
    ## 
    ## data:  mxm$Minute_In_Commercial
    ## Dickey-Fuller Z(alpha) = -538.02, Truncation lag parameter = 8,
    ## p-value = 0.01
    ## alternative hypothesis: stationary

``` r
pp.test(mxm$Total_Loss_perc)
```

    ## Warning in pp.test(mxm$Total_Loss_perc): p-value smaller than printed p-
    ## value

    ## 
    ##  Phillips-Perron Unit Root Test
    ## 
    ## data:  mxm$Total_Loss_perc
    ## Dickey-Fuller Z(alpha) = -1574.7, Truncation lag parameter = 8,
    ## p-value = 0.01
    ## alternative hypothesis: stationary

We adapted ADF test and Phillips-Perron test, and found that under 0.01 significant level, there p-values are all smaller than the printed p-value. We can see that they all follow AR(0) in the same stage, without the need to make a difference calculation. All the three sequences showed no sign of unit root, and no need for Johansen Co-integration test. Let's move on to the next stage.

### 2.2 Structural VAR

Considering the influence between variables includes the pulses from the same minute as well as time lags, we need to use a SVAR model instead of the usual VAR. To give an estimation of our relationships, we need to specify the Kronecker indices of the data. The Kronecker index approach is used to specify and estimate a VARMA model can be carried out via the MTS package using commands Kronid, Kronfit, and refKronfit, respectively.

The time lags need to be determined to approximate the past vector *P*<sub>*t* − 1</sub> in our computing matrix. Normally we would expect a larger stages setting for time lags in stationary sequences. Here we directly use the default 5 stages.

``` r
library(MTS)

mxmc <- subset(mxm1, select=c(Minute_In_Commercial, Total_Loss_perc, Rating))
summary(mxmc)
```

    ##  Minute_In_Commercial Total_Loss_perc      Rating     
    ##  Min.   :0.000        Min.   : 0.000   Min.   :1.231  
    ##  1st Qu.:0.000        1st Qu.: 1.583   1st Qu.:1.496  
    ##  Median :0.000        Median : 2.262   Median :1.781  
    ##  Mean   :0.337        Mean   : 3.198   Mean   :1.884  
    ##  3rd Qu.:1.000        3rd Qu.: 3.500   3rd Qu.:2.157  
    ##  Max.   :1.000        Max.   :29.634   Max.   :3.736

``` r
Kronid(mxmc)
```

    ## h =  0 
    ## Component =  1 
    ## square of the smallest can. corr. =  0.4713223 
    ##     test,   df, &  p-value: 
    ## [1] 866.513  15.000   0.000
    ## Component =  2 
    ## square of the smallest can. corr. =  0.186861 
    ##     test,   df, &  p-value: 
    ## [1] 281.114  14.000   0.000
    ## Component =  3 
    ## square of the smallest can. corr. =  0.1788388 
    ##     test,   df, &  p-value: 
    ## [1] 267.673  13.000   0.000
    ## ============= 
    ## h =  1 
    ## Component =  1 
    ## Square of the smallest can. corr. =  0.1663728 
    ##     test,     df, p-value & d-hat: 
    ## [1] 218.395  12.000   0.000   1.120
    ## Component =  2 
    ## Square of the smallest can. corr. =  0.01742971 
    ##     test,     df, p-value & d-hat: 
    ## [1] 23.134 11.000  0.017  1.032
    ## Component =  3 
    ## Square of the smallest can. corr. =  0.006485841 
    ##     test,     df, p-value & d-hat: 
    ## [1]  8.733 10.000  0.558  1.011
    ## A Kronecker found 
    ## ============ 
    ## h =  2 
    ## Component =  1 
    ## Square of the smallest can. corr. =  0.01458685 
    ##     test,     df, p-value & d-hat: 
    ## [1] 17.769 10.000  0.059  1.121
    ## A Kronecker found 
    ## Component =  2 
    ## Square of the smallest can. corr. =  0.008954431 
    ##     test,     df, p-value & d-hat: 
    ## [1]  7.412 10.000  0.686  1.644
    ## A Kronecker found 
    ## ============ 
    ##     
    ## Kronecker indexes identified: 
    ## [1] 2 2 1

Then we move on to find the needed feature for the maximum order needed for VARMA model estimation based on the Kronecker indexes we just found out.

``` r
kdx <- c(2, 2, 1)
Kronspec(kdx)
```

    ## Kronecker indices:  2 2 1 
    ## Dimension:  3 
    ## Notation:  
    ##  0: fixed to 0 
    ##  1: fixed to 1 
    ##  2: estimation 
    ## AR coefficient matrices:  
    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
    ## [1,]    1    0    0    2    2    0    2    2    2
    ## [2,]    0    1    0    2    2    0    2    2    2
    ## [3,]    2    2    1    2    2    2    0    0    0
    ## MA coefficient matrices:  
    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
    ## [1,]    1    0    0    2    2    2    2    2    2
    ## [2,]    0    1    0    2    2    2    2    2    2
    ## [3,]    2    2    1    2    2    2    0    0    0

Based on the Kronecker indices, a VARMA(2, 2) model is specified for the data. 2 stands for maximum order in AR feature of the data, and 2 stands for maximum MA order that we just computed.

#### 2.2 Estimation

A specified VARMA model via Kronecker index we just computed can be estimated by the maximum likelihood method. If some of the estimated parameters are not statistically significant, then one can further refine the model by removing insignificant parameters. However, there exists no unique way to remove insignificant parameters of a fitted VARMA model. But we can adapt an iterated procedure by removing insignificant parameter one at a time, remove the least significant parameter and re-estimate the model. Now we need to estimate 33 parameters in our model.

``` r
m2=Kronfit(mxmc,kdx)
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
    ## [1,]    1    0    0    2    2    0    2    2    2
    ## [2,]    0    1    0    2    2    0    2    2    2
    ## [3,]    2    2    1    2    2    2    0    0    0
    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
    ## [1,]    1    0    0    2    2    2    2    2    2
    ## [2,]    0    1    0    2    2    2    2    2    2
    ## [3,]    2    2    1    2    2    2    0    0    0
    ## Number of parameters:  33 
    ## initial estimates:  0.0658 1.1356 0.0593 -0.4685 -0.0537 0.0157 -0.665 -0.0271 -0.8095 -0.0191 0.0641 -0.5436 1.2875 1.424 0.6197 -1.6205 0.0464 -0.0812 -1.1596 -0.8809 10.2073 1.5878 -0.3147 4.9365 0.0382 0.0675 -0.0271 0.0925 -0.0332 0.9848 0.0058 -0.0055 0.0621 
    ## Upper-bound:  0.1628 1.26 0.093 -0.3731 -0.0264 0.0643 -0.5258 0.0084 -0.5549 0.0702 0.1023 -0.0722 2.0661 2.4221 0.89 -0.8548 0.2653 0.3087 -0.0429 -0.596 12.2508 2.3038 -0.0079 8.7198 0.0725 0.1114 -0.0153 0.1261 -0.0235 1.002 0.0372 0.008 0.2285 
    ## Lower-bound:  -0.0312 1.0113 0.0257 -0.5639 -0.081 -0.0328 -0.8042 -0.0626 -1.0641 -0.1083 0.0259 -1.0151 0.5089 0.4258 0.3494 -2.3861 -0.1725 -0.4711 -2.2764 -1.1659 8.1639 0.8718 -0.6215 1.1533 0.0039 0.0236 -0.039 0.0588 -0.0428 0.9676 -0.0256 -0.019 -0.1043 
    ## 
    ## Coefficient(s):
    ##        Estimate  Std. Error  t value Pr(>|t|)    
    ##  [1,]  0.037601    1.000000    0.038   0.9700    
    ##  [2,]  1.211874    1.000000    1.212   0.2256    
    ##  [3,]  0.092954    1.000000    0.093   0.9259    
    ##  [4,] -0.563891    1.000000   -0.564   0.5728    
    ##  [5,] -0.070963    1.000000   -0.071   0.9434    
    ##  [6,]  0.006916    1.000000    0.007   0.9945    
    ##  [7,] -0.710897    1.000000   -0.711   0.4771    
    ##  [8,] -0.059185    1.000000   -0.059   0.9528    
    ##  [9,] -0.925812    1.000000   -0.926   0.3545    
    ## [10,]  0.070173    1.000000    0.070   0.9441    
    ## [11,]  0.078456    1.000000    0.078   0.9375    
    ## [12,] -0.622573    1.000000   -0.623   0.5336    
    ## [13,]  1.625334    1.000000    1.625   0.1041    
    ## [14,]  1.692359    1.000000    1.692   0.0906 .  
    ## [15,]  0.390978    1.000000    0.391   0.6958    
    ## [16,] -2.025514    1.000000   -2.026   0.0428 *  
    ## [17,]  0.107677    1.000000    0.108   0.9143    
    ## [18,]  0.029136    1.000000    0.029   0.9768    
    ## [19,] -1.629514    1.000000   -1.630   0.1032    
    ## [20,] -0.595969    1.000000   -0.596   0.5512    
    ## [21,] 10.152823    1.000000   10.153  < 2e-16 ***
    ## [22,]  1.687017    1.000000    1.687   0.0916 .  
    ## [23,] -0.272916    1.000000   -0.273   0.7849    
    ## [24,]  4.477324    1.000000    4.477 7.56e-06 ***
    ## [25,]  0.044389    1.000000    0.044   0.9646    
    ## [26,]  0.081832    1.000000    0.082   0.9348    
    ## [27,] -0.024660    1.000000   -0.025   0.9803    
    ## [28,]  0.098122    1.000000    0.098   0.9218    
    ## [29,] -0.031320    1.000000   -0.031   0.9750    
    ## [30,]  0.983916    1.000000    0.984   0.3252    
    ## [31,]  0.013982    1.000000    0.014   0.9888    
    ## [32,] -0.009181    1.000000   -0.009   0.9927    
    ## [33,]  0.136606    1.000000    0.137   0.8913    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## --- 
    ## Estimates in matrix form: 
    ## Constant term:  
    ## Estimates:  0.038 1.625 0.044 
    ## AR and MA lag-0 coefficient matrix 
    ##       [,1]   [,2] [,3]
    ## [1,] 1.000  0.000    0
    ## [2,] 0.000  1.000    0
    ## [3,] 0.082 -0.025    1
    ## AR coefficient matrix 
    ## AR( 1 )-matrix 
    ##       [,1]   [,2]  [,3]
    ## [1,] 1.212  0.093 0.000
    ## [2,] 1.692  0.391 0.000
    ## [3,] 0.098 -0.031 0.984
    ## AR( 2 )-matrix 
    ##        [,1]   [,2]  [,3]
    ## [1,] -0.564 -0.071 0.007
    ## [2,] -2.026  0.108 0.029
    ## [3,]  0.000  0.000 0.000
    ## MA coefficient matrix 
    ## MA( 1 )-matrix 
    ##        [,1]  [,2]    [,3]
    ## [1,]  0.711 0.059   0.926
    ## [2,]  1.630 0.596 -10.153
    ## [3,] -0.014 0.009  -0.137
    ## MA( 2 )-matrix 
    ##        [,1]   [,2]   [,3]
    ## [1,] -0.070 -0.078  0.623
    ## [2,] -1.687  0.273 -4.477
    ## [3,]  0.000  0.000  0.000
    ##   
    ## Residuals cov-matrix: 
    ##              [,1]      [,2]         [,3]
    ## [1,]  0.122522074 0.2083166 -0.003557424
    ## [2,]  0.208316640 8.7261318  0.288369478
    ## [3,] -0.003557424 0.2883695  0.016393699
    ## ---- 
    ## aic=  -5.052959 
    ## bic=  -4.927012

``` r
m3=refKronfit(m2,thres=1.6)
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
    ## [1,]    1    0    0    2    2    0    2    2    2
    ## [2,]    0    1    0    2    2    0    2    2    2
    ## [3,]    2    2    1    2    2    2    0    0    0
    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
    ## [1,]    1    0    0    2    2    2    2    2    2
    ## [2,]    0    1    0    2    2    2    2    2    2
    ## [3,]    2    2    1    2    2    2    0    0    0
    ## Number of parameters:  7 
    ## initial estimates:  1.6253 1.6924 -2.0255 -1.6295 10.1528 1.687 4.4773 
    ## Upper-bound:  3.6253 3.6924 -0.0255 0.3705 12.1528 3.687 6.4773 
    ## Lower-bound:  -0.3747 -0.3076 -4.0255 -3.6295 8.1528 -0.313 2.4773 
    ## 
    ## Coefficient(s):
    ##       Estimate  Std. Error  t value Pr(>|t|)    
    ## [1,]   -0.3747      1.0000   -0.375   0.7079    
    ## [2,]    1.1458      1.0000    1.146   0.2519    
    ## [3,]   -1.7175      1.0000   -1.717   0.0859 .  
    ## [4,]   -2.1707      1.0000   -2.171   0.0300 *  
    ## [5,]    8.1528      1.0000    8.153 4.44e-16 ***
    ## [6,]    1.9936      1.0000    1.994   0.0462 *  
    ## [7,]    2.4773      1.0000    2.477   0.0132 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## --- 
    ## Estimates in matrix form: 
    ## Constant term:  
    ## Estimates:  0 -0.375 0 
    ## AR and MA lag-0 coefficient matrix 
    ##      [,1] [,2] [,3]
    ## [1,]    1    0    0
    ## [2,]    0    1    0
    ## [3,]    0    0    1
    ## AR coefficient matrix 
    ## AR( 1 )-matrix 
    ##       [,1] [,2] [,3]
    ## [1,] 0.000    0    0
    ## [2,] 1.146    0    0
    ## [3,] 0.000    0    0
    ## AR( 2 )-matrix 
    ##        [,1] [,2] [,3]
    ## [1,]  0.000    0    0
    ## [2,] -1.717    0    0
    ## [3,]  0.000    0    0
    ## MA coefficient matrix 
    ## MA( 1 )-matrix 
    ##       [,1] [,2]   [,3]
    ## [1,] 0.000    0  0.000
    ## [2,] 2.171    0 -8.153
    ## [3,] 0.000    0  0.000
    ## MA( 2 )-matrix 
    ##        [,1] [,2]   [,3]
    ## [1,]  0.000    0  0.000
    ## [2,] -1.994    0 -2.477
    ## [3,]  0.000    0  0.000
    ##   
    ## Residuals cov-matrix: 
    ##            [,1]       [,2]       [,3]
    ## [1,]  0.3374817  -4.946194   0.609297
    ## [2,] -4.9461940 292.218843 -32.122027
    ## [3,]  0.6092970 -32.122027   3.752672
    ## ---- 
    ## aic=  2.73026 
    ## bic=  2.756976

For this part of analysis, we have the 33 parameters that needs estimation. We usually need a simplificaton function again to narrow down the significant parameters.

As it shows the significance level is pretty concentrated on several coefficient, we can further test the estimation.

``` r
MTSdiag(m3)
```

    ## [1] "Covariance matrix:"
    ##                      Minute_In_Commercial Total_Loss_perc  Rating
    ## Minute_In_Commercial               0.2238           0.524 -0.0258
    ## Total_Loss_perc                    0.5240          29.553 -1.6251
    ## Rating                            -0.0258          -1.625  0.2120
    ## CCM at lag:  0 
    ##        [,1]   [,2]   [,3]
    ## [1,]  1.000  0.204 -0.118
    ## [2,]  0.204  1.000 -0.649
    ## [3,] -0.118 -0.649  1.000
    ## Simplified matrix: 
    ## CCM at lag:  1 
    ## + + - 
    ## + + - 
    ## - - + 
    ## CCM at lag:  2 
    ## + . . 
    ## + + - 
    ## - - + 
    ## CCM at lag:  3 
    ## - . . 
    ## . + - 
    ## . - + 
    ## CCM at lag:  4 
    ## - - . 
    ## - + - 
    ## . - + 
    ## CCM at lag:  5 
    ## - - . 
    ## - + - 
    ## . - + 
    ## CCM at lag:  6 
    ## - - . 
    ## - + - 
    ## . - + 
    ## CCM at lag:  7 
    ## - . . 
    ## . + - 
    ## . - + 
    ## CCM at lag:  8 
    ## + . . 
    ## + + - 
    ## . - + 
    ## CCM at lag:  9 
    ## + + . 
    ## + + - 
    ## . - + 
    ## CCM at lag:  10 
    ## + + . 
    ## + + - 
    ## . - + 
    ## CCM at lag:  11 
    ## + + . 
    ## + + - 
    ## . - + 
    ## CCM at lag:  12 
    ## . . . 
    ## . + - 
    ## . - + 
    ## CCM at lag:  13 
    ## - - . 
    ## . + - 
    ## . - + 
    ## CCM at lag:  14 
    ## - - . 
    ## - + - 
    ## . - + 
    ## CCM at lag:  15 
    ## - - . 
    ## - + - 
    ## . - + 
    ## CCM at lag:  16 
    ## - - . 
    ## . + - 
    ## . - + 
    ## CCM at lag:  17 
    ## - . . 
    ## . + - 
    ## . - + 
    ## CCM at lag:  18 
    ## . . . 
    ## . + - 
    ## + - + 
    ## CCM at lag:  19 
    ## + . . 
    ## . + - 
    ## . - + 
    ## CCM at lag:  20 
    ## + + . 
    ## . + - 
    ## . - + 
    ## CCM at lag:  21 
    ## + + . 
    ## . + - 
    ## . - + 
    ## CCM at lag:  22 
    ## . . . 
    ## . + - 
    ## . - + 
    ## CCM at lag:  23 
    ## . . . 
    ## . + - 
    ## . - + 
    ## CCM at lag:  24 
    ## - - . 
    ## . + - 
    ## . - +

![](NBCU_files/figure-markdown_github/unnamed-chunk-12-1.png)

    ## Hit Enter for p-value plot of individual ccm:

![](NBCU_files/figure-markdown_github/unnamed-chunk-12-2.png)

    ## Hit Enter to compute MQ-statistics: 
    ## 
    ## Ljung-Box Statistics:  
    ##         m       Q(m)     df    p-value
    ##  [1,]     1      1770       9        0
    ##  [2,]     2      3089      18        0
    ##  [3,]     3      4349      27        0
    ##  [4,]     4      5747      36        0
    ##  [5,]     5      7148      45        0
    ##  [6,]     6      8394      54        0
    ##  [7,]     7      9491      63        0
    ##  [8,]     8     10573      72        0
    ##  [9,]     9     11703      81        0
    ## [10,]    10     12808      90        0
    ## [11,]    11     13839      99        0
    ## [12,]    12     14787     108        0
    ## [13,]    13     15735     117        0
    ## [14,]    14     16713     126        0
    ## [15,]    15     17700     135        0
    ## [16,]    16     18642     144        0
    ## [17,]    17     19519     153        0
    ## [18,]    18     20380     162        0
    ## [19,]    19     21244     171        0
    ## [20,]    20     22112     180        0
    ## [21,]    21     22934     189        0
    ## [22,]    22     23714     198        0
    ## [23,]    23     24478     207        0
    ## [24,]    24     25249     216        0

![](NBCU_files/figure-markdown_github/unnamed-chunk-12-3.png)

    ## Hit Enter to obtain residual plots:

![](NBCU_files/figure-markdown_github/unnamed-chunk-12-4.png)

Awesome! If the result is flooded in the messages, it is like this:

![Final Result](1.png)

From the result we find that Total loss percentage remains in a huge effect of itself, indicating most of the pulse-response is actually the product of time lags. And the Rating has much larger coefficient than Minute\_In\_Commercial, considering the spread of ratings is much much smaller. So here we are. As our conclusion, we should consider the creative narrative may impose a negative effect on the audience loss.
