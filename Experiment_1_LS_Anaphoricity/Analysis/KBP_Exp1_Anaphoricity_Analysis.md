Word order constraints on event-internal modifiers - Experiment 1:
Likert Scale study on Anaphoricity
================
Tibor Kiss
09.09.2021

``` r
library(ordinal)
library(tidyverse)
library(emmeans)

show(version)
```

    ##                _                           
    ## platform       x86_64-apple-darwin17.0     
    ## arch           x86_64                      
    ## os             darwin17.0                  
    ## system         x86_64, darwin17.0          
    ## status                                     
    ## major          4                           
    ## minor          1.2                         
    ## year           2021                        
    ## month          11                          
    ## day            01                          
    ## svn rev        81115                       
    ## language       R                           
    ## version.string R version 4.1.2 (2021-11-01)
    ## nickname       Bird Hippie

#### Introduction

This document describes the necessary analysis of Experiment 1 (LS) for
event-internal modifiers (second stage). We include a model with and
without interaction. As can be seen below, the difference between the
two models in terms of fit is just above 0.05.

#### Read in data set

``` r
exp_anaphoricity_ls.test.data <- 
  read.csv("../Data/LikertSkala_test.csv", fileEncoding = "UTF-8", stringsAsFactors = TRUE)

exp_anaphoricity_ls.test.data$CONDITION_NO <- factor(exp_anaphoricity_ls.test.data$CONDITION_NO)

exp_anaphoricity_ls.test.data$FCT_ANSWER <- 
  factor(exp_anaphoricity_ls.test.data$ANSWER, ordered = TRUE)

exp_anaphoricity_ls.test.data$ENCODING <- 
  factor(as.character(exp_anaphoricity_ls.test.data$ENCODING))

exp_anaphoricity_ls.test.data$KEY_CONDITION = relevel(exp_anaphoricity_ls.test.data$KEY_CONDITION, "PP>OBJ") ## CONDITION_NO 1

## more illustrative variable names
exp_anaphoricity_ls.test.data$POSITION <- factor(exp_anaphoricity_ls.test.data$KEY_CONDITION)
exp_anaphoricity_ls.test.data$subjects <- factor(exp_anaphoricity_ls.test.data$workerId)
exp_anaphoricity_ls.test.data$items <- factor(exp_anaphoricity_ls.test.data$ENCODING)
```

#### Empirical distribution of judgments

Number of accepted participants: 51.

``` r
exp_anaphoricity_ls.dist.summary <- exp_anaphoricity_ls.test.data %>% 
  select(workerId, CONDITION_NO) %>% table() %>% as.data.frame()
exp_anaphoricity_ls.dist.summary %>% filter(Freq < 18)
```

    ## [1] workerId     CONDITION_NO Freq        
    ## <0 Zeilen> (oder row.names mit Länge 0)

Each participant has seen 18 test items per condition, since no
participant has seen less than 18 test items per condition (8->).

``` r
exp_anaphoricity_ls.test.summary <- 
  exp_anaphoricity_ls.test.data %>% 
  group_by(POSITION, ADVERBIAL_TYPE, ANSWER) %>% 
  summarise(count = n())

ggplot(exp_anaphoricity_ls.test.summary, aes(x = ANSWER, y = count)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  facet_wrap(POSITION~ADVERBIAL_TYPE) + 
  labs(x = "Ratings", y = "Frequency of Ratings")
```

![](KBP_Exp1_Anaphoricity_Analysis_files/figure-gfm/distribution%20of%20judgments-1.png)<!-- -->

### Random slope model for items and subjects without interaction

#### Definition of model

``` r
exp_anaphoricity_ls.test.data$ADVERBIAL_TYPE <-
  relevel(factor(exp_anaphoricity_ls.test.data$ADVERBIAL_TYPE), ref = "INSTR")
exp_anaphoricity_ls.test.data$POSITION <-
  relevel(factor(exp_anaphoricity_ls.test.data$POSITION), ref = "PP>OBJ")

exp_anaphoricity_ls.clmm <- 
  clmm(FCT_ANSWER  ~ ADVERBIAL_TYPE + POSITION + 
         (0 + ADVERBIAL_TYPE*POSITION| subjects) + 
         (0 + ADVERBIAL_TYPE*POSITION| items), 
       exp_anaphoricity_ls.test.data)

summary(exp_anaphoricity_ls.clmm)
```

    ## Cumulative Link Mixed Model fitted with the Laplace approximation
    ## 
    ## formula: FCT_ANSWER ~ ADVERBIAL_TYPE + POSITION + (0 + ADVERBIAL_TYPE *  
    ##     POSITION | subjects) + (0 + ADVERBIAL_TYPE * POSITION | items)
    ## data:    exp_anaphoricity_ls.test.data
    ## 
    ##  link  threshold nobs logLik   AIC     niter       max.grad cond.H
    ##  logit flexible  1836 -2100.22 4298.44 9479(47723) 2.06e-03 NaN   
    ## 
    ## Random effects:
    ##  Groups   Name                                Variance Std.Dev. Corr         
    ##  subjects ADVERBIAL_TYPEINSTR                 2.3324   1.5272                
    ##           ADVERBIAL_TYPECOM(O)                2.3783   1.5422    0.749       
    ##           ADVERBIAL_TYPEILOC                  2.1142   1.4540    0.897  0.568
    ##           POSITIONOBJ>PP                      0.8985   0.9479   -0.046 -0.088
    ##           ADVERBIAL_TYPECOM(O):POSITIONOBJ>PP 0.7036   0.8388    0.197 -0.371
    ##           ADVERBIAL_TYPEILOC:POSITIONOBJ>PP   0.6641   0.8149   -0.448  0.061
    ##  items    ADVERBIAL_TYPEINSTR                 0.1707   0.4131                
    ##           ADVERBIAL_TYPECOM(O)                0.3068   0.5539   -0.342       
    ##           ADVERBIAL_TYPEILOC                  1.1923   1.0919   -0.097  0.064
    ##           POSITIONOBJ>PP                      0.3050   0.5522    1.000 -0.342
    ##           ADVERBIAL_TYPECOM(O):POSITIONOBJ>PP 0.2487   0.4987   -0.691 -0.376
    ##           ADVERBIAL_TYPEILOC:POSITIONOBJ>PP   0.2859   0.5347   -0.397  0.260
    ##                       
    ##                       
    ##                       
    ##                       
    ##  -0.057               
    ##   0.441 -0.189        
    ##  -0.603 -0.407 -0.755 
    ##                       
    ##                       
    ##                       
    ##  -0.097               
    ##   0.129 -0.691        
    ##  -0.450 -0.397  0.220 
    ## Number of groups:  subjects 51,  items 36 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)
    ## ADVERBIAL_TYPECOM(O)   -1.069        NaN     NaN      NaN
    ## ADVERBIAL_TYPEILOC     -1.168        NaN     NaN      NaN
    ## POSITIONOBJ>PP          1.197        NaN     NaN      NaN
    ## 
    ## Threshold coefficients:
    ##     Estimate Std. Error z value
    ## 1|2   -4.693        NaN     NaN
    ## 2|3   -2.212        NaN     NaN
    ## 3|4   -1.526        NaN     NaN
    ## 4|5    1.594        NaN     NaN

#### Effects in the model w/o interaction

Since we have chosen INSTR as reference value, the values for ILOC and
COM(O) both show significance, but it should be kept in mind that they
are practically identical. The general effect of `POSITION == OBJ > PP`
is 3.31. The effects for COM(O) and ILOC in `POSITION == PP > OBJ` are
0.343, and 0.311, respectively.

#### Model predictions (without interaction)

``` r
pred <-function(eta, theta, cat = 1:(length(theta)+1), inv.link= plogis){
  Theta <- c(-1e3, theta, 1e3)
  sapply(cat, function(j) inv.link(Theta[j+1] - eta) - inv.link(Theta[j] - eta))
}

exp_anaphoricity_ls.mat <- 
  data.frame(
    adv = rep(c(0, exp_anaphoricity_ls.clmm$beta[1], exp_anaphoricity_ls.clmm$beta[2]), 2),
    cond = c(rep(0, 3), rep(exp_anaphoricity_ls.clmm$beta[3], 3))
  )

pred.mat <- pred(eta=rowSums(exp_anaphoricity_ls.mat), 
                 theta=exp_anaphoricity_ls.clmm$Theta)

exp_anaphoricity_ls.mat$condition <- 
  c(rep("PP>OBJ", 3), rep("OBJ>PP", 3))
exp_anaphoricity_ls.mat$adv_type <- 
  rep(c("INSTR", "COM(O)", "ILOC"), 2)

exp_anaphoricity_ls.pred <- 
  cbind(exp_anaphoricity_ls.mat, pred.mat)

exp_anaphoricity_ls.pred.long <- 
  exp_anaphoricity_ls.pred %>%
  gather("1":"5", key = "ANSWER", value = "rating")

exp_anaphoricity_ls.pred.long$condition <- 
  relevel(factor(exp_anaphoricity_ls.pred.long$condition), ref = "PP>OBJ")

ggplot(exp_anaphoricity_ls.pred.long, aes(x = ANSWER, y = rating)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  facet_wrap(condition ~ adv_type) + 
  labs(x = "Ratings", y = "Pr(Ratings)")
```

![](KBP_Exp1_Anaphoricity_Analysis_files/figure-gfm/graphical%20presentation%20of%20model%20without%20interaction-1.png)<!-- -->

#### Model with interaction

``` r
options(width = 300)

exp_anaphoricity_ls.clmm2 <- 
  clmm(FCT_ANSWER  ~ ADVERBIAL_TYPE * POSITION + 
         (1 + POSITION| subjects) + 
         (1 + POSITION| items), 
       exp_anaphoricity_ls.test.data)

summary(exp_anaphoricity_ls.clmm2)
```

    ## Cumulative Link Mixed Model fitted with the Laplace approximation
    ## 
    ## formula: FCT_ANSWER ~ ADVERBIAL_TYPE * POSITION + (1 + POSITION | subjects) +      (1 + POSITION | items)
    ## data:    exp_anaphoricity_ls.test.data
    ## 
    ##  link  threshold nobs logLik   AIC     niter      max.grad cond.H 
    ##  logit flexible  1836 -2125.66 4281.31 1525(7726) 6.13e-03 4.4e+02
    ## 
    ## Random effects:
    ##  Groups   Name           Variance Std.Dev. Corr   
    ##  subjects (Intercept)    1.7637   1.3281          
    ##           POSITIONOBJ>PP 0.5289   0.7272   -0.243 
    ##  items    (Intercept)    0.4965   0.7047          
    ##           POSITIONOBJ>PP 0.1615   0.4019   -0.252 
    ## Number of groups:  subjects 51,  items 36 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error z value Pr(>|z|)    
    ## ADVERBIAL_TYPECOM(O)                 -1.1402     0.3304  -3.451 0.000558 ***
    ## ADVERBIAL_TYPEILOC                   -1.0680     0.3292  -3.244 0.001177 ** 
    ## POSITIONOBJ>PP                        0.8018     0.2270   3.532 0.000412 ***
    ## ADVERBIAL_TYPECOM(O):POSITIONOBJ>PP   0.5303     0.2820   1.880 0.060048 .  
    ## ADVERBIAL_TYPEILOC:POSITIONOBJ>PP     0.3833     0.2819   1.360 0.173968    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Threshold coefficients:
    ##     Estimate Std. Error z value
    ## 1|2  -4.4315     0.3299 -13.431
    ## 2|3  -2.0670     0.3044  -6.790
    ## 3|4  -1.4146     0.3021  -4.682
    ## 4|5   1.5583     0.3028   5.147

#### Comparison of Models

``` r
# Please notice that we are using anova.clm here.

comp <- anova(exp_anaphoricity_ls.clmm, exp_anaphoricity_ls.clmm2)
show(comp)
```

    ## Likelihood ratio tests of cumulative link models:
    ##  
    ##                           formula:                                                                                                                     
    ## exp_anaphoricity_ls.clmm2 FCT_ANSWER ~ ADVERBIAL_TYPE * POSITION + (1 + POSITION | subjects) + (1 + POSITION | items)                                  
    ## exp_anaphoricity_ls.clmm  FCT_ANSWER ~ ADVERBIAL_TYPE + POSITION + (0 + ADVERBIAL_TYPE * POSITION | subjects) + (0 + ADVERBIAL_TYPE * POSITION | items)
    ##                           link: threshold:
    ## exp_anaphoricity_ls.clmm2 logit flexible  
    ## exp_anaphoricity_ls.clmm  logit flexible  
    ## 
    ##                           no.par    AIC  logLik LR.stat df Pr(>Chisq)  
    ## exp_anaphoricity_ls.clmm2     15 4281.3 -2125.7                        
    ## exp_anaphoricity_ls.clmm      49 4298.4 -2100.2  50.875 34     0.0315 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Analysis of deviance reveals that the model with interaction does not
significantly reduce the deviance, but the value of 0.031 is of course
pretty close to the famous threshold of P = 0.05

#### Test items in disfavored positions with high ratings

``` r
exp_anaphoricity_ls.rating.summary <- 
  subset(exp_anaphoricity_ls.test.data, 
         ADVERBIAL_TYPE == "COM(O)" & POSITION == "PP>OBJ") %>% 
  group_by(ENCODING, ANSWER) %>% 
  summarise(count = n()) %>%
  as.data.frame()

ggplot(exp_anaphoricity_ls.rating.summary, aes(x = ANSWER, y = count)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ENCODING) +
  labs(x = "Rating", y = "Frequency of rating")
```

![](KBP_Exp1_Anaphoricity_Analysis_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
read.csv("../Data/stimuli_test_basic.csv", fileEncoding = "UTF-8") %>% 
  filter(ADVERBIAL_TYPE == "COM(O)" & KEY_CONDITION == "PP>OBJ") %>%
  select(ENCODING, ITEM) %>% knitr::kable()
```

| ENCODING | ITEM                                                                                                                          |
|:---------|:------------------------------------------------------------------------------------------------------------------------------|
| t13      | Lea hat erzählt, dass ein Kleinkrimineller zusammen mit einer Stichwaffe was vergraben hat. Was es war, weiß ich aber nicht.  |
| t14      | Ich habe gehört, dass eine Schuldirektorin zusammen mit einer Einladung was verschickt hat. Was es war, weiß ich aber nicht.  |
| t15      | Ich habe gehört, dass ein Abgeordneter zusammen mit einem Finanzplan was vorgelegt hat. Was es war, weiß ich aber nicht.      |
| t16      | Jan hat erzählt, dass ein Verleger zusammen mit einem Foto was veröffentlicht hat. Was es war, weiß ich aber nicht.           |
| t17      | Tim hat erzählt, dass eine Haushälterin zusammen mit einem Handtuch was gewaschen hat. Was es war, weiß ich aber nicht.       |
| t18      | Ich habe gehört, dass ein Verleger zusammen mit einem Literaturpreis was verliehen hat. Was es war, weiß ich aber nicht.      |
| t19      | Ich habe gehört, dass eine Küchenhilfe zusammen mit einem Schnitzel was eingefroren hat. Was es war, weiß ich aber nicht.     |
| t20      | Pia hat erzählt, dass ein Buchhalter zusammen mit einem Überweisungsträger was versandt hat. Was es war, weiß ich aber nicht. |
| t21      | Ich habe gehört, dass ein Minister zusammen mit einer Urkunde was überreicht hat. Was es war, weiß ich aber nicht.            |
| t22      | Kai hat erzählt, dass ein Händler zusammen mit einem Gemälde was verkauft hat. Was es war, weiß ich aber nicht.               |
| t23      | Eva hat erzählt, dass ein Kellner zusammen mit einer Süßspeise was serviert hat. Was es war, weiß ich aber nicht.             |
| t24      | Ich habe gehört, dass ein Hobbykoch zusammen mit einer Knoblauchzehe was püriert hat. Was es war, weiß ich aber nicht.        |
