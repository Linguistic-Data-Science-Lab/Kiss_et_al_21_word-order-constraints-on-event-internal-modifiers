Word order constraints on event-internal modifiers - Experiment 2:
Forced Choice study on Thematic Integration
================
Tibor Kiss
10.11.2021

### Analysis of experimental study on thematic integration

This document provides the analysis for the experimental study on
thematic integration used in Kiss, Pieper, Börner (2021), final version,
including the random slopes on the interaction of main effects.

#### Read in required libraries

``` r
library(lme4)
library(tidyverse)
library(emmeans)
```

#### Read in data set

``` r
exp_int_agentivity.fc.test.data <- 
  read.csv("../Data/ForcedChoice_test.csv", fileEncoding = "UTF-8")

exp_int_agentivity.fc.test.data$ANSWER <- 
  factor(exp_int_agentivity.fc.test.data$ANSWER)

exp_int_agentivity.fc.test.data$INTEGRATION <- 
  factor(exp_int_agentivity.fc.test.data$INTEGRATION)

exp_int_agentivity.fc.test.data$ADVERBIAL_TYPE <- 
  factor(exp_int_agentivity.fc.test.data$ADVERBIAL_TYPE)

## more illustrative variable names
exp_int_agentivity.fc.test.data$subjects <- factor(exp_int_agentivity.fc.test.data$workerId)
exp_int_agentivity.fc.test.data$items <- factor(exp_int_agentivity.fc.test.data$ENCODING)
```

The following table shows an equal distribution of test items to
subjects (number of accepted subjects: 33).

    ## 
    ## 220 221 222 223 224 225 227 228 229 230 231 232 234 236 237 238 239 241 242 243 
    ##  24  24  24  24  24  24  24  24  24  24  24  24  24  24  24  24  24  24  24  24 
    ## 244 246 249 253 254 256 257 258 263 266 267 270 271 
    ##  24  24  24  24  24  24  24  24  24  24  24  24  24

#### Empirical distribution of choices

``` r
thematic_int.labels <- c("No Thematic Integration", "Thematic Integration")
names(thematic_int.labels) <- c("no", "yes")

exp_int_agentivity.fc.summary <- 
  exp_int_agentivity.fc.test.data %>%
  group_by(ADVERBIAL_TYPE, INTEGRATION, ANSWER) %>%
  summarise(count = n()) %>%
  as.data.frame()

ggplot(exp_int_agentivity.fc.summary,
       aes(x = ANSWER, y = count, fill = ADVERBIAL_TYPE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Choice", y = "Frequency of Choice") + 
  facet_wrap(~INTEGRATION, labeller = labeller(INTEGRATION = thematic_int.labels)) +
  scale_fill_manual("ADVERBIAL_TYPE", values = c("COM(S)" = "black",
                                                 "INSTR" = "grey"),
                    labels = c("comitative (subj.)", "instrumental")) +
  guides(fill=guide_legend(title="Semantic type\nof adverbial")) 
```

![](KBP_Exp2_ThematicIntegration_Analysis_files/figure-gfm/empirical%20distribution-1.png)<!-- -->

The graphical representation of the empirical distribution of choices
suggests an interaction between the main effects, as is witnessed by the
inversion of the distribution of `COM(S)` under
`Thematic Integration == "yes"`.

#### Random Slope Model with interaction

``` r
exp_int_agentivity.fc.test.data$INTEGRATION <-
  relevel(exp_int_agentivity.fc.test.data$INTEGRATION, ref = "no")
exp_int_agentivity.fc.test.data$ADVERBIAL_TYPE <-
  relevel(exp_int_agentivity.fc.test.data$ADVERBIAL_TYPE, ref = "INSTR")


exp_int_agentivity.fc.glmm <- 
  glmer(formula = ANSWER ~ ADVERBIAL_TYPE * INTEGRATION + 
          (1 + ADVERBIAL_TYPE * INTEGRATION | subjects) , 
        data = exp_int_agentivity.fc.test.data, family = binomial())
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model failed to converge with max|grad| = 0.0106935 (tol = 0.001, component 1)

``` r
options(width = 300)
summary(exp_int_agentivity.fc.glmm)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
    ##  Family: binomial  ( logit )
    ## Formula: ANSWER ~ ADVERBIAL_TYPE * INTEGRATION + (1 + ADVERBIAL_TYPE *      INTEGRATION | subjects)
    ##    Data: exp_int_agentivity.fc.test.data
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    968.0   1033.5   -470.0    940.0      778 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2515 -0.6511 -0.3624  0.7384  2.5674 
    ## 
    ## Random effects:
    ##  Groups   Name                                Variance Std.Dev. Corr             
    ##  subjects (Intercept)                         1.6691   1.292                     
    ##           ADVERBIAL_TYPECOM(S)                0.2266   0.476    -0.43            
    ##           INTEGRATIONyes                      1.1509   1.073    -0.75  0.19      
    ##           ADVERBIAL_TYPECOM(S):INTEGRATIONyes 0.2333   0.483     0.55 -0.10 -0.97
    ## Number of obs: 792, groups:  subjects, 33
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                          -0.6317     0.2864  -2.206   0.0274 *  
    ## ADVERBIAL_TYPECOM(S)                  0.4087     0.2525   1.619   0.1055    
    ## INTEGRATIONyes                       -0.6283     0.3266  -1.924   0.0544 .  
    ## ADVERBIAL_TYPECOM(S):INTEGRATIONyes   1.6501     0.3632   4.543 5.54e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                       (Intr) ADVERBIAL_TYPECOM(S) INTEGR
    ## ADVERBIAL_TYPECOM(S)  -0.540                            
    ## INTEGRATION           -0.672  0.411                     
    ## ADVERBIAL_TYPECOM(S):  0.399 -0.626               -0.728
    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.0106935 (tol = 0.001, component 1)

We’ll have to consider possible spurious convergence warnings, and apply
tests from
<https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html>,
i.e. Ben Bolker’s troubleshooting page for spurious convergence errors.

``` r
tt <- getME(exp_int_agentivity.fc.glmm,"theta")
ll <- getME(exp_int_agentivity.fc.glmm,"lower")
```

The comparatively high value of 0.009 suggests that singularity is not
an issue here. Restarting solves the problem here.

``` r
ss <- 
  getME(exp_int_agentivity.fc.glmm,c("theta","fixef"))
exp_int_agentivity.fc.glmm <- 
  update(exp_int_agentivity.fc.glmm,
         start=ss,
         control=glmerControl(optCtrl=list(maxfun=2e4)))
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model failed to converge with max|grad| = 0.00107789 (tol = 0.001, component 1)

``` r
options(width = 300)
summary(exp_int_agentivity.fc.glmm)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
    ##  Family: binomial  ( logit )
    ## Formula: ANSWER ~ ADVERBIAL_TYPE * INTEGRATION + (1 + ADVERBIAL_TYPE *      INTEGRATION | subjects)
    ##    Data: exp_int_agentivity.fc.test.data
    ## Control: glmerControl(optCtrl = list(maxfun = 20000))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    968.0   1033.5   -470.0    940.0      778 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2511 -0.6510 -0.3625  0.7382  2.5671 
    ## 
    ## Random effects:
    ##  Groups   Name                                Variance Std.Dev. Corr             
    ##  subjects (Intercept)                         1.6709   1.2926                    
    ##           ADVERBIAL_TYPECOM(S)                0.2273   0.4767   -0.44            
    ##           INTEGRATIONyes                      1.1526   1.0736   -0.75  0.20      
    ##           ADVERBIAL_TYPECOM(S):INTEGRATIONyes 0.2354   0.4852    0.55 -0.10 -0.97
    ## Number of obs: 792, groups:  subjects, 33
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                          -0.6315     0.2865  -2.204   0.0275 *  
    ## ADVERBIAL_TYPECOM(S)                  0.4090     0.2526   1.619   0.1054    
    ## INTEGRATIONyes                       -0.6284     0.3266  -1.924   0.0544 .  
    ## ADVERBIAL_TYPECOM(S):INTEGRATIONyes   1.6495     0.3633   4.541  5.6e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                       (Intr) ADVERBIAL_TYPECOM(S) INTEGR
    ## ADVERBIAL_TYPECOM(S)  -0.540                            
    ## INTEGRATION           -0.672  0.412                     
    ## ADVERBIAL_TYPECOM(S):  0.399 -0.627               -0.729
    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.00107789 (tol = 0.001, component 1)

#### Model predictions

``` r
predictions <- 
  emmeans(exp_int_agentivity.fc.glmm, pairwise~ADVERBIAL_TYPE | INTEGRATION, 
          type = "response") 

## also response useful, now switched to predictor for effects
## if lp is used, y = emmean, if response is used y = prob

predictions.emm <- data.frame(predictions$emmeans)
 

predictions.emm
```

    ##   ADVERBIAL_TYPE INTEGRATION      prob         SE  df asymp.LCL asymp.UCL
    ## 1          INSTR          no 0.3471788 0.06493773 Inf 0.2327183 0.4825318
    ## 2         COM(S)          no 0.4446057 0.06424280 Inf 0.3246675 0.5713650
    ## 3          INSTR         yes 0.2210003 0.04319004 Inf 0.1478520 0.3168805
    ## 4         COM(S)         yes 0.6896843 0.04469537 Inf 0.5961214 0.7699374

``` r
ggplot(predictions.emm, aes(x = ADVERBIAL_TYPE, y = prob)) + 
  geom_bar(aes(), stat = "identity", fill = "grey", width = 0.3) +
  facet_wrap(~INTEGRATION, labeller = labeller(INTEGRATION = thematic_int.labels)) +
  labs(x = "Adverbial Types", y = "Pr(PP>OBJ)")
```

![](KBP_Exp2_ThematicIntegration_Analysis_files/figure-gfm/Model%20predictions-1.png)<!-- -->
