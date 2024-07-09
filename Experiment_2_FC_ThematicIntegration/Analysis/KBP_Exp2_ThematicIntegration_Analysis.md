Word order constraints on event-internal modifiers - Experiment 2:
Forced Choice study on Thematic Integration
================
Tibor Kiss
10.11.2021/24.01.2022

### Analysis of experimental study on thematic integration

This document provides the analysis for the experimental study on
thematic integration used in Kiss, Pieper, Börner (2021), final version,
including the random slopes on the interaction of main effects.

#### Read in required libraries

``` r
library(lme4)
library(tidyverse)
library(emmeans)

show(version)
```

    ##                _                           
    ## platform       aarch64-apple-darwin20      
    ## arch           aarch64                     
    ## os             darwin20                    
    ## system         aarch64, darwin20           
    ## status                                     
    ## major          4                           
    ## minor          2.1                         
    ## year           2022                        
    ## month          06                          
    ## day            23                          
    ## svn rev        82513                       
    ## language       R                           
    ## version.string R version 4.2.1 (2022-06-23)
    ## nickname       Funny-Looking Kid

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
thematic_int.labels <- c("Privative Interpretation", "Affirmative Interpretation")
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
  scale_fill_manual(name = "Adverbial Type", values = c("lightgrey", "darkgrey"),
                    labels = c("comitative", "instrumental")) +
  theme_bw()
```

![](KBP_Exp2_ThematicIntegration_Analysis_files/figure-gfm/empirical%20distribution-1.png)<!-- -->

``` r
ggsave("/Users/tibor/Documents/data_dist_fc2.pdf")

exp_int_agentivity.fc.summary.wide <-
  exp_int_agentivity.fc.summary %>%
  spread(key = ANSWER, value = count)

exp_int_agentivity.fc.summary.wide
```

    ##   ADVERBIAL_TYPE INTEGRATION OBJ>PP PP>OBJ
    ## 1         COM(S)          no    108     90
    ## 2         COM(S)         yes     65    133
    ## 3          INSTR          no    122     76
    ## 4          INSTR         yes    148     50

The graphical representation of the empirical distribution of choices
suggests an interaction between the main effects, as is witnessed by the
inversion of the distribution of `COM(S)` under
`Thematic Integration == "yes"`.

#### Random Slope Model with interaction

Please notice that the random structure does not assume intercepts,
because we want to obtain information on by-subject variance
(i.e. workerId) for each condition.

``` r
exp_int_agentivity.fc.test.data$INTEGRATION <-
  relevel(exp_int_agentivity.fc.test.data$INTEGRATION, ref = "no")
exp_int_agentivity.fc.test.data$ADVERBIAL_TYPE <-
  relevel(exp_int_agentivity.fc.test.data$ADVERBIAL_TYPE, ref = "INSTR")


exp_int_agentivity.fc.glmm <- 
  glmer(formula = ANSWER ~ ADVERBIAL_TYPE * INTEGRATION + 
          (0 + ADVERBIAL_TYPE * INTEGRATION | subjects) , 
        data = exp_int_agentivity.fc.test.data, family = binomial())
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model failed to converge with max|grad| = 0.0131335 (tol = 0.002, component 1)

``` r
options(width = 300)
summary(exp_int_agentivity.fc.glmm)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
    ##  Family: binomial  ( logit )
    ## Formula: ANSWER ~ ADVERBIAL_TYPE * INTEGRATION + (0 + ADVERBIAL_TYPE *      INTEGRATION | subjects)
    ##    Data: exp_int_agentivity.fc.test.data
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    968.0   1033.5   -470.0    940.0      778 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2503 -0.6507 -0.3624  0.7384  2.5680 
    ## 
    ## Random effects:
    ##  Groups   Name                                Variance Std.Dev. Corr             
    ##  subjects ADVERBIAL_TYPEINSTR                 1.6626   1.2894                    
    ##           ADVERBIAL_TYPECOM(S)                1.3585   1.1655    0.93            
    ##           INTEGRATIONyes                      1.1448   1.0700   -0.75 -0.75      
    ##           ADVERBIAL_TYPECOM(S):INTEGRATIONyes 0.2316   0.4812    0.55  0.57 -0.97
    ## Number of obs: 792, groups:  subjects, 33
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                          -0.6301     0.2860  -2.204   0.0276 *  
    ## ADVERBIAL_TYPECOM(S)                  0.4078     0.2523   1.616   0.1060    
    ## INTEGRATIONyes                       -0.6301     0.3262  -1.931   0.0534 .  
    ## ADVERBIAL_TYPECOM(S):INTEGRATIONyes   1.6509     0.3631   4.547 5.43e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                       (Intr) ADVERBIAL_TYPECOM(S) INTEGR
    ## ADVERBIAL_TYPECOM(S)  -0.539                            
    ## INTEGRATION           -0.671  0.411                     
    ## ADVERBIAL_TYPECOM(S):  0.398 -0.626               -0.728
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.0131335 (tol = 0.002, component 1)

We’ll have to consider possible spurious convergence warnings, and apply
tests from
<https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html>,
i.e. Ben Bolker’s troubleshooting page for spurious convergence errors.

``` r
tt <- getME(exp_int_agentivity.fc.glmm,"theta")
ll <- getME(exp_int_agentivity.fc.glmm,"lower")
```

The comparatively high value of 0.003 suggests that singularity is not
an issue here. Restarting solves the problem here.

``` r
ss <- 
  getME(exp_int_agentivity.fc.glmm,c("theta","fixef"))
exp_int_agentivity.fc.glmm <- 
  update(exp_int_agentivity.fc.glmm,
         start=ss,
         control=glmerControl(optCtrl=list(maxfun=2e4)))

options(width = 300)
summary(exp_int_agentivity.fc.glmm)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
    ##  Family: binomial  ( logit )
    ## Formula: ANSWER ~ ADVERBIAL_TYPE * INTEGRATION + (0 + ADVERBIAL_TYPE *      INTEGRATION | subjects)
    ##    Data: exp_int_agentivity.fc.test.data
    ## Control: glmerControl(optCtrl = list(maxfun = 20000))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    968.0   1033.5   -470.0    940.0      778 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2511 -0.6510 -0.3624  0.7382  2.5673 
    ## 
    ## Random effects:
    ##  Groups   Name                                Variance Std.Dev. Corr             
    ##  subjects ADVERBIAL_TYPEINSTR                 1.6708   1.2926                    
    ##           ADVERBIAL_TYPECOM(S)                1.3608   1.1666    0.93            
    ##           INTEGRATIONyes                      1.1526   1.0736   -0.75 -0.75      
    ##           ADVERBIAL_TYPECOM(S):INTEGRATIONyes 0.2355   0.4853    0.55  0.57 -0.97
    ## Number of obs: 792, groups:  subjects, 33
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                          -0.6314     0.2865  -2.204   0.0275 *  
    ## ADVERBIAL_TYPECOM(S)                  0.4090     0.2526   1.619   0.1053    
    ## INTEGRATIONyes                       -0.6285     0.3267  -1.924   0.0543 .  
    ## ADVERBIAL_TYPECOM(S):INTEGRATIONyes   1.6495     0.3633   4.541  5.6e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                       (Intr) ADVERBIAL_TYPECOM(S) INTEGR
    ## ADVERBIAL_TYPECOM(S)  -0.540                            
    ## INTEGRATION           -0.672  0.412                     
    ## ADVERBIAL_TYPECOM(S):  0.399 -0.627               -0.729

If we compare the two alternatives for INTEGRATION, we notice two
things:

-   The by-subject variation for `INTEGRATION == "no"` is similar for
    both `ADVERBIAL_TYPEs`, and there is a positive correlation between
    the random effects for `INSTR` (random intecept) and `COM(S)`
    (random slope), which means that subjects may have individual
    preferences, but these are the same for both `ADVERBIAL_TYPEs`.
-   In contrast, the by-subject variation for `INTEGRATION == "yes"`
    differs sharply: For `INSTR` it is comparable with the values for
    `INTEGRATION == "no"`, but for `COM(S)` it is much smaller. In
    addition, there is a negative correlation between the random slopes
    for `ADVERBIAL_TYPE`, so that lower probabilities for chosing
    `PP > OBJ` given `INSTR` correspond to higher probabilities for
    chosing `PP > OBJ` given `COM(S)`.

The model of the experiment thus suggests that there is considerable
variance among speakers in case of instrumentals or no thematic
integration, and much less variance in case of thematic integration with
comitatives. (The correlations are illustrated below.)

#### Model predictions

``` r
predictions <- 
  emmeans(exp_int_agentivity.fc.glmm, pairwise~ADVERBIAL_TYPE * INTEGRATION, 
          type = "response") 

## also response useful, now switched to predictor for effects
## if lp is used, y = emmean, if response is used y = prob

predictions.emm <- data.frame(predictions$emmeans)
 

predictions.emm
```

    ##   ADVERBIAL_TYPE INTEGRATION      prob         SE  df asymp.LCL asymp.UCL
    ## 1          INSTR          no 0.3471847 0.06493677 Inf 0.2327252 0.4825351
    ## 2         COM(S)          no 0.4446236 0.06424254 Inf 0.3246847 0.5713812
    ## 3          INSTR         yes 0.2209854 0.04319257 Inf 0.1478345 0.3168731
    ## 4         COM(S)         yes 0.6896791 0.04469603 Inf 0.5961149 0.7699335

``` r
ggplot(predictions.emm, aes(x = ADVERBIAL_TYPE, y = prob)) + 
  geom_bar(aes(), stat = "identity", fill = "grey", width = 0.3) +
  facet_wrap(~INTEGRATION, labeller = labeller(INTEGRATION = thematic_int.labels)) +
  labs(x = "Adverbial Types", y = "Pr(PP>OBJ)")
```

![](KBP_Exp2_ThematicIntegration_Analysis_files/figure-gfm/Model%20predictions-1.png)<!-- -->

#### Plotting correlations for random effects

In the following, we will plot predictions including random effects for
five arbitrary participants (just as illustration), and correlations
between random effects for `INTEGRATION == "no"` and
`INTEGRATION == "yes"`, as individual odds ratios. (I am not sure
whether the latter is common practice, but it is illustrative.)

``` r
ranef.df <- data.frame(ranef(exp_int_agentivity.fc.glmm)$subjects)
ranef.df$participants <- rownames(ranef.df)
colnames(ranef.df)[1:4] <- c("INSTR", "COM_S", "INT_YES_INSTR", "INT_YES_COM_S")

ranef.df <- ranef.df %>%
  mutate(INSTR = INSTR + exp_int_agentivity.fc.glmm@beta[1]) %>%
  mutate(COM_S = COM_S + exp_int_agentivity.fc.glmm@beta[2]) %>%
  mutate(INT_YES_INSTR = INT_YES_INSTR + exp_int_agentivity.fc.glmm@beta[3]) %>%
  mutate(INT_YES_COM_S = INT_YES_COM_S + exp_int_agentivity.fc.glmm@beta[4]) %>%
  arrange(INT_YES_COM_S)

set.seed(42)

ranef.sub.df <- subset(ranef.df, participants %in% sample(ranef.df$participants, 5))

ranef.sub.df <- 
  ranef.sub.df %>% 
  gather(condition, pred, INSTR:INT_YES_COM_S)

ranef.sub.df$condition <- 
  factor(ranef.sub.df$condition, levels = c("INSTR", "COM_S", "INT_YES_INSTR", "INT_YES_COM_S"))

ggplot(ranef.sub.df, aes(x = condition)) + 
  geom_line(aes(y = pred, group = participants, color = participants), show.legend = FALSE) +
  scale_x_discrete(labels = c("Instrumental/no int.", "Comitative/no int.", 
                              "Instrumental/Int.", "Comitative/Int.")) +
  labs(x = "Linear predictions for five arbitrary participants", y = "")
```

![](KBP_Exp2_ThematicIntegration_Analysis_files/figure-gfm/plots%20for%20random%20structure%20(participants)-1.png)<!-- -->

The figure shows nicely the spread between participants in the different
condition, but also the comparatively uniform behavior w.r.t. the last
condition.

``` r
ggplot(ranef.df, aes(x = INT_YES_COM_S, y = INT_YES_INSTR)) +
  geom_point(aes(color = participants), show.legend = FALSE) +
  labs(x = "PP > OBJ given COM(S)", 
       y = "PP > OBJ given INSTR", 
       subtitle = "Negative correlation of random slopes for ADVERBIAL_TYPE given INTEGRATION == yes")
```

![](KBP_Exp2_ThematicIntegration_Analysis_files/figure-gfm/plots%20of%20correlations-1.png)<!-- -->

``` r
ggplot(ranef.df, aes(x = COM_S, y = INSTR)) +
  geom_point(aes(color = participants), show.legend = FALSE) +
  labs(x = "PP > OBJ given COM(S)", 
       y = "PP > OBJ given INSTR", 
       subtitle = "Positive correlation of random slopes for ADVERBIAL_TYPE given INTEGRATION == no")
```

![](KBP_Exp2_ThematicIntegration_Analysis_files/figure-gfm/plots%20of%20correlations-2.png)<!-- -->

We notice that not only do the correlations between the random effects
for `ADVERBIAL_TYPE` differ, but also that the by-subject variability
for `COM(S)` given `INTEGRATION == "yes"` is much smaller.
