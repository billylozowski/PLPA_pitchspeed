``` r
set.seed(1618) # ensure all randomly generated data is repeatable
# as a side joke, this seed is the year in which the 'second defenestration of Prague' occurred
```

[*How to install Java*](https://www.java.com/en/download/)

## Load Packages

Before we do anything, we need load the packages required for our
analysis.

``` r
library(tidyverse)
library(rJava)
library(glmulti)
library(party)
library(flexplot)
library(tictoc)
library(flextable)
library(sjPlot)
library(performance)
```

## Load Data

We’ll load our data “Pitch Speed.csv” and create the object
“ball.speed”.

``` r
ball.speed <- read.csv("Pitch Speed.csv", header = TRUE)
```

## Filtering Fastball-Only Data

We’re going to subset the data to include only ‘fastball’ pitches, and
the variables we’re interested in in relation to this pitch type.

``` r
# subset data
ball.speed.subset <- ball.speed %>%
  filter(Pitch.Type == "Fastball") %>%
  # select only columns with data we're interested in
  select(RelSpeed..m.s., 24:ncol(ball.speed))
```

## Visualise Data (1)

Before we clean the data, we’ll visualise it to get an idea of where any
potential outliers might be.

![](Pitch-Speed_files/figure-gfm/data%20cleaning-1.png)<!-- -->

## Visualise Data (2)

Our first plot doesn’t really help us with values on the lower end
because of some extreme values. This is mainly due to one particular
variable “max trunk rotation acceleration”, so we’ll remove this
variable and re-plot the data.

![](Pitch-Speed_files/figure-gfm/replotting%20the%20data-1.png)<!-- -->

This is a bit better, but it’s still not very clear. Given the scale of
difference in variable values, it doesn’t seem appropriate to keep
removing the highest values. So, instead, since outliers do seem to be
present in the data, we’ll go ahead and remove these.

## Remove Outliers

``` r
# apply the function to your data frame
ball.speed.clean <- remove_outliers(ball.speed.subset)
```

## Remove “NA” values

Since we’ve removed outliers, we’ll need to account for this so our
random forest model can run properly. We’ll ensure no “NA” values are
included in our model.

``` r
# filter the missing values from the data set
ball.speed.filtered <- ball.speed.clean %>%
  filter(!if_any(everything(), is.na))
```

## Random Forest Model

To determine which variables we want to keep in our model later on, we
can now construct the random forest (rf) model for pitch speed. We’ll
use “variable importance” [(Mahieu et al.,
2023)](https://www.sciencedirect.com/science/article/abs/pii/S0169743923002368)
to make our decisions!

``` r
# construct the rf model
rf.ball.speed <- cforest(RelSpeed..m.s. ~ ., data = ball.speed.filtered)

# extract the estimates for rf.ball.speed
estimates.ball.speed <- estimates(rf.ball.speed)
```

    ##                                 Variable Importance (V.I.)
    ## 1         Lead.Ankle.Flexion.at.FC..deg.             0.249
    ## 2                     Stride.Orientation             0.248
    ## 3                         Step.Width..m.             0.246
    ## 4    Center.of.Mass.A.P.at.FC.Zeroed..m.             0.220
    ## 5  Max.Lead.Hip.Flexion.Velocity..deg.s.             0.206
    ## 6          Lead.Hip.Rotation.at.FC..deg.             0.202
    ## 7                 Trunk.Lean.at.BR..deg.             0.194
    ## 8              Trunk.Flexion.at.BR..deg.             0.174
    ## 9         Shoulder.Rotation.at.MER..deg.             0.173
    ## 10              Pelvis.Tilt.at.MER..deg.             0.164

## Plot Variable Importance (1)

Since the variables are arranged in order of their importance, we’ll
visualise this to see if there are any clear cut-offs (i.e. variables we
should vs. variables we maybe shouldn’t include).

![](Pitch-Speed_files/figure-gfm/plot%20estimates-1.png)<!-- -->

From this first plot, it seems as if there might be a cut-off in terms
of our variable importance. Let’s zoom in and see if we can get a better
idea how many variables this includes.

## Plot Variable Importance (2)

![](Pitch-Speed_files/figure-gfm/subset%201-1.png)<!-- -->

## Automated Model Selection and Model-Averaging - Summary

When deciding which variables to include in the next portion of our
analysis, we’re going to set a V.I. threshold of \>= 0.2. Though this
cut-off is arbitrary (it’s not based on any literature), 0.2 looks like
it might be suitable.

Six variables (below) have a V.I. of more than 0.2. As such, we’ll
subset the data again to include just these (i.e. V.I. values \>= 0.2),
and move onto our automated model selection and model-averaging analysis
using the **‘glmulti’** package.

1.  Lead.Ankle.Flexion.at.FC..deg.  
2.  Stride.Orientation
3.  Step.Width..m.
4.  Center.of.Mass.A.P.at.FC.Zeroed..m.
5.  Max.Lead.Hip.Flexion.Velocity..deg.s.
6.  Lead.Hip.Rotation.at.FC..deg.

We’ll output a general summary of how many regression models we’ll
assess.

- *Note. RF models sample the data at random, so the V.I. values can
  change for each variable after each run. To ensure only those with a
  V.I \>= 0.2 are included in our regression models below, we’ll
  dynamically pull the variables with a V.I. value \>= 0.2 from the
  “variable.names” object*

<!-- -->

    ## Initialization...
    ## TASK: Diagnostic of candidate set.
    ## Sample size: 311
    ## 0 factor(s).
    ## 6 covariate(s).
    ## 0 f exclusion(s).
    ## 0 c exclusion(s).
    ## 0 f:f exclusion(s).
    ## 0 c:c exclusion(s).
    ## 0 f:c exclusion(s).
    ## Size constraints: min =  0 max = -1
    ## Complexity constraints: min =  0 max = -1
    ## Your candidate set contains 64 models.

    ## [1] 64

## Automated Model Selection and Model-Averaging - Genetic Algorithm

We’ll now build our models using an genetic algorithm. This will compute
a number of regression models and return the best 100. This should then
give us an idea as to which combination of predictors might best explain
our pitched ball velocity. Whilst we’re using the genetic algorithm
here, it’s advised to refer to the documentation to ensure the
correct/desired model parameters are set.

[**glmulti
documentation**](https://www.rdocumentation.org/packages/glmulti/versions/1.0.8/topics/glmulti)

## Visualise the Best 100 Models

    ## glmulti.analysis
    ## Method: g / Fitting: glm / IC used: bic
    ## Level: 2 / Marginality: FALSE
    ## From 100 models:
    ## Best IC: 933.527425938421
    ## Best model:
    ## [1] "RelSpeed..m.s. ~ 1 + Lead.Ankle.Flexion.at.FC..deg. + Max.Lead.Hip.Flexion.Velocity..deg.s. + "          
    ## [2] "    Stride.Orientation:Lead.Ankle.Flexion.at.FC..deg. + Step.Width..m.:Lead.Ankle.Flexion.at.FC..deg. + "
    ## [3] "    Max.Lead.Hip.Flexion.Velocity..deg.s.:Lead.Ankle.Flexion.at.FC..deg."                                
    ## Evidence weight: 0.207148565199238
    ## Worst IC: 957.249242159758
    ## 3 models within 2 IC units.
    ## 19 models to reach 95% of evidence weight.
    ## Convergence after 540 generations.
    ## Time elapsed: 1.12269554932912 minutes.

![](Pitch-Speed_files/figure-gfm/model%20information-1.png)<!-- -->

## Extracting Model Information

Let’s extract some model information and see which variables the best
models contain (in this case, the best 3 models).

#### Model-Averaged Importance Terms

![](Pitch-Speed_files/figure-gfm/variable%20importance%20across%20models-1.png)<!-- -->

    ## png 
    ##   2

#### Best 3 Models

![](Pitch-Speed_files/figure-gfm/model%20variables,%20weights,%20and%20ICs-1.png)<!-- -->

**Lead.Ankle.Flexion.at.FC..deg.**,
**Max.Lead.Hip.Flexion.Velocity..deg.s.:Lead.Ankle.Flexion.at.FC..deg.**,
and **Max.Lead.Hip.Flexion.Velocity..deg.s.:Step.Width..m.** appear to
be the most common terms across all computed models (included in \>
80%). Incidentally, they are the sole 3 terms in our highest weighted
model, so we’ll summarise this to finish.

## Final Model

``` r
# summarise the strongest model
summary(model.one)
```

    ## 
    ## Call:
    ## lm(formula = RelSpeed..m.s. ~ Lead.Ankle.Flexion.at.FC..deg. + 
    ##     Max.Lead.Hip.Flexion.Velocity..deg.s.:Lead.Ankle.Flexion.at.FC..deg. + 
    ##     Max.Lead.Hip.Flexion.Velocity..deg.s.:Step.Width..m., data = ball.speed.filtered)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.51747 -0.68005 -0.09344  0.65772  3.13944 
    ## 
    ## Coefficients:
    ##                                                                         Estimate
    ## (Intercept)                                                          43.09481087
    ## Lead.Ankle.Flexion.at.FC..deg.                                       -0.08512386
    ## Lead.Ankle.Flexion.at.FC..deg.:Max.Lead.Hip.Flexion.Velocity..deg.s.  0.00009194
    ## Max.Lead.Hip.Flexion.Velocity..deg.s.:Step.Width..m.                  0.00729804
    ##                                                                       Std. Error
    ## (Intercept)                                                           0.47325458
    ## Lead.Ankle.Flexion.at.FC..deg.                                        0.01032710
    ## Lead.Ankle.Flexion.at.FC..deg.:Max.Lead.Hip.Flexion.Velocity..deg.s.  0.00001490
    ## Max.Lead.Hip.Flexion.Velocity..deg.s.:Step.Width..m.                  0.00125519
    ##                                                                      t value
    ## (Intercept)                                                           91.061
    ## Lead.Ankle.Flexion.at.FC..deg.                                        -8.243
    ## Lead.Ankle.Flexion.at.FC..deg.:Max.Lead.Hip.Flexion.Velocity..deg.s.   6.172
    ## Max.Lead.Hip.Flexion.Velocity..deg.s.:Step.Width..m.                   5.814
    ##                                                                                  Pr(>|t|)
    ## (Intercept)                                                          < 0.0000000000000002
    ## Lead.Ankle.Flexion.at.FC..deg.                                        0.00000000000000495
    ## Lead.Ankle.Flexion.at.FC..deg.:Max.Lead.Hip.Flexion.Velocity..deg.s.  0.00000000212710417
    ## Max.Lead.Hip.Flexion.Velocity..deg.s.:Step.Width..m.                  0.00000001526445500
    ##                                                                         
    ## (Intercept)                                                          ***
    ## Lead.Ankle.Flexion.at.FC..deg.                                       ***
    ## Lead.Ankle.Flexion.at.FC..deg.:Max.Lead.Hip.Flexion.Velocity..deg.s. ***
    ## Max.Lead.Hip.Flexion.Velocity..deg.s.:Step.Width..m.                 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.044 on 307 degrees of freedom
    ## Multiple R-squared:  0.2623, Adjusted R-squared:  0.2551 
    ## F-statistic: 36.39 on 3 and 307 DF,  p-value: < 0.00000000000000022

``` r
model_performance(model.one)
```

    ## # Indices of model performance
    ## 
    ## AIC     |    AICc |     BIC |    R2 | R2 (adj.) |  RMSE | Sigma
    ## ---------------------------------------------------------------
    ## 915.433 | 915.630 | 934.132 | 0.262 |     0.255 | 1.037 | 1.044

## Conclusion

The linear combination of **Lead.Ankle.Flexion.at.FC..deg.**,
**Max.Lead.Hip.Flexion.Velocity..deg.s.:Lead.Ankle.Flexion.at.FC..deg.**,
and **Max.Lead.Hip.Flexion.Velocity..deg.s.:Step.Width..m.** accounted
for ~26% of the variance in pitched ball velocity. For every 1 unit
change in each term (respectively), pitched ball velocity is only
expected to change minimally (\< 0.1 m/s). Considering that there is
still 74% of variance unexplained, we might conclude that biomechanical
variables other than those identified here may do a better job of
explaining pitched ball velocity in baseball.
