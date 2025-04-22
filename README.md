# Predicting Pitched Ball Velocity in Baseball

This project aims to determine which biomechanical factors relate to pitched fastball velocity in baseball. 
In-game biomechanical data collected with Kinatrax and ball data collected with Trackman at Plainsman Park will be used and subjected to two main analyses.

All data is stored within the folder "Final Project/PLPA_pitchspeed", and can be accessed by downloading the current repository.

Once the data and relevant packages are loaded, it will be filtered to ensure only the specific trials we're interested in are retained.

```
# subset data
ball.speed.subset <- ball.speed %>%
  filter(Pitch.Type == "Fastball") %>%
  # select only columns with data we're interested in
  select(RelSpeed..m.s., 24:ncol(ball.speed))
```

We then clean the data to remove any outliers.

```
remove_outliers <- function(data) {
  data[] <- lapply(data, function(column) {
    if (is.numeric(column)) {
      
      # calculate Q1, Q3, and IQR
      Q1 <- quantile(column, 0.25, na.rm = TRUE)
      Q3 <- quantile(column, 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1
      
      # set thresholds for outliers
      lower_limit <- Q1 - 2 * IQR_val
      upper_limit <- Q3 + 2 * IQR_val
      
      # replace outliers with NA (or you can choose to filter them out)
      column[column < lower_limit | column > upper_limit] <- NA
    }
    return(column)
  })
  return(data)
}

# apply the function to your data frame
ball.speed.clean <- remove_outliers(ball.speed.subset)
```

After removing outliers, and replacing them with "NA" values, we can create a "cleaned" dataset.

```
ball.speed.filtered <- ball.speed.clean %>%
  filter(!if_any(everything(), is.na))
```

This will then be used to run our random forest model.

```
rf.ball.speed <- cforest(RelSpeed..m.s. ~ ., data = ball.speed.filtered)
```

Once we have our random forest model, we can select the variables which have the highest 'Variable Importance' values. For this analysis, values above 0.2 were selected, but this threshold can be set at the user's discression.

+ sometimes 4 variables exhibit V.I. values > 0.2, but other times 5 do. If this is the case, ensure that the correct variables are included in the following portions of the analysis!

```
subset2 <- subset1 %>%
  filter(`Importance (V.I.)` >= 0.2)
```

We can now run the multiple regression configurations using the "glmulti" package. This computes every conceivable regression model, and returns a number based on model fit criteria.

```
g.model <- glmulti(RelSpeed..m.s. ~ 
                     Step.Width..m. +
                     Lead.Ankle.Flexion.at.FC..deg. +
                     Center.of.Mass.A.P.at.FC.Zeroed..m. +
                     Stride.Orientation,
                  data = ball.speed.filtered,
                  crit = bic,          # model fit criterion
                  level = 2,           # 1 without interactions, 2 with interactions
                  method = "h",        # exhaustive screening algorithm (better with fewer predictors)
                  family = gaussian,
                  fitfunction = glm,   # specify the model type (lm or glm)
                  confsetsize = 100)   # keep the 100 best models (confidence set)
```

We then visualise these models (in this instance we should have 6), and see which variables are most suitable for our final model.

```
weightable(g.model)[1:6,] %>% # select the best 6 models
  regulartable() %>%
  autofit()

plot(g.model, type = "s")
```

Based on our output from "glmulti", we have two potential models we can use. So, we'll compare these on information criterion, and the visuals created above.

```
first.model <- lm(RelSpeed..m.s. ~
                    # predictors
                    Lead.Ankle.Flexion.at.FC..deg. + 
                    Lead.Ankle.Flexion.at.FC..deg. +
                    # interactions
                    Lead.Ankle.Flexion.at.FC..deg.:Step.Width..m. +
                    Center.of.Mass.A.P.at.FC.Zeroed..m.:Lead.Ankle.Flexion.at.FC..deg. +
                    Stride.Orientation:Lead.Ankle.Flexion.at.FC..deg.,
                  data = ball.speed.filtered)

# summarise the final model
summary(first.model)

second.model <- lm(RelSpeed..m.s. ~
                    # predictors
                    Lead.Ankle.Flexion.at.FC..deg. +
                    # interactions
                    Center.of.Mass.A.P.at.FC.Zeroed..m.:Lead.Ankle.Flexion.at.FC..deg.,
                    data = ball.speed.filtered)
# summarise the final model
summary(second.model)

# create a table of the final model outputs, and save as a word .doc
tab_model(first.model, second.model, 
          show.df = TRUE,
          show.se = TRUE, 
          string.se = "SE",
          dv.labels = c("Strongest Model", "Second Strongest Model"),
          file = "Pitch Ball Velocity Models.doc")
```

To be sure we select the most appropriate model, we'll run a model comparison. 

```
model_performance(first.model)
model_performance(second.model)

# compare models with performance package
compare_performance(first.model, second.model, rank = T)

# compare models with flexplot package
model.comparison(first.model, second.model)
```

Based on all of this information, the second strongest model (second.model) appears to be the most appropriate. It contains the fewest terms and is comparable in terms of fit to a more complex model. Though it does account for less variance in our outcome, the Bayes Factor shows that both this and the strongest weighted model are almost the same. Therefore, we'll choose the simple model to proceed.
Now we can use this going forward.
