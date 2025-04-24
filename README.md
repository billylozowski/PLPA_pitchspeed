# Predicting Pitched Ball Velocity in Baseball

This project aims to determine whether biomechanical factors relate to pitched fastball velocity in baseball. 
In-game biomechanical data collected with Kinatrax and ball data collected with Trackman at Plainsman Park will be used and subjected to two main analyses.

All data is stored within the folder "Final Project/PLPA_pitchspeed", and can be accessed by downloading the current repository.

Data will be filtered, cleaned, and analysed to ensure only the specific trials we're interested in are retained.

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

# apply the function to the data frame
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

Once we have our random forest model, we can select the variables which have the highest 'Variable Importance' values. For this analysis, values above 0.2 were selected, but this threshold can be set at the user's discretion.

```
subset2 <- subset1 %>%
  filter(`Importance (V.I.)` >= 0.2)
```

We can now run the multiple regression configurations using the "glmulti" package. This computes every conceivable regression model, and returns a number based on model fit criteria.

```
h.model <- glmulti(RelSpeed..m.s. ~ 
                     Lead.Ankle.Flexion.at.FC..deg. +
                     Step.Width..m. +
                     Center.of.Mass.A.P.at.FC.Zeroed..m. +
                     Max.Lead.Hip.Flexion.Velocity..deg.s.,
                  data = ball.speed.filtered,
                  crit = bic,          # model fit criterion
                  level = 2,           # 1 without interactions, 2 with interactions
                  method = "g",        # genetic screening algorithm (better with fewer predictors)
                  family = gaussian,
                  fitfunction = glm,   # specify the model type (lm or glm)
                  confsetsize = 100)   # keep the 100 best models (confidence set)
```

We then visualise these models (in this instance we should have 6), and see which variables are most suitable for our final model.

```
plot(g.model, type = "s")

weightable(h.model)[1:3,] %>% # select the best 3 models
  regulartable() %>%
  autofit()
```

Based on our output from "glmulti", we have one model that includes the most common parameters, and only those. To finsih, we'll summarise this model.

```
fmodel.one <- lm(RelSpeed..m.s. ~
                  # predictors
                  Lead.Ankle.Flexion.at.FC..deg. + 
                  # interactions
                  Max.Lead.Hip.Flexion.Velocity..deg.s.:Lead.Ankle.Flexion.at.FC..deg. +
                  Max.Lead.Hip.Flexion.Velocity..deg.s.:Step.Width..m.,
                data = ball.speed.filtered)

# summarise the final model
summary(model.one)
model_performance(model.one)

# create a table of the final model outputs, and save as a word .doc
tab_model(model.one, 
          show.df = TRUE,
          show.se = TRUE, 
          string.se = "SE",
          dv.labels = c("Model 1"),
          file = "Pitch Ball Velocity Models.doc")
```

Based on all of this information, our highest weighted model (model.one) appears to be the most appropriate. It contains the fewest terms and is the simplest. Now we can use this going forward, and run any additional tests using this framework.
