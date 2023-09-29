### regression tree

library(tidyverse)
library(tidymodels)
library(vroom)
library(glmnet)
library(rpart)
library(ranger)

## Read in the data
bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("./test.csv")

## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, - registered)
#put count on log scale
logTrainData <- bikeTrain %>%
  mutate(count=log(count))
#view(logTrainData)

log_bike_recipe <- recipe(count~., data=logTrainData) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  #Try w/o hour factor first
  step_mutate(hour = factor(hour(datetime), levels=c(0:23), labels=c(0:23))) %>%
  #step_poly(temp, degree=2) %>%
  #step_dummy(all_nominal_predictors()) %>% #make dummy variables
  #step_normalize(all_numeric_predictors())%>% #make mean 0, sd=1
  #step_poly(atemp, degree=2) %>%
  step_rm(datetime)
prepped_recipe <- prep(log_bike_recipe)
bake(prepped_recipe, new_data = logTrainData) #Make sure recipe work on train
bake(prepped_recipe, new_data = bikeTest)


my_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=500) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")


randFor_wf <- workflow()%>%
  add_recipe(log_bike_recipe) %>%
  add_model(my_mod) 

#show what variables to tune
tuning_grid <- grid_regular(mtry(range=c(1,10)),
                            min_n(),
                            levels=5)
folds <- vfold_cv(logTrainData, v=5, repeats = 1)
#cross validate and prune your reg tree
CV_results <- randFor_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics = metric_set(rmse))
#the best tuning parameters
bestTune <- CV_results %>%
  select_best("rmse")

#plot various cross validated tuning parameters
collect_metrics(CV_results) %>%
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

final_wf <- 
  randFor_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=logTrainData)
summary(final_wf)

## Get Predictions for test set AND format for Kaggle
test_preds <- predict(final_wf, new_data = bikeTest) %>%
  mutate(.pred=exp(.pred))%>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write prediction file to CSV
vroom_write(x=test_preds, file="./RandTreeTestPreds.csv", delim=",")







