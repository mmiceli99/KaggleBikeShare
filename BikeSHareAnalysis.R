
library(vroom)
library(tidyverse)
library(tidymodels)

test <- vroom("C:/School/Stat348/KaggleBikeShare/test.csv")
vData <- vroom("C:/School/Stat348/KaggleBikeShare/train.csv")
vData <- vData%>%
  select(-casual, -registered) %>%
  mutate(season = as.factor(season)) %>%
  mutate(holiday = as.factor(holiday)) %>%
  mutate(workingday = as.factor(workingday)) %>%
  mutate(weather = as.factor(weather))
  
vData <- vData%>%
  select(-casual, -registered) %>%
  mutate(season = as.factor(season)) %>%
  mutate(holiday = as.factor(holiday)) %>%
  mutate(workingday = as.factor(workingday)) %>%
  mutate(weather = as.factor(weather))


my_recipe <- recipe(count ~ ., data=vData) %>%
  step_mutate(weather=ifelse(weather==4,3,weather))%>%
  step_time(datetime, features="hour")
prepped_recipe <- prep(my_recipe)
newData <- bake(prepped_recipe, new_data=vData)



my_mod <- linear_reg() %>%
  set_engine("lm")

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data=vData)

bike_predictions <- predict(bike_workflow,
                            new_data=test)



#### DR HEATON CODE
## Libraries I am going to need


library(tidyverse)
library(tidymodels)
library(vroom)
## Read in the data
bikeTrain <- vroom("./train.csv")
bikeTest <- vroom("./test.csv")
## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, - registered)

## Cleaning & Feature Engineering
bike_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_mutate(hour = factor(hour(datetime), levels=c(0:23), labels=c(0:23))) %>%
  step_poly(temp, degree=2) %>%
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors())%>% #make mean 0, sd=1
  #step_poly(atemp, degree=2) %>%
  step_rm(datetime)
prepped_recipe <- prep(bike_recipe)
bake(prepped_recipe, new_data = bikeTrain) #Make sure recipe work on train
bake(prepped_recipe, new_data = bikeTest) #Make sure recipe works on test

## Define the model
lin_model <- linear_reg() %>%
  set_engine("lm")
## Set up the whole workflow
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data=bikeTrain)
## Look at the fitted LM model this way
extract_fit_engine(bike_workflow) %>%
  summary()
## Get Predictions for test set AND format for Kaggle
test_preds <- predict(bike_workflow, new_data = bikeTest) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write prediction file to CSV
vroom_write(x=test_preds, file="./TestPreds.csv", delim=",") 


##### POISON REGRESSION
library(poissonreg)
## Define the model
pois_model <- poisson_reg() %>%
  set_engine("glm")
## Set up the whole workflow
bike_pois_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(pois_model) %>%
  fit(data=bikeTrain)
## Look at the fitted LM model this way
extract_fit_engine(bike_pois_workflow) %>%
  summary()
## Get Predictions for test set AND format for Kaggle
test_preds <- predict(bike_pois_workflow, new_data = bikeTest) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write prediction file to CSV
vroom_write(x=test_preds, file="./PoisTestPreds.csv", delim=",")

### PENALIZED REGRESSION MODEL + log scale
library(glmnet)
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
  step_mutate(hour = factor(hour(datetime), levels=c(0:23), labels=c(0:23))) %>%
  step_poly(temp, degree=2) %>%
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors())%>% #make mean 0, sd=1
  #step_poly(atemp, degree=2) %>%
  step_rm(datetime)
prepped_recipe <- prep(log_bike_recipe)
bake(prepped_recipe, new_data = logTrainData) #Make sure recipe work on train
bake(prepped_recipe, new_data = bikeTest)

preg_mod <- linear_reg(penalty=tune(),
                       mixture = tune()) %>%
  set_engine("glmnet") %>% set_mode('regression')
preg_wf <- workflow()%>%
  add_recipe(log_bike_recipe) %>%
  add_model(preg_mod) 
 
tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels=10)
folds <- vfold_cv(logTrainData, v=10, repeats = 1)

CV_results <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics = metric_set(rmse))
bestTune <- CV_results %>%
  select_best("rmse")

collect_metrics(CV_results) %>%
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

final_wf <- 
  preg_wf %>%
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
vroom_write(x=test_preds, file="./LogPRegTestPreds.csv", delim=",")




#### LOG COUNT MUTATION ####
logTrainData <- bikeTrain %>%
  mutate(count=log(count))

log_bike_recipe <- recipe(count~., data=logTrainData) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_poly(temp, degree=2) %>%
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors())%>% #make mean 0, sd=1
  #step_poly(atemp, degree=2) %>%
  step_rm(datetime)
prepped_recipe <- prep(log_bike_recipe)
bake(prepped_recipe, new_data = logTrainData) #Make sure recipe work on train
bake(prepped_recipe, new_data = bikeTest) #Make sure recipe works on test

## Define the model
lin_model <- linear_reg() %>%
  set_engine("lm")
## Set up the whole workflow
log_bike_workflow <- workflow() %>%
  add_recipe(log_bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data=logTrainData)

extract_fit_engine(log_bike_workflow) %>%
  summary()
## Get Predictions for test set AND format for Kaggle
test_preds <- predict(log_bike_workflow, new_data = logTrainData) %>%
  mutate(.pred=exp(.pred))%>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write prediction file to CSV
vroom_write(x=test_preds, file="./LogLinTestPreds.csv", delim=",")



##### LOG TRANSFORM POISON REGRESSION
library(poissonreg)
## Define the model
pois_model <- poisson_reg() %>%
  set_engine("glm")
## Set up the whole workflow
bike_pois_workflow <- workflow() %>%
  add_recipe(log_bike_recipe) %>%
  add_model(pois_model) %>%
  fit(data=logTrainData)
## Look at the fitted LM model this way
extract_fit_engine(bike_pois_workflow) %>%
  summary()
## Get Predictions for test set AND format for Kaggle
test_preds <- predict(bike_pois_workflow, new_data = bikeTest) %>%
  mutate(.pred=exp(.pred)) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write prediction file to CSV
vroom_write(x=test_preds, file="./LogPoisTestPreds.csv", delim=",")




