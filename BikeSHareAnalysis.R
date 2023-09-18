
library(vroom)
library(tidyverse)
library(tidymodels)

test <- vroom("C:/School/Stat348/KaggleBikeShare/test.csv")
vData <- vroom("C:/School/Stat348/KaggleBikeShare/train.csv")
vData <- vData%>%
  select(-casual, -registered)
my_recipe <- recipe(count ~ ., data=vData) %>%
  step_num2factor(season) %>%           
  step_num2factor(holiday) %>%        
  step_num2factor(workingday) %>%
  step_num2factor(weather)%>%
  step_mutate(weather=ifelse(weather==4,3,weather))%>%
  step_time(datetime, features="hour")
prepped_recipe <- prep(my_recipe) %>%
  juice()
newData <- bake(prepped_recipe, new_data=vData)



my_mod <- linear_reg() %>%
  set_engine("lm")

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data=vData)

bike_predictions <- predict(bike_workflow,
                            new_data=test)

