
library(vroom)
library(tidyverse)
library(tidymodels)

vData <- vroom("C:/School/Stat348/KaggleBikeShare/train.csv")

my_recipe <- recipe(count ~ ., data=vData) %>%
  step_mutate(season=as.factor(vData$season)) %>%           #change categorical data to factors
  step_mutate(holiday=as.factor(vData$holiday)) %>%        #change categorical data to factors
  step_mutate(workingday=as.factor(vData$workingday)) %>%
  step_mutate(weather= as.factor(vData$weather)) %>%    #change categorical data to factors
  step_mutate(rental = registered + casual) %>%
  step_time(datetime, features="hour")
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data=baked_Data)
