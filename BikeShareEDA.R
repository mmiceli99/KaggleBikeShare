library(vroom)
library(tidyverse)
library(patchwork)
data <- read.csv("C:/School/Stat348/KaggleBikeShare/test.csv")


vData <- vroom("C:/School/Stat348/KaggleBikeShare/train.csv")
vData$season <- as.factor(vData$season)           #change categorical data to factors
vData$holiday <- as.factor(vData$holiday)         #change categorical data to factors
vData$workingday <- as.factor(vData$workingday)   #change categorical data to factors
vData$weather <- as.factor(vData$weather)         #change categorical data to factors

plot_intro <- DataExplorer::plot_intro(vData)
plot_bar <- DataExplorer::plot_bar(vData)
plot_hist <- DataExplorer::plot_histogram(vData)
plot_missing <- DataExplorer::plot_missing(vData)

plot_intro + plot_bar

temp_atemp <- ggplot(vData, aes(x=temp, y=atemp)) +
   geom_point()


date_temp <- ggplot(vData, aes(x=vData$datetime, y=temp)) +
  geom_point() +
  geom_smooth()

box_humid <- ggplot(vData, aes(x=humidity, y=season))+
  geom_boxplot()

weather_bar <- ggplot(vData, aes(x=weather)) +
  geom_bar()

(temp_atemp + date_temp) / (box_humid + weather_bar)

ggplot(vData, aes(x=registered, y=casual)) +
  geom_point()

vData <- mutate(vData, rental = casual+registered)

ggplot(vData, aes(x=rental, y=count))+
  geom_point()
