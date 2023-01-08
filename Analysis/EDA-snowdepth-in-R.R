# Will There be a white Christmas this year in my hometown, Sault Ste. Marie, Michigan?
#Located at the top of the Upper Peninsula, there is often snowfall on the ground for some six months out of the year. But not every year. So can I predict if we'll have a white Christmas this year?
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)

snow <- read.csv("C:/Users/Marcy/Documents/GitHub/Will There Be a White Christmas this Year in Sault, Michigan/DATA/Sault-Weather.csv")
head(snow)
#Is there a significant difference in snowdepth in the first ten years of this dataset and the last ten years of this dataset? It begins Jan. 1, 1948 and continues through to Nov. 28, 2022. 
colnames(snow)
nrow(snow)

#Are there any outliers in this dataset?
boxplot(snow$SnowDepth)

#How many NA values are there?
sum(is.na(snow))
#There are 22,783 NAs mostly in the AvgTemp column. That's an easy column to delete, as I can calculate those myself.
snowX <- select (snow, -c(AvgTemp))
head(snowX)
sum(is.na(snowX))
summary(snowX)

#I am curious if there's been a difference in snowdepth recently, versus at the start of data collection. 
#To look at this, I'll subset the first ten years of data, and the last ten

first10years <- snow$SnowDepth[1:4019]
last10years <- snow$SnowDepth[23343:27361]
head(first10years)
tail(last10years)

#run a dependent t-test 
t.obj <- t.test(first10years, last10years, paired = TRUE)
t.obj

# My null hypothesis would be that there is not a significant difference between the first ten years of this dataset, and the last ten.
# The alternate hypothesis, as you might guess, is that there is a significant difference. The 
#A paired t test for two samples is the best way to see if there's a connection. A depepndent, or paired t-test will tell if the means of the two samples are different. 

# The second line of the output of this t.obj verifies the alternate hypothesis that the two groups are not equal to each other. (In other words, global warming is actually a thing).
#The first line gives us the p value, which is < 2.2e-16 which is less than .05. Because it is less than .05, you should reject the null hypothesis! There is a significant difference in temperatures between the first twenty five years and the last twenty-five years.
#graphing the dependent t test data begins with reshaping it using the melt() function. 

mean(first10years)
str(first10years)
mean(snow$SnowDepth)

#The function geom_vline() plots vertical lines on the graph, and they are plotted at values of the xintercept= argument. You can pull these directly from the t.test() function by providing the object name followed by the name of the information in the t.test() output. conf.int[1] is the lower confidence level, conf.int[2] is the upper confidence level, and null.value is the mean.

d <- ggplot(snow, aes(x=SnowDepth)) 
d + geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = t.obj$conf.int[1], color= "red") +
  geom_vline(xintercept = t.obj$conf.int[2], color = "red") +
  geom_vline(xintercept = t.obj$null.value, color="green")

#I can graph the difference between the two groups, as well
diff <- first10years - last10years

df <- data.frame(diff)
ggplot (df, aes(x=diff)) + geom_histogram(binwidth = 1)+
  xlab("Difference between first 10 and last 10")

#Is there a normal distribution for snowfall? (no, there isn't)
ggplot(snow, aes(sample = SnowDepth)) + geom_qq()

#compare boxplots for the two groups:
boxplot(first10years ~ last10years)

library(reshape2)
#ss <- melt(snow, measure.vars = c("first10years", "last10years"))

ggplot(snow) + geom_boxplot(aes(x= , y = last10years)) 


#heatmap of temperatures 
#ggplot(data=snow$SnowDepth, aes(x=Date,y=SnowDepth)) + 
  geom_tile(aes(fill = AvgTemp),colour = "white") + 
  scale_fill_gradientn(colours=rev(brewer.pal(10,'Spectral'))) + 
  theme(legend.title=element_blank(),axis.title.y=element_blank(),
        axis.title.x=element_blank(),legend.position="top") + 
    ggtitle("Temperature (daily average) in the Soo")


#Backward Elimination - Stepwise Regression to see which subset of variables can best predict if there will be fresh snowfall, or precipitation on that day. 
#Of course, this is the Great White North - there's snow on the ground some six months out of
#the year - but will there be a blanket of fresh white snow for Christmas morning? 
# Bit of wrangling first - eliminating that date column for this next bit.
Snowlm <- select (snow, -c(Date, AvgTemp))
head(Snowlm)
FitAll <- lm(Snowfall ~ ., data = Snowlm ) #Snowfall "explained by everything else" in the Snowlm dataset.
step(FitAll, direction = "backward") #everything is in the model to start with, one eliminated at a time.

summary(FitAll)
#The lower this AIC, the better, in terms of relative quality of the models. AIC starts at -5393.7 and it reaches its best chances for prediction if Precip is removed.Doesn't mean it's great - just could be better - still inconclusive yet.
#But this does make sense, for if it's raining, it usually won't be snowing - but this is Northern Michigan. Both can happen in the same day, depending on the temperature in the clouds. So the model that excludes Precipitation is the best, because it has the smallest AIC. 
#The coefficient of MaxTemp is (-0.009764). What that means is as long as everything else is equal, an increase of the maximum temperature by 1 degree will lead to an estimated decrease in the Snowfall by about .009 inches. 
#The coefficient of Precip is 1.197. Everything else being equal, as the precipitation increases, so does the Snowfall. So what that means is if there's clouds in the sky, either can come down on a Northern Michigan winter's day. 

#Hybrid Stepwise - starting with no predictors, then build an ideal model.
fitstart = lm(Snowfall ~ 1, data = Snowlm)
summary(fitstart)

step(fitstart, direction = "both", scope = formula(FitAll))
# https://www.kaggle.com/code/jonathanbouchet/new-delhi-20-years-of-weather-data
