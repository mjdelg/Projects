# STAT 260 Final project
# Anna, Alex, Mary Jo, Jenny

# ============ Setup: Read in data and packages ============ #

# Data set source: https://bechdeltest.com/
# Data set GitHub repository: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-09/readme.md

library(tidyverse)
library(ggplot2)

movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

# ============ Explore the data set ============ #
head(movies)
colnames(movies)
dim(movies) # 1794 rows, 34 columns (1794 movies total)
tail(movies)

# movies range from 1970 - 2013
table(movies$year)

# Table of Bechdel test results for all movies
table(movies$test)
# A cleaner version of the table above, with all "dubious" movies in one category
# "dubious" = The submitter considered the Bechdel rating dubious
table(movies$clean_test)

# Overall Bechdel results
# Note that "dubious" bechdel ratings are counted as a FAIL
table(movies$binary)

# ============ Clean the data set ============ #

movies <- as.data.frame(movies)

# convert dollar values to numbers
movies$domgross_2013 <- as.numeric(movies$domgross_2013)
movies$intgross_2013 <- as.numeric(movies$intgross_2013)


# ============ Impute missing values ============ #

# check for movies whose financial information is missing
movies$title[is.na(movies$domgross_2013)] # 18 NAs
movies$title[is.na(movies$intgross_2013)] # 11 NAs
movies$title[is.na(movies$budget_2013)] # no NAs

# Impute missing data for intgross_2013
# We will handle only international gross (not domestic gross) in our analysis 

# Fill in values that we found on https://www.the-numbers.com/
movies$intgross_2013[movies$title == "The Frozen Ground"] <- 5617460
movies$intgross_2013[movies$title == "Veronika Decides to Die"] <- 2243
movies$intgross_2013[movies$title == "My Beautiful Laundrette"] <- 9442
movies$intgross_2013[movies$title == "Day of the Dead" & movies$year == 1985] <- 34004262

# re-check for remaining NAs 
# (expect 7 values that we could not find)
movies$title[is.na(movies$intgross_2013)]

# impute based on budget values of other movies
# Use R squared value to justify the correlation between budget and gross
# Create linear model to impute missing gross values

# use log version for imputation (it has a higher R^2)
budget_v_gross_log <- lm(log(intgross_2013)~log(budget_2013), data=movies)
summary(budget_v_gross_log)
plot(log(movies$budget_2013), log(movies$intgross_2013))
abline(budget_v_gross_log)

# get residual standard deviation
resid_std_dev <- summary(budget_v_gross_log)$sigma
resid_std_dev

# row numbers with missing gross
missing_rows <- which(is.na(movies$intgross_2013))

# impute based on linear model prediction
for (row_num in missing_rows) {
	intgross_pred <- predict(budget_v_gross_log, new=movies[row_num,])
	noise <- rnorm(1, 0, resid_std_dev) # add noise based on residual variance
	movies[row_num,]$intgross_2013 <- exp(intgross_pred) + noise
}

# re-check for remaining NAs (expect 0)
movies$title[is.na(movies$intgross_2013)]

# missing imdb_rating
sum(is.na(movies$imdb_rating))

# since we are using imdb in a regression with other variables, 
# we can create a new variable for imdb missingness indicator
# and use it together with mean imputation.
movies$imdb_miss <- 0
movies$imdb_miss[is.na(movies$imdb_rating)]<- 1
table(movies$imdb_miss)

# impute mean imbd rating for missing values
movies$imdb_rating[movies$imdb_miss == 1] <- mean(movies$imdb_rating, na.rm=TRUE)
summary(movies$imdb_rating)

## ====== create pass/fail/dubious variable =====##

movies$clean_binary <-rep("fail",length(movies$binary))
movies$clean_binary[movies$clean_test == "dubious"] <- "dubious"
movies$clean_binary[movies$binary == "PASS"] <- "pass"

# ============ Create new variables ============ #

# Create a variable for net revenue (in 2013 dollars)
movies$net_2013 <- movies$intgross_2013 - movies$budget_2013
summary(movies$net_2013)

without_dubious$net_2013 <- without_dubious$intgross_2013 - without_dubious$budget_2013
summary(without_dubious$net_2013)

# Create a variable for % profit (profit as a percent of the cost)
movies$pct_profit <- movies$net_2013 / movies$budget_2013
summary(movies$pct_profit)

without_dubious$pct_profit <- without_dubious$net_2013 / without_dubious$budget_2013
summary(without_dubious$pct_profit)
 
# Check cleaning with Shrek example
shrek_net <- movies$intgross_2013[movies$title=='Shrek'] - movies$budget_2013[movies$title=='Shrek']

paste("Expected:", as.character(shrek_net))
paste("Got:", as.character(movies$net_2013[movies$title=='Shrek']))

paste("Expected:", as.character(shrek_net / movies$budget_2013[movies$title=='Shrek']))
paste("Got:", as.character(movies$pct_profit[movies$title=='Shrek']))

## ===== Create new variable for Language- English/or not ======== ##
missing.language <- which(is.na(without_dubious$language))

#replace all NAs with missing
for (row in missing.language){
  without_dubious$language[row] <- "Missing"
}

without_dubious$english <-rep("English",length(without_dubious$language))
without_dubious$english[without_dubious$language %in% c("Swedish",
                                                        "Spanish, Chinese, Wolof",
                                                        "Arabic, Hebrew",
                                                        "Norwegian",
                                                        "Hindi",
                                                        "Mandarin",
                                                        "Spanish, Japanese",
                                                        "Spanish",
                                                        "Hindi, Sanskrit",
                                                        "Cantonese, Japanese, Mandarin",
                                                        "Cantonese",
                                                        "Thai",
                                                        "Cantonese, Mandarin",
                                                        "Japanese")] <-"Not English"

without_dubious$english[without_dubious$language %in% c("N/A","Missing")] <-"Missing"

## ===== Create new variable for Country- USA/or not ======== ##
movies$country <- as.character(movies$country)
missing.country <- which(is.na(movies$country))

#replace all NAs with missing - 199 missing
for (row in missing.country){
  movies$country[row] <- "Missing"
}

movies$USA <-rep("USA",length(movies$country))
movies$USA[movies$country %in% c("Australia", "Cameroon, UK","Canada",
"Canada, Brazil, Japan","Canada, India","Canada, Spain", "Chile, Spain",
"China, South Korea, Hong Kong", "Denmark, Sweden, France, Germany",
"Finland, Sweden","France", "France, Belgium, Ireland", 
"France, Germany, Italy, Canada", "France, Hong Kong, Ireland, Spain, UK",
 "France, UK","France, UK, Germany", "Germany","Germany, Israel",
 "Germany, Russia, UK","Germany, UK","Hong Kong, China","India",
 "Ireland", "Ireland, UK, Germany, Italy, Spain, France, Belgium, Switzerland",
 "Japan","Mexico, Spain", "New Zealand, Germany","Norway",
 "South Korea","Spain", "Spain, Germany, UK, Lithuania", "Thailand","UK","UK, Australia",
  "UK, Australia, France","UK, Canada", "UK, Canada, France",
  "UK, Canada, Sweden", "UK, Czech Republic, France, Italy",
  "UK, France","UK, Ireland", "UK, Japan","UK, Luxembourg",
   "UK, Netherlands","UK, Spain", "West Germany")] <-"Not USA"

movies$USA[movies$country %in% c("N/A","Missing")] <-"Missing"

table(movies$USA)

# ============ Explore financial information ============ #

# summarize international gross for passing vs. failing movies
# EXCLUDES DUBIOUS
summary(movies$intgross_2013[movies$clean_binary == "pass"])
summary(movies$intgross_2013[movies$clean_binary == "fail"])

# summarize percent profit for passing vs. failing movies
summary(movies$pct_profit[movies$clean_binary == "pass"])
summary(movies$pct_profit[movies$clean_binary == "fail"])

# summarize budget for passing vs. failing movies
summary(movies$budget_2013[movies$clean_binary == "pass"])
summary(movies$budget_2013[movies$clean_binary == "fail"])

# ============ Financial visualizations ============ #
ggplot(data=movies, mapping = aes(x=clean_test, y=log(intgross_2013), color=clean_binary)) + geom_boxplot() + labs(title="Gross income by Bechdel test result", x="Bechdel test criteria satisfied", y="Gross income (2013 dollars, log scale)", color="Binary Bechdel result") + ylim(12,22) + scale_color_manual(values=c("#999999", "firebrick1", "limegreen"))
  
ggplot(data=movies, mapping = aes(x=clean_binary, y=log(budget_2013), color=clean_binary)) + geom_boxplot() + labs(title="Budget by Bechdel test result", x="Bechdel test result", y="Budget (2013 dollars, log scale)", color="Binary Bechdel result") + ylim(12, 20) + scale_color_manual(values=c("#999999", "firebrick1", "limegreen"))
  
ggplot(data=movies, mapping = aes(x=clean_binary, y=pct_profit, color=clean_binary)) + geom_boxplot() + labs(title="Percent profit by Bechdel Test Result", x="Bechdel test result", y="Percent profit", color="Binary Bechdel result")+ ylim(0,25) + scale_color_manual(values=c("#999999", "firebrick1", "limegreen"))

## ====== Create numeric version of Bechdel test result ===== ##

# create subset without dubious
# excludes 142 dubious movies
without_dubious <- movies[movies$clean_test!="dubious",]

#replace categorical variables with numeric values
without_dubious$result<-rep(0,length(without_dubious$clean_test))
without_dubious$result[without_dubious$clean_test == "notalk"] <- 1
without_dubious$result[without_dubious$clean_test == "men"] <- 2
without_dubious$result[without_dubious$clean_test == "ok"] <- 3

# 0 tests passed = nowomen = FAIL
# 1 test passed = notalk = FAIL
# 2 tests passed = men = FAIL
# 3 tests passed = ok = PASS

## ============== Data Modeling ================ ##
set.seed(1)

## Simple Linear Regression Models
colnames(without_dubious) # all column names

# relationship between gross and bechdel
# will be influenced by budget
anova_gross <- aov(intgross_2013 ~ result, data= without_dubious)
summary(anova_gross)

# relationship between percent profit and bechdel
anova_profit <- aov(pct_profit ~ result, data= without_dubious)
summary(anova_profit)
# p-value = 0.285, indicating no significant difference in means between 
# movies that pass more Bechdel test criteria

anova_binary <- aov(pct_profit ~ binary, data= without_dubious)
summary(anova_binary)
summary(lm(pct_profit ~ binary, data= without_dubious))
# p-value = 0.157, indicating no significant difference in means between 
# movies that pass / fail

# relationship between budget and bechdel
anova_budget <- aov(budget_2013 ~ result, data= without_dubious)
summary(anova_budget)
# p-value < 0.0001, indicating that the Bechdel test result
# significantly correlates with a film's budget.

summary(lm(budget_2013 ~ result, data=without_dubious))
# slope is -8,042,703. 
# This indicates the trend that a film's budget decreases by 8,042,703
# for each additional Bechdel test criteria that it passes.

summary(aov(budget_2013 ~ binary, data=without_dubious))

summary(lm(imdb_rating ~ result, data=without_dubious))
summary(lm(imdb_rating ~ binary, data=without_dubious))
# p-value for both is < 0.0001, indicating that 
# movies that pass the Bechdel test are rated significantly
# lower than movies that do not pass. 
boxplot(imdb_rating ~ result, data=without_dubious)
boxplot(imdb_rating ~ binary, data=without_dubious)
# current version of write-up does not include imdb rating but we could add it

# ======== Models for Bechdel test ========== #
colnames(without_dubious) #all column names

# any model with imdb_rating must also have imdb_miss 
model1 <- lm(log(budget) ~ result + imdb_rating + imdb_miss + year + USA, data= without_dubious)
summary(model1)
plot(model1$fitted.values, model1$resid)
points(model1$fitted.values, rep(0,length(without_dubious$result)), pch=16, col='red', cex=.5)

model2 <- lm(log(budget)~ result + imdb_rating + imdb_miss + year, data= without_dubious)
summary(model2)
plot(model2$fitted.values, model2$resid)
points(model2$fitted.values, rep(0,length(without_dubious$result)), pch=16, col='red', cex=.5)

model3 <- lm(log(budget) ~ result + imdb_rating + imdb_miss, data= without_dubious)
summary(model3)
plot(model3$fitted.values, model3$resid)
points(model3$fitted.values, rep(0,length(without_dubious$result)), pch=16, col='red', cex=.5)

## Choose the Best Models
#R^2 variables
# model 1: 0.1173, model 2: 0.07749, model 3: 0.02233

## 1) Adjusted R^2
# model 1: 0.1146, model 2: 0.07525, model 3: 0.02055

## 2) Compare models' BIC
BIC(model1) #5727.326
BIC(model2) #5792.849
BIC(model3) #5881.377
## 3) anova
anova(model1, model2) # 
anova(model1, model3) # 
anova(model2, model3) # 
# model 1 performed best!