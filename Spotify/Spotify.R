# Data Exploration STAT 318
# @author Majo del Granado and Julie Ma

library(GGally) 
library(ggplot2)
library(tidyverse)
library(ggExtra)
library(leaps)
library(caret)
library(ModelMetrics) # Load packages
library(raster)
library(car)

# read in data
spotify <- read.csv("/Users/mjdelg/Downloads/Spotify_STAT318.csv")
names(spotify)


#---------TAKE SUBSET OF DATA------#
dim(spotify) # 10% of data is 11400 observations
#set.seed(1234)
set.seed(12192001)
s.sample <- spotify[sample(1:nrow(spotify), 11400), ]
summary(s.sample)
attach(s.sample)
table(spotify$popularity)


#----------CLEAN GENRE---------#
s.sample$clean.genre = NA

s.sample$clean.genre[track_genre == "cantopop"| 
                       track_genre == "mandopop"|
                       track_genre == "j-idol"|
                       track_genre == "j-pop"|
                       track_genre == "k-pop" |
                       track_genre == "pop" |
                       track_genre == "power-pop" |
                       track_genre == "pop-film" |
                       track_genre == "synth-pop" |
                       track_genre == "kids" |
                       track_genre == "disney" |
                       track_genre == "children" |
                       track_genre == "happy" |
                       track_genre == "chill" |
                       track_genre == "sad" |
                       track_genre == "romance" |
                       track_genre == "comedy" |
                       track_genre == "show-tunes" |
                       track_genre == "anime"] <- "pop"

s.sample$clean.genre[track_genre == "acoustic"| 
                       track_genre == "indie-pop"|
                       track_genre == "indie"|
                       track_genre == "singer-songwriter"|
                       track_genre == "songwriter"] <- "indie"

s.sample$clean.genre[track_genre == "afrobeat"| 
                       track_genre == "blues"|
                       track_genre == "funk"|
                       track_genre == "groove"|
                       track_genre == "disco" |
                       track_genre == "jazz" |
                       track_genre == "r-n-b" |
                       track_genre == "hip-hop" |
                       track_genre == "trip-hop" |
                       track_genre == "reggae" |
                       track_genre == "soul" |
                       track_genre == "gospel" |
                       track_genre == "ska"] <- "r&b/rap"

s.sample$clean.genre[track_genre == "chicago-house"| 
                       track_genre == "breakbeat"|
                       track_genre == "club"|
                       track_genre == "dance"|
                       track_genre == "dancehall" |
                       track_genre == "deep-house" |
                       track_genre == "detroit-techno" |
                       track_genre == "edm" |
                       track_genre == "dub" |
                       track_genre == "dubstep" |
                       track_genre == "electro" |
                       track_genre == "electronic" |
                       track_genre == "garage"|
                       track_genre == "house"|
                       track_genre == "j-dance" |
                       track_genre == "new-age" |
                       track_genre == "minimal-techno" |
                       track_genre == "party" |
                       track_genre == "progressive-house" |
                       track_genre == "idm" |
                       track_genre == "drum-and-bass" |
                       track_genre == "hardstyle"|
                       track_genre == "trance" |
                       track_genre == "industrial"] <- "electronic"

s.sample$clean.genre[track_genre == "alt-rock"| 
                       track_genre == "alternative"|
                       track_genre == "black-metal"|
                       track_genre == "death-metal"|
                       track_genre == "emo" |
                       track_genre == "goth" |
                       track_genre == "grindcore" |
                       track_genre == "grunge" |
                       track_genre == "hard-rock" |
                       track_genre == "heavy-metal" |
                       track_genre == "hardcore" |
                       track_genre == "j-rock" |
                       track_genre == "metal"|
                       track_genre == "rock"|
                       track_genre == "metalcore" |
                       track_genre == "techno" |
                       track_genre == "psych-rock" |
                       track_genre == "punk" |
                       track_genre == "rock-n-roll" |
                       track_genre == "punk-rock"] <- "rock/metal"

s.sample$clean.genre[track_genre == "french"| 
                       track_genre == "german"|
                       track_genre == "british"|
                       track_genre == "indian"|
                       track_genre == "iranian" |
                       track_genre == "malay" |
                       track_genre == "turkish" |
                       track_genre == "swedish" |
                       track_genre == "world-music" |
                       track_genre == "brazil" |
                       track_genre == "forro" |
                       track_genre == "spanish" |
                       track_genre == "latin"|
                       track_genre == "latino"|
                       track_genre == "reggaeton" |
                       track_genre == "samba" |
                       track_genre == "salsa" |
                       track_genre == "tango" |
                       track_genre == "pagode" |
                       track_genre == "sertanejo"|
                       track_genre == "mpb"] <- "international"

s.sample$clean.genre[track_genre == "bluegrass"| 
                       track_genre == "country"|
                       track_genre == "folk"|
                       track_genre == "honky-tonk"|
                       track_genre == "rockabilly"] <- "country/folk"

s.sample$clean.genre[track_genre == "ambient"| 
                       track_genre == "opera"|
                       track_genre == "classical"|
                       track_genre == "guitar"|
                       track_genre == "study"|
                       track_genre == "sleep"|
                       track_genre == "piano"] <- "classical/instrumental"
sum(is.na(s.sample$clean.genre))

# check whether genre plays a role in a song's popularity
ggplot(s.sample, aes(x=clean.genre, y=popularity)) +
  geom_boxplot(fill='steelblue')

# ------CLEAN EXPLICIT----#
s.sample[s.sample$explicit == "True"] <-  1
s.sample[s.sample$explicit == "False"] <-  0

#-------VISUALIZE DURATION-----#
ggplot(data = s.sample,
       mapping = aes(x = log(duration_ms), y = popularity, fill = clean.genre)) +
  geom_point(aes(colour = clean.genre)) +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Duration", y = "Popularity") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  facet_wrap(~clean.genre)

#s.sample %>%
#  group_by(duration_ms, clean.genre) %>%
#  summarise(Median = median(popularity)) %>% 
#  ggplot(aes(x = duration_ms, y = Median, color = clean.genre, fill = clean.genre)) + 
#  geom_point() + 
#  geom_smooth(method = "loess", color = "red") +
#  theme_bw() + 
#  theme(legend.title = element_blank()) +
#  labs(x='Tempo', y='Popularity', title = 'Tempo vs Popularity') +
#  facet_wrap(~clean.genre)

#-------VISUALIZE LOUDNESS-----#
s.sample %>%
  group_by(loudness, clean.genre) %>%
  summarise(Median = median(popularity)) %>% 
  ggplot(aes(x = loudness, y = Median, color = clean.genre, fill = clean.genre)) + 
  geom_point() + 
  geom_smooth(color = "red") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  labs(x='Loudness', y='Popularity', title = 'Loudness vs Popularity') +
  facet_wrap(~clean.genre)
#-------VISUALIZE TEMPO-----#
s.sample %>%
  group_by(tempo, clean.genre) %>%
  summarise(Median = median(popularity)) %>% 
  ggplot(aes(x = tempo, y = Median, color = clean.genre, fill = clean.genre)) + 
  geom_point() + 
  geom_smooth(color = "red") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  labs(x='Tempo', y='Popularity', title = 'Tempo vs Popularity') +
  facet_wrap(~clean.genre)

#-------VISUALIZE ENERGY-----#
s.sample %>%
  group_by(energy, clean.genre) %>%
  summarise(Median = median(popularity)) %>% 
  ggplot(aes(x = energy, y = Median, color = clean.genre, fill = clean.genre)) + 
  geom_point() + 
  geom_smooth(color = "red") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  labs(x='Energy', y='Popularity', title = 'Energy vs Popularity') +
  facet_wrap(~clean.genre)

#-------VISUALIZE DANCEABILITY-----#
s.sample %>%
  group_by(danceability, clean.genre) %>%
  summarise(Median = median(popularity)) %>% 
  ggplot(aes(x = log(danceability), y = Median, color = clean.genre, fill = clean.genre)) + 
  geom_point() + 
  geom_smooth(color = "red") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  labs(x='Danceability', y='Popularity', title = 'Danceability vs Popularity') +
  facet_wrap(~clean.genre)

#-------VISUALIZE VALENCE-----#
s.sample %>%
  group_by(valence, clean.genre) %>%
  summarise(Median = median(popularity)) %>% 
  ggplot(aes(x = valence, y = Median, color = clean.genre, fill = clean.genre)) + 
  geom_point() + 
  geom_smooth(color = "red") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  labs(x='Valence', y='Popularity', title = 'Valence vs Popularity') +
  facet_wrap(~clean.genre)

#-------VISUALIZE LIVENESS-----#
s.sample %>%
  group_by(liveness, clean.genre) %>%
  summarise(Median = median(popularity)) %>% 
  ggplot(aes(x = log(liveness), y = Median, color = clean.genre, fill = clean.genre)) + 
  geom_point() + 
  geom_smooth(color = "red") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  labs(x='Liveness', y='Popularity', title = 'Liveness vs Popularity') +
  facet_wrap(~clean.genre)

#-------VISUALIZE ACOUSTICNESS-----#
s.sample %>%
  group_by(acousticness, clean.genre) %>%
  summarise(Median = median(popularity)) %>% 
  ggplot(aes(x = log(acousticness), y = Median, color = clean.genre, fill = clean.genre)) + 
  geom_point() + 
  geom_smooth(color = "red") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  labs(x='Acousticness', y='Popularity', title = 'Acousticness vs Popularity') +
  facet_wrap(~clean.genre)

#--------VISUALIZE INSTRUMENTALNESS--------#

s.sample %>%
  group_by(instrumentalness, clean.genre) %>%
  summarise(Median = median(popularity)) %>% 
  ggplot(aes(x = log(instrumentalness), y = Median, color = clean.genre, fill = clean.genre)) + 
  geom_point() + 
  geom_smooth(color = "red") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  labs(x='Intrumentalness', y='Popularity', title = 'Instrumentalness vs Popularity') +
  facet_wrap(~clean.genre)

#--------VISUALIZE SPEECHINESS--------#
s.sample %>%
  group_by(speechiness, clean.genre) %>%
  summarise(Median = median(popularity)) %>% 
  ggplot(aes(x = log(speechiness), y = Median, color = clean.genre, fill = clean.genre)) + 
  geom_point() + 
  geom_smooth(color = "red") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  labs(x='Speechiness', y='Popularity', title = 'Speechiness vs Popularity') +
  facet_wrap(~clean.genre)

#-------------EXPLICIT-----------#
ggplot(s.sample, aes(x=explicit, y=popularity)) +
  geom_boxplot(fill='steelblue') # doesn't seem to add much, don't include in model
summary(mode)

#-----OVERALL CORRELATION----#
names(s.sample)
cor(s.sample[,c(6:7,9:10,12, 13:18)]) # correlation matrix

ggcorr(data = s.sample[,c(6:7,9:10,12, 14:18)],
       method = c("pairwise.complete.obs", "pearson"),
       label = TRUE, label_size = 4)

#-----------------------------POP-------------------------------------#
# subset of only pop songs
pop <- s.sample[which(s.sample$clean.genre == "pop"),] 

cor(pop[,c(6:7,9:10,12, 14:18)]) # correlation matrix
ggcorr(data = pop[,c(6:7,9:10,12, 14:18)],
       label = TRUE, label_size = 4)

#---------------------BIC Model----------------------#
pop.results <- regsubsets(popularity~valence + energy + danceability + loudness 
                           + speechiness+ acousticness + liveness + tempo + mode
                           + duration_ms + explicit + key 
                           + instrumentalness, method = "exhaustive", data = pop)
BICs <- summary(pop.results)$bic
id_BIC = which.min(BICs)
summary(pop.results)$which[id_BIC,]

pop.BIC <- lm(popularity~valence + danceability + loudness + speechiness
                  + duration_ms, data=pop)

#---------------------R^2 Model----------------------#
X <- cbind(pop$valence,pop$energy, pop$danceability, pop$loudness, pop$speechiness, pop$acousticness,
           pop$liveness, pop$tempo, pop$duration_ms, pop$mode)

results1=leaps(x=X, y= pop$popularity, int=TRUE, method=c('r2'))
id_r2=which.max(results1$r2) #r squared
results1$which[id_r2, ]

pop.r2 <- lm(popularity~valence + energy + loudness + danceability + acousticness + speechiness
             + duration_ms + liveness + tempo + acousticness*energy, data=pop)

#---------------------Adj. R^2 Model----------------------#
results2=leaps(x=X, y= pop$popularity, int=TRUE, method=c('adjr2')) #adj. r squared
id_adjr2=which.max(results2$adjr2)
results2$which[id_adjr2, ]

pop.adj <- lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness
                 + liveness + duration_ms, data=pop)

#---------------------Mallow's Cp Model----------------------#

results3=leaps(x=X, y= pop$popularity, int=TRUE, method=c('Cp')) # cp
id_Cp=which.min(results3$Cp)
results3$which[id_Cp, ]

pop.cp <- lm(popularity~valence + danceability + loudness + speechiness 
             + liveness + duration_ms, data=pop)

#---------------------Cross Validation Pop---------------------#

k = 5 # number of folds
folds = createFolds(pop$popularity, k = k)
# Initialize vectors to store the results
results.bic = c()
results.r2 = c()
results.adj = c()
results.cp = c()

for(i in 1:k){
  # Split the data into training and test sets
  train = pop[-folds[[i]],]
  validate = pop[folds[[i]],]
  
  # Train the model on training data
  bic.mod = lm(popularity~valence + danceability + loudness + speechiness
               + duration_ms, data=train)
  r2.mod = lm(popularity~valence + energy + loudness + danceability + acousticness + speechiness
              + duration_ms + liveness + tempo + duration_ms + acousticness*energy, data=train)
  adj.mod = lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness
                  + liveness + duration_ms, data=train)
  cp.mod = lm(popularity~valence + danceability + loudness + speechiness 
              + liveness + duration_ms, data=train)
  
  # Make predictions using validation data
  predictions.bic = predict(bic.mod, validate)
  predictions.r2 = predict(r2.mod, newdata = validate)
  predictions.adj = predict(adj.mod, data = validate)
  predictions.cp = predict(cp.mod, data = validate)
  
  # Calculate accuracy
  accuracy.bic = rmse(pop$popularity[folds[[i]]], predictions.bic)
  accuracy.r2 = rmse(pop$popularity[folds[[i]]], predictions.r2)
  accuracy.adj = rmse(pop$popularity[folds[[i]]], predictions.adj)
  accuracy.cp = rmse(pop$popularity[folds[[i]]], predictions.cp)
  
  # Store results
  results.bic = c(results.bic, accuracy.bic)
  results.r2 = c(results.r2, accuracy.r2)
  results.adj = c(results.adj, accuracy.adj)
  results.cp = c(results.cp, accuracy.cp)
}

mean(results.bic)
mean(results.r2) # best model is R^2 
mean(results.adj)
mean(results.cp)

summary(pop.r2)
# Check Model Diagnostics
# check normality
e = resid(pop.r2)
qqnorm(e, pch=19)
qqline(e,col = "purple")

# check equal variance
y_hat = fitted(pop.r2)
plot(e~y_hat, ylab='Residuals', xlab= 'Predicted Popularity', 
     pch=19, cex = 0.5, main = "Variance of Residuals")
abline(h=0, col='red')

vif(pop.r2)

#----------------------------INDIE----------------------------#
# subset of only indie songs
indie <- s.sample[which(s.sample$clean.genre == "indie"),]

cor(indie[,c(6:7,9:10,12, 14:19)]) # correlation matrix
ggcorr(data = indie[,c(6:7,9:10,12, 14:19)],
       label = TRUE, label_size = 4)
names(indie)

#---------------------BIC Model----------------------#
indie.results <- regsubsets(popularity~valence + energy + danceability + loudness 
                            + speechiness+ acousticness + liveness + tempo + mode
                            + duration_ms + explicit + key, method = "exhaustive", data = indie)
BICs <- summary(indie.results)$bic
id_BIC = which.min(BICs)
summary(indie.results)$which[id_BIC,]

indie.bic <- lm(popularity~valence + danceability + valence*danceability, data = indie)
#---------------------R^2 Model----------------------#
X <- cbind(indie$valence,indie$energy, indie$danceability, indie$loudness, indie$speechiness, indie$acousticness,
           indie$liveness, indie$tempo, indie$duration_ms, indie$mode)

results1=leaps(x=X, y= indie$popularity, int=TRUE, method=c('r2'))
id_r2=which.max(results1$r2) #r squared
results1$which[id_r2, ]

indie.r2 <- lm(popularity~valence + energy + danceability + loudness + speechiness + liveness
               + tempo + duration_ms + mode, data = indie)

#---------------------Adj. R^2 Model----------------------#
results2=leaps(x=X, y= indie$popularity, int=TRUE, method=c('adjr2')) #adj. r squared
id_adjr2=which.max(results2$adjr2)
results2$which[id_adjr2, ]

indie.adj <- lm(popularity~valence + energy + danceability + loudness + acousticness 
                + mode, data = indie)

#---------------------Mallow's Cp Model----------------------#

results3=leaps(x=X, y= indie$popularity, int=TRUE, method=c('Cp')) # cp
id_Cp=which.min(results3$Cp)
results3$which[id_Cp, ]

indie.cp <- lm(popularity~valence + danceability + acousticness, data = indie)

#----------------------CROSS VALIDATION-------------------#

k = 5 # number of folds
folds = createFolds(indie$popularity, k = k)
# Initialize vectors to store the results
results.bic = c()
results.r2 = c()
results.adj = c()
results.cp = c()

for(i in 1:k){
  # Split the data into training and test sets
  train = indie[-folds[[i]],]
  validate = indie[folds[[i]],]
  
  # Train the model on training data
  bic.mod = lm(popularity~valence + danceability + valence*danceability, data=train)
  r2.mod = lm(popularity~valence + energy + danceability + loudness + speechiness + liveness
              + tempo + duration_ms + mode, data=train)
  adj.mod = lm(popularity~valence + energy + danceability + loudness + acousticness 
               + mode, data=train)
  cp.mod = lm(popularity~valence + danceability + acousticness, data=train)
  
  # Make predictions using validation data
  predictions.bic = predict(bic.mod, validate)
  predictions.r2 = predict(r2.mod, newdata = validate)
  predictions.adj = predict(adj.mod, data = validate)
  predictions.cp = predict(cp.mod, data = validate)
  
  # Calculate accuracy
  accuracy.bic = rmse(indie$popularity[folds[[i]]], predictions.bic)
  accuracy.r2 = rmse(indie$popularity[folds[[i]]], predictions.r2)
  accuracy.adj = rmse(indie$popularity[folds[[i]]], predictions.adj)
  accuracy.cp = rmse(indie$popularity[folds[[i]]], predictions.cp)
  
  # Store results
  results.bic = c(results.bic, accuracy.bic)
  results.r2 = c(results.r2, accuracy.r2)
  results.adj = c(results.adj, accuracy.adj)
  results.cp = c(results.cp, accuracy.cp)
}

mean(results.bic) # best model is BIC
mean(results.r2)
mean(results.adj) 
mean(results.cp)

vif(indie.bic)
range(predict(indie.bic))
summary(indie.bic)
# Model Diagnostics
e = resid(indie.bic)
qqnorm(e, pch=19)
qqline(e,col = "purple")
y_hat = fitted(indie.bic)

plot(e~y_hat, ylab='Residuals', xlab= 'Predicted Popularity', 
     pch=19, cex = 0.5, main = "Variance of Residuals")
abline(h=0, col='red')



#-----------------RNB/RAP---------------#
# subset of only rap/r&b songs
rnb.rap <- s.sample[which(s.sample$clean.genre == "r&b/rap"),]

cor(rnb.rap[,c(6:7,9:10,12, 14:18)]) # correlation matrix
ggcorr(data = rnb.rap[,c(6:7,9:10,12, 14:18)],
       label = TRUE, label_size = 4)

#----------------BIC Model--------------------------#
rap.results <- regsubsets(popularity~valence + energy + danceability + loudness 
                            + speechiness+ acousticness + liveness + tempo + mode
                            + duration_ms + explicit + key, method = "exhaustive", data = rnb.rap)
BICs <- summary(rap.results)$bic
id_BIC = which.min(BICs)
summary(rap.results)$which[id_BIC,]
rap.bic <- lm(popularity~valence + energy + speechiness + mode + duration_ms, data = rnb.rap)

#---------------------R^2 Model----------------------#
X <- cbind(rnb.rap$valence,rnb.rap$energy, rnb.rap$danceability, rnb.rap$loudness, rnb.rap$speechiness, rnb.rap$acousticness,
           rnb.rap$liveness, rnb.rap$tempo, rnb.rap$duration_ms, rnb.rap$mode)

results1=leaps(x=X, y= rnb.rap$popularity, int=TRUE, method=c('r2'))
id_r2=which.max(results1$r2) #r squared
results1$which[id_r2, ]


rap.r2 <- lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness
               + tempo + duration_ms + mode, data = rnb.rap)

#---------------------Adj. R^2 Model----------------------#
results2=leaps(x=X, y= rnb.rap$popularity, int=TRUE, method=c('adjr2')) #adj. r squared
id_adjr2=which.max(results2$adjr2)
results2$which[id_adjr2, ]

rap.adj <- lm(popularity~valence + energy + speechiness + acousticness 
                + duration_ms + mode, data = rnb.rap)

#---------------------Mallow's Cp Model----------------------#

results3=leaps(x=X, y= rnb.rap$popularity, int=TRUE, method=c('Cp')) # cp
id_Cp=which.min(results3$Cp)
results3$which[id_Cp, ]
rap.cp <- lm(popularity~valence + energy + speechiness + duration_ms + mode, data = rnb.rap)

#----------------------CROSS VALIDATION-------------------#

k = 5 # number of folds
folds = createFolds(rnb.rap$popularity, k = k)
# Initialize vectors to store the results
results.bic = c()
results.r2 = c()
results.adj = c()
results.cp = c()

for(i in 1:k){
  # Split the data into training and test sets
  train = rnb.rap[-folds[[i]],]
  validate = rnb.rap[folds[[i]],]
  
  # Train the model on training data
  bic.mod = lm(popularity~valence + energy + speechiness + mode 
               + duration_ms, data=train)
  r2.mod = lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness
              + tempo + duration_ms + mode, data=train)
  adj.mod = lm(popularity~valence + energy + speechiness + acousticness 
               + duration_ms + mode, data=train)
  cp.mod = lm(popularity~valence + energy + speechiness + duration_ms + mode, data=train)
  
  # Make predictions using validation data
  predictions.bic = predict(bic.mod, validate)
  predictions.r2 = predict(r2.mod, newdata = validate)
  predictions.adj = predict(adj.mod, data = validate)
  predictions.cp = predict(cp.mod, data = validate)
  
  # Calculate accuracy
  accuracy.bic = rmse(rnb.rap$popularity[folds[[i]]], predictions.bic)
  accuracy.r2 = rmse(rnb.rap$popularity[folds[[i]]], predictions.r2)
  accuracy.adj = rmse(rnb.rap$popularity[folds[[i]]], predictions.adj)
  accuracy.cp = rmse(rnb.rap$popularity[folds[[i]]], predictions.cp)
  
  # Store results
  results.bic = c(results.bic, accuracy.bic)
  results.r2 = c(results.r2, accuracy.r2)
  results.adj = c(results.adj, accuracy.adj)
  results.cp = c(results.cp, accuracy.cp)
}

mean(results.bic) # best model is BIC
mean(results.r2)
mean(results.adj) 
mean(results.cp)

vif(rap.bic)
range(predict(rap.bic))
summary(rap.bic)
# check normality
e = resid(rap.bic)
qqnorm(e, pch=19)
qqline(e,col = "purple")
# check equal variance
y_hat = fitted(rap.bic)
plot(e~y_hat, ylab='Residuals', xlab= 'Predicted Popularity', 
     pch=19, cex = 0.5, main = "Variance of Residuals")
abline(h=0, col='red')


#-------ELECTRONIC---------#

# subset of only electronic songs
elec <- s.sample[which(s.sample$clean.genre == "electronic"),] 

cor(elec[,c(6:7,9:10,12, 14:18)]) # correlation matrix
ggcorr(data = elec[,c(6:7,9:10,12, 14:18)],
       label = TRUE, label_size = 4)

#-----------------BIC Model-----------------------#
elec.results <- regsubsets(popularity~valence + energy + danceability + loudness 
                          + speechiness+ acousticness + liveness + tempo + mode
                          + duration_ms + explicit + key 
                          + instrumentalness, method = "exhaustive", data = elec)

BICs <- summary(elec.results)$bic
id_BIC = which.min(BICs)
summary(elec.results)$which[id_BIC,]

elec.bic <- lm(popularity~valence + energy + loudness + speechiness + duration_ms,data = elec)

#---------------------R^2 Model----------------------#
X <- cbind(elec$valence, elec$energy, elec$danceability, elec$loudness, elec$speechiness, elec$acousticness,
           elec$liveness, elec$tempo, elec$duration_ms, elec$mode)

results1=leaps(x=X, y= elec$popularity, int=TRUE, method=c('r2'))
id_r2=which.max(results1$r2) #r squared
results1$which[id_r2, ]


elec.r2 <- lm(popularity~valence + energy + danceability + loudness + speechiness 
              + acousticness + liveness
             + tempo + duration_ms + mode + acousticness*energy, data = elec)

#---------------------Adj. R^2 Model----------------------#
results2=leaps(x=X, y= elec$popularity, int=TRUE, method=c('adjr2')) #adj. r squared
id_adjr2=which.max(results2$adjr2)
results2$which[id_adjr2, ]

elec.adj <- lm(popularity~valence + energy + loudness + speechiness + liveness
              + duration_ms, data = elec)

#---------------------Mallow's Cp Model----------------------#

results3=leaps(x=X, y= elec$popularity, int=TRUE, method=c('Cp'))
id_Cp=which.min(results3$Cp)
results3$which[id_Cp, ]
# Cp model is same as Adj. R^2 model

#----------------------CROSS VALIDATION-------------------#

k = 5 # number of folds
folds = createFolds(elec$popularity, k = k)
# Initialize vectors to store the results
results.bic = c()
results.r2 = c()
results.adj = c()
results.cp = c()

for(i in 1:k){
  # Split the data into training and test sets
  train = elec[-folds[[i]],]
  validate = elec[folds[[i]],]
  
  # Train the model on training data
  bic.mod = lm(popularity~valence + energy + speechiness + mode + duration_ms, data=train)
  r2.mod = lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness
              + tempo + duration_ms + mode + acousticness*energy, data=train)
  adj.mod = lm(popularity~valence + energy + speechiness + acousticness 
               + duration_ms + mode, data=train)
  cp.mod = lm(popularity~valence + energy + speechiness + duration_ms + mode, data=train)
  
  # Make predictions using validation data
  predictions.bic = predict(bic.mod, validate)
  predictions.r2 = predict(r2.mod, validate)
  predictions.adj = predict(adj.mod, validate)
  predictions.cp = predict(cp.mod, validate)
  
  # Calculate accuracy
  accuracy.bic = rmse(elec$popularity[folds[[i]]], predictions.bic)
  accuracy.r2 = rmse(elec$popularity[folds[[i]]], predictions.r2)
  accuracy.adj = rmse(elec$popularity[folds[[i]]], predictions.adj)
  accuracy.cp = rmse(elec$popularity[folds[[i]]], predictions.cp)
  
  # Store results
  results.bic = c(results.bic, accuracy.bic)
  results.r2 = c(results.r2, accuracy.r2)
  results.adj = c(results.adj, accuracy.adj)
  results.cp = c(results.cp, accuracy.cp)
}

mean(results.bic)
mean(results.r2)  # best model is r^2
mean(results.adj) 
mean(results.cp)
vif(elec.r2)
range(predict(elec.r2))

summary(elec.r2)
# Model Diagnostics
# check normality
e = resid(elec.r2)
qqnorm(e, pch=19)
qqline(e,col = "purple")

# check equal variance
y_hat = fitted(elec.r2)
plot(e~y_hat, ylab='Residuals', xlab= 'Predicted Popularity', 
     pch=19, cex = 0.5, main = "Variance of Residuals")
abline(h=0, col='red')

coefficients(elec.r2)


#-------ROCK/METAL---------#
# subset of only rock/metal songs
r.metal <- s.sample[which(s.sample$clean.genre == "rock/metal"),]

cor(r.metal[,c(6:7,9:10,12, 14:18)]) # correlation matrix
ggcorr(data = r.metal[,c(6:7,9:10,12, 14:18)],
       label = TRUE, label_size = 4)

#-----------------BIC Model----------------#
rock.results <- regsubsets(popularity~valence + energy + danceability + loudness 
                           + speechiness+ acousticness + liveness + tempo + mode
                           + duration_ms + explicit + key 
                           + instrumentalness, method = "exhaustive", data = r.metal)
BICs <- summary(rock.results)$bic
id_BIC = which.min(BICs)
summary(rock.results)$which[id_BIC,]

rock.bic <- lm(popularity~valence + danceability + duration_ms + instrumentalness, data = r.metal)

#---------------------R^2 Model----------------------#
X <- cbind(r.metal$valence, r.metal$energy, r.metal$danceability, r.metal$loudness, r.metal$speechiness, r.metal$acousticness,
           r.metal$liveness, r.metal$tempo, r.metal$duration_ms, r.metal$mode)

results1=leaps(x=X, y= r.metal$popularity, int=TRUE, method=c('r2'))
id_r2=which.max(results1$r2) #r squared
results1$which[id_r2, ]


rock.r2 <- lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness
              + tempo + duration_ms + mode, data = r.metal)

#---------------------Adj. R^2 Model----------------------#
results2=leaps(x=X, y= r.metal$popularity, int=TRUE, method=c('adjr2')) #adj. r squared
id_adjr2=which.max(results2$adjr2)
results2$which[id_adjr2, ]

rock.adj <- lm(popularity~valence + energy + loudness + speechiness + liveness
               + duration_ms, data = r.metal)

#---------------------Mallow's Cp Model----------------------#
results3=leaps(x=X, y= r.metal$popularity, int=TRUE, method=c('Cp'))
id_Cp=which.min(results3$Cp)
results3$which[id_Cp, ]
rock.cp <- lm(popularity~valence + energy + danceability + loudness + acousticness
              + duration_ms, data = r.metal)

#----------------------CROSS VALIDATION-------------------#

k = 5 # number of folds
folds = createFolds(r.metal$popularity, k = k)
# Initialize vectors to store the results
results.bic = c()
results.r2 = c()
results.adj = c()
results.cp = c()

for(i in 1:k){
  # Split the data into training and test sets
  train = r.metal[-folds[[i]],]
  validate = r.metal[folds[[i]],]
  
  # Train the model on training data
  bic.mod = lm(popularity~valence + danceability + duration_ms + instrumentalness, data=train)
  r2.mod = lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness
              + tempo + duration_ms + mode, data=train)
  adj.mod = lm(popularity~valence + energy + loudness + speechiness + liveness
               + duration_ms, data=train)
  cp.mod = lm(popularity~valence + energy + danceability + loudness + acousticness
              + duration_ms, data=train)
  
  # Make predictions using validation data
  predictions.bic = predict(bic.mod, validate)
  predictions.r2 = predict(r2.mod, validate)
  predictions.adj = predict(adj.mod, validate)
  predictions.cp = predict(cp.mod, validate)
  
  # Calculate accuracy
  accuracy.bic = rmse(r.metal$popularity[folds[[i]]], predictions.bic)
  accuracy.r2 = rmse(r.metal$popularity[folds[[i]]], predictions.r2)
  accuracy.adj = rmse(r.metal$popularity[folds[[i]]], predictions.adj)
  accuracy.cp = rmse(r.metal$popularity[folds[[i]]], predictions.cp)
  
  # Store results
  results.bic = c(results.bic, accuracy.bic)
  results.r2 = c(results.r2, accuracy.r2)
  results.adj = c(results.adj, accuracy.adj)
  results.cp = c(results.cp, accuracy.cp)
}

mean(results.bic) # best model is BIC
mean(results.r2)
mean(results.adj) 
mean(results.cp)

range(predict(rock.bic))
summary(rock.bic)
vif(rock.bic)
# Model Diagnostics
# check normality
e = resid(rock.bic)
qqnorm(e, pch=19)
qqline(e,col = "purple")

# check equal variance
y_hat = fitted(rock.bic)
plot(e~y_hat, ylab='Residuals', xlab= 'Predicted Popularity', 
     pch=19, cex = 0.5, main = "Variance of Residuals")
abline(h=0, col='red')

#----------------------COUNTRY/FOLK------------------#
# subset of only country/folk songs
c.folk <- s.sample[which(s.sample$clean.genre == "country/folk"),]

cor(c.folk[,c(6:7,9:10,12, 14:18)]) # correlation matrix
ggcorr(data = c.folk[,c(6:7,9:10,12, 14:18)],
       label = TRUE, label_size = 4)

#---------------------BIC Model-----------------------------#
country.results <- regsubsets(popularity~valence + energy + danceability + loudness 
                           + speechiness+ acousticness + liveness + tempo + mode
                           + duration_ms + explicit + key 
                           + instrumentalness, method = "exhaustive", data = c.folk)
BICs <- summary(country.results)$bic
id_BIC = which.min(BICs)
summary(country.results)$which[id_BIC,]

country.bic <- lm(popularity~speechiness, data = c.folk)
#---------------------R^2 Model----------------------#
X <- cbind(c.folk$valence, c.folk$energy, c.folk$danceability, c.folk$loudness, c.folk$speechiness, c.folk$acousticness,
           c.folk$liveness, c.folk$tempo, c.folk$duration_ms, c.folk$mode)

results1=leaps(x=X, y= c.folk$popularity, int=TRUE, method=c('r2'))
id_r2=which.max(results1$r2) #r squared
results1$which[id_r2, ]


country.r2 <- lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness
              + tempo + duration_ms + mode, data = c.folk)

#---------------------Adj. R^2 Model----------------------#
results2=leaps(x=X, y= c.folk$popularity, int=TRUE, method=c('adjr2')) #adj. r squared
id_adjr2=which.max(results2$adjr2)
results2$which[id_adjr2, ]

country.adj <- lm(popularity~valence + energy + loudness + speechiness + acousticness + liveness + tempo
               + duration_ms + mode, data = c.folk)

#---------------------Mallow's Cp Model----------------------#
results3=leaps(x=X, y= c.folk$popularity, int=TRUE, method=c('Cp'))
id_Cp=which.min(results3$Cp)
results3$which[id_Cp, ]
country.cp <- lm(popularity~valence + speechiness + tempo
              + mode, data = c.folk)

#----------------------CROSS VALIDATION-------------------#
k = 5 # number of folds
folds = createFolds(c.folk$popularity, k = k)
# Initialize vectors to store the results
results.bic = c()
results.r2 = c()
results.adj = c()
results.cp = c()

for(i in 1:k){
  # Split the data into training and test sets
  train = c.folk[-folds[[i]],]
  validate = c.folk[folds[[i]],]
  
  # Train the model on training data
  bic.mod = lm(popularity~speechiness, data=train)
  r2.mod = lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness
              + tempo + duration_ms + mode, data=train)
  adj.mod = lm(popularity~valence + energy + loudness + speechiness + acousticness + liveness + tempo
               + duration_ms + mode, data=train)
  cp.mod = lm(popularity~valence + speechiness + tempo + mode, data=train)
  
  # Make predictions using validation data
  predictions.bic = predict(bic.mod, validate)
  predictions.r2 = predict(r2.mod, validate)
  predictions.adj = predict(adj.mod, validate)
  predictions.cp = predict(cp.mod, validate)
  
  # Calculate accuracy
  accuracy.bic = rmse(c.folk$popularity[folds[[i]]], predictions.bic)
  accuracy.r2 = rmse(c.folk$popularity[folds[[i]]], predictions.r2)
  accuracy.adj = rmse(c.folk$popularity[folds[[i]]], predictions.adj)
  accuracy.cp = rmse(c.folk$popularity[folds[[i]]], predictions.cp)
  
  # Store results
  results.bic = c(results.bic, accuracy.bic)
  results.r2 = c(results.r2, accuracy.r2)
  results.adj = c(results.adj, accuracy.adj)
  results.cp = c(results.cp, accuracy.cp)
}

mean(results.bic) 
mean(results.r2)
mean(results.adj) 
mean(results.cp) # best model is Mallow's CP

range(predict(country.cp))
summary(country.cp)
# Check Model Diagnostics
# check normality
e = resid(country.cp)
qqnorm(e, pch=19)
qqline(e,col = "purple")

# check equal variance
y_hat = fitted(country.cp)
plot(e~y_hat, ylab='Residuals', xlab= 'Predicted Popularity', 
     pch=19, cex = 0.5, main = "Variance of Residuals")
abline(h=0, col='red')


#------------------CLASSICAL/INSTRUMENTAL--------------------#
# subset of only classical/instrumental songs
class.ins <- s.sample[which(s.sample$clean.genre == "classical/instrumental"),]


cor(class.ins[,c(6:7,9:10,12, 14:18)]) # correlation matrix
ggcorr(data = class.ins[,c(6:7,9:10,12, 14:18)],
       label = TRUE, label_size = 4)

# Variable Screening
ins.results <- regsubsets(popularity~valence + energy + danceability + loudness 
                              + speechiness+ acousticness + liveness + tempo + mode
                              + duration_ms + explicit + key 
                              + instrumentalness, method = "exhaustive", data = class.ins)
BICs <- summary(ins.results)$bic
id_BIC = which.min(BICs)
summary(ins.results)$which[id_BIC,]

ins.bic <- lm(popularity~loudness + acousticness, data = class.ins)

#---------------------R^2 Model----------------------#
X <- cbind(class.ins$valence, class.ins$energy, class.ins$danceability, class.ins$loudness, class.ins$speechiness, 
           class.ins$acousticness, class.ins$liveness, class.ins$tempo, class.ins$duration_ms, class.ins$mode)

results1=leaps(x=X, y= class.ins$popularity, int=TRUE, method=c('r2'))
id_r2=which.max(results1$r2) #r squared
results1$which[id_r2, ]

ins.r2 <- lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness
                 + tempo + duration_ms + mode, data = class.ins)

#---------------------Adj. R^2 Model----------------------#
results2=leaps(x=X, y= class.ins$popularity, int=TRUE, method=c('adjr2')) #adj. r squared
id_adjr2=which.max(results2$adjr2)
results2$which[id_adjr2, ]

ins.adj <- lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness, 
              data = class.ins)

#---------------------Mallow's Cp Model----------------------#
results3=leaps(x=X, y= class.ins$popularity, int=TRUE, method=c('Cp'))
id_Cp=which.min(results3$Cp)
results3$which[id_Cp, ]
# same model as adjusted r-squared 

#----------------------CROSS VALIDATION-------------------#
k = 5 # number of foldsx
folds = createFolds(class.ins$popularity, k = k)
# Initialize vectors to store the results
results.bic = c()
results.r2 = c()
results.adj = c()
results.cp = c()

for(i in 1:k){
  # Split the data into training and test sets
  train = class.ins[-folds[[i]],]
  validate = class.ins[folds[[i]],]
  
  # Train the model on training data
  bic.mod = lm(popularity~loudness + acousticness, data=train)
  r2.mod = lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness
              + tempo + duration_ms + mode, data=train)
  adj.mod = lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness,
               data=train)
  cp.mod = lm(popularity~valence + energy + danceability + loudness 
              + speechiness + acousticness + liveness, 
              data=train)
  
  # Make predictions using validation data
  predictions.bic = predict(bic.mod, validate)
  predictions.r2 = predict(r2.mod, validate)
  predictions.adj = predict(adj.mod, validate)
  predictions.cp = predict(cp.mod, validate)
  
  # Calculate accuracy
  accuracy.bic = rmse(class.ins$popularity[folds[[i]]], predictions.bic)
  accuracy.r2 = rmse(class.ins$popularity[folds[[i]]], predictions.r2)
  accuracy.adj = rmse(class.ins$popularity[folds[[i]]], predictions.adj)
  accuracy.cp = rmse(class.ins$popularity[folds[[i]]], predictions.cp)
  
  # Store results
  results.bic = c(results.bic, accuracy.bic)
  results.r2 = c(results.r2, accuracy.r2)
  results.adj = c(results.adj, accuracy.adj)
  results.cp = c(results.cp, accuracy.cp)
}

mean(results.bic) 
mean(results.r2)
mean(results.adj) 
mean(results.cp) # adjusted r-squared and cp are best model

range(predict(ins.adj))
hist(class.ins$popularity)
summary(ins.adj)
#-------------Model Diagnostics----------#
# Check Model Diagnostics
# check normality
e = resid(ins.adj)
qqnorm(e, pch=19)
qqline(e,col = "purple")

# check equal variance
y_hat = fitted(ins.adj)
plot(e~y_hat, ylab='Residuals', xlab= 'Predicted Popularity', 
     pch=19, cex = 0.5, main = "Variance of Residuals")
abline(h=0, col='red')

#---------------------INTERNATIONAL----------------------#
# subset of only international songs
inter <- s.sample[which(s.sample$clean.genre == "international"),]

cor(inter[,c(6:7,9:10,12, 14:18)]) # correlation matrix
ggcorr(data = inter[,c(6:7,9:10,12, 14:18)],
       label = TRUE, label_size = 4)

#--------------------BIC Model---------------------#
inter.results <- regsubsets(popularity~valence + energy + danceability + loudness 
                          + speechiness+ acousticness + liveness + tempo + mode
                          + duration_ms + explicit + key 
                          + instrumentalness, method = "exhaustive", data = inter)
BICs <- summary(inter.results)$bic
id_BIC = which.min(BICs)
summary(inter.results)$which[id_BIC,]

inter.bic <- lm(popularity~valence + loudness + liveness + instrumentalness, data=inter)

#---------------------R^2 Model----------------------#
X <- cbind(inter$valence, inter$energy, inter$danceability, inter$loudness, inter$speechiness, 
           inter$acousticness, inter$liveness, inter$tempo, inter$duration_ms, inter$mode)

results1=leaps(x=X, y= inter$popularity, int=TRUE, method=c('r2'))
id_r2=which.max(results1$r2) #r squared
results1$which[id_r2, ]

inter.r2 <- lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness
             + tempo + duration_ms + mode, data = inter)

#---------------------Adj. R^2 Model----------------------#
results2=leaps(x=X, y= inter$popularity, int=TRUE, method=c('adjr2')) #adj. r squared
id_adjr2=which.max(results2$adjr2)
results2$which[id_adjr2, ]

inter.adj <- lm(popularity~energy + danceability + loudness + speechiness + liveness + tempo + duration_ms
              + mode, data = inter)

#---------------------Mallow's Cp Model----------------------#
results3=leaps(x=X, y= inter$popularity, int=TRUE, method=c('Cp'))
id_Cp=which.min(results3$Cp)
results3$which[id_Cp, ]

inter.cp <- lm(popularity~energy + danceability + loudness + liveness + tempo + duration_ms
                + mode, data = inter)

#----------------------CROSS VALIDATION-------------------#
k = 5 # number of folds
folds = createFolds(inter$popularity, k = k)
# Initialize vectors to store the results
results.bic = c()
results.r2 = c()
results.adj = c()
results.cp = c()

for(i in 1:k){
  # Split the data into training and test sets
  train = inter[-folds[[i]],]
  validate = inter[folds[[i]],]
  
  # Train the model on training data
  bic.mod = lm(popularity~ valence + loudness + liveness + instrumentalness, data=train)
  r2.mod = lm(popularity~valence + energy + danceability + loudness + speechiness + acousticness + liveness
              + tempo + duration_ms + mode, data=train)
  adj.mod = lm(popularity~energy + danceability + loudness + speechiness + liveness + tempo + duration_ms
               + mode, data=train)
  cp.mod = lm(popularity~energy + danceability + loudness + liveness + tempo + duration_ms
              + mode, data=train)

  # Make predictions using validation data
  predictions.bic = predict(bic.mod, validate)
  predictions.r2 = predict(r2.mod, validate)
  predictions.adj = predict(adj.mod, validate)
  predictions.cp = predict(cp.mod, validate)
  
  # Calculate accuracy
  accuracy.bic = rmse(inter$popularity[folds[[i]]], predictions.bic)
  accuracy.r2 = rmse(inter$popularity[folds[[i]]], predictions.r2)
  accuracy.adj = rmse(inter$popularity[folds[[i]]], predictions.adj)
  accuracy.cp = rmse(inter$popularity[folds[[i]]], predictions.cp)
  
  # Store results
  results.bic = c(results.bic, accuracy.bic)
  results.r2 = c(results.r2, accuracy.r2)
  results.adj = c(results.adj, accuracy.adj)
  results.cp = c(results.cp, accuracy.cp)
}

mean(results.bic) # bic is best model
mean(results.r2)
mean(results.adj) 
mean(results.cp) 

range(predict(inter.bic))
summary(inter.bic)
#-------------Model Diagnostics----------#
# check normality
hist(e)
e = resid(inter.bic)
qqnorm(e, pch=19)
qqline(e,col = "purple")

# check equal variance
y_hat = fitted(inter.bic)
plot(e~y_hat, ylab='Residuals', xlab= 'Predicted Popularity', 
     pch=19, cex = 0.5, main = "Variance of Residuals")
abline(h=0, col='red')


#------------------LOOK AT CERTAIN SONGS-------------#
# Despacito, actual popularity = 80
predict(inter.bic, newdata = data.frame(valence = 0.839, loudness = -4.787, liveness = 0.067, 
                                        instrumentalness= 0), interval = "prediction")

# Sedated by Hozier, actual popularity = 55
predict(indie.bic, newdata = data.frame(valence = 0.29, danceability = 0.66), interval ="confidence" ) 

# DNA by BTS, actual popularity = 73
predict(pop.r2, newdata = data.frame(valence = 0.696, energy = 0.775, loudness = -4.018, 
                                     danceability = 0.599, acousticness = 0.0217,
                                     speechiness = 0.0543, duration_ms = 223122, 
                                     liveness = 0.0678, tempo = 129.817), interval = "confidence")

# Madira by Multani, actual = 33
predict(elec.r2, newdata = data.frame(valence = 0.0747, energy = 0.575, loudness = -11.764, 
                                     danceability = 0.542, acousticness = 0.129,
                                     speechiness = 0.0808, duration_ms = 79830, 
                                     liveness = 0.272, tempo = 170.135, mode = 1), interval = "confidence")


predict(rap.bic, newdata = data.frame(valence = 0.363, energy = 0.728, 
                                      speechiness = 0.1, duration_ms = 107750, 
                                      mode = 1), interval = "confidence")

# take my name by parmalee
predict(country.cp, newdata = data.frame(valence = 0.574, tempo = 163.957, 
                                         speechiness = 0.0512, 
                                         mode = 1), interval = "confidence")

# Robbers by The 1975
predict(rock.bic, newdata = data.frame(valence = 0.381, danceability = 0.621, 
                                         duration_ms = 254509, 
                                         instrumentalness = 1.75E-05), interval = "confidence")

# Le petit by Mozart 
predict(ins.adj, newdata = data.frame(valence = 0.229, energy = 0.0626, 
                                       danceability = 0.177, loudness = -21.204, speechiness =0.0387, acousticness =0.978,  
                                       liveness = 0.388), interval = "predict")
summary(pop.r2)
summary(elec.r2)
summary(rap.bic)
summary(inter.bic)
summary(country.cp)
summary(rock.bic)
summary(ins.adj)
summary(indie.bic)
