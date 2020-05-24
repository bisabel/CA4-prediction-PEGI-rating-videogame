#
# load PEGI descriptor dataset
#
pegiDataFrame <- read.csv("source/pegi_all_to2019.csv", 
                          sep= "|", 
                          header = TRUE, 
                          strip.white=TRUE, 
                          encoding = "UTF-8",
                          quote=""
)

# data corrections
# videogames from the dataset that descriptors dont match
# with the descriptor in pegi.info

# Example wrongly assigned sex tag
pegiDataFrame$Sex[pegiDataFrame$Title == "Wer weiß denn sowas? - Das Spiel"] <- "N"

# examples wronly assigned violence tag
#pegiDataFrame$Violence[pegiDataFrame$Title == "Team Sonic Racing"] <- "N"
#pegiDataFrame$Violence[pegiDataFrame$Title == "Nickelodeon Kart Racers"] <- "N"
#pegiDataFrame$Violence[pegiDataFrame$Title == "ACA NEOGEO THRASH RALLY"] <- "N"
#pegiDataFrame$Violence[pegiDataFrame$Title == "Carnival Games"] <- "N"
#pegiDataFrame$Violence[pegiDataFrame$Title == "Mutant Mudds Collection"] <- "N"
#pegiDataFrame$Violence[pegiDataFrame$Title == "Scintillatron 4096"] <- "N"
#pegiDataFrame$Rating[pegiDataFrame$Title == "Zombie Virus"] <- "12"

# remove some columns
colnames(pegiDataFrame)
colnames(pegiDataFrame)[5] <- "Release"
pegiDataFrame[1:2] <- list(NULL)
pegiDataFrame$Platform <- NULL
pegiDataFrame$Descriptor <- NULL
pegiDataFrame$Consumer.Advice <- NULL
pegiDataFrame$Discrimination <- NULL

# some videogames has one record per platform
# so need to redux them
pegiDataFrame <- unique(pegiDataFrame)
summary(pegiDataFrame$Rating)


#
# function for print info about the ratings
#
printratinginfo <- function(df){
  nrow(df)
  print(paste("  3 Ratings count: ",nrow(df[df$Rating == "3",])))
  print(paste("  7 Ratings count: ",nrow(df[df$Rating == "7",])))
  print(paste(" 12 Ratings count: ",nrow(df[df$Rating == "12",])))
  print(paste(" 16 Ratings count: ",nrow(df[df$Rating == "16",])))
  print(paste(" 18 Ratings count: ",nrow(df[df$Rating == "18",])))
}

printdescriptorinfo <- function(df){
  nrow(df)
  print(paste("  Violence in ", 100 * nrow(df[df$Violence   == "Y",])/nrow(df)))
  print(paste("  Drugs in ", 100 * nrow(df[df$Drugs   == "Y",])/nrow(df)))
  print(paste("  Sex in ", 100 * nrow(df[df$Sex   == "Y",])/nrow(df)))
  print(paste("  Fear in ", 100 * nrow(df[df$Fear   == "Y",])/nrow(df)))
  print(paste("  Gambling in ", 100 * nrow(df[df$Gambling   == "Y",])/nrow(df)))
}


#
# 1 Previous analysis of the variable that are going to ue in the regresion
#

#cheack if some descriptor is identity of one rating
printdescriptorinfo(pegiDataFrame[pegiDataFrame$Rating == "3",])
printdescriptorinfo(pegiDataFrame[pegiDataFrame$Rating == "7",])
printdescriptorinfo(pegiDataFrame[pegiDataFrame$Rating == "12",])
printdescriptorinfo(pegiDataFrame[pegiDataFrame$Rating == "16",])
printdescriptorinfo(pegiDataFrame[pegiDataFrame$Rating == "18",])

# amount of release of each ratings
amountOfEachRating <- c(nrow(pegiDataFrame[pegiDataFrame$Rating == "3",]),
                        nrow(pegiDataFrame[pegiDataFrame$Rating == "7",]),
                        nrow(pegiDataFrame[pegiDataFrame$Rating == "12",]),
                        nrow(pegiDataFrame[pegiDataFrame$Rating == "16",]),
                        nrow(pegiDataFrame[pegiDataFrame$Rating == "18",])
)
amountOfEachRating

# vector with each rating label id
ratingLabels <- c("Pegi 3", "Pegi 7", "Pegi 12", "Pegi 16", "Pegi 18")
percentageRatings <- round(amountOfEachRating/sum(amountOfEachRating)*100)
percentageLabels <- paste(ratingLabels," - ", percentageRatings) # add percents to labels
percentageLabels <- paste(percentageLabels,"%",sep="") # ad % to labels

# vector with colors used in each rating label.
# Different pegi rating use the same color.
ratingColors <- c("#99ca3b","#99ca3b","#f7a100","#f7a100","#e2011a")

?pie
# pie with percentages of videogames by PEGI Ratings
pie(amountOfEachRating, 
    labels = percentageLabels, 
    main="Videogames by PEGI Ratings",
    radius = 1.5,
    col = ratingColors
)


# the half of the release has the pegi label violent
nrow(pegiDataFrame[pegiDataFrame$Violence == "Y",]) / nrow(pegiDataFrame)

# So then graph the percentage of the ratings in videogames tag with violence
slicesviolent <- c(nrow(pegiDataFrame[pegiDataFrame$Rating == "3" & pegiDataFrame$Violence == "Y",]),
            nrow(pegiDataFrame[pegiDataFrame$Rating == "7" & pegiDataFrame$Violence == "Y",]),
            nrow(pegiDataFrame[pegiDataFrame$Rating == "12" & pegiDataFrame$Violence == "Y",]),
            nrow(pegiDataFrame[pegiDataFrame$Rating == "16" & pegiDataFrame$Violence == "Y",]),
            nrow(pegiDataFrame[pegiDataFrame$Rating == "18" & pegiDataFrame$Violence == "Y",])
)

# Pie Chart with Percentages
percentageRatings <- round(slicesviolent/sum(slicesviolent)*100)
percentageLabels <- paste(ratingLabels," - ", percentageRatings) # add percents to labels
percentageLabels <- paste(percentageLabels,"%",sep="") # ad % to labels
pie(slicesviolent,
    labels = percentageLabels, 
    main="Videogames by PEGI Ratings",
    radius = 2,
    col=ratingColors
    ) 


# the pegi 3 represent almost the half of the release
# need to be analyce for understand how is tag
nrow(pegiDataFrame[pegiDataFrame$Rating == "3" &
                   pegiDataFrame$Violence == "N" &
                   pegiDataFrame$Drugs == "N" &
                   pegiDataFrame$Sex == "N" &
                   pegiDataFrame$Fear == "N" &
                   pegiDataFrame$Gambling == "N" &
                   pegiDataFrame$Discrimination == "N"
                   ,]) / nrow(pegiDataFrame[pegiDataFrame$Rating == "3",])
# the 99% percentage of the pegi 3 havent got any descriptor

# on the other hand, 
# few videogames with none descriptor and distint rating pegi 3
nrow(pegiDataFrame[pegiDataFrame$Rating != "3" &
                     pegiDataFrame$Violence == "N" &
                     pegiDataFrame$Drugs == "N" &
                     pegiDataFrame$Sex == "N" &
                     pegiDataFrame$Fear == "N" &
                     pegiDataFrame$Gambling == "N" &
                     pegiDataFrame$Discrimination == "N"
                   ,])


# distribution
library(ggplot2)
str(pegiDataFrame)
pegiDataFrame$Rating <- as.factor(pegiDataFrame$Rating)


# chart Fear
pFear <- ggplot(pegiDataFrame, 
       aes(x = Rating, 
           fill = Fear)) + 
  geom_bar(position = "stack") +
  labs(y = "Releases", 
       fill = "Fear",
       x = "PEGI Ratings",
       title = "Releases with Fear descriptor") 

# chart Drugs
pDrugs <- ggplot(pegiDataFrame, 
       aes(x = Rating, 
           fill = Drugs)) + 
  geom_bar(position = "stack") +
  labs(y = "Releases", 
       fill = "Drugs",
       x = "PEGI Ratings",
       title = "Releases with Drugs descriptor") 

# chart BadLanguage
pBadLanguage <- ggplot(pegiDataFrame, 
       aes(x = Rating, 
           fill = BadLanguage)) + 
  geom_bar(position = "stack") +
  labs(y = "Releases", 
       fill = "BadLanguage",
       x = "PEGI Ratings",
       title = "Releases with BadLanguage descriptor") 

# chart Sex
pSex <- ggplot(pegiDataFrame, 
       aes(x = Rating, 
           fill = Sex)) + 
  geom_bar(position = "stack") +
  labs(y = "Releases", 
     fill = "Sex ",
     x = "PEGI Ratings",
     title = "Releases with Sex descriptor") 

library(cowplot)
plot_grid(pFear, pDrugs, pBadLanguage, pSex , labels = "AUTO")



#
# 2 Predictive model
#

#
# have the column factor ratings orderer
# preparing the training data and test data
pegiDataFrame$Rating <- as.factor(pegiDataFrame$Rating)
#set seed always in 100
set.seed(100)
#training data set will be 70% of the pegi data frame
trainingRows <- sample(1:nrow(pegiDataFrame), 0.7 * nrow(pegiDataFrame))
trainingData <- pegiDataFrame[trainingRows, ]
testData <- pegiDataFrame[-trainingRows, ]
#str(trainingData)

#print info about training and test dataset
printratinginfo(trainingData)
printratinginfo(testData)

#
# building the model
library(MASS)
polrMod <- polr(Rating ~ Violence + Drugs + BadLanguage + Sex + Fear + Gambling, data=trainingData)
summary(polrMod)


# make predictions with the model using the testing set
predictedRatings <- predict(polrMod, testData)
testData$predicted <- predictedRatings
head(predictedRatings)

# predict the probabilites
predictedRatingsProbabilities <- predict(polrMod, testData, type="p")  
head(predictedRatingsProbabilities)

## Confusion matrix and misclassification error
t <- table(testData$Rating, testData$predicted)
t
mean(as.character(testData$Rating) != as.character(testData$predicted))  
# misclassification error
printratinginfo(testData)

barplot(t, main="1th Model predictions Distribution",
        xlab="PEGI Ratings",
        ylab="Numbers of predictions",
        col=ratingColors,
        beside=TRUE)

# analysis of data test prediction
# try firgure out why model made mistakes

# why model did predict ani pegi 7
nrow(testData[testData$Rating == "7", ])
testData[testData$predicted == "7" & testData$Rating == "7", ]
reviewwtest <- testData[testData$predicted == "7" & testData$Rating == "7", ]

# wrong model predict for pegi 18
nrow(testData[testData$Rating == "18", ])
testData[testData$Rating == "18" & testData$predicted == "18", ]
reviewwtest <- testData[testData$Rating == "18" & testData$predicted != "18" , ]

reviewwtest <- pegiDataFrame[pegiDataFrame$Rating == "18" &
              pegiDataFrame$Gambling == "Y",]



#
#
# second model, only videogames plus 2008
#

# have the column factor ratings orderer
# preparing the training data and test data
pegiDataFrame$Release <- as.character(pegiDataFrame$Release.Date)
pegiDataFrame$Release <- substr(pegiDataFrame$Release, 0, 4)
pegiDataFrame$Release <- as.numeric(pegiDataFrame$Release)
#add weights columns
pegiDataFramePlus2009 <- pegiDataFrame[pegiDataFrame$Release > 2008,]
pegiDataFramePlus2009 <- pegiDataFramePlus2009[pegiDataFramePlus2009$Release <= 2017,]
pegiDataFramePlus2009$weight <- 1 

pegiDataFramePlus2009$weight[pegiDataFramePlus2009$Fear == "Y"] <- 5
pegiDataFramePlus2009$weight[pegiDataFramePlus2009$Sex == "Y"] <- 2
pegiDataFramePlus2009$weight[pegiDataFramePlus2009$Drugs == "Y"] <- 2
#pegiDataFramePlus2009$weight[pegiDataFramePlus2009$BadLanguage == "Y"] <- 3
#pegiDataFramePlus2009$weight[pegiDataFramePlus2009$Rating == "3" &
#                             pegiDataFramePlus2009$Violence == "N" &
#                             pegiDataFramePlus2009$Drugs == "N" &
#                             pegiDataFramePlus2009$Sex == "N" &
#                             pegiDataFramePlus2009$Fear == "N" &
#                             pegiDataFramePlus2009$Gambling == "N" &
#                             pegiDataFramePlus2009$Discrimination == "N"] <- 2

set.seed(100)
trainingRows <- sample(1:nrow(pegiDataFramePlus2009), 0.7 * nrow(pegiDataFramePlus2009))
trainingData <- pegiDataFramePlus2009[trainingRows, ]
testData <- pegiDataFramePlus2009[-trainingRows, ]
str(trainingData)

#print info about training and test dataset
printratinginfo(trainingData)
printratinginfo(testData)

#
# building the model
library(MASS)
?polr
polrMod <- polr(Rating ~ Violence + Drugs + BadLanguage + Sex + Fear + Gambling, weights = weight,  data=trainingData)
summary(polrMod)


#test with the testing set
### Predict
predictedRatings <- predict(polrMod, testData) 
testData$predicted <- predictedRatings
head(predictedRatings)

predictedRatingsProbabilities <- predict(polrMod, testData, type="p")  # predict the probabilites
head(predictedRatingsProbabilities)

## Confusion matrix and misclassification error

t <- table(testData$Rating, predictedRatings)
t
mean(as.character(testData$Rating) != as.character(predictedRatings))  
# misclassification error
printratinginfo(testData)

counts <- table(testData$Rating)
counts
counts <- table(t[,1])
barplot(t[,2:5], main="2sd model prediction Distribution",
        xlab="PEGI ratings",
        ylab="Numbers of predictions",
        col=ratingColors,
        beside=TRUE)


# analysis of data test prediction
# try firgure out why model made mistakes

# wrong model predict for pegi 18
nrow(testData[testData$Rating == "18", ])
testData[testData$Rating == "18" & testData$predicted == "18", ]
reviewwtest <- testData[testData$Rating == "18" & testData$predicted != "18" , ]

reviewwtest <- testData[testData$Rating != "3" &
                          testData$predicted == "3",]

reviewwtest <- pegiDataFrame[pegiDataFrame$Rating == "18" &
                               pegiDataFrame$Gambling == "Y",]

nrow(pegiDataFrame)
reviewwtest <- pegiDataFrame[pegiDataFrame$Drugs == "Y",]
printratinginfo(reviewwtest)
reviewwtest <- testData[testData$Drugs == "Y",]
reviewwtest$Rating <- reviewwtest$predicted
printratinginfo(reviewwtest)

reviewwtest <- pegiDataFrame[pegiDataFrame$Fear == "Y",]
printratinginfo(reviewwtest)
reviewwtest <- testData[testData$Fear == "Y",]
reviewwtest$Rating <- reviewwtest$predicted
printratinginfo(reviewwtest)

reviewwtest <- pegiDataFrame[pegiDataFrame$BadLanguage == "Y",]
printratinginfo(reviewwtest)
reviewwtest <- testData[testData$BadLanguage == "Y",]
reviewwtest$Rating <- reviewwtest$predicted
printratinginfo(reviewwtest)

reviewwtest <- testData[testData$predicted == "3" & 
                               testData$Rating == "18",]

nrow(pegiDataFrame[pegiDataFrame$Rating == "3",])
reviewwtest <- pegiDataFrame[pegiDataFrame$Rating == "3" &
                               pegiDataFrame$Violence == "Y",]
