#################################################################################
#################################################################################
#### 'To eat, or not to eat: that is the question.' An application of random ####
#### forest to distinguish edible from poisonous mushrooms with 100% accuracy ###
#################################################################################
#################################################################################
############################ Jordi Sanjuán Belda ################################
############################### December 2020 ###################################
#################################################################################

#First of all, we install and load the required packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(gridExtra)
library(GGally)
library(rpart)
library(Rborist)

#We read the data from a .csv file previously downloaded
data <- read.csv("mushrooms.csv")

#We convert characters into factors to properly process the data
data <- data %>%
  mutate(class=as.factor(class),
         cap.shape=as.factor(cap.shape),
         cap.surface=as.factor(cap.surface),
         cap.color=as.factor(cap.color),
         bruises=as.factor(bruises),
         odor=as.factor(odor),
         gill.attachment=as.factor(gill.attachment),
         gill.spacing=as.factor(gill.spacing),
         gill.size=as.factor(gill.size),
         gill.color=as.factor(gill.color),
         stalk.shape=as.factor(stalk.shape),
         stalk.root=as.factor(stalk.root),
         stalk.surface.above.ring=as.factor(stalk.surface.above.ring),
         stalk.surface.below.ring=as.factor(stalk.surface.below.ring),
         stalk.color.above.ring=as.factor(stalk.color.above.ring),
         stalk.color.below.ring=as.factor(stalk.color.below.ring),
         veil.type=as.factor(veil.type),
         veil.color=as.factor(veil.color),
         ring.number=as.factor(ring.number),
         ring.type=as.factor(ring.type),
         spore.print.color=as.factor(spore.print.color),
         population=as.factor(population),
         habitat=as.factor(habitat))


###############################
######## DATA OVERVIEW ########
###############################

#Let's take a first quick look at the data
summary(data)
#The first thing we realize is that veil.type has a unique value: p (partial)

#Therefore, it doesn't provide us with any information
data <- data %>%select(-veil.type) #we delete the variable veil.type


#Let's see how how many edible and poisonous mushrooms do we have in the dataset
data %>% ggplot(aes(class,fill=class)) + geom_bar() #Both classes are balanced
mean(data$class == "e") #In fact, 51.8% of mushrooms from the dataset are edible
#That means we won't have problems of low prevalence

#Now, we want to visually detect class differences according to different features
#We are going to create a plot for each variable, to later put them together
p1 <- data %>% ggplot(aes(cap.shape,fill=class)) + geom_bar()
p2 <- data %>% ggplot(aes(cap.surface,fill=class)) + geom_bar()
p3 <- data %>% ggplot(aes(cap.color,fill=class)) + geom_bar()
p4 <- data %>% ggplot(aes(bruises,fill=class)) + geom_bar()
p5 <- data %>% ggplot(aes(odor,fill=class)) + geom_bar()
p6 <- data %>% ggplot(aes(gill.attachment,fill=class)) + geom_bar()
p7 <- data %>% ggplot(aes(gill.spacing,fill=class)) + geom_bar()
p8 <- data %>% ggplot(aes(gill.size,fill=class)) + geom_bar()
p9 <- data %>% ggplot(aes(gill.color,fill=class)) + geom_bar()
p10 <- data %>% ggplot(aes(stalk.shape,fill=class)) + geom_bar()
p11 <- data %>% ggplot(aes(stalk.root,fill=class)) + geom_bar()
p12 <- data %>% ggplot(aes(stalk.surface.above.ring,fill=class)) + geom_bar()
p13 <- data %>% ggplot(aes(stalk.surface.below.ring,fill=class)) + geom_bar()
p14 <- data %>% ggplot(aes(stalk.color.above.ring,fill=class)) + geom_bar()
p15 <- data %>% ggplot(aes(stalk.color.below.ring,fill=class)) + geom_bar()
p16 <- data %>% ggplot(aes(veil.color,fill=class)) + geom_bar()
p17 <- data %>% ggplot(aes(ring.number,fill=class)) + geom_bar()
p18 <- data %>% ggplot(aes(ring.type,fill=class)) + geom_bar()
p19 <- data %>% ggplot(aes(spore.print.color,fill=class)) + geom_bar()
p20 <- data %>% ggplot(aes(population,fill=class)) + geom_bar()
p21 <- data %>% ggplot(aes(habitat,fill=class)) + geom_bar()

#We arrange all these plots
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,
             p13,p14,p15,p16,p17,p18,p19,p20,p21,
             ncol=3)
#There are some differences: some characteristics are mainly typical
#of edible mushrooms and others of poisonous ones


#Now we want to see if there are correlations between some of these features.
#To do this, we will use a correlation matrix.
#But first we have to convert the data into a numerical matrix (with 1s and 0s)
#separating all possible characteristics into different variables.
data %>% mutate(mushroomId = 1:8124) %>%
  gather (feature,type,-mushroomId) %>%
  mutate (value = 1) %>%
  unite (featype,feature,type,sep="_") %>%
  spread (featype,value) %>%
  gather (feature,value,-mushroomId) %>%
  mutate (value = case_when (value=="1"~1,TRUE~0)) %>%
  spread (feature,value) %>%
  select(-mushroomId,-class_e,-class_p) %>%
  ggcorr()
#We obtain a visual correlation matrix that crosses all the features
#We can see that there are some elements related to each other, in a positive and negative way


####################################
############# METHOD ###############
####################################

#First of all, we split the data into the train set (80% observations) and the test set (20%)
set.seed(1,sample.kind="Rounding")
test_index<-createDataPartition(y=data$class,times=1,p=0.2,list=FALSE)
train_set <- data[-test_index,]
test_set <- data[test_index,]

### Classification Tree ###

#The first method we adopt is a classification tree:
set.seed(1,sample.kind="Rounding")
fit_tree <- train(class~.,
                  data=train_set,
                  method="rpart",
                  tuneGrid=data.frame(cp=seq(0,0.05,len=25)))

#We see that the best accuracy is achieved with a complexity parameter (cp) of 0:
fit_tree$bestTune
ggplot(fit_tree,highlight=TRUE)


### Random Forest ###

#Now, we apply random forest
set.seed(1,sample.kind="Rounding")
#CAUTION: This code can take a long time
fit_rf<-train(class~.,
              method="Rborist",
              tuneGrid=data.frame(predFixed=seq(2,95,1),minNode=2),
              data=train_set)

#We check the features of the best tune model: 12 predictors
fit_rf$bestTune

ggplot(fit_rf,highlight=TRUE)
#From 8 predictors to 40 (aprox), the results are quite similar, near 100%

###################
##### RESULTS #####
###################

### Classification Tree ###

#First of all, we illustrate the decision tree that has been generated
plot(fit_tree$finalModel,margin=0.1)
text(fit_tree$finalModel,cex=0.75)

#We use the test set to get predictions and check the overall accuracy
y_hat_tree <- predict(fit_tree,test_set)
mean(test_set$class == y_hat_tree) #0.998155

#We observe the confusion matrix and the complete statistics
confusionMatrix(y_hat_tree,test_set$class)
#3 poisonous mushrooms have been identified as edible: that's very dangerous!


### Random Forest ###

#These are the more important variables in our random forest model:
varImp(fit_rf)
ggplot(varImp(fit_rf))

#We make the prediction for the test set and check the overall accuracy
y_hat_rf < -predict(fit_rf,test_set)
mean(test_set$class == y_hat_rf) #100%!

#And finally, here we have the confusion matrix and the complete statistics
confusionMatrix(y_hat_rf,test_set$class)
