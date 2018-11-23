  library(tidyverse)
  library(plyr)
  library(dplyr)
  library(gridExtra) #for plotting
  library(Amelia) #for missing values
  library(corrplot) #for correlation
  library(mice) #for missing values
  library(naniar) #For missing values plot
  library(boot) #For diognastic plots
  library(car) # for avplots
  library(caret)
  
  # Data Read
  
  set.seed(10)
  train=read_csv("train.csv")
  test=read_csv("test.csv")
  
  #Explore Data
  
  str(train)
  
  str(test)
  
  #From observing both data frames we can see that test does not have the Survived column 
  #(since it's what we plan to predict). Llet's combine them so that we can work on them at the same time
  
  test$Survived=NA
  data = rbind(train, test)
  str(data)
  
  #Data Visualization¶
  #Let's look at the data in a couple of plots.
  #histogram cahrt is for quantitative variable
  
  ggplot(data [!is.na(data$Age),], aes(x=Age, fill= Sex)) +
    geom_histogram(position="dodge", bins=25)
  
  
  pt1= ggplot(data, aes(x=Embarked, fill= Sex)) +
    geom_bar(position="dodge") +
    geom_text( stat = 'count', aes(label=..count..))
  
  #+   geom_text(stat='count', aes(label=..count..), vjust=-0, color="black", size=2)
  
  
  pt2 = ggplot(data, aes(x=Pclass, fill= Sex)) +
    geom_bar(position = "dodge") +
    geom_text( stat = "count", aes(label=..count..))
  
  pt2
  
  # the aurgument should be in quotes else below error
  #object of type 'closure' is not subsettable
  #bar chart is always for categorial variable
  
  pt3 = ggplot(data, aes(x=SibSp, fill=Sex))+
    geom_bar(position = "dodge") +
    geom_text(stat = "count", aes(label = ..count..), vjust=0.5, color="black", size=2)
  
  pt3
  
  pt4 = ggplot(data[!is.na(data$Survived),], aes(x=Survived, fill = Sex)) +
    geom_bar(position = "dodge") +
    geom_text(stat = "count", aes(label = ..count..))
  pt4
  
  pt5 = ggplot(data, aes(x=Parch, fill=Sex)) +
    geom_bar(position = "dodge")+
    geom_text(stat = "count", aes(label = ..count..))
  pt5
  
  pt6 = ggplot(data, aes(x=Sex))+
  geom_bar(position = "dodge")+
    geom_text(stat= "count", aes(label = ..count..))
  
  library(gridExtra)
  grid.arrange(pt1,pt2,pt3,pt4,pt5,pt6, nrow=3)
  
  #Check for Missing Values
  
  gg_miss_var(data)
  
  ##to check where these missing values are located in the dataset
  missmap(data[-1], col=c('grey', 'Maroon'), y.cex=0.5, x.cex=1)
  
  #sort(sapply(data, function (x) sum(is.na(x))), decreasing=T)
   sort(sapply(data, function(x) sum(is.na(x))), decreasing=T)
  
   #Check for Correlation
   
   data %>% 
     select_if(is.numeric) %>% 
     replace(is.na(.),0) %>% 
     cor() %>%
     corrplot()
   
   #We don't see strong correlations among the numeric values. 
   #The negative correlations seem to be stronger than the positive ones. 
   #For example, the negative correlations betwen Fare and Pclass mean that when one 
   #increases the other decreases which in this case makes sense since Pclass 1 has a 
   #higher Fare than Pclass 2. So the higher the Pclass number, the lower the Fare.
   
   
  #Check for Duplication
   #Check if we have duplicated rows
   duplication = data.frame(duplicated = duplicated(data), row = 1: nrow(data)) %>%
     filter(duplicated==T)
   
   print("The number of duplicated row in data set is: ")
   duplicated(data) %>% sum()
     
     
   #Data Processing
   #work on the missing values
   
   which(is.na(data$Fare))
   
   data[1044,]
   
   #It seems the only information I can use to find the Fare value is the PClass and Embarked, 
   #let's see the average Fare for passengers in
  #Pclass=3 and Embarked=S and use it for this passenger's Fare value.
   
   d = subset(data, !is.na(data$Fare))
   mean(d$Fare[d$Pclass == "3"& d$Embarked == "S"] )
   
   data$Fare[1044] = 14.44
   
   table(data$Embarked)
   
   which(is.na(data$Embarked))
   
   data[62,]
   
   data[830,]
   
   data$Embarked[c(62,830)] = "S"
   
   #Imputing the Age Variable using mice
   
   empty <- mice(data, maxit=0)
   empty
   
   
   
   method <- empty$method
   predictorMatrix <- empty$predictorMatrix
   
   method
   
   predictorMatrix
   
   imdata <- mice(data, method, predictorMatrix, m=5) # create multiple imputations
   imdata <- complete(imdata) #select one of them
   
   imdata
   
   newdata = data
   newdata$Age = imdata$Age
   summary(newdata)
   
   AgeOld = ggplot(data=data[!is.na(data$Age),], aes(x= Age )) +
     geom_histogram(position = "dodge", bins=25, color = "Maroon", fill = "LightGrey") +
     labs("Age before imputation")
   
   AgeNew = ggplot(data=newdata[!is.na(data$Age),], aes(x= Age )) +
     geom_histogram(position = "dodge", bins=25, color = "Maroon", fill = "LightGrey") +
     labs("Age After imputation")
   
   grid.arrange(AgeOld, AgeNew, nrow=1)
   
   
   #Feature Engineering
   #In this section, I want to find some more infomration 
   #from the existing data that could help with the analysis later on.
   #First lets create the Title variable and add it to the data set.
   
   #Create Title and Family Name
   
   pnames=newdata$Name
   title <- gsub("^.*, (.*?)\\..*$", "\\1", pnames)
   newdata$Title=title
   
   table(newdata$Sex, newdata$Title)
   
   newdata$Title[newdata$Title  %in% c("Lady", "Dona", "the Countess", "Mlle", "Ms", "Dr", "Mme") &
    newdata$Sex == "female" ] = "Miss"
  
  newdata$Title[newdata$Title %in% c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major","Rev")] = "Sir"
  
  table(newdata$Sex, newdata$Title)
  
  #Using the SibSp and the Parch variables I want to find the family members.
  #First let's find the family names of the passengers.
  
  newdata$FamilyName=gsub(",", "",word(newdata$Name)) #extract first word in Name and remove the c
  omma
  head(newdata[,c("Name","FamilyName", "SibSp", "Parch")], 10)
  
  #Create Solo Passenger and Family Size
  
  newdata$SoloPass=0
  newdata$SoloPass[newdata$SibSp+newdata$Parch==0]=0 #Not a solo passenger
  newdata$SoloPass[newdata$SibSp+newdata$Parch!=0]=1 #A solo passenger
  newdata$FamilyCount=0
  newdata$FamilyCount=newdata$SibSp+newdata$Parch + 1
  #+1 is to count the passenger with the family member
  #Let's check if we got it right in a sample
  
  head(newdata[,c("Name","FamilyName", "SibSp", "Parch", "SoloPass", "FamilyCount", "Survived")],
       15)
  
  ggplot(data=newdata, aes(x=FamilyCount, fill =Sex)) +
    geom_bar(position="dodge")
  
  
  #We can see that the families of size 2, 4, 5, 7 members, have more females 
  #than males. And the solo passengers are mostly male.
  
  fam = newdata %>% filter(FamilyCount != 1)
  
  ggplot(newdata, aes(x=Survived, fill=Sex)) +
    geom_bar(position = "dodge")+
    geom_text(stat = "count", aes(label = ..count..)) +
    labs(title="Passengers Survival by Family Size [Survived = 1]") +
    facet_wrap(~FamilyCount)+
    theme(legend.position="top") 
  
  #We can see that the all members of families that consist of 8 and 11 members did not survive. And it seems the larger the families get,
  #the less they are to survive. I wonder does that have to do with the Pclass they belong to. I assume the larger families are mostly in
  #Class 3. Let's find out.
  
  ggplot(data=newdata, aes(x=Pclass))+
    geom_bar(position="dodge")+
    facet_wrap(~FamilyCount)+
    geom_text(stat = "count", aes(label = ..count..))
  
  
  
  # find the which family servived most in which class
  ggplot(data=newdata, aes(x=Survived, fill =Sex)) +
    geom_bar(position = "dodge") +
    facet_wrap(~Pclass)+
    geom_text(stat = "count", aes(label = ..count..))
  
  
  #As I expected, the larger families are in class 3. However, that 16 members in the family of size 7 doesn't look right. If we had two
  #families in that family size than the total number of memebrs should be 14. Let's investigate.
  
  #fam = newdata %>% filter(FamilyCount != 1)
  
  fam7 = subset(fam, fam$FamilyCount==7 & fam$Pclass==3 & FamilyName=="Andersson")
  head(fam7)
  
  #Let's take the Andersson family and check out their members.
  fam7Ander=subset(fam, FamilyName=="Andersson")
  
  dim(fam7Ander)
  head(fam7Ander[,c("Name","FamilyName", "Age", "SibSp", "Parch", "SoloPass",
                    "FamilyCount", "Survived")],9)
  
  famandersson=subset(newdata, FamilyName=="Andersson")
  head(famandersson[,c("Name","FamilyName", "Age", "SibSp", "Parch", "SoloPass",
                       "FamilyCount", "Survived")],11)
  
  #Adults vs Children
  #Let's find how many children and adults we have and their survival.
  
  
  children = subset(newdata, newdata$Age <= 17)
  adult = subset(newdata, newdata$Age > 17)
  
  dim(children)
  dim(adult)
  
  child = ggplot(data = children, aes(x=Survived, fill=Sex))+
    geom_bar(position = "dodge")+
    geom_text(stat = "count", aes(label = ..count..)) 
   # theme(legend.position="top") +
    labs(title="children survival")
  
  adults = ggplot(data = adult, aes(x=Survived, fill=Sex))+
    geom_bar(position = "dodge")+
    geom_text(stat = "count", aes(label = ..count..)) 
    #theme(legend.position="top") +
    labs(title="Adult survival") 
  
  grid.arrange(child, adults, nrow=1)
  
  sum(is.na(newdata$Cabin))
  head(newdata$Cabin,10)
  
  
  #Convert Categorical to Numeric
  #Sex: female will become 1, male 0
  #Embarked: C= 1, Q=2, S=3
  
  sapply(newdata, class)
  
  newdata$Sex[newdata$Sex == "female"] = "1"
  newdata$Sex[newdata$Sex == "male"] = "0"
  newdata$Sex = as.numeric(newdata$Sex)
  
  
  newdata$Embarked[newdata$Embarked == "C"] = "1"
  newdata$Embarked[newdata$Embarked == "Q"] = "2"
  newdata$Embarked[newdata$Embarked == "S"] = "3"
  
  newdata$Embarked = as.numeric(newdata$Embarked)
  
  
  sapply(newdata, class)
  
  #Now that we have new variables, let's check the correlation. for corellation all should numeric
  
  newdata %>% select_if(is_numeric) %>% replace(is.na(.), 0) %>% cor() %>% corrplot()
  
  
  #Regression Analysis
#  After checking the data and processing it, let's start with the analysis
#and see if our data can correctly predict the passengers survivals. I plan to use 
#Regression Analysis. But before that we need to seperate the test data from the 
#training data.
  
training = subset(newdata, !is.na(newdata$Survived))
dim(training)

testing = subset(newdata, is,na(newdata$Survived))
dim(testing)

#First Model with All Numeric Variables¶
str(newdata)
modelone = glm(Survived ~ Pclass + Sex +  Age + SibSp + Parch + Fare + 
                 Embarked + SoloPass, data = training, family= binomial)

glm.diag.plots(modelone) #plot to see if our model is a good fit for the data and proble

table (training$Survived)

summary(modelone)

par(mfrow = c(2,2))

plot(modelone)

avPlots(modelone)

#Now let's find our predictions
training$survive_prob <- predict(modelone, training, type="response")

mean(training$Survived)

training$survive_pred = ifelse(training$survive_prob > 0.5, 1 , 0)

mean(training$survive_pred == training$Survived)

table(training$Survived, training$survive_pred)












  
  
  
  
  
    
    
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
     
     
     
     
   
   
   
  
   
   
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
