#read the train and test data

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#create new column Survived in test 

test.survive <- data.frame(Survived = rep("None", nrow(test)), test[,])

#match the column order with train
test.survived <- test.survive[, colnames(test.survive)[c(2,1,3:12)]]

#combine test.survived column and train
data.combined <- rbind(train, test.survived)


str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

table(data.combined$Survived)

table(data.combined$Pclass)

train$Pclass <- as.factor(train$Pclass)

 library(ggplot2)

#plot Pclass vs Survived as a bar graph
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + geom_bar(width = 0.3) +
  xlab("Pclass") + ylab("Total Count") + labs(fill = "Survived")

head(as.character(train$Name))

#check if records are unique
length(unique(train$Name))
length(unique(data.combined$Name))

#get duplicated names
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
dup.names

#get rows with duplicated names
data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)

#get data of rows with Miss.,Mrs, and all the males

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5]

males <- data.combined[which(data.combined$Sex == "male"),]
males[1:5]

#function to extract title from all the people
extractTitle <- function(Name) {
  Name <- as.character(Name)
  if(length(grep("Miss.", Name))>0){
    return("Miss.")
  } else if(length(grep("Mrs.", Name))>0){
    return ("Mrs.")
} else if (length(grep("Mr.", Name))>0){
  return("Mr.")
} else if(length(grep("Master", Name))>0){
  return("Master")
  }else {
  return("Others")
}  
}

#get all the titles 
titles <- NULL
for(i in 1 : nrow(data.combined)){
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}

titles

#Create new column Title
data.combined$Title <- as.factor(titles)
data.combined

#plot bar chart of Title vs Survived for each Pclass
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill = "Survived")

table(train$Sex, train$Survived)


#plot bar chart of Sex vs Survived for each Pclass
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill = "Survived")

#plot bar chart of Age vs Survived for each Pclass and Sex
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~Sex + Pclass)+
  ggtitle("Survival by Class and Gender")+
  xlab("age")+
  ylab("Total Count")+
  labs(fill = "Survived")

boys <- data.combined[which(data.combined$Title =="Master"),]
summary(boys)  

# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)


# plot chart with female children Age vs Survived for each Pclass
ggplot(misses[misses$survived != "None",], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")


#female children with no siblings or parents
misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))


#  summarize sibsp variable
summary(data.combined$sibsp)


# Can we treat as a factor?
length(unique(data.combined$sibsp))


data.combined$sibsp <- as.factor(data.combined$sibsp)


#  Visualize survival rates by sibsp, pclass, and title
ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Treat the parch vaiable as a factor and visualize
data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# feature engineering. create a family size feature.
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)


# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
