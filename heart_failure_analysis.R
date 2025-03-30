### REQUIRED LIBRARIES
library(klaR)
library(cvms)
library(tibble) 
library(sampling)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(ggcorrplot)
library(MASS)
library(GGally)
library(mlbench)
library(tree)
library(MASS)
library(rpart)
library(rpart.plot)
library(patchwork)

### READING DATA

multi <- read.csv("heart_failure.csv")
multi$DEATH_EVENT <- as.factor(multi$DEATH_EVENT)
multi$smoking <- as.factor(multi$smoking)
multi$sex<- as.factor(multi$sex)
multi$anaemia <- as.factor(multi$anaemia)
multi$diabetes <- as.factor(multi$diabetes)
multi$high_blood_pressure <- as.factor(multi$high_blood_pressure)
head(multi)
dim(multi) #Our data set contains 299 rows and 13 columns.


#### Obtain mean vector
colMeans(multi[, unlist(lapply(multi, is.numeric))])

#### Obtain var-cov matrix
var(multi[, unlist(lapply(multi, is.numeric))])

#### Correlation matrix
corr <- cor(multi[, unlist(lapply(multi, is.numeric))]) 
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

### EXPLORATORY DATA ANALYSIS
multi %>% glimpse()
summary(multi) #The dataset ‘multi’ contains 12 clinical features for predicting death events.

### Barplots of categorical variables

ggplot(multi, aes(x = anaemia, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)"))+
  labs(x = "Anaemia") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5)

ggplot(multi, aes(x = diabetes, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  labs(x = "Diabetes") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5)


ggplot(multi, aes(x = high_blood_pressure, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  labs(x = "High blood pressure") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5)

ggplot(multi, aes(x = sex, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (Female)", "1 (Male)")) +
  labs(x = "Sex") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5)

ggplot(multi, aes(x = smoking, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  labs(x = "Smoking") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5)

ggplot(multi, aes(x = DEATH_EVENT, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  labs(x = "DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5)


### Boxplots of numerical variables

ggplot(multi, aes(x=age, fill = DEATH_EVENT)) + 
  geom_boxplot()+ coord_flip() +theme_minimal(base_size = 12) +
  stat_boxplot(  position = "dodge2")

ggplot(multi, aes(x=creatinine_phosphokinase, fill = DEATH_EVENT)) + 
  geom_boxplot()+ coord_flip()+ theme_minimal(base_size = 12) +
  stat_boxplot(position = "dodge2")

ggplot(multi, aes(x=ejection_fraction, fill = DEATH_EVENT)) + 
  geom_boxplot()+ coord_flip() +theme_minimal(base_size = 12) +
  stat_boxplot(  position = "dodge2")

ggplot(multi, aes(x=platelets, fill = DEATH_EVENT)) + 
  geom_boxplot()+ coord_flip() + theme_minimal(base_size = 12) +
  stat_boxplot(  position = "dodge2")

ggplot(multi, aes(x=serum_creatinine, fill = DEATH_EVENT)) + 
  geom_boxplot()+ coord_flip() + theme_minimal(base_size = 12) +
  stat_boxplot(  position = "dodge2")

ggplot(multi, aes(x=serum_sodium, fill = DEATH_EVENT)) + 
  geom_boxplot()+ coord_flip() +theme_minimal(base_size = 12) +
  stat_boxplot(  position = "dodge2")

## MODELLING

### Logistic Regression

# Splitting the data into two as a training (%80) and test (%20) data.

set.seed(467)
sample <- sample(c(TRUE, FALSE), nrow(multi), replace=TRUE, prob=c(0.8,0.2))
train <- multi[sample, ]
test <- multi[!sample, ]

# Fitting the logistic regression model to the training data.
lr <- glm(DEATH_EVENT~., family=binomial, data = train)
summary(lr)

# Making predictions on the test set and print out classification matrix to evaluate model performance.

preds <- predict(lr, test, type="response")
Predict <- rep(0,dim(test)[1])
Predict[preds>=0.5]=1
Actual <- test$DEATH_EVENT
table(Predict, Actual)
misclassification_rate <- 12/(38+6+6+19)

# We get a misclassification rate of 0.17% with the logistic regression model.


#### Removing non-significant variables from the model.

lr1 <- glm(DEATH_EVENT~age+ejection_fraction+serum_creatinine+serum_sodium+time, family=binomial, data=train)
summary(lr1)

# Make predictions on the test set and print out classification matrix to evaluate new model performance
preds<-predict(lr1, test, type="response")
Predict<-rep(0,dim(test)[1])
Predict[preds>=0.5]=1
Actual<-test$DEATH_EVENT
table(Predict, Actual)

#Model performance stayed the same after removing non-significant variables, still with a misclassification rate of 22.3%.

#Can we improve the model further by standardizing our variables?
  
#We get a subset of our data consisting of just the significant features

features = subset(multi,select=c(age, ejection_fraction, serum_creatinine, serum_sodium, time))

#Standardize the values
scaled<-scale(features)

#Step 7: Add label column (DEATH_EVENT) back to our scaled data

DEATH_EVENT = multi$DEATH_EVENT
new_df = data.frame(scaled, DEATH_EVENT)

head(new_df)

#Step 8: Create train (75%) and test (25%) set again with our newly scaled data
idx=sample(1:nrow(new_df),3/4*nrow(new_df))
train=new_df[idx,]
test=new_df[-idx,]

set.seed(467)
sample1 <- sample(c(TRUE, FALSE), nrow(new_df), replace=TRUE, prob=c(0.75,0.25))
train1 <- multi[sample1, ]
test1 <- multi[!sample1, ]

#Step 9: Fitting logistic regression model to training data (with now standardized features)
new_lr <- glm(DEATH_EVENT~age+ejection_fraction+serum_creatinine+time, family=binomial, data=train1)
summary(new_lr)

#Step 10: Make predictions on the test set and print out classification matrix to evaluate new model performance

preds<-predict(new_lr, test1, type="response")
Predict<-rep(0,dim(test1)[1])
Predict[preds>=0.5]=1
Actual<-test1$DEATH_EVENT
table(Predict, Actual)

### CLASSIFICATION TREE

set.seed(467)
sample <- sample(c(TRUE, FALSE), nrow(multi), replace=TRUE, prob=c(0.8,0.2))
train <- multi[sample, ]
test <- multi[!sample, ] 
str(train)

#To verify if the randomization process is correct.
prop.table(table(train$DEATH_EVENT)) 
prop.table(table(test$DEATH_EVENT))
# In both dataset, the amount of death is the same, about 30 percent.

fit <- rpart(DEATH_EVENT~., data = train,method = 'class')
summary(fit)
rpart.plot(fit, extra = 106)

#The classification tree tells us what features impact the likelihood of death by choosing the variables based on their importance from the output. When we look at the tree, At the top of the tree shows that 32 percent of patients have survived from heart failure. This node asks whether follow-up days are bigger than 68 or not. If days are lower than 68, then 21 percent of patients have the probability of death is 90 percent. If yes, then we go down to the root’s left child node (depth 2), 79 percent of patients has bigger than 68 have a probability of death is 16 percent. In the second node, we ask if the ejection fraction is bigger than or equal to 33. If yes, then 59 percent of patients who satisfies node 1 and node 2, have the chance of death is 0.08 percent. If not, then 19 percent of patients who satisfies node 1 but not node 2, have a death probability is 44 percent. In the third node, we ask whether serum creatinine less than 1.4 or not. If yes, then the patients who satisfies node 1,3, but not node 2 have a chance of death with 24 percent. If not the chance of death is 72 percent.

#In the second step, we use the fitted (unpruned) tree to predict the death event of the test set. In other words, we want to predict which patients are more likely to die after the collision from the test set. It means, we will know among those 50 patients, which one will die or not.

## test data
predict_unseen <-predict(fit, test, type = 'class')
table <- table(test$DEATH_EVENT, predict_unseen)

table%>%
  kbl() %>%
  kable_paper(bootstrap_options = "striped", full_width = F)%>%
  kable_styling(fixed_thead = T)

##### train data
predict_unseen <-predict(fit, train, type = 'class')
table1 <- table(train$DEATH_EVENT, predict_unseen)

table1 %>%
  kbl() %>%
  kable_paper(bootstrap_options = "striped", full_width = F)%>%
  kable_styling(fixed_thead = T)


print(mean(predict_unseen!=test$DEATH_EVENT)) #misclassification rate
mean(predict_unseen==test$DEATH_EVENT)#the accuracy

print(mean(predict_unseen!=train$DEATH_EVENT)) #misclassification rate train
mean(predict_unseen==train$DEATH_EVENT)#the accuracy train


### LINEAR DISCRIMINANT ANALYSIS

#Exploratory analysis in order to have an insight about the heart failure data.
multi %>% glimpse() %>%
  kbl(caption = "Summary of the Heart Failure Data", bold=T) %>%
  kable_paper(bootstrap_options = "striped", full_width = F)%>%
  kable_styling(fixed_thead = T)

#Determine the number of values in each level of dependent variable.
multi %>% 
  group_by(DEATH_EVENT) %>%
  count() %>% 
  ungroup() %>%
  kable(align = rep("c", 2)) %>% kable_styling("full_width" = F)

multi %>%
  group_by(DEATH_EVENT) %>%
  summarise(disp = mean(creatinine_phosphokinase), sd = sd(creatinine_phosphokinase))

multi %>%
  group_by(DEATH_EVENT) %>%
  summarise(disp = mean(platelets), sd = sd(platelets))

#After the numerical descriptive analysis, we can use the visual tools. Note that LDA performs well when the x variables, i.e. features, has the normal distribution.

GGally::ggpairs(multi)
GGally::ggpairs(multi[, unlist(lapply(multi, is.numeric))],  aes(color = as.factor(DEATH_EVENT), alpha = 0.5))

shapiro.test(multi$age)
shapiro.test(multi$ejection_fraction)
shapiro.test(multi$creatinine_phosphokinase)
shapiro.test(multi$platelets)
shapiro.test(multi$serum_creatinine)
shapiro.test(multi$serum_sodium)
shapiro.test(multi$time)

#The plot and shapiro test shows that the almost all variables in the data does not follow the normality. However, we still tried and see what the default linear discriminant function does.

#80% of dataset as training set and remaining 20% as testing set

set.seed(467)
sample <- sample(c(TRUE, FALSE), nrow(multi), replace=TRUE, prob=c(0.8,0.2))
train <- multi[sample, ]
test <- multi[!sample, ]

model <- lda(DEATH_EVENT~.,data = train)
model

plot(model)

model.values <- predict(model)
names(model.values)

#The first element, class, contains LDA’s predictions about the patients. The second element, posterior, is a matrix whose kth column contains the posterior probability that the corresponding observation belongs to the kth class. Finally, x contains the linear discriminants.

#Last Step
#The performances of the model using confusion matrix.
#Train performance

set.seed(467)
train_predict<- predict(model,train)$class
table_train <- table(Predicted =train_predict, Actual = train$DEATH_EVENT)
table_train

basic_table <- table(Predicted =train_predict, Actual = train$DEATH_EVENT)

cfm <- as_tibble(basic_table)
cfm
plot_confusion_matrix(cfm, 
                      target_col = "Predicted", 
                      prediction_col = "Actual",
                      counts_col = "n")

sum(diag(table_train))/sum(table_train) #accuracy of the train data.

#The model correctly classifies the patients with 0.85 probability for the training data. The classification error rate (misclassification rate) for training data is 1−0.85=0.15

#Test Performance
test_predict<- predict(model,test)$class
table_test <- table(Predicted =test_predict, Actual = test$DEATH_EVENT)
table_test
basic_table1 <- table(Predicted =test_predict, Actual = test$DEATH_EVENT)
cfm1 <- as_tibble(basic_table1)
cfm1
plot_confusion_matrix(cfm1, 
                      target_col = "Predicted", 
                      prediction_col = "Actual",
                      counts_col = "n")

sum(diag(table_test))/sum(table_test) #accuracy of the test data

#The model correctly classifies the patients with 0.8115942 probability for the test data. The classification error rate (misclassification rate) for test data is 1−0.81=0.19


