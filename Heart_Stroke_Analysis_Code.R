#Group Beta: Final Project: Initial Analysis Report

#install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("cowplot")
install.packages("plyr")
install.packages("gmodels")
install.packages("dummies")
install.packages("caret")
install.packages("glmnet")
install.packages("Metrics")
install.packages("pROC")
install.packages("regclass")

#load library
library(dplyr)
library(ggplot2)
library(cowplot)
library(plyr)
library(gmodels)
library(dummies)
library(caret)
library(glmnet)
library(Metrics)
library(pROC)
library(regclass)

#load dataset
df <- read.csv(file.choose(), header = TRUE, stringsAsFactors = TRUE)

#view the structure and summary of dataset
str(df)
summary(df)

################################################################################
#data cleaning
################################################################################
#check gender summary
summary(df$gender)

#delete other from variable gender
df <- df %>%
  filter(gender != "Other")

#turn variable stroke from int to factor and check
df$stroke <- as.factor(df$stroke)
str(df$stroke)

#turn variable hypertension from int to factor and check
df$hypertension <- as.factor(df$hypertension)
str(df$hypertension)

#turn variable heart disease from int to factor and check
df$heart_disease<- as.factor(df$heart_disease)
str(df$heart_disease)

#turn variable bmi from factor to numeric and check
df$bmi <- as.numeric(as.character(df$bmi))
str(df$bmi)

#count NA in variable bmi
sum(is.na(df$bmi))

#remove NA value from variable bmi
df1 <- df %>%
  na.omit(df$bmi)

################################################################################
# for numeric variable -- analyzing age, average glucose level, bmi vs. stroke
################################################################################
#calculate mean age by stroke status
mu <- ddply(df1, "stroke", summarise, grp.mean=mean(age))

#age distribution by stroke status with mean age
ggplot(df1, aes(x = age, color = stroke)) +
 geom_histogram(fill = "darkgrey", position = "identity") +
 geom_vline(data = mu, aes(xintercept = grp.mean, color = stroke), linetype = "dashed") +
 theme_bw() 

#calculate mean average glucose level by stroke status
mu2 <- ddply(df1, "stroke", summarise, grp.mean=mean(avg_glucose_level))

#glucose distribution by stroke status
ggplot(df1, aes(x = avg_glucose_level, fill = stroke)) +
  geom_histogram(position = "dodge") +
  theme_bw()

#boxplot for average glucose level vs. stroke
ggplot(df1,aes(x=stroke,y=avg_glucose_level,color=stroke))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.2,cex=0.6)+
  ggtitle("stroke and avg glucose level")+
  xlab("stroke")+
  ylab("avg glucose level" )+
  theme_bw()

#scatterplot of age vs. glucose level by stroke status 
ggplot(df1,aes(x=age,avg_glucose_level,
              color = stroke, shape = stroke))+
  geom_jitter(width=0.2, alpha = 0.4)+
  geom_smooth(method = "lm")+
  facet_wrap(~gender)+
  theme(panel.grid=element_blank(), 
        panel.background=element_rect(fill='transparent', color='black'))

#calculate mean bmi by stroke status
mu1 <- ddply(df1, "stroke", summarise, grp.mean=mean(bmi))

#bmi distribution by stroke status
ggplot(df1, aes(x = bmi, color = stroke)) +
  geom_histogram(fill = "darkgrey", position = "identity") +
  geom_vline(data = mu1, aes(xintercept = grp.mean, color = stroke), linetype = "dashed") +
  theme_bw() 

#extract people who have stroke
df_stroke1 <- df1 %>%
  filter(stroke == "1")

#initial randomizer
set.seed(123)

#randomly select a sample of 25 observations
sampleindex_25 <- sample(nrow(df_stroke1), 25)
sample_25 <- df_stroke1[sampleindex_25,]

#t test for bmi of people who have stroke vs. average (25)
#Ho: bmi of people who have stroke is the same as average bmi
#H1: bmi of people who have stroke is different than average bmi
test_bmi_stroke <- t.test(sample_25$bmi, alternative = "two.side", mu = 25, conf.level = 0.99)

#view result of t test 
test_bmi_stroke

################################################################################
# for factor variable -- hypothesis test of stroke vs. resident type and work type
################################################################################
#summarize resident type count by stroke status
CrossTable(df1$stroke,df1$Residence_type,prop.r =FALSE,
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE,
           dnn =c("stroke","resident type"))

# State the hypothesis
# H0: Residence type is independent upon Stroke
# H1: Residence type is dependent upon Stroke

# Set significant level
alpha_rt<- 0.05

# Create a vector for each row
r1_rt<- c(2318,2381)
r2_rt<- c(100,109)

# State the number of rows for the matrix
rows_rt<- 2

# Create a matrix from the rows
mtrx_rt = matrix(c(r1_rt,r2_rt),nrow = rows_rt, byrow = TRUE)

# Name the rows and columns
rownames(mtrx_rt) = c("Rural","Urban")
colnames(mtrx_rt) = c("Stroke_0","Stroke_1")

# View the matrix
mtrx_rt

# Run the test and save the result
result_rt<- chisq.test(mtrx_rt)

# View the test statistic and p-value
result_rt$statistic  #chi-square test value
result_rt$p.value    #chi-square p-value
result_rt$parameter  #degree of freedom
result_rt

#Compare the p-value to the alpha and make decision
ifelse(result_rt$p.value > alpha_rt, "Fail to reject H0", "Reject H0")

#summarize work type by stroke status in the table
CrossTable(df1$stroke,df1$work_type,prop.r =FALSE,
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE,
           dnn =c("stroke","work type"))

#Since there are columns with only 0 and 1 values, it is insignificant to perform the hypothesis test
#################################################################################
# Logistic Regression
#################################################################################
# Split the data into a train and test set
#initialize randomizer
set.seed(123)

#get 70% of random row numbers
trainIndex <- createDataPartition(df1$stroke,p=0.7,list=FALSE)

#get training set includes 70% of rows
train <- df1[trainIndex,]

#get test set excludes 70% of rows
test <- df1[-trainIndex,]

# Fit a logistic regression model
model1<-glm(stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level
            + bmi,data=train,family=binomial(link="logit"))
summary(model1)

# Interpret the GLS model
# Display regression coefficients(log-odds)
coef(model1)

# Display regression coefficients(odds)
exp(coef(model1))

# Update model2
model2<-glm(stroke ~ age + hypertension + avg_glucose_level,
            data = train, family = binomial(link = "logit"))
summary(model2)

# Interpret the GLS model
# Display regression coefficients(log-odds)
coef(model2)

# Display regression coefficients(odds)
exp(coef(model2))

##################################################################
# Train set prediction
##################################################################
#prepare for making confusion matrix
pro_train <- predict(model2, newdata=train, type="response")
pre_class1<- as.factor(ifelse(pro_train>=0.1, "1", "0"))

# Confusion matrix
confusionMatrix(pre_class1, train$stroke)

# Roc curve
ROC <- roc(train$stroke,pro_train)
plot(ROC, print.acu=TRUE, auc.polygon=TRUE,grid=c(0.1,0.2),
     grid.col=c("Green","Red"),max.auc.polygon=TRUE,
     auc.polygon.col="skyblue",print.thres=TRUE,
     ylab="Sensitivity - TP Rate", xlab= "Specificity - Fp Rate")

# Calculate the area under the ROC curve
AUC<-ROC$auc
AUC

#################################################################################
#-Lasso Regression
#################################################################################
# Split dataset
# initialize randomizer
set.seed(123)
# get 80% of random row numbers
trainIndex1<-createDataPartition(df1$stroke,p=0.8,list = FALSE)
# get training set includes 80% of rows
train1 <- df1[trainIndex1,]
# get test set excludes 80% of rows
test1 <- df1[-trainIndex1,]
# Romove Grad.Rate from test_x and train_x
train1_x <- model.matrix(stroke~.,train1)[,-1]
test1_x <- model.matrix(stroke~.,test1)[,-1]
# Add Grad.Rate Column to test_y and train_y
train1_y <- train1$stroke
test1_y <- test1$stroke

# Find the best lamada with cross validation
set.seed(123)

# Find the best lamada with cross validation
cv.lasso <- cv.glmnet(train1_x, train1_y,alpha=1, nfolds=10, family="binomial")
#lambda min
cv.lasso$lambda.min
#lambda.1se
cv.lasso$lambda.1se

#plotting 
plot(cv.lasso)

#Fitting a lasso regression model to the training set
# lasso regression coefficients for lambda min
model.lasso.min<-glmnet(x = train1_x, y = train1_y, alpha = 1, 
                        lambda = cv.lasso$lambda.min,family="binomial")
# find coef of lasso model at lasso.min
coef(model.lasso.min)
# lasso regression coefficients for lambda 1se
model.lasso.1se<-glmnet(x = train1_x, y = train1_y, alpha = 1, 
                        lambda = cv.lasso$lambda.1se,family="binomial")
# find coef of lasso model at lasso.1se
coef(model.lasso.1se)

#Train set prediction of lasso model by calculating RMSE
#lamda min
p1<- predict(model.lasso.min, newx = train1_x)
train.lasso.rmse.min <- rmse(as.numeric(train1_y), p1)
train.lasso.rmse.min
#lamda 1se
p2 <- predict(model.lasso.1se, newx = train1_x)
train.lasso.rmse.1se <- rmse(as.numeric(train1_y), p2)
train.lasso.rmse.1se

#Test set prediction of lasso model by calculating RMSE
#lamda min
p3<- predict(model.lasso.min, newx = test1_x)
test.lasso.rmse.min <- rmse(as.numeric(test1_y), p3)
test.lasso.rmse.min
#lamda 1se
p4 <- predict(model.lasso.1se, newx = test1_x)
test.lasso.rmse.1se <- rmse(as.numeric(test1_y), p4)
test.lasso.rmse.1se

# Update model3 - final model
model3<-glm(stroke ~ age + hypertension + heart_disease + avg_glucose_level,
            data = train, family = binomial(link = "logit"))
summary(model3)

# Confusion matrix
pro_train3 <- predict(model3, newdata=train, type="response")
pre_class3<- as.factor(ifelse(pro_train3>=0.1, "1", "0"))
confusionMatrix(pre_class3, train$stroke)

# Roc curve
ROC1 <- roc(train$stroke,pro_train3)
plot(ROC1, print.acu=TRUE, auc.polygon=TRUE,grid=c(0.1,0.2),
     grid.col=c("Green","Red"),max.auc.polygon=TRUE,
     auc.polygon.col="skyblue",print.thres=TRUE,
     ylab="Sensitivity - TP Rate", xlab= "Specificity - Fp Rate")

# Calculate the area under the ROC curve
AUC1<-ROC1$auc
AUC1
# Display regression coefficients(odds)
exp(coef(model3))

# Each unit increase in age increases the odds of getting stroke by 1.07
# People who have had hypertension have about 67% more odds of having stroke than people who have not
# People who have had heart disease have about 48% more odds of having stroke than people who have not
# Each unit increase in average glucose level increases the odds of getting stroke by about 1

# Check multicollinearity by using VIF
VIF(model3)

