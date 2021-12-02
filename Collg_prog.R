setwd("C:/Users/honey/OneDrive/Desktop/Study material/simplilearn/R program - 1/Assesments/Assesment 5")
library(caTools)
#Reading Data
df= read.csv('College_admission.csv')

# finding missing value
sum(is.na(df))
#There are no missing data

# Finding and treating the outliers
head(df)

Gre.boxplot = boxplot(df, col = 'blue')
summary(df$gre)
Gre.boxplot$stats
df$gre = ifelse(df$gre<340,580, df$gre)
boxplot(df, col = 'blue')
summary(df)

# Changing structure as requirement

str(df)
df$admit= factor(df$admit,levels = c(0,1))

counts = table(df$gre)
barplot(counts, xlab = 'Gre Score', ylab = 'Counts of student')
Df_gre= density(df$gre)
Df_gre
plot(Df_gre)
shapiro.test (df$gre)
# From Shapiro-Wilk test P value< 0.05 so data is normally distributed

# checking significance
cor(df, df$admit)

#logistic model
set.seed(123)
split = sample.split(df$admit, SplitRatio = 0.8)
train = subset(df, split == TRUE)
sum(row(train))
sum(row(df))
test = subset(df, split == FALSE)
sum(row(test))
regressor = glm(formula = admit ~ ., family = binomial,
               data = train)

summary(regressor)

pred = predict(regressor, type = 'response', newdata = test[-1])
pred = ifelse(pred > 0.5, 1, 0)

#checking for accuracy
Conf_mat = table(test[, 1], pred > 0.5)
Conf_mat
#accuracy is 61%


#backward elimination to check variable significane
regressor2 = glm(formula = admit ~gre+gpa+ses+Gender_Male+rank,
                 family = binomial, data = train)
summary(regressor2)

regressor3 = glm(formula = admit ~gre+gpa+ses+Gender_Male+rank,
                 family = binomial, data = train)
summary(regressor3)


regressor4 = glm(formula = admit ~gre+gpa+ses+rank,
                 family = binomial, data = train)
summary(regressor4)



#Decision tree

dataset=df[c(1,2,3,4,7)] 
dataset

# install.packages("party")
library(party)

DT_regres = rpart(formula = admit ~ .,
                  data = dataset)
summary(DT_regres)

pred1 = predict(DT_regres, newdata = dataset[-1], type = 'class')
pred1
DT_CM = table(dataset[, 1], pred1)
DT_CM
#accuracy is 75.75%

#SVM model

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)

SVM_regress = svm(formula = admit ~., 
                  data = dataset)
summary(SVM_regress)
pred2 = predict(SVM_regress, 
                newdata = dataset)
SVM_CM = table( dataset[, 1], pred2)
SVM_CM

#accuracy is 72.5%


#linear model

dataset$admit = as.integer(dataset$admit)
lm_regress = lm(formula = admit ~.,
                data = dataset)
pred3 = predict(lm_regress, 
                newdata = dataset)
pred4 = table(dataset[, 1], pred3 > 0.5 )
pred4

#Accuracy is 68.4%

#best model would be Decision tree with 75%
