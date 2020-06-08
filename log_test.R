setwd("D:/Learning/Machine Learning/Classification")

db = read.csv("LoanKuber Case Data.csv")

head(db)

summary(db)

write.csv(summary(db),"summary.csv")

write.csv(is.na(db),"NA.csv")

db1 = db[,-c(21,22,23,2,3)]

write.csv(db1,"db1.csv")
names(db1)

#only numerical
db2 = db1[,-c(1,3,4,25)]
install.packages("car")
library(car)

db1.vif = lm(No..of.bounces~. - Customer.Name, data = db1)
vif(db1.vif)

cor(db2,db2)
write.csv(cor(db2,db2),"cor.csv")

#High Correlation:
#Total.Income.Score.out.of.24., Monthly.Household.Income.Score
#Average.Monthly.Credit.Score, Total.Banking.Score.out.of.9.


#Deleting Monthly.Household.Income.Score - Total.Banking.Score.out.of.9.- Total.Demographic.Score.out.of.6. - Total.Property.Score.out.of.36.

names(db2)

db3 = db2[,-c(13,5,6,2)]
write.csv(cor(db3,db3),"cordb3.csv")

names(db1)

db4 = db1[,-c(16,8,9,5)]
names(db4)
db5=db4[,-4]

db5.vif = lm(No..of.bounces~., data = db5)
vif(db4.vif)
write.csv(db4,"db4.csv")



#Train and Test
install.packages("caTools")
library("caTools")
require(caTools)
set.seed(101)
sample = sample.split(db5, SplitRatio = .7)
train = subset(db5, sample == TRUE)
test  = subset(db5, sample == FALSE)

#Logistic regression
model  = glm(No..of.bounces~. - Customer.Name, family= binomial(logit), data = train)

summary(model)
step(model)

model1 = glm(formula = No..of.bounces ~ Salaried.Self.Employed + Total.Income.Score.out.of.24. + 
               Total.CIBIL.Score.out.of.18. + Property.Chain.Score + Last.Registered.Book.1.Document.Score + 
               Technical.Deviation.Score + Locality.Development.Score + 
               Stability.Score + ABB.EMI.Score + No..of.presentations.till.Feb..19., 
             family = binomial(logit), data = train)
summary(model1)

predict1 = predict(model1,type = 'response')

table(train$No..of.bounces, predict > 0.5)









