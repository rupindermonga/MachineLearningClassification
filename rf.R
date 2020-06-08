head(db5)

install.packages("randomforest")
library("randomforest")

modelrf = randomForest(No..of.bounces~. - Customer.Name,  data = db5)

summary(modelrf)

print(modelrf)
modelrf$pred

table(db5$No..of.bounces, modelrf$pred > 0.5)
