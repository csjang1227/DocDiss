FAET <- read.csv("PIAAC17USr-v2-rf-faet.csv", fileEncoding = 'UTF-8-BOM')
str(FAET)
FAET$faet <- as.factor(FAET$faet)

install.packages("randomForest")
library(randomForest)

set.seed(2467) #12
ind1 <- sample(2, nrow(FAET), replace = T, prob = c(0.7, 0.3))
train1 <- FAET[ind1 == 1,]
test1 <- FAET[ind1 == 2,]

set.seed(2467)
rf1 <- randomForest(faet~., data = train1, importance = T, proximity = T)
print(rf1)

varImpPlot(rf1)
importance(rf1)

attributes(rf1)
rf1$importance
rf1$mtry
rf1$confusion

install.packages("caret")
library(caret)

plot(rf1)
t <- tuneRF(train1[, -15], train1[, 15],
            stepFactor = 0.5,
            plot = T,
            ntreeTry = 2000,
            trace = T,
            improve = 0.05)

rf1 <- randomForest(faet~., data = train1, importance = T, proximity = T,
                    ntree = 2000, mtry = 3)
print(rf1)

varImpPlot(rf1)
importance(rf1)

install.packages("ggplot2")
install.packages("magrittr")
install.packages("dplyr")
library(ggplot2)
library(magrittr)
library(dplyr)

importance = importance(rf1)
varImportance = data.frame(Variables = row.names(importance),
                           Importance = round(importance[, "MeanDecreaseAccuracy"], 2))
rankImportance = varImportance%>%mutate(Rank = paste('#', dense_rank(desc(Importance))))

ggplot(rankImportance,
       aes(x = reorder(Variables, Importance),
           y = Importance, fill = Importance)) +
  geom_bar(stat = "identity", position = "dodge") + coord_flip() +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust = 0, vjust = 0.55, size = 4, colour = "white") +
  labs(x = "Variables", y = "Mean Decrease Accuracy") +
  theme_dark(base_size = 16)
ggsave(file = "faet (MDA)-resize.png", width = 16, height = 8)

# Decision trees visualization

install.packages("rpart")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("party")
install.packages("partykit")
install.packages("caret")
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)
library(partykit)
library(caret)

train1$faet <- factor(train1$faet, 
                      levels=c(0, 1),
                      labels=c("non-partcp", "partcp"))

form <- as.formula(faet ~.)
dt1 <- rpart(form, data = train1, control = rpart.control(minsplit = 10, cp = 0.01))

plot(dt1)
text(dt1, use.n = TRUE, xpd = TRUE)

prp(dt1)
prp(dt1, varlen = 25, extra = 1)
rpart.plot(dt1, type = 1, extra = 2, digits = 3, tweak = 1.35,
           box.palette = "Blues", fallen.leaves = FALSE, shadow.col = "gray") #size: 1200/800
fancyRpartPlot(dt1, palettes = "Blues")

#revised (using "partcp")
rpart.plot(dt1, type = 1, extra = 2, digits = 3, tweak = 1.6, space = 0,
           box.palette = "Blues", fallen.leaves = FALSE, shadow.col = "gray")

dt2 <- train(faet ~., data = train1, method = "ctree2",
             trControl = trainControl("cv", number = 10),
             tuneGrid = expand.grid(maxdepth = 3, mincriterion = 0.01))
plot(dt2$finalModel)
