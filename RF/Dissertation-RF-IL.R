IL <- read.csv("PIAAC17USr-v2-rf-il.csv", fileEncoding = 'UTF-8-BOM')
str(IL)
IL$il <- as.factor(IL$il)

install.packages("randomForest")
library(randomForest)

set.seed(27)
ind3 <- sample(2, nrow(IL), replace = T, prob = c(0.7, 0.3))
train3 <- IL[ind3 == 1,]
test3 <- IL[ind3 == 2,]

set.seed(27)
rf3 <- randomForest(il~., data = train3, importance = T, proximity = T)
print(rf3)

varImpPlot(rf3)
importance(rf3)

attributes(rf3)
rf3$importance
rf3$mtry
rf3$confusion

install.packages("caret")
library(caret)

plot(rf3)
t <- tuneRF(train3[, -15], train3[, 15],
            stepFactor = 0.5,
            plot = T,
            ntreeTry = 2000,
            trace = T,
            improve = 0.05)

rf3 <- randomForest(il~., data = train3, importance = T, proximity = T,
                   ntree = 2000, mtry = 3)
print(rf3)

varImpPlot(rf3)
importance(rf3)

install.packages("ggplot2")
install.packages("magrittr")
install.packages("dplyr")
library(ggplot2)
library(magrittr)
library(dplyr)

importance = importance(rf3)
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
ggsave(file = "il (MDA)-resize.png", width = 16, height = 8)
