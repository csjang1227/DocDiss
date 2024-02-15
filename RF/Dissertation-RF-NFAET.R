NFAET <- read.csv("PIAAC17USr-v2-rf-nfaet.csv", fileEncoding = 'UTF-8-BOM')
str(NFAET)
NFAET$nfaet <- as.factor(NFAET$nfaet)

install.packages("randomForest")
library(randomForest)

set.seed(2467) #2478
ind2 <- sample(2, nrow(NFAET), replace = T, prob = c(0.7, 0.3))
train2 <- NFAET[ind2 == 1,]
test2 <- NFAET[ind2 == 2,]

set.seed(2467)
rf2 <- randomForest(nfaet~., data = train2, importance = T, proximity = T)
print(rf2)

varImpPlot(rf2)
importance(rf2)

attributes(rf2)
rf2$importance
rf2$mtry
rf2$confusion

install.packages("caret")
library(caret)

plot(rf2)
t <- tuneRF(train2[, -15], train2[, 15],
            stepFactor = 0.5,
            plot = T,
            ntreeTry = 2000,
            trace = T,
            improve = 0.05)

rf2 <- randomForest(nfaet~., data = train2, importance = T, proximity = T,
                    ntree = 2000, mtry = 3)
print(rf2)

varImpPlot(rf2)
importance(rf2)

install.packages("ggplot2")
install.packages("magrittr")
install.packages("dplyr")
library(ggplot2)
library(magrittr)
library(dplyr)

importance = importance(rf2)
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
ggsave(file = "nfaet (MDA)-resize.png", width = 16, height = 8)
