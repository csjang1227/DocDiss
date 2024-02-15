##### LCA Elbow Plot #####
ep <- read.csv("PIAAC17USr-v3-R2-ep.csv", fileEncoding = 'UTF-8-BOM')

install.packages("reshape2")
install.packages("dplyr")
install.packages("ggplot2")
library(reshape2)
library(dplyr)
library(ggplot2)

epT1 <- melt(ep, id.vars = "group")
epT1 <- group_by(epT1, group)

label <- c('2', '3', '4', '5', '6', '7')

ggplot(epT1, aes(variable, value, color = group)) +
  geom_line(aes(group = group, color = group, linetype = group)) +
  geom_point(aes(color = group, shape = group), size = 2) +
  labs(color = "Class", linetype = "Class", shape = "Class",
       x = "Class", y = "Value") +
  scale_x_discrete(labels = label) +
  theme(legend.title = element_blank(), text = element_text(size = 12)) #font size = 12
  ylim(11130, 11530)
ggsave(file = "Elbow plot-v3-R2-resize-16.png", width = 16, height = 8)
ggsave(file = "Elbow plot-v3-R2-resize-12.png", width = 12, height = 6)
