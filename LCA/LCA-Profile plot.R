##### LCA: 4 Groups #####
lca1 <- read.csv("PIAAC17USr-v3-R2-pp4_rev.csv", fileEncoding = 'UTF-8-BOM')

install.packages("reshape2")
install.packages("dplyr")
install.packages("ggplot2")
library(reshape2)
library(dplyr)
library(ggplot2)

lcaR1 <- melt(lca1, id.vars = "group")
lcaR1 <- group_by(lcaR1, group)

# Original profile plot
ggplot(lcaR1, aes(variable, value, color = group)) +
  geom_line(aes(group = group, color = group)) +
  labs(color = "Group", x = "Indicator", y = "Probability") +
  ylim(0.15, 1.00)

# Profile plot w/ linetype & shape
ggplot(lcaR1, aes(variable, value, color = group)) +
  geom_line(aes(group = group, color = group, linetype = group)) +
  geom_point(aes(color = group, shape = group), size = 2) +
  labs(color = "Class", linetype = "Class", shape = "Class",
       x = "Indicator", y = "Probability") +
  theme(text = element_text(size = 12)) + #font size = 12
  theme(legend.position = "bottom", legend.direction = "vertical", legend.title = element_blank()) + #legend positioning
  ylim(0.15, 1.00)
ggsave(file = "Profile plot-v3-R2-4 grpR-resize-16.png", width = 16, height = 8)
ggsave(file = "Profile plot-v3-R2-4 grpR-resize-12.png", width = 12, height = 6)

# Profile plot w/ linetype & shape by groups
ggplot(lcaR1, aes(variable, value, color = group, group = group)) +
  geom_line(aes(linetype = group)) +
  geom_point(aes(shape = group), size = 2) +
  facet_wrap(~group, nrow = 2) +
  labs(color = "Class", linetype = "Class", shape = "Class", 
       x = "Indicator", y = "Probability") +
  theme(text = element_text(size = 12)) + #font size = 12
  theme(legend.position = "none") + #legend positioning
  ylim(0.15, 1.00)
ggsave(file = "Profile plot-v3-R2-4 grpR sep-resize-16.png", width = 16, height = 8)
ggsave(file = "Profile plot-v3-R2-4 grpR sep-resize-12.png", width = 12, height = 6)

##### LCA: 3 Groups #####
lca2 <- read.csv("PIAAC17USr-v3-R2-pp3.csv", fileEncoding = 'UTF-8-BOM')

install.packages("reshape2")
install.packages("dplyr")
install.packages("ggplot2")
library(reshape2)
library(dplyr)
library(ggplot2)

lcaR2 <- melt(lca2, id.vars = "group")
lcaR2 <- group_by(lcaR2, group)

# Original profile plot
ggplot(lcaR2, aes(variable, value, color = group)) +
  geom_line(aes(group = group, color = group)) +
  labs(color = "Group", x = "Indicator", y = "Probability") +
  ylim(0.05, 0.70)

# Profile plot w/ linetype & shape
ggplot(lcaR2, aes(variable, value, color = group)) +
  geom_line(aes(group = group, color = group, linetype = group)) +
  geom_point(aes(color = group, shape = group), size = 2) +
  labs(color = "Class", linetype = "Class", shape = "Class",
       x = "Indicator", y = "Probability") +
  ylim(0.10, 1.00)

# Profile plot w/ linetype & shape by groups
ggplot(lcaR2, aes(variable, value, color = group, group = group)) +
  geom_line(aes(linetype = group)) +
  geom_point(aes(shape = group), size = 2) +
  facet_wrap(~group, ncol = 1) +
  labs(color = "Class", linetype = "Class", shape = "Class", 
       x = "Indicator", y = "Probability") +
  ylim(0.10, 1.00)
