#-------------------------------------
# juyoung's paper graph
# 2020.07.28
#-------------------------------------

setwd("S:/GitHub/toy-project/regression prediction graph_juyoung")
data <- read.csv("data.csv", header=TRUE)

model = lm(data$MCDR ~ data$PTCR, data = data)
plot(model)

library(ggplot2)

g <- ggplot(data, aes(PTCR, MCDR))+
  geom_point(shape="O", aes(size=n, colour=content)) +
  theme_classic() +
  xlab("Mast Cell Degranulation Ratio (%)") +
  ylab("Pain Threshoold Change Ratio (%)") +
  geom_text(x=13, y=90, label="y = 1.084x-27.14") +
  geom_text(x=11, y=85,  label="R² = 0.6682     ") +
  geom_text(x=11, y=80,  label="p < 0.0001      ") +
  stat_smooth(method = 'lm', color='black', size=1, level=0.95,
              span=0.9, se=F)
  
g
