#---------------------------------------------------------
# 주영 논문 상관계수 그래프
#---------------------------------------------------------

install.packages(c("readxl", "dplyr", "corrplot"))
library(readxl); library(dplyr); library(corrplot)


# 데이터 불러오기
setwd("S:/GitHub/toy-project/correlation plot_juyoung")
data <- read_xlsx("connectome.xlsx", sheet="Sheet1")

# control 만 뽑기
CONT <- dplyr::filter(data, grepl("Control", group))
#뽑고싶은애들기리 Control, MPTP 이런거 적어


# 상관계수 구하기
cor_CONT <- cor(CONT[,-1], method = "pearson", use = "complete.obs")

# color 빠레뜨 
col <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
                          "yellow", "#FF7F00", "red", "#7F0000"))

# 그림그리기
corrplot(cor_CONT, method = "color", tl.col='black', tl.srt=90, col = col(100))


# R2 <= 0.427
tmp <- cor_CONT

for(i in 1:30){
  for(j in 1:30){
    ifelse(tmp[i,j]^2 <= 0.427, tmp[i,j] <- 0, tmp[i,j] <- tmp[i,j]^2)
}}


corrplot(tmp, cl.lim=c(0, 1),  col=col(100), tl.col='black', method="color")

