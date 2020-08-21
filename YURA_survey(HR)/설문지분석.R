#-----------------------------------------------------
# 사내 설문조사 데이터 분석
#
# 분석내용
# 1. 설문 신뢰도: 크론바흐알파계수(신뢰도 측정 가능한 설문문항 대상)
# 2. 직급/직책 별 응답문항 차이여부
#
# 일자: 2019.12.05
#-----------------------------------------------------

library(readxl)

setwd("S:/GitHub/toy-project/YURA_survey(HR)")
data <- read_excel("data/2020 니즈조사결과취합.xlsx", sheet = "Sheet1")

str(data)
data <- data[-1,]

# column name
colnames(data)[3:34] <- paste("Q", colnames(data)[3:34], sep="_")


#-------------------------------------------------------------------
# 1. 설문 신뢰도: 크론바흐알파계수(신뢰도 측정 가능한 설문문항 대상)
#-------------------------------------------------------------------

# value: to ordinal
sum(is.na(data$Q_1번))
data$Q_1번 <- ifelse(data$Q_1번 == "매우 그렇다", 5,
                    ifelse(data$Q_1번 == "그렇다", 4,
                           ifelse(data$Q_1번 == "보통이다", 3,
                                  ifelse(data$Q_1번 == "아니다", 2, 1))))

sum(is.na(data$Q_3번))
data$Q_3번 <- ifelse(data$Q_3번 == "매우 그렇다", 5,
                    ifelse(data$Q_3번 == "그렇다", 4,
                           ifelse(data$Q_3번 == "보통이다", 3,
                                  ifelse(data$Q_3번 == "아니다", 2, 1))))


sum(is.na(data$Q_4번))
table(data$Q_4번)
data$Q_4번 <- ifelse(data$Q_4번 == "매우 그렇다", 5,
                    ifelse(data$Q_4번 == "그렇다", 4,
                           ifelse(data$Q_4번 == "보통이다", 3,
                                  ifelse(data$Q_4번 == "아니다", 2, 1))))


sum(is.na(data$Q_6번))
table(data$Q_6번)
data$Q_6번 <- ifelse(data$Q_6번 == "매우 그렇다", 5,
                    ifelse(data$Q_6번 == "그렇다", 4,
                           ifelse(data$Q_6번 == "보통이다", 3,
                                  ifelse(data$Q_6번 == "아니다", 2, 1))))

sum(is.na(data$Q_8번))
table(data$Q_8번)
data$Q_8번 <- ifelse(data$Q_8번 == "매우 그렇다", 5,
                    ifelse(data$Q_8번 == "그렇다", 4,
                           ifelse(data$Q_8번 == "보통이다", 3,
                                  ifelse(data$Q_8번 == "아니다", 2, 1))))


Q <- data[,c("Q_3번", "Q_4번", "Q_6번", "Q_8번")]
pairs(Q, panel=panel.smooth)


# 크론바흐알파계수 
install.packages("psy")
library(psy)
cronbach(Q)


install.packages("psych")
library(psych)
alpha(Q)





#-------------------------------------------------------------------
# 2. 직급/직책 별 응답문항 차이여부
#-------------------------------------------------------------------
sum(is.na(data$Q_20번))
table(data$Q_20번)
data$Q_20번 <- ifelse(data$Q_20번 == "매우 그렇다", 5,
                     ifelse(data$Q_20번 == "그렇다", 4,
                            ifelse(data$Q_20번 == "보통이다", 3,
                                   ifelse(data$Q_20번 == "아니다", 2, 1))))

sum(is.na(data$Q_28번))
table(data$Q_28번)
data$Q_28번 <- ifelse(data$Q_28번 == "매우 그렇다", 5,
                     ifelse(data$Q_28번 == "그렇다", 4,
                            ifelse(data$Q_28번 == "보통이다", 3,
                                   ifelse(data$Q_28번 == "아니다", 2,
                                          ifelse(data$Q_28번 == "매우아니다", 1, 0)))))


Q2 <- data[,c("Q_20번", "Q_28번")]
alpha(Q2)


data$Q_31번 <- as.factor(data$Q_31번)
data$Q_4번 <- as.factor(data$Q_4번)
table(data$Q_24번)

data$Q_10번 <- as.factor(data$Q_10번)

data$Q_24번 <- ifelse(data$Q_24번 == "OJT(일을 통한 학습)", "A",
                     ifelse(data$Q_24번 == "사내교육(사내강사)", "B",
                            ifelse(data$Q_24번 == "사외교육(외부전문가)", "C",
                                   ifelse(data$Q_24번 == "온라인교육", "D",
                                          ifelse(data$Q_24번 == "독서", "E",
                                                 ifelse(data$Q_24번 == "자습", "F", "G"))))))

table(data$Q_25번)
data$Q_26번 <- ifelse(substr(data$Q_26번, 1, 2) == '기타', "기타", data$Q_26번)
data$Q_27번 <- ifelse(substr(data$Q_27번, 1, 2) == '기타', "기타", data$Q_27번)

chisq.test(data$Q_10번, data$Q_28번)



# 직급별 응답 시각화
library(ggplot2)
ggplot(data, aes(x=data$Q_31번, fill=data$Q_15번)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  #xlab("직급") +
  #ylab("") +
  coord_flip() +
  scale_fill_discrete(limits = c("매우 그렇다", "그렇다", "보통이다", "아니다", "매우 아니다"))
