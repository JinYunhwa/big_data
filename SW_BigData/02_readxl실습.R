readxl_data <- read_excel("sample1.xlsx")
head(readxl_data)
tail(readxl_data)
dim(readxl_data)
str(readxl_data)

readxl_data$ID <- as.character(readxl_data$ID)
readxl_data$Gender <- as.character(readxl_data$Gender)
readxl_data$AREA <- as.character(readxl_data$AREA)

str(readxl_data)

summary(readxl_data)
describe(readxl_data[,c(3,5:8)])

hist(readxl_data$AGE,
     xlim=c(0,60),
     ylim=c(0,5),
     main="연령대 분포",
     xlab="연령대",
     ylab="빈도수",
     col="Yellow",
     breaks=8
     )

quantile(readxl_data$AMT17)

boxplot(readxl_data$AMT17, readxl_data$AMT16)

boxplot(readxl_data$AMT17, readxl_data$AMT16,
        ylim =c(0,1500000), # y축조정
        main = "카드사용 BoxPlot", # “제목＂
        names =c("17년 사용액","16년 사용액"), # 박스플랏 명“
        col = c("green","yellow")
        ) # 박스프랏 색상

outlier <- c(1,2,3,4,5,6,7,8,80,30)
boxplot(outlier)

# 3. 범주형 변수별 수치형 변수
describeBy(readxl_data$AMT16, group=readxl_data$Gender)

# 사용건수, 사용금액, 지역별로
describeBy(readxl_data[,c(5:8)], group=readxl_data$AREA)

# 4. 범주형 변수별 히스토그램

par(mfrow=c(1,2))
hist(readxl_data$AMT17[readxl_data$Gender=="F"])


#

hist(readxl_data$AMT17)
hist(readxl_data$AMT17[readxl_data$Gender=="F"])
hist(readxl_data$AMT17[readxl_data$Gender=="M"])

hist(readxl_data$AMT17[readxl_data$Gender=="F"],
     main="2017카드사용액(여성)",
     xlab="카드사용액",
     ylab="빈도수",
     col="yellow"
     )
hist(readxl_data$AMT17[readxl_data$Gender=="M"],
     main="2017카드사용액(남성)",
     xlab="카드사용액",
     ylab="빈도수",
     col="yellow"
     )

# 5번 문제

boxplot (readxl_data$AMT17 ~ readxl_data$Gender)
boxplot(readxl_data[, 5] ~ readxl_data[, 2])
readxl_data <-as.data.frame(readxl_data)
boxplot(AMT17 ~ Gender, data=readxl_data)


# 6번 문제
cor(readxl_data[,c(5:8)])


plot(readxl_data[,c(5:8)])

plot(readxl_data$AMT17,readxl_data$Y17_CNT,
     main = "2017
년 카드사용액과 사용건수", # 제목
     xlab = "사용액",
     ylab = "사용건수",
     xlim= c(0,1500000),
     cex = 3,
     pch= "*",
     col= "blue")

#7번 문제
plot(readxl_data$AMT17,readxl_data$Y17_CNT)

install.packages("descr")
library(descr)

# 8번
plot(readxl_data[,c(5:8)])
# 10번

barplot(table(readxl_data$Gender),
        main = "연령대분포",
        names =c("여성","남성"))

pie(table(readxl_data$AREA),
    main ="지역분포", # 파이차트명
    labels=c("서울","경기","인천","제주"), # label순서
    col=rainbow(4)) # 범주별 색상

#11번 

install.packages("dplyr")
library(dplyr)

readxl_data %>%
  group_by(Gender) %>%
  summarise(Sum_AMT <- sum(AMT),
            MEAN_AMT <- mean(AMT),
            MIN_AMT <- min(AMT),
            MAX_AMT <- max(AMT),
            n=n()) 
