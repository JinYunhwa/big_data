# 1.필요 라이브러리 로딩
#데이터 핸들링
library(readxl)
library(dplyr)
# 기술통계량
library(psych)
library(descr)
# 분류기법
library(rpart)
library(rpart.plot)
library(adabag)
library(randomForest)
library(caret)
# 시각화 (그래프로 표현)
library(ggplot2)

box_office <- read_excel("train_box_office.xlsx")

head(box_office)
tail(box_office)

View(box_office)

colSums(is.na(box_office))
