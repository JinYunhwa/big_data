# 데이터 셋 확인
# iris 데이터 셋은 R에서 제공하는 데이터임
# 데이터를 불러온 후, 데이터셋 구조를 살펴보자.

data("iris")
View(iris)
str(iris)

library(caret)
# 타겟 변수인 Species를 기준으로 각 종류별로 80%씩 추출하여 훈련용(train) 데이터 셋으로
iris_row_idx <- createDataPartition(iris$Species, p=0.8, list=FALSE) # list=FALSE => 추출한 정보를 factor로
iris_train <- iris[iris_row_idx,]
str(iris_train)

# iris-train 데이터 셋의 꽃 종류별 데이터 수 확인
table(iris_train$Species)

# test data set 생성

iris_test <- iris[-iris_row_idx,]
# iris_row_idx 벡터 내에 존재하는 인덱스는 제외한 행 추출
# "-" 기호 사용
table(iris_test$Species)

# summary
summary(iris_train)
summary(iris_test)

# 의사결정나무 생성하기
library(rpart)
# 아래에서 오류발생생
rpart(formula, iris_test, control)
iris_result <- rpart(Species ~., data=iris_train, control=rpart.control(minsplit=2))
library(rpart.plot)
rpart.plot(iris_result)

# 가지치기
iris_result$cptable
plotcp(iris_result)

iris_prune <- prune(iris_result, cp=0.0125)
rpart.plot(iris_prune)

# ctree 함수 이용

# install.packages("party")
library(party)

iris_ctree_result <- ctree(Species ~., data=iris_train, control = ctree_control(minsplit=2))
iris_ctree_result
plot(iris_ctree_result) # 도식화

# 모형 적용하여 예측 값 생성
expect <- predict(iris_prune, iris_test, type="class")
# 실제 종속변수
actual <- iris_test$Species
# 데이터셋 만들기
iris_performance <- data.frame(actual, expect)
# 3. 혼동행렬을 이용하여 모형평가 하기
confusionMatrix(expect,actual,mode="everything")

## ctree로 적용하기
# ctree 인 경우 범주형으로 예측값을 출력하고자 할때 “response” 사용

expect1 <- predict(iris_ctree_result, iris_test, type="response")
iris_performance1 <- data.frame(actual,expect1)
table(iris_performance1)

confusionMatrix(expect1,actual,mode="everything")

