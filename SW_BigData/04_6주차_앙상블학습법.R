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

#bagging(formaula, data=train_data, mfial = number)

install.packages("adabag")
library(adabag)
iris.bagging <- bagging(Species~., data=iris, mfinal=10)

#분류시 변수별 중요도, 모델 결과$importance
iris.bagging$importance

# 도식화
plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

expect2 <- predict(iris.bagging, iris_test, type="response")
# -> Species를 예측하여 분류한 변수를 팩터화 한다.
expect2$class<-as.factor(expect2$class) 
confusionMatrix(expect2$class,iris_test$Species,mode="everything")

iris.ada <- boosting(Species~., data=iris_train, mfinal=10)
iris.ada$importance # 분류 영향정도
barchart(iris.ada$importance) # 도식화하여 변수의 중요도

plot(iris.ada$trees[[10]]) # tree 그림 그리기
text(iris.ada$trees[[10]])

expect3 <- predict(iris.ada, iris_test, type="response")
expect3$class<-as.factor(expect3$class)
confusionMatrix(expect3$class,iris_test$Species,mode="everything")

library(rpart)
data(stagec)

View(stagec) 

colSums(is.na(stagec))
stagec1 <- na.omit(stagec)

set.seed(1234) # 초기값 설정
# Random Sampling을 통해 70%와 30%로 구분한다
# stagec3가 오류나서 3을 1로 고침
ind <- sample(2, nrow(stagec1), replace=TRUE, prob=c(0.7, 0.3))

trainData <- stagec1[ind==1, ] # n=102개
testData <- stagec1[ind==2, ] # n=32

# randomForest 설치 및 로딩
install.packages("randomForest")
library(randomForest)

# 알고리즘 적용
### 여기서 에러남
#randomForest(Formula ., data=trainData, ntree= number)
# ntree ; tree 생성 개수 ( default =500)

rf <- randomForest(ploidy ~ ., data=trainData, ntree=100)

importance(rf)
varImpPlot(rf)
plot(rf)
legend("topright",colnames(rf$err.rate),cex=0.8,fill=1:4) #범례 정리

library(caret)
predict <- predict(rf, testData)
confusionMatrix(predict, testData$ploidy, method="everything"
                
                