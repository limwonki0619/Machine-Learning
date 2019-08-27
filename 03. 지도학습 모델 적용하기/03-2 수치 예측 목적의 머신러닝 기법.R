setwd("D:/limworkspace/Machine-Learning/03. 지도학습 모델 적용하기")

# 3.2 수치 예측 목적의 머신러닝 기법 적용 

library(MASS)

# 데이터 샘플링 --------------------------------------------------------------

str(Boston)

idx <- sample(1:nrow(Boston), size=nrow(Boston)*0.7, replace = F) # sample(데이터, 크기, 비복원추출)

Boston_train <- Boston[idx,]
Boston_test <- Boston[-idx,]

dim(Boston_train); dim(Boston_test)

# 변수 설명 

# medv : 주택 가격 중앙값(종속변수)
# 나머지 12개의 독립변수 


# (1) 다중회귀분석 -----------------------------------------------------------

lm_fit <- lm(medv~., data=Boston_train)
summary(lm_fit) # indus, age 변수는 유의하지 않은 것으로 나타남, adj R : 0.74 

# 변수 선택법을 사용한 다중회귀분석 
# foward : 가장 유의한 변수부터 모델에 추가  
# backward : 가장 유의하지 않은 변수를 삭제 
# both : forward + both 

lm_fit2 <- step(lm_fit, moethod='both')
summary(lm_fit2)

lm_pdt <- predict(lm_fit2, newdata=Boston_test) # 평가 데이터 이용, 예측 결과 생성 
lm_pdt_95 <- predict(lm_fit2, newdata=Boston_test, interval = 'confidence') # 평가 데이터 이용, 예측 결과 생성, 95% 신뢰구간  
lm_pdt_99 <- predict(lm_fit2, newdata=Boston_test, interval = 'prediction') # 평가 데이터 이용, 예측 결과 생성, 99% 신뢰구간  

par(mfrow=c(2,2))
plot(lm_fit2)
par(mfrow=c(1,1))

GetRMSE <- function(pred, test_data){
  MSE <- mean((pred - test_data)^2) # 평균제곱오차 
  print(paste0('MSE : ', round(MSE, 2)))
  print(paste0('RMSE : ', round(sqrt(MSE), 2)))
}

GetRMSE(lm_pdt, Boston_test$medv)

# (2) 의사결정트리 -----------------------------------------------------------------------------

# 1. tree 패키지의 tree() 함수 ------------------

install.packages('tree')
library(tree)

# 모델 적용 
tree_fit <- tree(medv~., data=Boston_train)
summary(tree_fit)

# 그래프 생성 
plot(tree_fit)
text(tree_fit,pretty=0)

# 모델 평가 
tree_pdt <- predict(tree_fit, newdata = Boston_test)
GetRMSE(tree_pdt, Boston_test$medv)

# 2. rpart 패키지의 rpart() 함수 -------------------

library(rpart)

# 모델 적용 
rpart_fit <- rpart(medv~.,data=Boston_train)
summary(rpart_fit)

# 그래프 생성 
plot(rpart_fit)
text(rpart_fit, pretty = 0)

# 모델 평가 
rpart_pdt <- predict(rpart_fit, newdata = Boston_test)
GetRMSE(rpart_pdt, Boston_test$medv)

# 보기 좋은 그래프 생성 패키지 
install.packages('rpart.plot')
library(rpart.plot)

rpart.plot(rpart_fit, digits = 3, type = 1, extra=1, fallen.leaves = T, cex=1) 
# rpart.plot(데이터, 소수점 자리수, 노드배치 타입, n 포함 여부, 그래프 출력 타입, 글자크기)





# (3) 인공 신경망 -----------------------------------------------------------------------

# 인공 신경망 모델을 적용하기 위해서는 변수들을 정규화 혹은 표준화해야하낟.
# 표준화는 scale 함수를 사용하면 되는데, 데이터가 정규성을 따르지 않는다면 
# 표준화 보다는 0 ~ 1 사이로 정규화를 하는 것이 분석 시에 보다 유리하다.
# 여기서는 MinMaxScaling 방법을 사용자 정의 함수를 사용해 변수를 정규화 한다.

# 데이터 정규화 

normalize <- function(x){ return( (x - min(x)) / (max(x) - min(x)) )} # MinMaxScaling 
Boston_train_norm <- as.data.frame(sapply(Boston_train, normalize))   
Boston_test_norm <- as.data.frame(sapply(Boston_test, normalize))

# 데이터셋 비교 

head(Boston_test$medv, 2)
head(Boston_test_norm$medv, 2)


# nnet 함수를 사용한 인공신경망 모델 
library(nnet)
nnet_fit <- nnet(medv~., data=Boston_train_norm, size=5) 
nnet_pdt <- predict(nnet_fit, newdata=Boston_test_norm, type='raw')
GetRMSE(nnet_pdt, Boston_test_norm$medv)


# neuralnet 함수를 사용한 인공신경망 모델 
install.packages('neuralnet')
library(neuralnet)

colnames(Boston_train)

neural_fit <- neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,
                        data=Boston_train_norm, hidden=5) # 독립변수를 모두 입력해야하는 번거로움, hidden : 출력층의 노드 

neural_result <- compute(neural_fit, Boston_test_norm[1:13]) # 인공신경망 모델 적용 
neural_pdt <- neural_result$net.result     # 예측 결과
GetRMSE(neural_pdt, Boston_test_norm$medv) # 모델 평가 

# 이 패키지의 장점은 시각화가 가능함 
plot(neural_fit)



# (4) 앙상블 기법 중 랜덤 포레스트 기법 ------------------------------------------------------

library(randomForest)
set.seed(1)

rf_fit <- randomForest(medv~., data=Boston_train, mtry=6, importance=T) # mtry : 독립변수 개수를 제한, importance : 변수 중요도를 나타내줌  
rf_fit

# 트리 개수 변화에 따른 오류 감소 추이 그래프 
plot(rf_fit)

# 변수 중요도 파악 
importance(rf_fit)

# 변수 중요도 시각화
varImpPlot(rf_fit)

# 모델 평가 
rf_pdt <- predict(rf_fit, newdata = Boston_test)
GetRMSE(rf_pdt, Boston_test$medv)


# 스케일링한 데이터를 가지고 해보자
rf_fit2 <- randomForest(medv~., data=Boston_train_norm, mtry=6, importance=T)
rf_pdt2 <- predict(rf_fit2, newdata = Boston_test_norm)
GetRMSE(rf_pdt2, Boston_test_norm$medv)



# 모든 모델링 방법 비교 

GetRMSE(lm_pdt, Boston_test$medv) # 다중회귀 

GetRMSE(tree_pdt, Boston_test$medv) # 의사결정나무 
GetRMSE(rpart_pdt, Boston_test$medv)

GetRMSE(nnet_pdt, Boston_test_norm$medv) # 인공신경망 
GetRMSE(neural_pdt, Boston_test_norm$medv)

GetRMSE(rf_pdt, Boston_test$medv) # 랜덤포레스트 
GetRMSE(rf_pdt2, Boston_test_norm$medv)

# 변수를 스케일링한 모델과 하지 않은 모델을 직접 비교할 순 없다. 
# 스케일링한 모델의 예측값을 다시 풀어줘야 한다. 
