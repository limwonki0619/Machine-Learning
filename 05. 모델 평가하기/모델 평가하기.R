setwd("D:/limworkspace/Machine-Learning/05. 모델 평가하기")

data <- read.csv('data/disease.csv')

head(data)

# 나이, 사회경제적위치, 지역, 질병 여부 

model <- glm(disease~., data=data, family = binomial(logit)) # 로지스틱 회귀분석 
summary(model)

model2 <- glm(disease~ age+sector, data=data, family = binomial(logit)) # 유의하지 않은 변수를 제거한 모델 
summary(model2)

anova(model, model2, test="Chisq") # 두 모델은 독립적이다 

table(data$disease)
31/98

# 혼동 행렬 
CM <- table(data$disease, model2$fitted.values > 0.31) # 실제값, 예측값 > (31/98) 실제 질병 발병 비율 
CM

# 0 = False, 1 = True
c(정확도 = (CM[1,1] + CM[2,2]) / sum(CM),
  민감도 =  CM[2,2] / sum(CM[2,]),
  특이도 =  CM[1,1] / sum(CM[1,]),
  에러율 = (CM[1,2]+CM[2,1]) / sum(CM))


# 모델의 ROC curve 시각화 
install.packages('Deducer')
library(Deducer)
rocplot(model2)


# 
install.packages('caret')
library(caret)
idx = createDataPartition(iris$Species, p=0.7, list=F)

iris_train <- iris[idx, ]
iris_test <-  iris[-idx, ]

table(iris_train$Species) # 데이터셋 비율 확인 
table(iris_test$Species)

# 모델 평가 
library(rpart)
library(e1071)
library(randomForest)

rpart_tr <- rpart(Species~., data=iris_train)      # 의사결정나무 모델 
bayes_tr <- naiveBayes(Species~., data=iris_train) # 나이브 베이지 모델 
rf_tr <- randomForest(Species~., data=iris_train)  # 랜덤포레스트 모델 

rpart_pdt <- predict(rpart_tr, newdata = iris_test, type='class')
bayes_pdt <- predict(bayes_tr, newdata = iris_test, tpye='class')
rf_pdt <- predict(rf_tr, newdata = iris_test, type='response')

table(iris_test$Species, rpart_pdt)
table(iris_test$Species, bayes_pdt)
table(iris_test$Species, rf_pdt)

# 혼동 행렬(Confusion Matrix)
confusionMatrix(rpart_pdt, iris_test$Species)
confusionMatrix(bayes_pdt, iris_test$Species)
confusionMatrix(rf_pdt, iris_test$Species)


# 좀더 발전된 변수선택법 
install.packages('leaps')
library(leaps)

head(attitude)
out <- lm(rating~., data=attitude)
summary(out)

out2 <- step(out, direction = 'both')
summary(out2)

leaps <- regsubsets(rating~., data=attitude, nbest = 5)
summary(leaps)

plot(leaps, scale = 'bic') # bic는 가장 적을수록 좋은 모형 
out_bic <- lm(rating~complaints, data=attitude)
summary(out_bic)

plot(leaps, scale='Cp') # CP도 가장 적을수록 좋은 모형 
out_cp <- lm(rating~complaints+learning, data=attitude)
summary(out_cp)

plot(leaps, scale = "adjr2")
out_adjr2 <- lm(rating ~ complaints+learning+advance, data=attitude)
summary(out_cp)

