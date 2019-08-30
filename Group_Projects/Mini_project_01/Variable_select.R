setwd("D:/limworkspace/Machine-Learning/Group_Projects/Mini_project_01")

data <- read.csv("data/sales_건강음료.csv")
head(data)

# 데이터 처리 -----------------------------------
library(stringr)

data['MON'] <- str_sub(data$YM, 5, 6)
head(data)

data1 <- data[,-c(1:2)]
data1$MON <- as.numeric(data1$MON)
head(data1)

# 다중회귀모델 ----------------------------------
model <- lm(QTY~., data=data1)
summary(model)

# 단계선택법 ------------------------------------
model_step <- step(model, method='both')
summary(model)

# 좀더 발전된 변수선택방법 ----------------------
install.packages('leaps')
library(leaps)

leaps <- regsubsets(QTY~., data=data1, nbest=3)
plot(leaps, scale = 'bic')
plot(leaps, scale = 'Cp')
plot(leaps, scale = 'adjr2')


# 모델 선택하기 ---------------------------------
bic_model <- lm(QTY~ PRICE + SALEDAY + RAIN_DAY + MON, data=data1)
summary(bic_model)

Cp_model <- lm(QTY ~ ITEM_CNT + PRICE + MAXTEMP + SALEDAY + RAIN_DAY + MON, data=data1)
summary(Cp_model)

adjr2_model <- lm(QTY ~., data=data1)
summary(adjr2_model)



