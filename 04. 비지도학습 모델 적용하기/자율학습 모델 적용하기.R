setwd("D:/limworkspace/Machine-Learning/04. 비지도학습 모델 적용하기")

# 자율학습 모델 적용하기

# 4.1 클러스터링(군집) 분석 

# (1) 클러스터링(군집) 분석 ------------------------------------------------------------------

iris2 <- iris[, -5] # 종속변수 제거

km_out_withness <- c() # 집단 내 제곱합(SSW) 리스트 생성 
km_out_between <- c()  # 집단 간 제곱합(SSB) 리스트 생성 

# 적절한 군집 수 찾기 
for (k in 2:7) {
  set.seed(1)
  km_out <- kmeans(iris2, centers = k)
  km_out_withness[k-1] <- km_out$tot.withinss
  km_out_between[k-1] <- km_out$betweenss
}

km_df <- data.frame(k=c(2:7), km_out_withness, km_out_between)
km_df

par(mfrow=c(1,2))
plot(x=km_df$k, y=km_df$km_out_withness, type='o', col='red')
plot(x=km_df$k, y=km_df$km_out_between, type='o', col='deepskyblue')


# 군집분석(K=3)

km_out_k3 <- kmeans(iris2, centers = 3)
km_out_k3$centers # 각 군집의 중심점 출력 
km_out_k3$cluster # 각 관측치의 할당된 군집번호 출력 
km_out_k3$size # 각 군집의 데이터 관측치 개수 출력 

table(km_out_k3$cluster, iris$Species) # 군집결과와 원래 품종개수 비교 


# 시각화 
par(mfrow=c(1,1))
plot(iris2[,1:2], col=km_out_k3$cluster, pch=ifelse(km_out_k3$cluster==1, 16, 
                                             ifelse(km_out_k3$cluster==2, 17, 18)), cex=2)

points(km_out_k3$centers, col=1:3, pch=16:18, cex=5)



# 차원축소 기법 ------------------------------------------------------------------------


crime = read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
head(crime)

rownames(crime) <- crime[,1] 
stars(crime[, 2:8], flip.labels = FALSE, key.loc = c(1,20), draw.segments = T)


# (1) 주성분 분석(Principal Component Analysis)

# prcomp 함수 
data <- read.csv('data/20140528_baseball.csv')

rownames(data) <- data[,1]
data
model <- prcomp(data[, 2:6],scale=T)
summary(model)
plot(model)

biplot(model)

# princomp 함수 
head(USArrests)
pc1 = princomp(USArrests, cor=T) # 상관행렬 사용 
summary(pc1) # 주성분 분석 결과 요약 

plot(pc1, type='l') # Scree plot
pc1$center
pc1$scale
pc1$loadings # 원 변수들이 각 주성분 변수에 기여하는 가중치 확인 
# 1주성분에는 각 변수들이 Urbanpop을 제외한 나머지 3개 범죄수가 비슷한 기여를 하지만,
# 2주성분에는 Murder와 Assault, Urbanpop과 Rape 변수간에 부호가 다르다.
# 그러나 로딩된 값의 크기는 Urbanpop과 Murder가 제2주성분에 많이 로딩되었다. 
# 따라서 2주성분은 살인범죄 수와 도시 인구 비율 정보를 많이 담고있는 주성분이다. 
pc1$scores

plot(pc1$scores[,1], pc1$scores[,2], xlab='Z1', ylab='Z2')

biplot(pc1, cex=0.7) # 행렬도 
abline(v=0, h=0, col="gray")

# 1주성분에는 Assault, Rape, Murder가 많은 영향을 미침
# 2주성분에는 UrbanPop, Rape가 많은 영향을 미침 




# (2) 연관성 분석 ----------------------------------------------------------------------------------------------------

install.packages('arules')    # 연관성 분석 패키지 
install.packages('arulesViz') # 연관성 분석 시각화 패키지 

library('arules') 
library('arulesViz')

# 데이터 탐색적 분석 

data(package='arules')
class(Groceries)          # 희박행렬 형태(Sparse Format)
inspect(Groceries[1:10])  # 희박행렬 데이터 확인 

summary(Groceries) # 트렌잭션 데이터 요약 

head(sort(itemFrequency(Groceries, type='absolute'), decreasing = T)) # type = 'absolute' 절대도수 

itemFrequencyPlot(Groceries, topN=10, type='absolute', col=rainbow(10))


# 연관성 분석 

apriori(Groceries) # 지지도(0.1), 신뢰도(0.8)이 기본값 
# 기본설절된 값으로 연관규칙을 찾을 수 없다. 
# 따라서 적절한 지지도와 신뢰도를 선정해줘야 하낟.


result <- apriori(Groceries, parameter = list(support=0.005, confidence=0.5, minlen=2)) # minlen : 규칙의 최소 길이, 즉 아이템이 2개 이상인 경우만 생성 
result

summary(result)
inspect(result[1:5]) # 연관성 확인 


# 향샹도를 기준으로 정렬 

result_lift <- sort(result, by='lift', decreasing=T) # 향상도 기준으로 내림차순 
inspect(result_lift[1:5])
# [1] : yogurt를 살 때, toripical fruit, curd와 같이 살 확률이 그렇지 않은 경우에 비해 3.69배 높다는 의미 


milk_rule_both <- subset(result, items %in% "whole milk") # lhs or rhs에 "whole milk"가 있는 경우 
milk_rule_both
inspect(sort(milk_rule_both, by="lift", decreasing = T)[10:15])

milk_rule_rhs <- subset(result, rhs %in% "whole milk") # rhs에 "whole milk"가 있는 경우 
milk_rule_rhs
inspect(sort(milk_rule_rhs, by="lift", decreasing = T)[1:5])


# 시각화 
plot(milk_rule_both[1:10], method = 'graph', measure = 'lift', shading = 'confidence')

