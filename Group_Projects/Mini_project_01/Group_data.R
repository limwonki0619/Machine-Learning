# level test, 1조 임원기 20190823

getwd()
setwd('D:/limworkspace/Machine-Learning/Group_Projects/Mini_project_01')
data = read.csv('sales.csv')[,c(1:9)]
str(data)

data2 = data[data$CATEGORY == '건강음료',]
shapiro.test(data2$QTY)

qqnorm(data2$QTY); qqline(data2$QTY, col=2)

write.csv(data2, 'sales_건강음료.csv')