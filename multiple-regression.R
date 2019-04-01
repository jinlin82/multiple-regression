rm=list()
getwd()
setwd("E://newfiles")
#一、读取数据
data=read.csv("E:/newfiles/exampledata.csv")
colnames(data)=c("year","gdp","invest","employment","consume","gdpdex","investdex","consumedex")
#二、数据处理，剔除价格因素影响
newdata=data.frame(data$year,data$gdp/data$gdpdex*100,data$invest/data$investdex*100,data$employment,data$consume/data$consumedex*100)
colnames(newdata)=c("year","gdp","invest","employment","consume")

#三、模型拟合
fit=lm(gdp~invest+employment+consume,newdata)#lm函数需要的是数据框，若是矩阵需要转换
summary(fit)
coefficients(fit)
confint(fit,level=0.95)#列出参数的置信区间
help("confint")
#fitted.values(fit)
#help("fitted")
anova(fit)
#各项系数的符号符合经济意义；固定资产投资和社会商品消费总额的系数不显著，R^2和F检验显示模型效果良好

##利用公式进行回归系数计算
x1=as.matrix(newdata[,c(3,4,5)])
x=cbind(rep(1,length(newdata$year)),x1)
y=as.matrix(newdata[,2])
beta=solve(t(x)%*%x)%*%t(x)%*%y
beta


#四、经济计量检验
##1.正态性检验
install.packages("car")
getwd()
#library(car)
help(scatterplotMatrix)
??scatterplotMatrix
data()
#.libPaths()#看R包安装到了地方
#installed.packages()
#available.packages()
library(car)
#install.packages("car")
plot(fit)
#正态性
#Normal q-q图中所有的点大致落在45度直线上，满足正态分布
qqPlot(fit,labels=row.names(newdata),id.method='identify',simulate=TRUE,main='Q-Q plot')
row.names(newdata)
#所有的点都落在置信区间内，表明正态性假设符合

##2.多重共线性
cor(newdata[3:5])
#employment和invest间的相关系数高达0.78，consume与invest0.99;employment与consume0.83，存在严重共线性，导致
#invest和consume 的系数不显著
install.packages("car")
library(car)
scatterplotMatrix(newdata,spread=FALSE,smoother.args=list(lty=2),main="sactter plot matrix")
vif(fit)
#sqrt(vif)>2就表明存在多重共线性

##3.误差序列的序列不相关性
durbinWatsonTest(fit)
#表明无序列相关性

##4.异方差检验
ncvTest(fit)
#表明同方差

#五、逐步回归
library(MASS)
stepAIC(fit,direction='backward')
#<none>中的AIC表示没有变量被删除时模型的AIC
help(stepAIC)
stepAIC(fit,direction='both')#根据AIC选择最优模型

#六、模型预测
##1.点预测
x0=c(1,200000,78000,140000)#默认生成的是列向量
y0=x0%*%beta
y0
#predict()与fitted.values()在没有新数据时，效果一样，都是输出拟合值

##2.方差估计
sigma=sqrt(sum((fit$residuals)^2)/21)


##3.置信区间估计函数
###公式计算
fun=function(x0){
  upper1=x0%*%beta+qt(0.975,df=21)*sqrt(sigma^2*(1+t(x0)%*%solve((t(x)%*%x))%*%x0))#个值预测
  lower1=x0%*%beta-qt(0.975,df=21)*sqrt(sigma^2*(1+t(x0)%*%solve((t(x)%*%x))%*%x0))
  upper2=x0%*%beta+qt(0.975,df=21)*sqrt(sigma^2*(t(x0)%*%solve((t(x)%*%x))%*%x0))#均值预测
  lower2=x0%*%beta-qt(0.975,df=21)*sqrt(sigma^2*(t(x0)%*%solve((t(x)%*%x))%*%x0))
  conf=data.frame(lower1,upper1,lower2,upper2)
}
conf=apply(x,1,fun)
conf
###函数计算
predict(fit,interval = "confidence")#相当于均值预测
predict(fit,interval = "prediction")#相当于个值预测

###计算x0=c(c(1,200000,78000,140000))的置信区间
confx0=fun(x0)
confx0

ninvest=c(200000,230000,250000)
nemployment=c(78000,78800,78800)
nconsume=c(140000,140000,140000)
new=data.frame(ninvest,nemployment,nconsume)
x1=matrix(c(200000,78000,140000,230000,78800,140000,250000,78800,140000),nrow = 3,byrow =T)
predict(fit,new)
predict(fit)
