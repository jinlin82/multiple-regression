## ----setup, echo=F-------------------------------------------------------
knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(echo = FALSE, results = 'hide')
knitr::opts_chunk$set(warning = FALSE, message=FALSE)

## ----prepare-------------------------------------------------------------
rm(list=ls())
options(digits=4)
options(scipen=100)
graphics.off()
Sys.setlocale("LC_ALL", "Chinese")

## ----tab-11, eval=T,results='markup', cache=F----------------------------
tab1 <- read.csv('.\\result\\varience analysis.csv')
knitr::kable(tab1, row.names =F, align = "l", caption="方差分析表",
      longtable = TRUE, booktabs = TRUE, linesep  = "")

## ----tab-12, eval=T,results='markup', cache=F----------------------------
tab1 <- read.csv('.\\result\\varience analysis.csv')
knitr::kable(tab1, row.names =F, align = "l", caption="方差分析表",
      longtable = TRUE, booktabs = TRUE, linesep  = "")

## ----tab-13, eval=T,results='markup', cache=F----------------------------
tab1 <- read.csv('.\\result\\varience analysis.csv')
knitr::kable(tab1, row.names =F, align = "l", caption="方差分析表",
      longtable = TRUE, booktabs = TRUE, linesep  = "")

## ----tab-14, eval=T,results='markup', cache=F----------------------------
tab1 <- read.csv('.\\result\\varience analysis.csv')
knitr::kable(tab1, row.names =F, align = "l", caption="方差分析表",
      longtable = TRUE, booktabs = TRUE, linesep  = "")

## ----tab-15, eval=T,results='markup', cache=F----------------------------
tab1 <- read.csv('.\\result\\varience analysis.csv')
knitr::kable(tab1, row.names =F, align = "l", caption="方差分析表",
      longtable = TRUE, booktabs = TRUE, linesep  = "")

## ----tab-1, eval=T,results='markup', cache=F-----------------------------
tab1 <- read.csv('.\\result\\varience analysis.csv')
knitr::kable(tab1, row.names =F, align = "l", caption="多元线性回归方差分析表",
      longtable = TRUE, booktabs = TRUE, linesep  = "")

## ----tab-4-1, eval=T,results='markup', cache=F---------------------------
tab2 <- read.csv('.\\result\\example data.csv')
knitr::kable(tab2, row.names =F, align = "l", caption="影响我国2000-2017年国内生产总值的主要因素",
      longtable = TRUE, booktabs = TRUE, linesep  = "")

## ----fig1, echo=FALSE, fig.cap="回归", cache=F, dev="png", results='markup'----
knitr::include_graphics(".\\result\\regression.png")

## ----fig2, echo=FALSE, fig.cap="数据输入", cache=F, dev="png", results='markup'----
knitr::include_graphics(".\\result\\dataentry.png")

## ----tab-9, eval=T,results='markup', cache=F-----------------------------
tab9 <- read.csv('.\\result\\regstatistic.csv')
knitr::kable(tab9, row.names =F, align = "l", caption="回归统计表",
      longtable = TRUE, booktabs = TRUE, linesep  = "")

## ----tab-10, eval=T,results='markup', cache=F----------------------------
tab10 <- read.csv('.\\result\\varience analysis.csv')
knitr::kable(tab10, row.names =F, align = "l", caption="方差分析表",
      longtable = TRUE, booktabs = TRUE, linesep  = "")

## ----tab-16, echo=FALSE, cache=F, results='markup'-----------------------
tab16 <- read.csv('.\\result\\coefficient.csv')
knitr::kable(tab16, row.names =F, align = "l", caption="回归系数表",
      longtable = TRUE, booktabs = TRUE, linesep  = "")

## ----include=FALSE-------------------------------------------------------
#一、读取数据
newdata=tab2
colnames(newdata)=c("year","gdp","invest","consume","employment")

#三、模型拟合
fit=lm(gdp~invest+consume+employment,newdata)#lm函数需要的是数据框，若是矩阵需要转换
beta=fit$coefficients
#六、模型预测
##1.点预测
predict=read.csv("./result/predict.csv")
x1=as.matrix(predict[,c(2,3,4)][1,])
x0=cbind(1,x1)
haty0=x0%*%beta
haty0
#predict()与fitted.values()在没有新数据时，效果一样，都是输出拟合值

## ----echo=FALSE----------------------------------------------------------
#六、模型预测
##2.方差估计
sigma=sqrt(sum((fit$residuals)^2)/18)
sigma

##3.置信区间估计函数
###公式计算
x2=as.matrix(newdata[,c(3,4,5)])
x=cbind(rep(1,length(newdata$year)),x2)[-19,]
fun=function(x0){
  upper1=x0%*%beta+qt(0.975,df=18)*sqrt(sigma^2*(1+t(x0)%*%solve((t(x)%*%x))%*%x0))#个值预测
  lower1=x0%*%beta-qt(0.975,df=18)*sqrt(sigma^2*(1+t(x0)%*%solve((t(x)%*%x))%*%x0))
  upper2=x0%*%beta+qt(0.975,df=18)*sqrt(sigma^2*(t(x0)%*%solve((t(x)%*%x))%*%x0))#均值预测
  lower2=x0%*%beta-qt(0.975,df=18)*sqrt(sigma^2*(t(x0)%*%solve((t(x)%*%x))%*%x0))
  conf=data.frame(lower1,upper1,lower2,upper2)
}
sigma*sqrt((1+t(x0[1,])%*%solve((t(x)%*%x))%*%x0[1,]))
sigma*sqrt((t(x0[1,])%*%solve((t(x)%*%x))%*%x0[1,]))
conf=apply(x0,1,fun)#求置信区间上下限
new=predict[,c(2,3,4)][1,]
##4.利用predict计算置信区间
predict(fit,new,interval="none")#个值预测
predict(fit,new,interval="confidence")#均值区间预测
predict(fit,new,interval="prediction")#个值区间预测，与3通过计算得出的置信区间相同

