Y <- trees[,3]
X <- as.matrix(cbind(rep(1, 31),trees[,1:2]))
n <- 31

betahat <- solve(t(X)%*%X)%*%t(X)%*%Y

Yhat <- X%*%betahat
r <- Y-Yhat
SST <- (n-1)*var(Y)
SSE <- t(r)%*%r
SSR <- SST-SSE


MST <- var(Y)
MSR <- SSR/(2)
MSE <- SSE/(n-2-1)

R2 <- SSR/SST
Adj_R2 <- 1-MSE/MST

sigma2hat <- MSE[1,1]

## F 检验
F_value <- MSR/MSE
F_crit <- qf(0.95, 2, 28)
p.value <- 1-pf(F_value,2,28)

x <- seq(0,10,by = 0.1)
plot(x,df(x,2,28),type = "l")


## t 检验
D_betahat <- sigma2hat*solve(t(X)%*%X)
se_betahat <- sqrt(diag(D_betahat))
t_value <- betahat/se_betahat
p_value <- 2*(1-pt(abs(t_value),28))

## 置信区间
cbind(betahat, betahat-qt(0.975,28)*se_betahat,
betahat+qt(0.975,28)*se_betahat)


fm <- lm(Volume~Girth+Height, data = trees)
summary(fm)

#### 预测
### 点预测
x_new <- rbind(c(1, 15, 75),
               c(1, 16, 80))
yhat_new <- x_new%*%betahat

### 个别值区间预测
cbind(yhat_new,
      yhat_new-qt(0.975,28)*sigma2hat^0.5*sqrt(1+diag(x_new%*%solve(t(X)%*%X)%*%t(x_new))),
      yhat_new+qt(0.975,28)*sigma2hat^0.5*sqrt(1+diag(x_new%*%solve(t(X)%*%X)%*%t(x_new))),

### 平均值区间预测
      yhat_new-qt(0.975,28)*sigma2hat^0.5*sqrt(diag(x_new%*%solve(t(X)%*%X)%*%t(x_new))),
      yhat_new+qt(0.975,28)*sigma2hat^0.5*sqrt(diag(x_new%*%solve(t(X)%*%X)%*%t(x_new))))

#### 方差分析
dat <- read.table("clipboard", sep = "\t")
Y <- dat$V1
company <- factor(dat$V2, labels = c("零", "旅", "航空", "家电"))

company <- relevel(company, ref = "家电")

X <- model.matrix(~company)

betahat <- solve(t(X)%*%X)%*%t(X)%*%Y

tapply(Y,company, mean)


## 模拟

x1 <- seq(0,5, length = 100)
x2 <- seq(10,20,length = 100)
e <- rnorm(100,0,2)
y <- 1+2*x1+0.5*x2+e

lm(y~x1+x2)
