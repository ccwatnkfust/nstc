rm(list=ls(all=TRUE))
library(tidyverse)
df_7v <- read.csv("./df_住宅大樓_7v.csv")
df_7v <- df_7v %>% mutate_if(is.character,as.factor)

df_13v <- read.csv("./df_住宅大樓_13v.csv")
df_13v <- df_13v %>% mutate_if(is.character,as.factor)

##############################################
k <- 10
誤差_7變數 = rep(0, k)
誤差_13變數 = rep(0, k)
for (i in 1:k){
  
n <- sample(1:length(df_7v$單價),10)
(test_7v <- df_7v[n,])
(test_13v <- df_13v[n,])
price_7v <- lm(單價~., data=df_7v[-n,])
price_13v <- lm(單價~., data=df_13v[-n,]) 

#predict 7v
原價_7v <- test_7v$單價
test_7v$單價 <- NULL
(預測_7v <- predict(price_7v, test_7v))
new_7v <- data.frame(cbind(原價_7v,round(預測_7v,1)))
colnames(new_7v) <- c("原價", "預測")
new_7v$誤差 <- ((new_7v$原價-new_7v$預測)^2)^0.5
new_7v$預測結果 <- with(new_7v,ifelse(預測 > 原價, '高估', '低估'))
#View(cbind(new_7v, df_7v[n,]))

#predict 13v
原價_13v <- test_13v$單價
test_13v$單價 <- NULL
預測_13v <- predict(price_13v, test_13v)
new_13v <- data.frame(cbind(原價_13v,round(預測_13v,1)))
#str(new_13v)
colnames(new_13v) <- c("原價", "預測")
new_13v$誤差 <- ((new_13v$原價-new_13v$預測)^2)^0.5
#str(new_13v)
new_13v$預測結果 <- with(new_13v,ifelse(預測 > 原價, '高估', '低估'))
誤差_7變數[i] <- sum(new_7v[,2])
誤差_13變數[i] <- sum(new_13v[,2])
}

誤差_7變數
誤差_13變數
cbind(誤差_7變數,誤差_13變數)
sum(誤差_7變數);sum(誤差_13變數)

誤差比較 <- as.data.frame(cbind(誤差_7變數,誤差_13變數))
colnames(誤差比較) <- c("迴歸7變數","迴歸13變數")
x <- apply(誤差比較, 2, sum)
ans <- ifelse(x[[1]] < x[[2]], paste("7變數比13變數的誤差小，「7變數迴歸模型」較佳"),
                               paste("13變數比7變數的誤差小，「13變數迴歸模型」較佳"))
a <- paste("用7個變數所建立的迴歸模型，其預測誤差  =",x[1])
b <- paste("用13個變數所建立的迴歸模型，其預測誤差 =",x[2])

#印出比較結果
cat(a,b,ans,sep ="\n")


