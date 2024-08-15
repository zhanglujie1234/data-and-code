# install.packages("car")
# install.packages("htmltools")
library(car)
library(carData)
library(tidyverse)
library(dplyr)
library(agricolae)
library(htmltools)
setwd("G:/NJ+10个样地/核糖体操纵子/抽平后")
Z <- read.table("单因素数据.csv", header=T, sep=",")
#一个分成11个样地，一个分成4个样地
Z <- Z[,-2]
Z$group <- as.factor(Z$group)
#options(digits = 2)
treatment<-Z[,1]
#treatment<-as.numeric(treatment)
ncol_v<-ncol(Z)-1
newdata<-c()
for(i in 1:ncol_v){
  Z1<-select(Z,i+1)
  Z2<-data.frame(treatment,Z1)
  Z1<-unlist(Z1)
  #单因素方差检验
  attach(Z2)
  table(treatment)
  n<-c(6)
  value<-Z2[,2]
  mean_t<-aggregate(value,by=list(treatment),FUN=mean)
  sd_t<-aggregate(value,by=list(treatment),FUN=sd)
  se_t<-sd_t/sqrt(n)
  Totaldata<-data.frame(mean_t[,2],se_t[,2])
  #打断连续型变量
  #treatment_1<-cut(treatment,breaks = 10)
  #单因素方差分析检验组间差异性
  ftreatment<-factor(treatment)
  fit<-aov(value~ftreatment)
  summary(fit)
  #方差齐次性检验
  bartlett.test(value~treatment,data=Z)
  #多重比较
  
  TukeyHSD(fit)
  #LSD检验
  out<-LSD.test(fit,"ftreatment",p.adj = "none")
  out
  #Duncan检验
  out1<-duncan.test(fit,"ftreatment")
  out1
  out2<-data.frame(out$groups)
  nout2<-as.numeric(rownames(out2))
  nout2
  out3<-data.frame(nout2,out$groups)
  out3
  A<-arrange(out$groups,out3[,1])
  A
  data_T<-data.frame(Totaldata,A[,2])
  data_T
  #data_T1<-mutate(data_T,y=paste(data_T[,2],paste(data_T[,3],data_T[,4],seq=","),seq="±")
  y<-paste(round(data_T[,1],8),seq="±",paste(round(data_T[,2],8),data_T[,3]))
  #newdata[[i]]<-y
  newdata<-rbind(newdata,y)
  #write table
}
#rowname <- levels(treatment)
#rowname
write.csv(t(newdata),file = "单因素结果-2.csv")

