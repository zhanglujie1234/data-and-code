library(ggplot2)
library(ggbreak)
dat<-read.table("柱状图.csv", header=T, sep=",")
dat1 <- dat[,c(1:2)]
b <- colnames(dat)
#不同的指数作图调整d值
d <- 21
a <- 3*d
b <- colnames(dat)
c <- b[a]
c
dat1$mean <- dat[,a]
dat1$se <- dat[,a+1]
dat1$sign1 <- dat[,a+2]
dat1$group <- factor(dat1$group)
cbbPalette1<-c('#d78e8b','#e7b65b','#71b476','#116fb2')
dat1$taxonomy<-factor(dat1$taxonomy,levels = c("Antarctica","Temperate","Subtropic","Tropic"))
p1 <- ggplot(dat1,aes(taxonomy,mean,fill = taxonomy)) +
  #这句注意，fill=group，是一个处理两根柱子的关键
  scale_x_discrete(limits = levels(dat1$taxonomy)) +
  scale_y_continuous(breaks = seq(0,80000, by = 15000))+
  coord_flip() +
  xlab("") +
  ylab(c)+
  theme(panel.background = element_rect(fill = 'transparent',color="black",linewidth=0.6),
        panel.grid = element_blank(),
        #定义刻度线的长度
        axis.ticks.length = unit(0.2,"lines"),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_line(), 
        #定义刻度线的颜色和粗度
        axis.ticks = element_line(color="black",linewidth = 0.6),
        #定义坐标轴线的颜色和粗度
        axis.line = element_line(colour = "black",linewidth=0),
        #定义坐标轴名称，文本等参数
        axis.title.x=element_text(colour='black', size=15,face = NULL,family="sans"),
        axis.title.y=element_text(colour='black', size=15,face = NULL,family="sans"),
        #定义坐标轴上的文字
        axis.text.x=element_text(colour='black',size=12,face = NULL,family="sans"),
        axis.text.y=element_text(colour='black',size=12,face = NULL,family="sans"),
        legend.position = "none")

p1 <- p1 + 
  #position：用于设置条形图的摆放位置，默认为 'stack'，表示绘制堆叠条形图；
  #如果指定为 'dodge'，表示绘制水平交错条形图；
  #如果为 'fill'，表示绘制百分比堆叠条形图；
  geom_bar(stat = "identity",position = position_dodge(width = 0.1),width = 0.5) +
  ##截断图，省略y轴c（a,b）中a-b的区域
  #scale_y_break(c(0,1500))+
  #width控制上下两条短横线的长短
  #size控制线条整体粗细 
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.1,size=0.3)+
  geom_text(aes(label = sign1, y = mean + se + 1200), hjust=0.5,size=4,family="sans")+
  scale_fill_manual(values=cbbPalette1,guide = guide_legend(reverse = TRUE))

p1


ggsave(p1,file=paste(d,"-",c,".pdf",sep=""),width = 5.5,height = 4) 

                     