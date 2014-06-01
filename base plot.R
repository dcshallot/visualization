
# File-Name:       base plot.R           
# Date:            2014-06-01                   
# Author:          Chong Ding (chong.ding83@gmail.com)
# Purpose:         讨论常见图形的命令、参数和效果，尽量用R基础功能实现，效果不输excel
# Data Used:       iris, VADeaths
# Packages Used:   MASS,RColorBrewer
# R 基础绘图 v1.0

### 离散变量图，一般都用汇总后的数据

## 条形图

# 频数统计条形图
barplot( table(iris[,5]) )

# 统计汇总条形图
l <- aggregate( Sepal.Length ~ Species, data= iris, mean)
x <- barplot( l[,2] ,  col=terrain.colors(3) , xlim=c(0,5), ylim=c(0,8) ,axe=F, names.arg = l[,1] )
y <- as.matrix( l[,2] )
text( x, y+1, labels=l[,2], col="black" ) #柱顶标注，y +n调节标注高度，横放图则调x+n
legend(legend=l[,1], "right", pch=15, col=terrain.colors(3) )

# 堆砌和分组
l <- table( mtcars$cyl , mtcars$gear);l
barplot( l, beside=T, main = "car tpyes: cyl and gear", 
         names.arg=c( "gear=3","gear=4","gear=5"), legend=c("cyl=4", "cly=6", "cyl=8"))
barplot( l, beside=F, main = "car tpyes: cyl and gear", 
         names.arg=c( "gear=3","gear=4","gear=5"), legend=c("cyl=4", "cly=6", "cyl=8"))

##饼图

# Pie Chart with Percentages
slices <- c(10, 12, 4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")

# 3D Exploded Pie Chart
library(plotrix)
slices <- c(10, 12, 4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of Countries ")


## 点图：适合很多列别的数据

# Dotplot: Grouped Sorted and Colored
# Sort by mpg, group and color by cylinder 
x <- mtcars[order(mtcars$mpg),] # sort by mpg
x$cyl <- factor(x$cyl) # it must be a factor
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen" 
dotchart(x$mpg,labels=row.names(x),cex=.7,groups= x$cyl,
         main="Gas Milage for Car Models\ngrouped by cylinder",
         xlab="Miles Per Gallon", gcolor="black", color=x$color)


### 连续变量图：一般都用原始数据，而非汇总数据做

## 直方图：连续变量的分布

# 分布函数
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon", prob=F ,
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) # 正态分布可以这么画，其他分布参考分布函数
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

# 密度函数
x <- mtcars$mpg 
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon", prob=T ,
        main="Histogram with Normal Curve") 
lines(density(x))

# 离散变量直方图
plot( table(mtcars$carb), type="h" , lwd=5 )
head(iris)


### 多变量

## 箱线图：连续变量VS离散变量
boxplot( Petal.Length ~ Species , data = iris , outline = F  ) # 离群值不显示
boxplot( mpg ~ cyl * gear, data = mtcars , varwidth=T ) # 箱体宽度由样本量决定


## plot族

# 简单漂亮的散点图：两个连续变量之间的关系
plot(cars$dist~cars$speed, # y~x
     main="Relationship between car distance & speed", #Plot Title
     xlab="Speed (miles per hour)", #X axis title
     ylab="Distance travelled (miles)", #Y axis title
     xlim=c(0,30), #Set x axis limits from 0 to 30 ylim=c(0,140), #Set y axis limits from 0 to  30140  xaxs="i", #Set x axis style as internal 
     yaxs="i", #Set y axis style as internal  
     col="red", #Set the colour of plotting symbol to red 
     pch=19) #Set the plotting symbol to filled dots

# 分类散点图
# 先将鸢尾花的类型转化为整数1 、2 、3，便于使用向量
idx = as.integer(iris[["Species"]])
plot(iris[, 3:4], pch = c(24, 21, 25)[idx], col = c("black","red", "blue")[idx], panel.first = grid())
legend("topleft", legend = levels(iris[["Species"]]), col = c("black", "red", "blue"),
       pch = c(24, 21, 25), bty = "n")

# 散点矩阵图
pairs( ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width, data=iris)

# 用散点图表示时间序列数据
data <- VADeaths
plot(data[,1] ,type="b" , lwd =2, xaxt="n" , ylim=c(0, 75) ,
     main ="Death Rates in Virginia (1940)", xlab="age", ylab="Death Rate"  )
axis( 1, at=1:nrow(data) ,labels = row.names(data) )
lines( data[,2], type="b" , lwd =2, col="red" )
lines( data[,3], type="b" , lwd =2, col="orange" )
lines( data[,4], type="b" , lwd =2, col="purple" )
legend( 4,30 , legend=colnames(data) , lty=1, lwd=2, pch=21, bty="n" ,
        col=c("black","red","orange","purple") , cex =0.8, inset=0.01)
grid()

## matplot 分类散点图，强化类别差异的表现
nam.var <- colnames(iris)[-5]
nam.spec <- as.character(iris[1+50*0:2, "Species"])
iris.S <- array(NA, dim = c(50,4,3),
                dimnames = list(NULL, nam.var, nam.spec))
for(i in 1:3) iris.S[,,i] <- data.matrix(iris[1:50+50*(i-1), -5])
matplot(iris.S[, "Petal.Length",], iris.S[, "Petal.Width",], pch = "SCV",
        col = rainbow(3, start = 0.8, end = 0.1),
        sub = paste(c("S", "C", "V"), dimnames(iris.S)[[3]],
                    sep = "=", collapse= ",  "),
        main = "Fisher's Iris Data")

### 复杂图形(复杂一点而已)

## 对应分析：两个或多个变量之间的对应关系
library(MASS)
cal<-corresp(USPersonalExpenditure,nf=2) ;
biplot(cal,expand=1.5, xlim=c(-0.5 , 0.5), ylim=c(-0.1 , 0.15))
abline(v=0,h=0,lty=3) 

## 系统聚类：样本距离的衡量
dist <-dist(scale(iris[,c(1:4)]))
hc <- hclust(dist, "ward")
plclust(hc,hang=-1 ,  labels=iris[,5]  )
re<-rect.hclust(hc,k=4,border="red")

# 随机点的艺术作品
par(mar = c(0.2, 0.2, 0.2, 0.2), mfrow = c(2, 2))
for (n in c(63, 60, 76, 74)) {
  set.seed(711)
  plot.new()
  size = c(replicate(n, 1/rbeta(2, 1.5, 4)))
  center = t(replicate(n, runif(2)))
  center = center[rep(1:n, each = 2), ]
  color = apply(replicate(2 * n, sample(c(0:9,
                                          LETTERS[1:6]), 8, TRUE)), 2, function(x) sprintf("#%s", paste(x, collapse = "")))
  points(center, cex = size, pch = rep(20:21, n),  col = color)
  box()
}
dev.off()

# 热力图，用于展现相同数值在两个维度上的水平/相关系数
library(RColorBrewer)
data <- VADeaths
pal=brewer.pal(4,"YlOrRd")
breaks<-c(0, 15, 26, 44, 72)
layout(matrix(data=c(1,2),  nrow=1, ncol=2), widths=c(8,1),
       heights=c(1,1))  ## 画一个空白的图形画板，按照参数把图形区域分隔好
## 看layout的分割可以这样：
## xx <- layout(matrix(data=c(1,2),  nrow=1, ncol=2), widths=c(8,1), heights=c(1,1)) ; layout.show(xx)
par(mar = c(2,6,4,1 ), oma=c(0.1, 0.1 ,0.1 , 0.1), mex = 1.2 ) #Set margins for the heatmap
image(x=1:nrow(data),
      y=1:ncol(data),
      z=data,axes=FALSE,
      xlab="Month",   ylab="", main="Sales Heat Map" ,
      col=pal[1:(length(breaks)-1)],
      breaks=breaks ) # breaks 颜色块对应的数值（数值分组），要比颜色数量多1个
axis(1, col="white",las=1 , at=1:nrow(data), labels=rownames(data)  )
axis(2, col="white",las=1 , at=1:ncol(data), labels=colnames(data)  )
abline(h=c(1:ncol(data))+0.5, v=c(1:nrow(data))+0.5,  col="white",lwd=2,xpd=FALSE)
# 画标尺
breaks2 <- breaks[-length(breaks)]  # breaks 少一个
par(mar = c(2,1,4,2))
image(x = 1, y= 0:length(breaks2),
      z=t(matrix(breaks2))*1.001,
      col=pal[1:length(breaks)-1],
      axes=FALSE,breaks=breaks,
      xlab="", ylab="",xaxt="n")
axis(4,at=0:(length(breaks2)-1), labels=breaks2, col="white", las=1)
abline(h=c(1:length(breaks2)),col="white",lwd=2, xpd=F )
dev.off()

### 参数参考

# 玩线条形状
x <- c(1:5); y <- x # create some data 
par(pch=22, col="red") # plotting symbol and color 
par(mfrow=c(2,4)) # all plots on one page 
opts = c("p","l","o","b","c","s","S","h") 
for(i in 1:length(opts)){ 
  heading = paste("type=",opts[i]) 
  plot(x, y, type="n", main=heading) 
  lines(x, y, type=opts[i]) 
}
dev.off()

# 玩颜色
library( RColorBrewer)
display.brewer.all(n=10, exact.n=FALSE) # 调色板
col= brewer.pal( n  ,"Set1") # 引用颜色

# 玩坐标轴
x <- c(1:10); y <- x; z <- 10/x
# 为一个坐标在右边创建额外页边空间 
par(mar=c(5, 4, 4, 8) + 0.1)
# 绘制 x 相对 y 
plot(x, y,type="b", pch=21, col="red", 
     yaxt="n", lty=3, xlab="", ylab="")
# 添加 x 相对 1/x 
lines(x, z, type="b", pch=22, col="blue", lty=2)
# 在左边画一条轴线 
axis(2, at=x,labels=x, col.axis="red", las=0)
# 用小字体标记在右边画一条轴线
axis(4, at=z,labels=round(z,digits=2),
     col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
# 为右边轴线添加标题 
mtext("y=1/x", side=4, line=3, cex.lab=1,las=2, col="blue")
# 添加主标题，底和左轴标签 
title("An Example of Creative Axes", xlab="X values",
      ylab="Y=X")
dev.off()

# 玩辅助线和图例
plot(1:10, type = "n", xlim = c(0, 20), ylim = c(0, 20)) # 不作图，只画出框架，且指定坐标轴范围
lines(1:10, abs(rnorm(10)))  # 10个正态随机数绝对值的波动线
# 不同的直线
abline(a = 0, b = 1, col = "gray")
abline(v = 2, lty = 2)
abline(h = 2, lty = 2)
#添加文本
text(8, 3, "abline(a = 0, b = 1)")
# 添加箭头
arrows(8, 3.5, 6, 5.7, angle = 40)
# 参数用了向量：不同灰度的线段
segments(rep(3, 4), 6:9, rep(5, 4), 6:9, col = gray(seq(0.2, 0.8, length = 4)))
text(4, 9.8, "segments")
dev.off()

####################################

# 待增补





