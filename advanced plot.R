
# File-Name:       advanced plot.R           
# Date:            2014-09-26                   
# Author:          Chong Ding (chong.ding83@gmail.com)
# Purpose:         elegant and powerful data exploration
# Data Used:       iris, VADeaths
# Packages Used:   MASS,RColorBrewer

library(ggplot2)

# area plot
sunspotyear <- data.frame( 
  year = as.numeric( time(sunspot.year)), 
  sunspots = as.numeric(sunspot.year))
ggplot( sunspotyear, aes( x=year, y=sunspots)) +
  geom_area( color="black", fill="blue", alph = 0.9 ) + # alph透明度
  geom_line()


# 多变量图：颜色、面积
head(iris)
ggplot( iris, aes( x = Petal.Width, y = Sepal.Width, size = Sepal.Length , color = Species)) + 
  geom_point()

# 密集图形处理: 分块算密度
sp <- ggplot( diamonds, aes( x =carat, y= price ))
sp + stat_bin2d( bins = 50 ) + 
  scale_fill_gradient( low = "lightblue", high = "red", limits=c(0,6000) )


# 点标签
sp <- ggplot( subset( iris, Sepal.Length > 2.1), aes( x= Sepal.Length , y= Sepal.Width )) +
  geom_point()
# simgle point
sp + annotate( "text", x = 4.9, y=3.0, label ="setosa" ) + 
  annotate( "text", x = 4.6, y=3.1, label="virg ")
# all point
sp + geom_text( aes(label = Species), size = 4) 

head(iris)


# 分组密度曲线
library(MASS)
birthwt1 <- birthwt
birthwt1$smoke <- factor( birthwt1$smoke )
ggplot( birthwt1, aes( x = bwt, color=smoke)) + geom_density( alpha = 0.3)


# 相关矩阵图
mcor <- cor(mtcars , use="complete.obs" )
round( mcor, digits = 2 )
library(corrplot)
corrplot(mcor , method="shade", addCoef.col = "white", type="lower", tl.srt =45 )

# 列联表的可视化：马赛克图
ftable(UCBAdmissions)
dimnames(UCBAdmissions)
library(vcd)
mosaic( ~ Admit + Gender + Dept, data = UCBAdmissions, 
        highlighting ="Admit", highlighting_fill =c("lightblue", "pink" ),
        direction =c("v","h","v"))

mosaic( ~ Admit + Gender + Dept, data = UCBAdmissions, 
        highlighting ="Admit", highlighting_fill =c("lightblue", "pink" ),
        direction =c("v","v","h"))

mosaic( ~ Admit + Gender + Dept, data = UCBAdmissions, 
        highlighting ="Admit", highlighting_fill =c("lightblue", "pink" ),
        direction =c("h","h","v"))


# 决策树的图！
library(caret)
data(iris)
inTrain <- createDataPartition( y = iris$Species, p=0.75, list=F)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]
modFit <- train( Species ~ . , method="rpart", data= training)
print( modFit$finalModel)
library(rattle);library(rpart.plot)
fancyRpartPlot( modFit$finalModel)


## 以及特征选择图！