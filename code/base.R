# R的常用函数练习

# 1.初识R
x1 <- 1:100
x2 <-  x1*2*pi/100
y <- sin(x2)
plot(x2, y , type = "l", col = "blue", lwd = 2,
     xlab = "x",ylab = "sin(x)",main = "Sine Function")

# 练习画出 在[-1，2]上画y=e^2x +sin(3x^2)的图形
x1 <- -100:200
x <- x1/100
y <- exp(2*x) + sin(3*x^2)
plot(x, y , type = "l", col = "blue", lwd = 2,
     xlab = "x",ylab = "y",main = "y=e^2x +sin(3x^2)")

# 一些其他的常见数学函数
# 1.2.绝对值和平方根
abs(-1) # 绝对值
sqrt(4) # 平方根
# 1.3.对数函数与指数函数
log(2) # 自然对数
log2(4) # 以2为底的对数
log10(100) # 以10为底的对数
exp(2) # 自然指数
# 1.4.三角函数
sin(pi/2) # 正弦
cos(pi/2) # 余弦
tan(pi/4) # 正切
# 1.5.反三角函数
asin(1) # 反正弦
acos(0) # 反余弦
atan(1) # 反正切
# 1.6.统计量函数
max(1,2,3) # 最大值
min(1,2,3) # 最小值
sum(1,2,3) # 求和
mean(c(1,2,3)) # 平均值
var() # 方差
sd() # 标准差
range()  # 范围
median() # 中位数
IQR() # 四分位数
sort() # 排序
order() # 排序
rank() # 排名
ave() # 平均值
fivenum() # 五数概括
mad() # 中位数绝对偏差
quantile() # 分位数
stem() # 茎叶图


# R的数据类型


# 2.1.向量
x <- c(1,2,3,4,5) # 创建一个向量
x <- c(1:5) # 创建一个向量

x <- seq(-10, 10, length= 30)
y <- x
f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")

?floor()

?cor()

?lm()

library(ggplot2)
cars <- mpg[mpg$cyl != 5, ]
cars$Cylinders <- factor(cars$cyl)
cars$Year <- factor(cars$year)

ggplot(cars, aes(x=Cylinders, y=cty)) +
  geom_boxplot() +
  labs(x="Number of Cylinders", y="Miles Per Gallon", title ="Car Mileage Data")





