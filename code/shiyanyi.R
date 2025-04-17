
# 编写大约50条指令

# -----------------------------------------------------------
# 1. 编写函数：计算一组数据的均值、中位数、方差、偏度与峰度
# -----------------------------------------------------------


# 计算均值
my_mean <- function(x) {
  if(length(x) == 0) {
    return(NULL) # 如果向量为空，返回NULL
  }
  return(sum(x) / length(x))
}

data_1 <-  c(2, 4, 6, 8, 10)
print(paste("均值为：",my_mean(data_1))) # 输出均值

# 计算中位数
my_median <- function(x) {
    if(length(x) == 0) {
        return(NULL) # 如果向量为空，返回NULL
    }
  x_sorted <- sort(x)
  n <- length(x)
  if(n %% 2 == 1) {
    # 当元素个数为奇数，取中间那个元素
    return(x_sorted[(n + 1) / 2])
  } else {
    # 当元素个数为偶数，取中间两个数的平均值
    return((x_sorted[n/2] + x_sorted[n/2 + 1]) / 2)
  }
}

data_2 <- c(2, 5, 3, 8, 10)
print(paste("中位数为：",my_median(data_2))) # 输出中位数

# 计算样本方差
my_var <- function(x) {
  n <- length(x)
  mu <- my_mean(x)
  return(sum((x - mu)^2) / (n))
}

data_3 <- c(2, 4, 4, 4, 5, 5, 7, 9)
print(paste("方差为：",my_var(data_3))) # 输出方差

# 计算样本偏度
my_skewness <- function(x) {
  n <- length(x)
  mu <- my_mean(x)
  s <- sd(x)
  skewness <- (sum((x-mu)^3) / n) / (s^3)
  return(skewness)
}

print(paste("偏度为：",my_skewness(data_3))) # 输出偏度

# 计算样本峰度（Excess Kurtosis）
my_kurtosis <- function(x) {
  n <- length(x)
  mu <- my_mean(x)
  s <- sd(x)
  kurtosis <- (sum((x - mu)^4) / n) / (s^4) - 3
  return(kurtosis)
}

print(paste("峰度为：",my_kurtosis(data_3))) # 输出峰度

# 汇总以上统计指标的函数
my_statistics <- function(x) {
  stats <- list(
    mean     = my_mean(x),
    median   = my_median(x),
    variance = my_var(x),
    skewness = my_skewness(x),
    kurtosis = my_kurtosis(x)
  )
  return(stats)
}

# -----------------------------------------------------------
# 2. 编写函数：计算两组数据的协方差与相关系数
# -----------------------------------------------------------

# 计算协方差
my_cov <- function(x, y) {
    if(length(x) != length(y)) {
        stop("x 和 y 的长度不一致")
    }
  n <- length(x)
  mu_x <- my_mean(x)
  mu_y <- my_mean(y)
  return(sum((x - mu_x) * (y - mu_y)) / n)
}

data_4 <- c(1, 2, 3, 4, 5)
data_5 <- c(2, 3, 4, 5, 6)
print(paste("协方差为：",my_cov(data_4, data_5))) # 输出协方差

# 计算相关系数
my_corr <- function(x, y) {
  if(length(x) != length(y)) {
    stop("x 和 y 的长度不一致")
  }
  cov_xy <- my_cov(x, y)
  sd_x <- sd(x)
  sd_y <- sd(y)
  corr <- cov_xy / (sd_x * sd_y)
  return(corr)
}
print(paste("相关系数为：",my_corr(data_4, data_5))) # 输出相关系数

# -----------------------------------------------------------
# 3. 对数据集 faithful 的分析
# -----------------------------------------------------------

# (1) 查看数据集 faithful 的帮助文档并简单描述
?faithful
summary(faithful) #数据集包含黄石国家公园老忠实间歇泉的爆发时间和等待时间，
# 主要变量包括 eruptions（喷发时间，单位为分钟）和 waiting（上一次喷发结束到下一次喷发开始之间的等待时间，单位为分钟）。

# 加载数据集（faithful 是 R 内置数据集）
data(faithful)

# (2) 均值、中位数、方差、偏度与峰度;用 2 中你所编写的函数计算该数据集中两个变量的协方差与相关系数。
# 对 eruptions 和 waiting 两个变量分别计算统计指标：
eruption_stats <- my_statistics(faithful$eruptions)
waiting_stats  <- my_statistics(faithful$waiting)

# 计算 eruptions 和 waiting 的均值、中位数、方差、偏度与峰度
eruption_stats <- list(
  mean     = my_mean(faithful$eruptions),
  median   = my_median(faithful$eruptions),
  variance = my_var(faithful$eruptions),
  skewness = my_skewness(faithful$eruptions),
  kurtosis = my_kurtosis(faithful$eruptions)
)
waiting_stats <- list(
  mean     = my_mean(faithful$waiting),
  median   = my_median(faithful$waiting),
  variance = my_var(faithful$waiting),
  skewness = my_skewness(faithful$waiting),
  kurtosis = my_kurtosis(faithful$waiting)
)

# 计算两变量之间的协方差和相关系数：
cov_fw   <- my_cov(faithful$eruptions, faithful$waiting)
corr_fw  <- my_corr(faithful$eruptions, faithful$waiting)

# 输出计算结果
cat("利用自定义函数计算的统计指标：\n")
cat("== eruptions ==\n")
print(eruption_stats)
cat("\n== waiting ==\n")
print(waiting_stats)
cat("\n== eruptions 与 waiting 的关系 ==\n")
cat("协方差 =", cov_fw, "\n")
cat("相关系数 =", corr_fw, "\n\n")

# (3) 用 R 内置函数计算相同的统计量，并与自定义函数结果比较
builtin_eruptions <- list(
  mean     = mean(faithful$eruptions),
  median   = median(faithful$eruptions),
  variance = var(faithful$eruptions)
)

builtin_waiting <- list(
  mean     = mean(faithful$waiting),
  median   = median(faithful$waiting),
  variance = var(faithful$waiting)
)

cat("利用 R 内置函数计算的统计指标：\n")
cat("== eruptions ==\n")
print(builtin_eruptions)
cat("\n== waiting ==\n")
print(builtin_waiting)

# (4) 用 R 内置函数计算协方差与相关系数，并比较结果
builtin_cov  <- cov(faithful$eruptions, faithful$waiting)
builtin_corr <- cor(faithful$eruptions, faithful$waiting)

cat("\n利用 R 内置函数计算的 eruptions 与 waiting 的协方差与相关系数：\n")
cat("协方差 =", builtin_cov, "\n")
cat("相关系数 =", builtin_corr, "\n")

# 从输出结果来看，均值和中位数完全一致；而方差、协方差、相关系数可能存在非常细微的数值波动，
# 这主要是由于在计算过程中精度或中间量舍入导致的差异。

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

# 绘制条形图
install.packages("vcd")
install.packages("ggplot2")
data(Arthritis,package = "vcd")
library(ggplot2)

# 简单条形图
ggplot(Arthritis, aes(x=Improved)) + geom_bar(fill="blue") +
  labs(title="Simple Bar Chart", x="Improved", y="Frequency")
# 水平条形图
ggplot(Arthritis, aes(x=Improved)) + geom_bar(fill="red") +
  coord_flip() +
  labs(title="Horizontal Bar Char", x="Improved", y="Frequency")
# 堆积条形图
ggplot(Arthritis, aes(x=Treatment, fill=Improved)) +
  geom_bar(position="stack") +
  labs(title="Stacked Bar Chart", x="Treatment", y="Proportion")
# 分组条形图
ggplot(Arthritis, aes(x=Treatment, fill=Improved)) +
  geom_bar(position="dodge") +
  labs(title="Grouped Bar Chart", x="Treatment", y="Proportion")
# 填充条形图
ggplot(Arthritis, aes(x=Treatment, fill=Improved)) +
  geom_bar(position="fill") +
  labs(title="Filled Bar Chart", x="Treatment", y="Proportion")

# 树状图
install.packages("dplyr")
install.packages("treemapify")
library(dplyr)
library(treemapify)

plotdata <- mpg %>% count(manufacturer)

ggplot(plotdata, aes(fill=manufacturer, area=n, label=manufacturer)) +
  geom_treemap() +
  geom_treemap_text() +
  theme(legend.position = "none")
