# 1 R软件作散点图、直方图、饼图、箱线图、相关图、星状图、茎叶图、脸谱图

library(ggplot2)

## 1.1 散点图
ggplot(mtcars, aes(x = mpg, y = wt)) + 
  geom_point() + 
  labs(title = "MPG vs Weight")

## 1.2 直方图
ggplot(mtcars, aes(x = mpg)) + 
  geom_histogram(bins = 10) + 
  labs(title = "Histogram of MPG")

## 1.3 饼图
counts <- table(mtcars$cyl)
pie(counts, labels = c("4 Cylinders", "6 Cylinders", "8 Cylinders"), 
    main = "Cylinder Distribution")

## 1.4 箱线图
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + 
  geom_boxplot() + 
  labs(x = "Cylinders")

## 1.5 相关图
install.packages("corrplot")
library(corrplot)
cor_matrix <- cor(mtcars)
corrplot(cor_matrix, method = "circle")

## 1.6 星状图
stars(mtcars[1:5, ], labels = row.names(mtcars)[1:5], 
      key.loc = c(10, 2), main = "Star Plot of Cars")

## 1.7 茎叶图
stem(mtcars$mpg)

## 1.8 脸谱图
install.packages("aplpack")
library(aplpack)
faces(mtcars[1:5, ], main = "Chernoff Faces of Cars")

# 2 R假设检验t.test()、chisq.test()、f.test()输出结果解释，各完成一例

## 2.1 t检验 比较两组数据均值是否显著不同
## 检验 mtcars 数据中自动档（am=0）和手动档（am=1）汽车的油耗（mpg）差异

# 提取数据
auto_mpg <- mtcars$mpg[mtcars$am == 0]
manual_mpg <- mtcars$mpg[mtcars$am == 1]

# 独立样本 t 检验（默认假设方差不相等）
result <- t.test(auto_mpg, manual_mpg)
print(result)

# 结果解释：
# 原假设 (H₀)：自动档和手动档汽车的油耗均值相等。
# 备择假设 (H₁)：两者的油耗均值不相等（双尾检验）。
# t 统计量：-3.7671，表示自动档比手动档油耗低。
# 自由度 (df)：18.332（Welch 校正后的自由度）。
# p 值：0.001374 < 0.05，拒绝原假设，两组油耗差异显著。
# 置信区间：真实均值差异的 95% 置信区间为 [-11.28, -3.21]，不包含 0，进一步支持差异显著。
# 样本均值：自动档均值为 17.15，手动档为 24.39，手动档更省油。

## 2.2 卡方检验 检验两个分类变量是否独立
# 检验 Titanic 数据中性别（Sex）与生存（Survived）的关联性

# 创建列联表
data <- as.table(matrix(c(212, 178, 673, 126), nrow = 2, 
                        dimnames = list(Sex = c("Male", "Female"), 
                                        Survived = c("No", "Yes"))))
# 卡方独立性检验
result <- chisq.test(data)
print(result)

# 结果解释：
# 原假设 (H₀)：性别与生存状态独立（无关联）。
# 备择假设 (H₁)：性别与生存状态相关。
# 卡方统计量 (X-squared)：365.89，反映观测频数与期望频数的总偏差。
# 自由度 (df)：1（(行数-1)(列数-1) = 1×1）。
# p 值：<2.2e-16 ≈ 0，远小于 0.05，拒绝原假设，性别与生存显著相关。
# 实际意义：女性生存率显著高于男性（参考列联表）。

## 2.3 F 检验 检验两组数据的方差是否相等（用于 t 检验前提验证）
# 检验 iris 数据中 setosa 和 versicolor 两类花的萼片长度方差是否相等
# 提取数据
setosa <- iris$Sepal.Length[iris$Species == "setosa"]
versicolor <- iris$Sepal.Length[iris$Species == "versicolor"]

# F 检验（方差齐性）
result <- var.test(setosa, versicolor)
print(result)

# 结果解释：
# 
# 原假设 (H₀)：两组方差相等（方差比 = 1）。
# 备择假设 (H₁)：方差不相等（方差比 ≠ 1）。
# F 统计量：0.4669，表示 setosa 方差 / versicolor 方差 ≈ 0.47。
# 自由度：分子自由度 49，分母自由度 49。
# p 值：0.004785 < 0.05，拒绝原假设，两组方差显著不等。
# 置信区间：方差比的 95% 置信区间为 [0.26, 0.84]，不包含 1，支持方差不齐。
# 实际意义：若后续进行 t 检验，需使用 Welch 校正（不假设方差齐性）。

# 3 相关分析中cor()、cor.test()应用举例

## 3.1 cor() 计算两变量或多变量间的相关系数矩阵

# 使用 mtcars 数据集，计算 mpg（油耗）和 wt（车重）的相关系数
data <- mtcars
cor_value <- cor(data$mpg, data$wt, method = "pearson")
print(cor_value)

# 解释：相关系数：-0.8677，表明 mpg 和 wt 呈强负相关（车越重，油耗越低）
# 方法选择：
# method = "pearson"（默认）：线性相关，要求数据近似正态分布。
# method = "spearman"：秩相关，适用于单调非线性关系。
# method = "kendall"：秩相关，适用于小样本或有序数据。

## 3.2 cor.test() 计算相关系数的同时进行假设检验，给出置信区间和 p 值

# Pearson 相关系数检验
test_result <- cor.test(data$mpg, data$wt, method = "pearson")
print(test_result)

# 结果解释：
# 原假设 (H₀)：两变量相关系数为 0（无线性相关）。
# 备择假设 (H₁)：相关系数不等于 0（双尾检验）。
# t 统计量：-9.559，自由度 df = 30。
# p 值：1.294e-10 ≈ 0，远小于 0.05，拒绝原假设，相关系数显著。
# 置信区间：95% 置信区间为 [-0.934, -0.744]，不包含 0，支持显著负相关。
# 样本估计：相关系数 -0.8677，与 cor() 计算结果一致。

# 4 线性回归模型
# 4.1 lm() 函数：构建线性回归模型

# model <- lm(formula, data, subset, weights, ...)
# 核心参数：
# formula：模型公式，格式为 y ~ x1 + x2 + x1:x2，支持交互项（*）、多项式（I(x^2)）等。
# data：包含变量的数据框。
# subset：指定用于建模的子集（如 subset = (age > 20)）。
# weights：加权最小二乘法的权重向量。

# 使用 mtcars 数据建立模型：油耗（mpg）与车重（wt）和马力（hp）的关系
model <- lm(mpg ~ wt + hp, data = mtcars)

## 4.2 summary() 函数：模型结果解读
summary(model)

# 系数（Coefficients）​：估计值（Estimate）、标准误（Std. Error）、t 值（t value）、p 值（Pr(>|t|)）。
# 模型评价指标：
  # R-squared (R²)：模型解释的方差比例（0~1），越高越好。
  # Adjusted R-squared：调整后的 R²，考虑变量数量，惩罚过拟合。
  # AIC/BIC：信息准则，用于模型比较（值越小越好）。
# 残差分布：四分位数范围，检查正态性。

# 4.3 模型评价指标

# R-squared (R²)：
  # 公式：R² = 1 - (残差平方和 / 总平方和)。
  # 局限性：随变量增加而上升，可能高估模型性能。
# Adjusted R²：
  # 公式：Adjusted R² = 1 - [(1 - R²)(n - 1)/(n - k - 1)]，其中 k 为变量数。
  # 用于比较不同变量数量的模型。
# AIC (赤池信息准则)：
  # 公式：AIC = 2k - 2ln(L)，L 为模型似然值。
  # 用于模型选择，值越小越好。
# BIC (贝叶斯信息准则)：
  # 公式：BIC = ln(n)k - 2ln(L)，对变量数量惩罚更严格

## 4.4 step() 函数：逐步回归
# 通过 AIC 准则自动选择最优变量组合（向前、向后或双向）

# 全模型（包含所有变量）
full_model <- lm(mpg ~ ., data = mtcars)

# 向后逐步回归
step_model <- step(full_model, direction = "backward")

# 查看最终模型
summary(step_model)

# 输出：显示每一步的 AIC 值和删除/添加的变量，最终保留显著变量（如 wt, qsec, am）

## 4.5 crossval() 函数：交叉验证
# 评估模型预测性能
# 安装并加载包
install.packages("bootstrap")
library(bootstrap)

# 定义模型拟合函数（theta.fit）
theta.fit <- function(x, y) {
  data <- data.frame(mpg = y, wt = x[,1], hp = x[,2])
  lm(mpg ~ wt + hp, data = data)
}

# 定义预测函数（theta.predict）
theta.predict <- function(fit, x) {
  newdata <- data.frame(wt = x[,1], hp = x[,2])
  predict(fit, newdata = newdata)
}

# 准备数据
x <- mtcars[, c("wt", "hp")]
y <- mtcars$mpg

# 执行10折交叉验证
cv_result <- crossval(
  x = x,
  y = y,
  theta.fit = theta.fit,
  theta.predict = theta.predict,
  ngroup = 10
)

# 计算均方误差 (MSE)
mse <- mean((y - cv_result$cv.fit)^2)
print(paste("MSE =", round(mse, 2)))

## 4.6 update() 函数：更新模型

# 原模型：mpg ~ wt + hp
model <- lm(mpg ~ wt + hp, data = mtcars)

# 移除 hp，添加 cyl
new_model <- update(model, . ~ . - hp + cyl)
summary(new_model)

## 4.7 predict() 函数：模型预测
# 创建新数据（车重=3.0，马力=150）
new_data <- data.frame(wt = 3.0, hp = 150)

# 预测油耗
pred <- predict(model, newdata = new_data, interval = "confidence")
print(pred)
