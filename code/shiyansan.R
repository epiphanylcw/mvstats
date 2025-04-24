
# 对给定的数据’bankloan.xlsx’进行数据清洗
# 并分成训练集和测试集
# 用训练集建立逻辑斯特回归模型
# 用测试集数据对模型进行检验

# 加载必要的包
library(readxl)    # 读取Excel文件
library(dplyr)     # 数据清洗
library(caTools)    # 数据分割
library(caret)      # 建模评估

# 读取数据（确保文件路径正确）
df <- read_excel("bankloan.xlsx")

# --------------------------
# 数据清洗
# --------------------------
# 检查结构
str(df)

# 1. 转换分类变量为因子
df <- df %>%
  mutate(
    教育 = factor(教育, levels = c("未完成高中", "高中", "大专", "大学", "研究生")),
    违约 = factor(违约, levels = c("否", "是"))
  )

# 2. 删除ID列（非特征）
df <- select(df, -ID)

# 3. 检查缺失值
sum(is.na(df)) # 若无缺失可跳过下一步
# 若有缺失值处理：
# df <- na.omit(df)  # 或使用插补方法

# 4. 检查异常值（示例：收入>0）
df <- filter(df, 收入 > 0)

# --------------------------
# 数据分割
# --------------------------
set.seed(123) # 确保可重复性
split <- sample.split(df$违约, SplitRatio = 0.7)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

# --------------------------
# 建立逻辑回归模型
# --------------------------
model <- glm(违约 ~ 年龄 + 教育 + 工龄 + 收入 + 负债率 + 信用卡负债 + 其他负债,
             family = "binomial", data = train)

# 查看模型摘要
summary(model)

# --------------------------
# 模型评估
# --------------------------
# 预测概率
test$pred_prob <- predict(model, newdata = test, type = "response")

# 转换为类别预测（默认阈值为0.5）
test$pred_class <- ifelse(test$pred_prob > 0.5, "是", "否")
test$pred_class <- factor(test$pred_class, levels = c("否", "是"))

# 混淆矩阵
confusionMatrix(test$pred_class, test$违约, positive = "是")

# ROC曲线和AUC值（需要pROC包）
# install.packages("pROC")
library(pROC)
roc_obj <- roc(test$违约, test$pred_prob)
plot(roc_obj, print.auc = TRUE)