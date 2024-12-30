# 讀入主資料
data <- read.csv("C:/Users/yinchi/Desktop/113-1/STATA/traveler108.csv")

# 收入分低中高三組
library(tidyverse)

# 將 vb5 分組為新變數 income_group
data <- data %>%
  mutate(income_group = case_when(
    vb5 >= 1 & vb5 <= 4 ~ 0, # 低收入
    vb5 >= 5 & vb5 <= 7 ~ 1, # 中收入
    vb5 >= 8 & vb5 <= 10 ~ 2 # 高收入
  ))

# 敘述統計
summary(data$vq1)

#根據數學式建立模型
model <- lm(vq1 ~ as.factor(income_group) + vb1 + as.factor(vb2) + as.factor(vb7) + as.factor(vb4), data = data)

# 顯示回歸結果
summary(model)




#子群集分析---------------------------------------------------------------------


#已婚vs未婚

# 已婚群體
data_married <- subset(data, vb4 == 2)

# 未婚群體
data_single <- subset(data, vb4 == 1)

# 已婚群體的模型
model_married <- lm(vq1 ~ as.factor(income_group) + vb1 + as.factor(vb2) + as.factor(vb7), data = data_married)

# 未婚群體的模型
model_single <- lm(vq1 ~ as.factor(income_group) + vb1 + as.factor(vb2) + as.factor(vb7), data = data_single)

# 已婚群體的回歸結果
summary(model_married)

# 未婚群體的回歸結果
summary(model_single)

#-------------------------------------------------------------------------------

#六都

data_NWT <- subset(data, vb6 == 1) #新北
data_TPE <- subset(data, vb6 == 2) #台北
data_TAO <- subset(data, vb6 == 3) #桃園
data_TXG <- subset(data, vb6 == 4) #台中
data_TNN <- subset(data, vb6 == 5) #台南
data_KHH <- subset(data, vb6 == 6) #高雄

model_NWT <- lm(vq1 ~ as.factor(income_group) + vb1 + as.factor(vb2) + as.factor(vb7) + as.factor(vb4), data = data_NWT)
model_TPE <- lm(vq1 ~ as.factor(income_group) + vb1 + as.factor(vb2) + as.factor(vb7) + as.factor(vb4), data = data_TPE)
model_TAO <- lm(vq1 ~ as.factor(income_group) + vb1 + as.factor(vb2) + as.factor(vb7) + as.factor(vb4), data = data_TAO)
model_TXG <- lm(vq1 ~ as.factor(income_group) + vb1 + as.factor(vb2) + as.factor(vb7) + as.factor(vb4), data = data_TXG)
model_TNN <- lm(vq1 ~ as.factor(income_group) + vb1 + as.factor(vb2) + as.factor(vb7) + as.factor(vb4), data = data_TNN)
model_KHH <- lm(vq1 ~ as.factor(income_group) + vb1 + as.factor(vb2) + as.factor(vb7) + as.factor(vb4), data = data_KHH)

summary(model_NWT)
summary(model_TPE)
summary(model_TAO)
summary(model_TXG)
summary(model_TNN)
summary(model_KHH)



#2016-2020資料------------------------------------------------------------------

setwd("C:/Users/yinchi/Desktop/113-1/STATA/2016-2020_sas")

library(haven)
library(dplyr)


# 定義檔案路徑
files <- list(
  "2016" = "traveler105.sav",
  "2017" = "traveler106.sav",
  "2018" = "traveler107.sav",
  "2019" = "traveler108.sav",
  "2020" = "traveler109.sav"
)

# 定義變數對應表
variables <- list(
  "2016" = c("vq1", "vb6", "vb2", "vb3", "vb5", "vb1"),
  "2017" = c("vq1", "vb5", "vb1", "vb2", "vb4", "vb8"),
  "2018" = c("vq32", "vb5", "vb1", "vb2", "vb4", "vb7"),
  "2019" = c("vq1", "vb5", "vb1", "vb2", "vb4", "vb7"),
  "2020" = c("vq1", "vb5", "vb1", "vb2", "vb4", "vb7")
)

# 初始化空的資料列表
data_list <- list()

# 逐年讀取資料並統一命名
for (year in names(files)) {
  # 讀取資料
  data <- read_sav(files[[year]])
  
  # 提取對應的變數並重新命名
  selected_data <- data %>%
    select(
      Trips = all_of(variables[[year]][1]),
      Income = all_of(variables[[year]][2]),
      Age = all_of(variables[[year]][3]),
      Edu = all_of(variables[[year]][4]),
      Marriage = all_of(variables[[year]][5]),
      Gender = all_of(variables[[year]][6])
    )
  
  # 添加年份標記
  selected_data <- selected_data %>%
    mutate(Year = as.numeric(year))
  
  # 儲存到列表中
  data_list[[year]] <- selected_data
}

# 合併所有年份的資料
final_data <- bind_rows(data_list)

# 檢查資料結構
str(final_data)

# 儲存結果為新的檔案
# write.csv(final_data, "merged_data_2016_2020.csv", row.names = FALSE)

final_data <- read.csv("merged_data_2016_2020.csv")

final_data <- final_data %>%
  mutate(income_group = case_when(
    Income >= 1 & Income <= 4 ~ 0, # 低收入
    Income >= 5 & Income <= 7 ~ 1, # 中收入
    Income >= 8 & Income <= 10 ~ 2 # 高收入
  ))

# 包含缺失值的樣本直接刪除
final_data <- na.omit(final_data)

all_model <- lm(Trips ~ as.factor(income_group) + Age + as.factor(Edu) + as.factor(Marriage) + as.factor(Age) + as.factor(Year), data = final_data)
summary(all_model)



# 原始資料旅次分布圖------------------------------------------------------------


library(ggplot2)

ggplot(data, aes(x = vq1)) +
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue", alpha = 0.7) +
  labs(title = "Histogram of Domestic Travel Frequency", 
       x = "Number of Domestic Trips", 
       y = "Frequency") +
  theme_minimal()

















