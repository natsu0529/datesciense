setwd("~/Desktop/データサイエンス/5_lec9_nonlinearmodel/assignment2")

data <- read.csv("~/Desktop/データサイエンス/5_lec9_nonlinearmodel/assignment2/internet.csv", stringsAsFactors = FALSE)

# 最大値の行
data[which.max(data$Int), ]

# 最小値の行
data[which.min(data$Int), ]

mean(data$Int, na.rm = TRUE)

# Int列の平均を計算
avg_int <- mean(data$Int, na.rm = TRUE)

# 平均に最も近い行のインデックスを取得
closest_index <- which.min(abs(data$Int - avg_int))

# 対応する国名を取得
closest_country <- data$Country[closest_index]

# 結果を表示
print(closest_country)



library(ggplot2)

ggplot(data, aes(x = Gdp, y = Int)) +
  geom_point(color = "blue") +
  labs(title = "Gdp と Int の散布図",
       x = "Gdp",
       y = "Int") +
  theme_minimal()


# 単回帰モデルを作成
model <- lm(Int ~ Gdp, data = data)

# 結果を表示
summary(model)


res <- resid(model)

min_index <- which.min(res)  # 最もマイナスが大きい（≒一番下に外れている）

# 国名を表示
worst_country <- data$Country[min_index]
print(worst_country)



max_index <- which.max(res)
best_country <- data$Country[max_index]
print(best_country)


data$residuals <- resid(model)
data$fitted <- fitted(model)

# プロット
ggplot(data, aes(x = fitted, y = residuals)) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "残差 vs 予測値（Fitted Values）プロット",
    x = "予測値（Fitted Values）",
    y = "残差（Residuals）"
  ) +
  theme_minimal()
