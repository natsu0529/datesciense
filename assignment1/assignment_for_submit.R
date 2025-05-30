load("~/Desktop/データサイエンス/assignment1/pima.RData")
setwd("~/Desktop/データサイエンス/assignment1")
# 新しいカテゴリー変数を作成
pima$bmi_Category <- cut(pima$bmi,
                              breaks = c(0, 25, 40, Inf),
                              labels = c("Normal", "Overweight", "Obese"))

# 新しい変数を factor 型に変換
pima$bmi_Category <- factor(pima$bmi_Category)

# BMI カテゴリーごとの糖尿病血統要因の平均値を計算
mean_pedigree_by_bmi <- aggregate(diabetes ~ bmi_Category,
                                  data = pima,
                                  FUN = mean)

# 結果を表示
print(mean_pedigree_by_bmi)

# 分散分析の実行
aov_result <- aov(diabetes ~ bmi_Category, data = pima)

# 結果要約（F値とp値を確認）
summary(aov_result)

TukeyHSD(aov_result)

# オプション: 等分散性の検定（Bartlett検定）
bartlett.test(diabetes ~ bmi_Category, data = pima)

# オプション: 正規性の検定（Shapiro-Wilk）
shapiro.test(residuals(aov_result))     

boxplot(diabetes ~ bmi_Category, data = pima)

# ペアワイズt検定（ボンフェローニ補正）
pairwise_result <- pairwise.t.test(
  pima$diabetes,
  pima$bmi_Category,
  p.adjust.method = "bonferroni"
)

# 結果表示
print(pairwise_result)


library(ggplot2)
library(dplyr)

# ボンフェローニ法の結果をデータフレーム化
bonf_df <- pairwise_result$p.value %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("group1") %>% 
  tidyr::pivot_longer(
    cols = -group1,
    names_to = "group2",
    values_to = "p_adj"
  ) %>% 
  na.omit() %>% 
  mutate(
    significance = case_when(
      p_adj < 0.001 ~ "***",
      p_adj < 0.01 ~ "**",
      p_adj < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

# 箱ひげ図と組み合わせたプロット
ggplot(pima, aes(x = bmi_Category, y = diabetes)) +
  geom_boxplot(width = 0.5, fill = "lightblue") +
  geom_jitter(width = 0.1, alpha = 0.3) +
  geom_text(
    data = bonf_df,
    aes(x = (as.numeric(factor(group1)) + as.numeric(factor(group2)))/2,
        y = max(pima$diabetes) + 0.1,
        label = significance),
    size = 6,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Diabetes Risk by BMI Category",
    subtitle = "Bonferroni-adjusted pairwise comparisons",
    x = "BMI Category",
    y = "Diabetes Probability"
  ) +
  theme_minimal(base_size = 14) +
  ylim(0, 1.2)




