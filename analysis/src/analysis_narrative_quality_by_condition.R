# analysis_narrative_quality_by_condition
# 物語に対する評価の分析

# 0. ライブラリの読み込み ----
library(tidyr)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(psych)
library(lme4)
library(lmerTest)
library(scales)
source("./anovakun_489.txt")
source("http://aoki2.si.gunma-u.ac.jp/R/src/mycor.R", encoding="euc-jp") #mycorを持ってくる

# 1. 各物語に対する評定結果を物語ごとにまとめる ----
# 1.1. 基本的なデータの整形 ----

# 各種データの読み込み
narrative_dat <- read.csv("../data/narrative_evaluation_data.csv", fileEncoding="shift-jis")

# データの整形: 参加者の年齢、性別、最終学歴、回答時間のメモ
dat_demographic <- narrative_dat %>%
  dplyr::select(ID, Duration..in.seconds., Sex, Age, AcademicDegree) %>%
  dplyr::mutate(ID = as.factor(ID),
                DurationSeconds = as.integer(Duration..in.seconds.),
                Sex = as.factor(Sex),
                Age = as.integer(Age),
                AcademicDegree = as.factor(AcademicDegree))

# 年齢
dat_age_all <- dat_demographic %>% 
  dplyr::select(Age)  %>% 
  summarise(m_age = mean(Age), sd_age = sd(Age), min_age = min(Age), max_age = max(Age))
dat_age_all

# 性別
dat_sex_all <- dat_demographic %>% 
  dplyr::select(Sex) %>% 
  group_by(Sex) %>% 
  summarise(count = n())
dat_sex_all

# 学歴
dat_degree_all <- dat_demographic %>% 
  dplyr::select(AcademicDegree) %>% 
  group_by(AcademicDegree) %>% 
  summarise(count = n())
dat_degree_all

# 回答時間
dat_duration_all <- dat_demographic %>% 
  dplyr::select(DurationSeconds) %>% 
  summarise(m_sec = mean(DurationSeconds), sd_sec = sd(DurationSeconds), min_sec = min(DurationSeconds), max_sec = max(DurationSeconds))
dat_duration_all

# データの整形: 分析対象のデータ
dat_evaluation <- narrative_dat %>%
  dplyr::select(ID ,
                starts_with("A_"), starts_with("B_"), starts_with("C_"),
                starts_with("D_"), starts_with("E_"), starts_with("F_")) %>% #必要列の読み込み
  tidyr::gather(key = cols, value = Value, -ID) %>% 
  tidyr::separate(col = cols, into = c("Group", "Condition", "CondID", "QuestionID"), sep="_") %>% #colsをgroup/cond/condid/quest/valueに分解
  dplyr::filter(Value != "") %>%
  dplyr::mutate(ID = as.factor(ID),
                Group = as.factor(Group),
                Condition = as.factor(Condition),
                CondID = as.factor(CondID),
                QuestionID = as.factor(QuestionID),
                Value = as.integer(Value))


# 1.2 各物語に対する評定結果を物語ごとにまとめて、回答傾向との関連をみるための準備 ----
# ConditionのCondIDごと (つまり、実験1Aで参加者が作った物語ごと)のQuestionID(回答項目)でプールした
dat_ave_evaluation_by_item <- dat_evaluation %>%
  dplyr::group_by(Condition, CondID, QuestionID) %>%
  dplyr::summarise(Average = mean(Value), SD = sd(Value))

# dat_ave_evaluation_by_itemの出力
# write.csv(x = dat_ave_evaluation_by_item, 
#           file = "../result/dat_ave_evaluation_by_item.csv",
#           fileEncoding = "shift-jis",
#           row.names=FALSE)

# 1.3 条件ごとの評定結果をまとめて、分散分析 ----
# Conditionごとでプールした結果 (すなわち、条件間の平均値と標準偏差)
# -> 全体に非有意

# データの整形
dat_ave_evaluation_by_condition <- dat_evaluation %>%
  dplyr::group_by(Condition, QuestionID) %>%
  dplyr::summarise(Average = mean(Value), SD = sd(Value))

# QuestionID==1(物語の面白さ)で比較: 項目分析
# -> 非有意
dat_anova_funniness <- dat_ave_evaluation_by_item %>%
  dplyr::filter(QuestionID == "1") %>%
  dplyr::select(CondID, Condition, Average)
anovakun(dat_anova_funniness, "As", long=T)

# QuestionID==2(物語の独創性)で比較: 項目分析
# -> 非有意
dat_anova_original <- dat_ave_evaluation_by_item %>%
  dplyr::filter(QuestionID == "2") %>%
  dplyr::select(CondID, Condition, Average)
anovakun(dat_anova_original, "As", long=T)

# QuestionID==3(物語の構成の良さ)で比較: 項目分析
# -> 非有意
dat_anova_well_structured <- dat_ave_evaluation_by_item %>%
  dplyr::filter(QuestionID == "3") %>%
  dplyr::select(CondID, Condition, Average)
anovakun(dat_anova_well_structured, "As", long=T)

