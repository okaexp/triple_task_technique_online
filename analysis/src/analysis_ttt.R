# analysis_ttt

# 0. ライブラリの読み込み ----
library(tidyr)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(psych)
library(lme4)
library(lmerTest)
library(scales)

# ggplotのフォント
library(showtext)
font_add("ipaexg", "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc")
showtext_auto()

#井関先生のanovakunを落とす
#see: https://riseki.cloudfree.jp/?ANOVA%E5%90%9B
source("./anovakun_489.txt")


source("http://aoki2.si.gunma-u.ac.jp/R/src/mycor.R", encoding="euc-jp") #mycorを持ってくる

# 1. 条件間の参加者の等質性関連 ----
# 1.1. QualtricsのStartToDemographicとEndで分析対象とするべき参加者をfilterする ----

# 各種データの読み込み
raw_start <- read.csv("../data/ttt_question_start.csv", fileEncoding="shift-jis")
raw_finish <- read.csv("../data/ttt_question_end.csv", fileEncoding="shift-jis")

# 分析対象のIDとconditionをraw_finishから抽出
dat_finish <- raw_finish %>%
  dplyr::select(ID, condition) %>%
  dplyr::mutate(ID = as.factor(ID),
                condition = as.factor(condition))

# 分析対象のlst
id_finished_lst <- unique(dat_finish$ID)

# 実験条件ごとの人数のカウント
dat_finish_condition <- dat_finish %>%
  dplyr::select(condition)  %>% 
  group_by(condition) %>% 
  summarise(count = n())
dat_finish_condition

# 必要な情報を抽出
dat_demographic <- raw_start %>%
  dplyr::select(ID, Sex, Age, PrefHandOrType, FreqHandAndType_1, FreqHandAndType_2) %>%
  dplyr::mutate(ID = as.factor(ID),
                Sex = as.factor(Sex),
                Age = as.integer(Age),
                PrefHandOrType = as.integer(PrefHandOrType),
                FreqHand = as.integer(FreqHandAndType_1),
                FreqType = as.integer(FreqHandAndType_2),
                ) %>% #変換
  dplyr::filter(ID %in% id_finished_lst) %>% #finishにいるIDを抽出
  dplyr::distinct(ID, .keep_all = TRUE) %>% #重複している人を除外
  dplyr::select(-FreqHandAndType_1, -FreqHandAndType_2)

# dat_demographicにIDとconditionをくっつける
dat_demographic <- dat_demographic %>%
  dplyr::inner_join(dat_finish, by="ID")

# 平均年齢
# M = 42.6, SD = 7.8
dat_age <- dat_demographic %>% 
  dplyr::select(Age)  %>% 
  summarise(m_age = mean(Age), sd_age = sd(Age), min_age = min(Age), max_age = max(Age))
dat_age

# 性別
# male: 37, female: 23
dat_sex <- dat_demographic %>% 
  dplyr::select(Sex) %>% 
  group_by(Sex) %>% 
  summarise(count = n())
dat_sex

# 1.2. control/outline/structured条件ごとの人数、年齢、執筆の好み、執筆頻度を比較する(anovakun) ----
# 1.2.1 年齢の比較 -> 平均年齢に差はない ----
dat_condition_age <- dat_demographic %>%
  dplyr::select(ID, condition, Age)
anovakun(dat_condition_age, "As", long=T)

# 1.2.2 執筆の好みの比較 -> 執筆の好みに差はない ----
dat_condition_pref_hand_or_type <- dat_demographic %>%
  dplyr::select(ID, condition, PrefHandOrType)
anovakun(dat_condition_pref_hand_or_type, "As", long=T)

# 1.2.3 手書きの執筆頻度 -> 手書きの執筆頻度に差はない ----
dat_condition_freqhand <- dat_demographic %>%
  dplyr::select(ID, condition, FreqHand)
anovakun(dat_condition_freqhand, "As", long=T)

# 1.2.4 タイピングの執筆頻度 -> タイピングの執筆頻度に差はない ----
dat_condition_freqtype <- dat_demographic %>%
  dplyr::select(ID, condition, FreqType)
anovakun(dat_condition_freqtype, "As", long=T)


# 2. 単純反応課題の反応時間関係 (conditionでの比較) ----

# 単純反応課題のデータの読み込み
raw_simple_rt <- read.csv("../data/1_ttt_simple_rt.csv", fileEncoding = "shift-jis")

dat_simple_rt <- raw_simple_rt %>%
  dplyr::select(ID, condition, trial_type_custom, rt_pra_or_main, delay, rt) %>%
  dplyr::mutate(ID = as.factor(ID),
                condition = as.factor(condition),
                trial_type_custom = as.factor(trial_type_custom),
                rt_pra_or_main = as.factor(rt_pra_or_main)) %>% #変換
  dplyr::filter(ID %in% id_finished_lst) %>%
  dplyr::filter(trial_type_custom == "rt_trial") %>%
  dplyr::mutate(delay = as.integer(delay),
                rt = as.integer(rt))#変換

# 分析対象とするmainの、平均反応時間を求める→histの通り
dat_simple_rt_main <- dat_simple_rt %>%
  dplyr::filter(rt_pra_or_main == "main") %>%
  dplyr::mutate(rt_for_beep = rt - delay) %>%
  dplyr::select(ID, condition, rt_for_beep)
hist(dat_simple_rt_main$rt_for_beep)
nrow(dat_simple_rt_main)

# 明らかな外れ値があるので、これを除外
# -> 平均±3SDのトライアルを除外8回分の試行を除外(残試行: 1792/1800)
dat_simple_rt_main_rm_outlier <- dat_simple_rt_main %>%
  dplyr::mutate(mean_rt_for_beep = mean(rt_for_beep, na.rm=TRUE),
                sd_rt_for_beep   = sd(rt_for_beep, na.rm = TRUE),
                rt_for_beep_outlier = (rt_for_beep < mean_rt_for_beep - 3*sd_rt_for_beep) | (rt_for_beep > mean_rt_for_beep + 3*sd_rt_for_beep)
                ) %>%
  filter(!rt_for_beep_outlier)
hist(dat_simple_rt_main_rm_outlier$rt_for_beep)
nrow(dat_simple_rt_main_rm_outlier)

# dat_simple_rt_main_rm_outlierの結果を条件間で比較
# control = outline < structuredの順
dat_condition_mean_rt_for_beep <- dat_simple_rt_main_rm_outlier %>%
  dplyr::select(ID, condition, rt_for_beep) %>%
  dplyr::group_by(ID, condition) %>%
  dplyr::summarise(mean_rt_for_beep = mean(rt_for_beep))
anovakun(dat_condition_mean_rt_for_beep, "As", long=T)

# 3. pre_writing_pause/writing_durationを条件間で比較する(anovakun) ----

# 条件ごとのデータの読み込み
raw_control <- read.csv("../data/1_ttt_control.csv", fileEncoding = "shift-jis")
raw_outline <- read.csv("../data/1_ttt_outline.csv", fileEncoding = "shift-jis")
raw_structured <- read.csv("../data/1_ttt_structured.csv", fileEncoding = "shift-jis")

# 条件ごとのデータの整形
dat_control <- raw_control %>%
  dplyr::select(ID, condition, task, writing_duration_rt, pre_writing_pause) %>%
  dplyr::mutate(ID = as.factor(ID),
                condition = as.factor(condition),
                task = as.factor(task)) %>% #変換
  dplyr::filter(ID %in% id_finished_lst) %>%
  dplyr::filter(task == "writing_block") %>%
  dplyr::mutate(writing_duration_rt = as.integer(writing_duration_rt),
                pre_writing_pause = as.integer(pre_writing_pause))

dat_outline <- raw_outline %>%
  dplyr::select(ID, condition, task, writing_duration_rt, pre_writing_pause) %>%
  dplyr::mutate(ID = as.factor(ID),
                condition = as.factor(condition),
                task = as.factor(task)) %>% #変換
  dplyr::filter(ID %in% id_finished_lst) %>% #finishにいるIDを抽出
  dplyr::filter(task == "writing_block") %>%
  dplyr::mutate(writing_duration_rt = as.integer(writing_duration_rt),
                pre_writing_pause = as.integer(pre_writing_pause))

dat_structured <- raw_structured %>%
  dplyr::select(ID, condition, task, writing_duration_rt, pre_writing_pause) %>%
  dplyr::mutate(ID = as.factor(ID),
                condition = as.factor(condition),
                task = as.factor(task)) %>% #変換
  dplyr::filter(ID %in% id_finished_lst) %>% #finishにいるIDを抽出
  dplyr::filter(task == "writing_block") %>%
  dplyr::mutate(writing_duration_rt = as.integer(writing_duration_rt),
                pre_writing_pause = as.integer(pre_writing_pause))

#データの結合
dat_condition_writing_rt <- dat_control %>%
  dplyr::bind_rows(dat_outline) %>%
  dplyr::bind_rows(dat_structured)

# 3.1 pre_writing_pauseを条件間で比較する(anovakun) -> 条件差なし ----
#データの結合
dat_condition_pwp <- dat_condition_writing_rt %>%
  dplyr::select(-task, -writing_duration_rt)

#外れ値の確認
hist(dat_condition_pwp$pre_writing_pause)

#外れ値の除外
dat_condition_pwp_rm_outlier <- dat_condition_writing_rt %>%
  dplyr::mutate(mean_pwp = mean(pre_writing_pause, na.rm=TRUE),
                sd_pwp   = sd(pre_writing_pause, na.rm = TRUE),
                outlier = (pre_writing_pause < mean_pwp - 3*sd_pwp) | (pre_writing_pause > mean_pwp + 3*sd_pwp)
  ) %>%
  dplyr::filter(!outlier) %>%
  dplyr::select(ID, condition, pre_writing_pause)

anovakun(dat_condition_pwp_rm_outlier, "As", long=T)

# 3.2 writing_durationを条件間で比較する(anovakun) -> 条件差なし ----
#データの結合
dat_condition_wd <- dat_condition_writing_rt %>%
  dplyr::select(-task, -pre_writing_pause)

#外れ値の確認 -> 最後までやった人が30名以上でデータはskew(分けて考える?)
hist(dat_condition_wd$writing_duration_rt)

anovakun(dat_condition_wd, "As", long=T)

# 4. thought probesの条件間での比較 ----
# 4.1 thought probesの条件*時点ごとの図の描画 → outlineは他の条件と振る舞いがことなるように見える ----
# 条件ごとのデータの読み込み
raw_control_probes <- read.csv("../data/1_ttt_control_probes.csv", fileEncoding = "shift-jis")
raw_outline_probes <- read.csv("../data/1_ttt_outline_probes.csv", fileEncoding = "shift-jis")
raw_structured_probes <- read.csv("../data/1_ttt_structured_probes.csv", fileEncoding = "shift-jis")

# 条件ごとのデータの整形
dat_control_probes <- raw_control_probes %>%
  dplyr::filter(ID %in% id_finished_lst) %>%
  #必要なデータの選別
  dplyr::select(ID, condition, order, probe_interval, response_time_for_beep, response) %>%
  dplyr::mutate(ID = as.factor(ID),
                condition = as.factor(condition),
                order = as.integer(order),
                probe_interval = as.integer(probe_interval),
                response_time_for_beep = as.integer(response_time_for_beep),
                response = as.factor(response)
                ) %>%
  dplyr::arrange(ID, order)

dat_outline_probes <- raw_outline_probes %>%
  dplyr::filter(ID %in% id_finished_lst) %>%
  dplyr::select(ID, condition, order, probe_interval, response_time_for_beep, response) %>%
  dplyr::mutate(ID = as.factor(ID),
                condition = as.factor(condition),
                order = as.integer(order),
                probe_interval = as.integer(probe_interval),
                response_time_for_beep = as.integer(response_time_for_beep),
                response = as.factor(response)
  ) %>%
  dplyr::arrange(ID, order)

dat_structured_probes <- raw_structured_probes %>%
  dplyr::filter(ID %in% id_finished_lst) %>%
  #必要なデータの選別
  dplyr::select(ID, condition, order, probe_interval, response_time_for_beep, response) %>%
  dplyr::mutate(ID = as.factor(ID),
                condition = as.factor(condition),
                order = as.integer(order),
                probe_interval = as.integer(probe_interval),
                response_time_for_beep = as.integer(response_time_for_beep),
                response = as.factor(response)
  ) %>%
  dplyr::arrange(ID, order)

#1/3ごとにPhase1, Phase2, Phase3を割り当てる
dat_control_probes_with_phase <- dat_control_probes %>%
  group_by(ID) %>%
  arrange(order, .by_group = TRUE) %>%   # 回答順序で並べる
  mutate(
    phase_num = ntile(row_number(), 3),  # 3等分
    Phase = factor(
      phase_num,
      levels = 1:3,
      labels = c("Phase1", "Phase2", "Phase3")
    )
  ) %>%
  ungroup() %>%
  select(-phase_num)  # 補助列を消すなら

dat_outline_probes_with_phase <- dat_outline_probes %>%
  group_by(ID) %>%
  arrange(order, .by_group = TRUE) %>%   # 回答順序で並べる
  mutate(
    phase_num = ntile(row_number(), 3),  # 3等分
    Phase = factor(
      phase_num,
      levels = 1:3,
      labels = c("Phase1", "Phase2", "Phase3")
    )
  ) %>%
  ungroup() %>%
  select(-phase_num)  # 補助列を消すなら

dat_structured_probes_with_phase <- dat_structured_probes %>%
  group_by(ID) %>%
  arrange(order, .by_group = TRUE) %>%   # 回答順序で並べる
  mutate(
    phase_num = ntile(row_number(), 3),  # 3等分
    Phase = factor(
      phase_num,
      levels = 1:3,
      labels = c("Phase1", "Phase2", "Phase3")
    )
  ) %>%
  ungroup() %>%
  select(-phase_num)  # 補助列を消すなら

#各参加者の各Phaseごとの各responseの総数を集計
#残課題: otherの出現頻度を数える
dat_freq_response_by_phase_control <- dat_control_probes_with_phase %>%
  dplyr::group_by(ID, condition, Phase, response) %>%
  dplyr::summarise(freq = n(), .groups = "drop") %>%
  dplyr::filter(response != "other")

dat_freq_response_by_phase_outline <- dat_outline_probes_with_phase %>%
  dplyr::group_by(ID, condition, Phase, response) %>%
  dplyr::summarise(freq = n(), .groups = "drop") %>%
  dplyr::filter(response != "other")

dat_freq_response_by_phase_structured <- dat_structured_probes_with_phase %>%
  dplyr::group_by(ID, condition, Phase, response) %>%
  dplyr::summarise(freq = n(), .groups = "drop") %>%
  dplyr::filter(response != "other")

#横軸にPhase1, Phase2, Phase3、縦軸にresponseの頻度、凡例にresponseの種類（A, B, C）をまとめた折線グラフを書く
# 平均回答をPhaseごとにまとめる
avg_freq_probes_control <- dat_freq_response_by_phase_control %>%
  group_by(condition, Phase, response) %>%
  summarise(mean_freq = mean(freq),
            se_freq   = sd(freq) / sqrt(n()),
            .groups = "drop")

# 折れ線＋エラーバー
ggplot(avg_freq_probes_control, aes(x = Phase, y = mean_freq,
                         color = response, group = response)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_freq - se_freq,
                    ymax = mean_freq + se_freq),
                width = 0.1) +
  labs(x = "Phase", y = "Mean Frequency (±SE)", color = "Response") +
  theme_minimal(base_size = 14)

avg_freq_probes_outline <- dat_freq_response_by_phase_outline %>%
  group_by(condition, Phase, response) %>%
  summarise(mean_freq = mean(freq),
            se_freq   = sd(freq) / sqrt(n()),
            .groups = "drop")

ggplot(avg_freq_probes_outline, aes(x = Phase, y = mean_freq,
                                    color = response, group = response)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_freq - se_freq,
                    ymax = mean_freq + se_freq),
                width = 0.1) +
  labs(x = "Phase", y = "Mean Frequency (±SE)", color = "Response") +
  theme_minimal(base_size = 14)

avg_freq_probes_structured <- dat_freq_response_by_phase_structured %>%
  group_by(condition, Phase, response) %>%
  summarise(mean_freq = mean(freq),
            se_freq   = sd(freq) / sqrt(n()),
            .groups = "drop")

ggplot(avg_freq_probes_structured, aes(x = Phase, y = mean_freq,
                                    color = response, group = response)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_freq - se_freq,
                    ymax = mean_freq + se_freq),
                width = 0.1) +
  labs(x = "Phase", y = "Mean Frequency (±SE)", color = "Response") +
  theme_minimal(base_size = 14)

# 3枚まとめて1枚の図に
# 条件ごとの参加者の各Phaseにおける各responseの回答数のデータ(dat_freq_response_by_phase_*)を結合
dat_freq_response_by_phase_conditions <- dat_freq_response_by_phase_control %>%
  dplyr::bind_rows(dat_freq_response_by_phase_outline) %>%
  dplyr::bind_rows(dat_freq_response_by_phase_structured)

# データテーブルの作成
avg_freq_probes_conditions <- dat_freq_response_by_phase_conditions %>%
  group_by(condition, Phase, response) %>%
  summarise(
    mean_freq = mean(freq),                # 各条件の平均
    se_freq   = sd(freq) / sqrt(n()),      # 標準誤差
    .groups = "drop"
  )

# 全体での最大値を取得
y_max <- max(avg_freq_probes_conditions$mean_freq + avg_freq_probes_conditions$se_freq, na.rm = TRUE)

# 折れ線＋エラーバーの図を条件ごとに横並び
ggplot(avg_freq_probes_conditions, aes(x = Phase, y = mean_freq,
                         color = response, group = response)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_freq - se_freq,
                    ymax = mean_freq + se_freq),
                width = 0.1) +
  facet_wrap(~ condition, nrow = 1) +   # 条件ごとに横に並べる
  scale_y_continuous(limits = c(0, y_max)) +
  labs(x = "Phase", y = "Mean Frequency (±SE)", color = "Response") +
  theme_minimal(base_size = 14)

# 3枚まとめて1枚の図に (認知科学用) ----
# 条件ごとの参加者の各Phaseにおける各responseの回答数のデータ(dat_freq_response_by_phase_*)を結合

#copyしてlevelsをrename
#otherを消す
avg_freq_probes_conditions_jpn <- avg_freq_probes_conditions %>%
  mutate(思考プローブ = recode(response,
                         "planning" = "プランニング",
                         "output"   = "出力",
                         "revision" = "修正"),
         condition = recode(condition,
                            "control" = "control",
                            "outline" = "outline",
                            "structured" = "structured outline")) %>%
  dplyr::filter(思考プローブ != "other") %>%
  droplevels() %>% # ★ここで因子レベルを整理
  dplyr::mutate(
    Phase = factor(Phase, labels = c("前半", "中盤", "後半")),
    思考プローブ = factor(思考プローブ, levels = c("プランニング", "出力", "修正"))
  )

# 折れ線＋エラーバーの図を条件ごとに横並び
# 線の種類とかもLimpo & Alvesに合わせた
ggplot()+theme_set(theme_classic(base_size = 14,base_family="ipaexg"))
p <- ggplot(avg_freq_probes_conditions_jpn, aes(x = Phase, y = mean_freq, group = 思考プローブ)) +
  geom_line(aes(linetype = 思考プローブ), size = 1) +
  geom_point(aes(shape = 思考プローブ), size = 2) +
  geom_errorbar(aes(ymin = mean_freq - se_freq,
                    ymax = mean_freq + se_freq),
                width = 0.1,
                linetype="solid") +
  scale_linetype_manual(values = c("dotted", "solid", "dashed")) +
  scale_shape_manual(values = c(17, 15, 16)) +
  facet_wrap(~ condition, nrow = 1) +   # 条件ごとに横に並べる
  scale_y_continuous(limits = c(0, y_max)) +
  theme(axis.title.y = element_text(angle=0, vjust=0.5, size=20),
       axis.text.y = element_text(size=16),
       #axis.title.x = element_blank(),
       axis.text.x = element_text(size = 16)) +
  labs(x = "時点", y = "出\n現\n頻\n度", color = "思考プローブ")
plot(p)
showtext_auto()
#ggsave("../result/line_plot_of_thought_probe_responses_jpn.pdf", plot = p, width = 10, height = 5, dpi = 300)

# 4.2 thought probesの条件*時点ごとの比較(anova) -> 交互作用見られず----

#anova実施(Limpo and Alvesはancovaだが) -> 交互作用見られず
anovakun(dat_freq_response_by_phase_conditions, "AsBC", long=T, peta=TRUE)

# 4.2.1 thought probesの条件*時点ごとの比較 (contorl, anova) -> 交互作用は見られたが、全体傾向は変わらず ----
# dat_freq_response_by_phase_control <- dat_freq_response_by_phase_conditions %>%
#   dplyr::filter(condition == "control") %>%
#   dplyr::select(ID, Phase, response, freq)
# anovakun(dat_freq_response_by_phase_control, "sAB", long=T, peta=TRUE)

# 5. 分析対象 (60名)の執筆内容の保存 ----

# 条件ごとのデータの整形
dat_control_text <- raw_control %>%
  dplyr::select(ID, condition, writing_txt) %>%
  dplyr::mutate(ID = as.factor(ID),
                condition = as.factor(condition)) %>% #変換
  dplyr::filter(ID %in% id_finished_lst) %>%
  dplyr::filter(writing_txt != "NULL")

dat_outline_text <- raw_outline %>%
  dplyr::select(ID, condition, writing_txt) %>%
  dplyr::mutate(ID = as.factor(ID),
                condition = as.factor(condition)) %>% #変換
  dplyr::filter(ID %in% id_finished_lst) %>%
  dplyr::filter(writing_txt != "NULL")

dat_structured_text <- raw_structured %>%
  dplyr::select(ID, condition, writing_txt) %>%
  dplyr::mutate(ID = as.factor(ID),
                condition = as.factor(condition)) %>% #変換
  dplyr::filter(ID %in% id_finished_lst) %>% #finishにいるIDを抽出
  dplyr::filter(writing_txt != "NULL")

#データの結合
dat_condition_text <- dat_control_text %>%
  dplyr::bind_rows(dat_outline_text) %>%
  dplyr::bind_rows(dat_structured_text) %>%
  dplyr::mutate(text_length = nchar(writing_txt))

# # データの出力
# write.csv(x = dat_condition_text, 
#           file = "../result/dat_condition_text_complete_v0.2.csv",
#           fileEncoding = "shift-jis")

# 6. 評定値と他の指標の相関 ----
'''
相関係数を算出する指標
 - sex, age, other demographics (dat_demographic_rev)
 - simple reaction time (dat_condition_mean_rt_for_beep_rev)
 - writing_duration/pre_writing_pause (dat_condition_writing_rt_rev)
 - text_length (dat_condition_text_rev)
 - phaseごとのplanning/output/revisionの頻度 (dat_freq_response_by_phase_all)
 - 物語の面白さ/創造性/構成の良さ (dat_ave_evaluation_by_item, dat_id_to_item)
'''
# ここまで (10/12, 19:34)

# データの読み込み
dat_ave_evaluation_by_item <- read.csv("../result/dat_ave_evaluation_by_item.csv")#物語の評価での参加者の評定値
dat_id_to_item <- read.xlsx("../result/dat_condition_text_complete.xlsx", sheet="テキスト")

# 6.1 データの準備 ----
# demographicの準備
dat_demographic_rev <- dat_demographic %>%
  dplyr::select(ID, Sex, Age, PrefHandOrType, FreqHand, FreqType)

# dat_condition_mean_rt_for_beepの準備
dat_condition_mean_rt_for_beep_rev <- dat_condition_mean_rt_for_beep %>%
  dplyr::select(ID, mean_rt_for_beep)

# dat_condition_writing_rt_revの準備
dat_condition_writing_rt_rev <- dat_condition_writing_rt %>%
  dplyr::select(ID, writing_duration_rt, pre_writing_pause)

# 実験1Aの参加者IDを項目IDに変換してワイド型にして、その後、IDと紐付ける (dat_ave_evaluation_by_item, dat_id_to_item)
dat_ave_evaluation_by_item_condid <- dat_ave_evaluation_by_item %>%
  tidyr::unite(CondID, Condition, CondID, remove=T, sep="_") %>%
  dplyr::select(-SD) %>%
  tidyr::spread(key=QuestionID, value=Average) %>%
  dplyr::rename(funniness = 2, original = 3, well_structured = 4)

# CondIDを軸にIDと紐付け
dat_ave_evaluation_by_item_w_condid_id <- dat_id_to_item %>%
  dplyr::select(ID, CondID) %>% #必要列の抽出
  dplyr::right_join(dat_ave_evaluation_by_item_condid, by="CondID") %>%
  dplyr::mutate(ID = as.factor(ID),
                CondID = as.factor(CondID))

# phaseごとのplanning/output/revisionの頻度: 1つにまとめてwideにする (dat_freq_response_by_phase_all)
dat_freq_response_by_phase_all <- dat_freq_response_by_phase_control %>%
  dplyr::bind_rows(dat_freq_response_by_phase_outline) %>%
  dplyr::bind_rows(dat_freq_response_by_phase_structured) %>%
  tidyr::pivot_wider(names_from = c("Phase", "response"), values_from = freq, names_sep="_")

#NAを0埋め
dat_freq_response_by_phase_all[is.na(dat_freq_response_by_phase_all)] <- 0
head(dat_freq_response_by_phase_all)

# dat_condition_text_revの準備
dat_condition_text_rev <- dat_condition_text %>%
  dplyr::select(ID, text_length)

# wideで全結合したデータ
dat_correlations <- dat_demographic_rev %>%
  dplyr::right_join(dat_condition_mean_rt_for_beep_rev, by="ID") %>%
  dplyr::right_join(dat_condition_writing_rt_rev, by="ID") %>%
  dplyr::right_join(dat_ave_evaluation_by_item_w_condid_id, by="ID") %>%
  dplyr::right_join(dat_freq_response_by_phase_all, by="ID") %>%
  dplyr::right_join(dat_condition_text_rev, by="ID") %>%
  dplyr::select(ID, CondID, condition, everything())

# 6.2 相関分析/記述統計量 ----
mycor(5:24, dat_correlations, latex = FALSE)
describe(dat_correlations[5:24])

# 6.3 control/outline/structuredの条件ごとの相関係数 ----
# control
dat_correlations_control <- dat_correlations %>%
  dplyr::filter(condition == "control")
mycor(5:24, dat_correlations_control, latex = FALSE)
describe(dat_correlations_control[5:24])

# outline
dat_correlations_outline <- dat_correlations %>%
  dplyr::filter(condition == "outline")
mycor(5:24, dat_correlations_outline, latex = FALSE)
describe(dat_correlations_outline[5:24])

# structured
dat_correlations_structured <- dat_correlations %>%
  dplyr::filter(condition == "structured")
mycor(5:24, dat_correlations_structured, latex = FALSE)
describe(dat_correlations_structured[5:24])
