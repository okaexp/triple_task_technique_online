# Triple Task Techniqueを用いたオンライン実験

### Abstract

本レポジトリは以下の論文のデータと分析結果を共有するためのものである: "文章執筆過程を明らかにするためのオンライン実験手法の開発: Triple Task Techniqueを用いた研究事例" (投稿中)

### Directories
- experiment (Triple Task Technique (以下, TTT)の実験プログラムをまとめたディレクトリ。jsPsych 7.2.1 (https://github.com/jspsych/jsPsych/releases/tag/jspsych%407.2.1)から、必要なpluings, css, そしてjspsychファイルをダウンロードして、配置して下さい。。配置するべきファイルについては、各ページのテキストファイルで指示しています。)
    - C01_ttt_demo.html (図1に表示したTTTのデモ)
    - C02_ttt_simple_rt.html (単純反応課題)
    - C03_ttt_task_explanation.html (思考プローブの学習課題)
    - C04_ttt_lnst.html (control条件のLetter Number Sequence test)
    - C05a_ttt_structured.html (structured outline条件の執筆課題)
    - C05b_ttt_outline.html (outline条件の執筆課題)
    - C05c_ttt_control.html (control条件の執筆課題)
    - experimentに置くべきファイル.txt
    - css (cssファイルを置くフォルダ)
        - cssに置くべきファイル.txt
    - plugins (jsPsychの各種プラグインを置くフォルダ)
        - pluginsに置くべきファイル.txt
    - stimulus (TTTで用いる音声刺激)
        - beep.mp3: 短いビープ音

- analysis (3章で報告している実験結果)
    - data (実験データ)
        - 1_ttt_control_probes.csv (control条件の思考プローブの回答)
        - 1_ttt_control.csv (control条件の執筆内容)
        - 1_ttt_explanation.csv (思考プローブの学習課題)
        - 1_ttt_lnst.csv (Letter Number Sequence test)
        - 1_ttt_outline_probes.csv (outline条件の思考プローブの回答)
        - 1_ttt_outline.csv (outline条件の執筆内容)
        - 1_ttt_simple_rt.csv (単純反応課題)
        - 1_ttt_structured_probes.csv (structured outline条件の思考プローブの回答)
        - 1_ttt_structured.csv (structured outline条件の執筆内容)
        - narrative_evaluation_data.csv (各物語に対する評定者の評価結果)
        - ttt_question_end.csv (TTTに最後まで参加した人のID)
        - ttt_question_start.csv (TTTに最初に参加した人のIDとデモグラフィック)
    - result (実験結果)
        - dat_ave_evaluation_by_item.csv (各物語に対する評定者の評価結果の評定平均値)
        - dat_condition_text_complete.xlsx (参加者の執筆した物語)
        - ttt_desc_and_corrs.xlsx (記述統計量と相関係数)
    - src (分析ファイル)
        - analysis_narrative_quality_by_condition.R (narrative_evaluation_data.csvから物語品質の条件ごとの比較を行う)
        - analysis_ttt.R (TTT全体の分析を行う)

- document (その他投稿論文に関わる資料)
    - TTT_EvaluationTask_Instruction_and_Practice.pdf (物語の評価課題の教示と練習試行の内容)

### Contact
岡 隆之介 (oka.exp@gmail.com)