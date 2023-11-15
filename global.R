if(!require(pacman)){install.packages("pacman")}
p_load(shiny, shinythemes, tidyverse, DT)

# mm_3lot <- mean(c(param1[1], param2[1], param3[1])) # 代表値

# svar_3lot <- (param1[2]^2 * n + param2[2]^2 * n + param3[2]^2 * n) / 3 / n # 下の関数とおなじ
# ssd_3lot <- svar_3lot ^ 0.5 # 分散から標準偏差の代表値を得る

m_seq_fn <- function(mm_3lot, sixsigma){
  seq(mm_3lot - 2 * sixsigma, mm_3lot + 2 * sixsigma, by = 0.1)
}

## 平均値と標準偏差の期待分布をデータフレームとする
df_meandist <- function(m_seq, mm_3lot, ssd_3lot){
  tibble(val = m_seq, dn = dnorm(m_seq, mean=mm_3lot, sd=ssd_3lot), pn = pnorm(m_seq, mean=mm_3lot, sd=ssd_3lot))
}

## 分散の計算は複雑なので、以下の関数を用いて計算する
# 母分散の確率密度関数を得る関数
calc_psd <- function(n, s){ # nはサンプル数，sは不偏標準偏差
  ss <- s ^ 2 * (n-1) # 偏差平方和（分散 × 自由度）
  s_seq <- seq(0.0001, 1, by=0.0001) %>% round(4) # 確率の数列
  q_chisq <- qchisq(s_seq, n-1) # 自由度n-1の累積確率密度
  
  sig <- ss/q_chisq # 母分散の累積確率密度
  
  df_qchisq <- tibble(prob_ = s_seq, var_ = sig) # 確率と累積確率密度の関係（確率から推定範囲を計算できる）
  
  df_psd <- tibble(psd = ((sig[-1] + sig[-length(sig)])/2)^0.5, prob_ = (-diff(s_seq)/diff(sig)))
  
  return(list(df_qchisq, df_psd))
}

# 母分散の確率密度関数からサンプリングする関数（lstはcalc_psdの返り値）
sample_psd <- function(d_l, n_ = 3){
  sample(x = d_l[[2]] %>% .[,1] %>% unlist() , size=n_ , prob = d_l[[2]] %>% .[,2] %>% unlist())
}


df_meandist_plot <- function(m_seq, mm_3lot, ssd_3lot){
  d_m <- df_meandist(m_seq, mm_3lot, ssd_3lot) |> 
    mutate(
      col_90 = if_else(pn > 0.05 & pn < 0.95, dn, NA),
      col_95 = if_else(pn > 0.025 & pn < 0.975, dn, NA),
      col_99 = if_else(pn > 0.005 & pn < 0.995, dn, NA),
      col_3s = if_else(pn > pnorm(-3) & pn < pnorm(+3), dn, NA))
  
  ggplot() +
    geom_area(aes(x=val, y=dn), color=NA, fill="#2B5D59", data = d_m) +
    geom_area(aes(x=val, y=col_3s), color=NA, fill="#44968F", data = d_m |> filter(!is.na(col_3s))) +
    geom_area(aes(x=val, y=col_99), color=NA, fill="#56bcb3", data = d_m |> filter(!is.na(col_99))) +
    geom_area(aes(x=val, y=col_95), color=NA, fill="#99D6D1", data = d_m |> filter(!is.na(col_95))) +
    geom_area(aes(x=val, y=col_90), color=NA, fill="#BBE4E0", data = d_m |> filter(!is.na(col_90))) +
    geom_vline(xintercept=mm_3lot, color="red", linewidth=0.1)+
    geom_vline(xintercept=c(95, 105), color="black", linewidth=0.5)+
    geom_vline(xintercept=c(97, 103), color="blue", linewidth=0.5)+
    labs(x="個々の錠剤の含量（%）", y="確率密度", caption="青線は100±3%、黒線は100±3%を示す")+
    theme_light()
}


df_sddist_plot <- function(d_l, intr, ssd_3lot){
  d_s <-  d_l[[2]] %>% 
    mutate(
      col_90 = if_else((psd < intr$col_90 %>% na.omit() %>% .[1]) & (psd > intr$col_90 %>% na.omit() %>% .[2]), psd, NA),
      col_95 = if_else((psd < intr$col_95 %>% na.omit() %>% .[1]) & (psd > intr$col_95 %>% na.omit() %>% .[2]), psd, NA),
      col_99 = if_else((psd < intr$col_99 %>% na.omit() %>% .[1]) & (psd > intr$col_99 %>% na.omit() %>% .[2]), psd, NA)) %>%
    slice(c(1, 1:100 * 99, 9901:10000))
  
  ggplot() +
    geom_area(aes(x=psd, y=prob_), color=NA, fill="#2B5D59", data = d_s) +
    geom_area(aes(x=psd, y=prob_), color=NA, fill="#56bcb3", data = d_s |> filter(!is.na(col_99))) +
    geom_area(aes(x=psd, y=prob_), color=NA, fill="#99D6D1", data = d_s |> filter(!is.na(col_95))) +
    geom_area(aes(x=psd, y=prob_), color=NA, fill="#BBE4E0", data = d_s |> filter(!is.na(col_90))) +
    geom_vline(xintercept=ssd_3lot, color="red", linewidth=0.1)+
    labs(x="各ロットの含量の標準偏差（%）", y="確率密度")+
    theme_light()
}

control_chart_fn <- function(mm_3lot, ms_3lot, d_l){
  # 100ロット製造時の個々の値
  sample100 <- replicate(100, rnorm(10, rnorm(1, mm_3lot, ms_3lot), sample_psd(d_l, 1)))
  
  # 期待される工程管理図
  control_chart <- tibble(
    batches = 1:100,
    mean100batch = sample100 |> as_tibble() |> apply(2, mean),
    sd100batch = sample100 |> as_tibble() |> apply(2, sd),
    lower3sigma = sample100 |> as.vector() |> mean() - sample100 |> as.vector() |> sd(),
    upper3sigma = sample100 |> as.vector() |> mean() + sample100 |> as.vector() |> sd(),
    AccepValue = abs(sample100 |> apply(2, mean) - 100) * 2.4 * sample100 |>  apply(2, sd)
  )
  
  # 工程管理図（3シグマ管理）
  cc <- control_chart |> 
    ggplot(aes(x=batches, y=mean100batch, ymax=mean100batch+sd100batch, ymin=mean100batch-sd100batch))+
    geom_ribbon(color=NA, fill="#99D6D1")+
    geom_line(linewidth=1, color="#2B5D59")+
    geom_hline(yintercept=c(control_chart$lower3sigma[1], control_chart$upper3sigma[1]), color="red", alpha=0.5)+
    geom_hline(yintercept=c(97, 103), color="blue", alpha=0.5)+
    geom_hline(yintercept=c(95, 105), color="black", alpha=0.5)+
    labs(x="ロット", y="各ロットの平均含量（%）（リボンは標準偏差）", caption="赤線は3σ、青線は100±3%、黒線は100±5%を示す")+
    theme_light()
    
    
  
  # 判定値の系列
  av <- control_chart |> 
    ggplot(aes(x=batches, y=AccepValue))+
    geom_line(linewidth=1, color="#2B5D59")+
    geom_hline(yintercept=15, color="red", alpha=0.5)+
    labs(x="ロット", y="各ロットの判定値", caption="黒線はL1判定値（15）を示す")+
    theme_light()
  
  return(list(control_chart, cc, av))
}

# 100ロット製造時に，個々の値が±5%を外れる確率
# (sample100 < 95 | sample100 > 105) |> as.vector() |> sum() / 1000

# 3ロットのシミュレーション
sim_uniform <- function(mm_3lot, ms_3lot, d_l){
  sim_m <- rnorm(3, mm_3lot, ms_3lot) # 平均値
  sim_s <- sample_psd(d_l) # 標準偏差
  
  sim_result <- 
    mapply(rnorm, 10, sim_m, sim_s) |> 
    as_tibble() |> 
    rename(lot1=V1, lot2=V2, lot3=V3) # 10錠ずつの結果
  
  sim_summary <- sim_result |> 
    pivot_longer(1:3) |> 
    group_by(name) |> 
    summarise(
      mean_=mean(value) %>% round(digits=2), 
      s=sd(value) %>% round(digits=2), 
      max_ = max(value) %>% round(digits=2),
      min_ = min(value) %>% round(digits=2),
      upper3s = (mean(value) + 3 * sd(value)) %>% round(digits=2),
      lower3s = (mean(value) - 3 * sd(value)) %>% round(digits=2),
      AccepValue = (abs(mean(value) - 100) * 2.4 * sd(value)) %>% round(digits=2)
    ) 
  
  list(sim_result, sim_summary)
}
