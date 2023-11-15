function(input, output, session) {
  
  mm_3lot <- reactive({return(mean(c(input$lot1mean, input$lot2mean, input$lot3mean)))})
  ssd_3lot <- reactive({return(mean(c(input$lot1sd, input$lot2sd, input$lot3sd)))})
  ms_3lot <- reactive({return(ssd_3lot() / input$n_sample ^ 0.5)})
  sixsigma <- reactive({return((ssd_3lot() * 3))})
  m_seq <- reactive({return(m_seq_fn(mm_3lot(), sixsigma()))})
  d_l <- reactive({return(calc_psd(input$n_sample, ssd_3lot()))})
  intr <- reactive({return(
    d_l() %>% .[[1]] %>% 
      mutate(
        col_90 = if_else(prob_ == 0.05 | prob_ == 0.95, var_^0.5, NA),
        col_95 = if_else(prob_ == 0.025 | prob_ == 0.975, var_^0.5, NA),
        col_99 = if_else(prob_ == 0.005 | prob_ == 0.995, var_^0.5, NA)
      ) %>% 
      filter(!is.na(col_90) | !is.na(col_95) | !is.na(col_99))
  )})
  
  observeEvent(input$runCalc, {
    output$meandist_plot <- renderPlot(
      df_meandist_plot(m_seq(), mm_3lot(), ssd_3lot())
    )
    
    output$meandist_90 <- renderText(
      paste0(
        "90%の錠剤が含まれる含量の範囲は<span style=\"color:#99D6D1;font-weight:bold;\">", 
        round(mm_3lot() + 1.644854 * ssd_3lot(), digits = 2), "～", round(mm_3lot() - 1.644854 * ssd_3lot(), digits = 2), 
        "</span>%です。"
      )
    )
    
    output$meandist_95 <- renderText(
      paste0(
        "95%の錠剤が含まれる含量の範囲は<span style=\"color:#56bcb3;font-weight:bold;\">", 
        round(mm_3lot() + 1.959964 * ssd_3lot(), digits = 2), "～", round(mm_3lot() - 1.959964 * ssd_3lot(), digits = 2), 
        "</span>%です。"
      )
    )
    
    output$meandist_99 <- renderText(
      paste0(
        "99%の錠剤が含まれる含量の範囲は<span style=\"color:#44968F;font-weight:bold;\">", 
        round(mm_3lot() + 2.575829 * ssd_3lot(), digits = 2), "～", round(mm_3lot() - 2.575829 * ssd_3lot(), digits = 2), 
        "</span>%です。"
      )
    )
    
    output$meandist_3s <- renderText(
      paste0(
        "3σの範囲は<span style=\"color:#2B5D59;font-weight:bold;\">", 
        round(mm_3lot() + 3 * ssd_3lot(), digits = 2), "～", round(mm_3lot() - 3 * ssd_3lot(), digits = 2), 
        "</span>%です。99.8%の錠剤の含量がこの範囲に含まれます。"
      )
    )
    
    output$meandist_3per <- renderText(
      paste0(
        paste0(
          "含量が103%以上、97%以下になる確率は約<span style=\"color:red;font-weight:bold;\">", 
          round(100 * (pnorm(97, mean=mm_3lot(), sd=ssd_3lot()) + 1 - pnorm(103, mean=mm_3lot(), sd=ssd_3lot())), digits = 3), 
          "</span>%です。"), "<br>",
        paste0(
          "30錠中に含量が103%以上、97%以下になる錠剤が含まれる確率は約<span style=\"color:red;font-weight:bold;\">", 
          round(100 * (1 - ((1 - (pnorm(97, mean=mm_3lot(), sd=ssd_3lot()) + 1 - pnorm(103, mean=mm_3lot(), sd=ssd_3lot()))) ^ 30)), digits = 2), 
          "</span>%です。"), "<br>",
        paste0(
          "100錠中に含量が103%以上、97%以下になる錠剤が含まれる確率は約<span style=\"color:red;font-weight:bold;\">", 
          round(100 * (1 - ((1 - (pnorm(97, mean=mm_3lot(), sd=ssd_3lot()) + 1 - pnorm(103, mean=mm_3lot(), sd=ssd_3lot()))) ^ 100)), digits = 2), 
          "</span>%です。")
      )
    )
    
    output$meandist_5per <- renderText(
      paste0(
        paste0(
          "含量が105%以上、95%以下になる確率は約<span style=\"color:red;font-weight:bold;\">", 
          round(100 * (pnorm(95, mean=mm_3lot(), sd=ssd_3lot()) + 1 - pnorm(105, mean=mm_3lot(), sd=ssd_3lot())), digits = 3), 
          "</span>%です。"), "<br>",
        paste0(
          "30錠中に含量が105%以上、95%以下になる錠剤が含まれる確率は約<span style=\"color:red;font-weight:bold;\">", 
          round(100 * (1 - ((1 - (pnorm(95, mean=mm_3lot(), sd=ssd_3lot()) + 1 - pnorm(105, mean=mm_3lot(), sd=ssd_3lot()))) ^ 30)), digits = 2), 
          "</span>%です。"), "<br>",
        paste0(
          "100錠中に含量が105%以上、95%以下になる錠剤が含まれる確率は約<span style=\"color:red;font-weight:bold;\">", 
          round(100 * (1 - ((1 - (pnorm(95, mean=mm_3lot(), sd=ssd_3lot()) + 1 - pnorm(105, mean=mm_3lot(), sd=ssd_3lot()))) ^ 100)), digits = 2), 
          "</span>%です。")
      )      
    )
  })
  
  observeEvent(input$runCalcsd, {
    output$sddist_plot <- renderPlot(
      df_sddist_plot(d_l(), intr(), ssd_3lot())
    )
    
    output$sddist_90 <- renderText(
      paste0(
        "90%のロットでは標準偏差は<span style=\"color:#99D6D1;font-weight:bold;\">", 
        round(intr()$col_90 %>% na.omit() %>% .[1], digits=2), "～", round(intr()$col_90 %>% na.omit() %>% .[2], digits=2), 
        "</span>%の範囲に含まれます。"
      )
    )
    
    output$sddist_95 <- renderText(
      paste0(
        "95%のロットでは標準偏差は<span style=\"color:#56bcb3;font-weight:bold;\">", 
        round(intr()$col_95 %>% na.omit() %>% .[1], digits=2), "～", round(intr()$col_95 %>% na.omit() %>% .[2], digits=2),  
        "</span>%の範囲に含まれます。"
      )
    )
    
    output$sddist_99 <- renderText(
      paste0(
        "99%のロットでは標準偏差は<span style=\"color:#2B5D59;font-weight:bold;\">", 
        round(intr()$col_99 %>% na.omit() %>% .[1], digits=2), "～", round(intr()$col_99 %>% na.omit() %>% .[2], digits=2), 
        "</span>%の範囲に含まれます。"
      )
    )
  })
  
  observeEvent(input$runSim, {
    sim_u <- sim_uniform(mm_3lot(), ms_3lot(), d_l())
    
    output$sim_3lot <- renderDT(
      sim_u[[1]] %>% round(digits=1), 
      rownames=FALSE, 
      options = list(dom="t", autoWidth = F)
    )
    
    output$sim_3lot_summary <- renderDT(
      sim_u[[2]], 
      rownames=FALSE, 
      options = list(dom="t", autoWidth = F),
      colnames=c("ロット名", "平均値", "標準偏差", "最大値", "最小値", "3σ上限", "3σ下限", "判定値")
    )
    
    output$sim_3lot_plot <- renderPlot(
      sim_u[[1]] %>% 
        pivot_longer(1:3) %>%
        ggplot(aes(x=value, fill=name))+
        geom_histogram(binwidth=0.5)+
        geom_vline(xintercept=c(95, 105))+
        labs(x="含量", y="個数", title="個々の含量の分布", caption="縦線は100±5%を示す")+
        guides(fill=guide_legend(title="ロット"))+
        theme_light()
    )
    
    output$sim_3lot_summary_plot <- renderPlot(
      sim_u[[1]] %>% 
        pivot_longer(1:3) %>% 
        group_by(name) %>% 
        summarise(
          m=mean(value), 
          s=sd(value)
        ) %>% 
        ggplot(aes(x=m, xmax=m+s, xmin=m-s, y=name, color=name))+
        geom_point(size=3)+
        geom_linerange(linewidth=1)+
        geom_vline(xintercept=c(95, 105))+
        labs(x="含量（平均値±標準偏差）", y="ロット", title="ロット含量の平均値と標準偏差", caption="縦線は100±5%を示す")+
        guides(color=guide_legend(title="ロット"))+
        theme_light()
    )
  })
  
  observeEvent(input$printControlChart, {
    cc <- control_chart_fn(mm_3lot(), ms_3lot(), d_l())
    output$cchart <- renderPlot(cc[[2]])
    output$ACchart <- renderPlot(cc[[3]])
  })
  
  output$chisq_f <- renderUI(
    withMathJax(
      helpText("式1：以下の式は自由度kのカイ二乗分布に従う: $$ \\frac{k \\cdot s^2}{\\sigma^2} \\sim \\chi^2(k)$$")
    )
  )
  
  output$chisq_2 <- renderUI(
    withMathJax(
      helpText("式2：式1にn=10，s=2を代入する: $$ \\frac{9 \\cdot 4}{\\sigma^2} \\sim \\chi^2(9)$$")
    )
  )
  
  output$chisq_p9 <- renderPlot(
    plot(x = qchisq(seq(0.0001, 1, by=0.0001), 9), y = seq(0.0001, 1, by=0.0001), type="l", xlab="値", ylab="累積確率")
  )
  
  output$chisq_psd <- renderPlot({
    # サンプル数
    n = 10
    # 標本標準偏差
    s = 2
    # 偏差平方和（分散 × 自由度）
    ss <- s ^ 2 * (n-1)
    # 確率を示す数値
    s_seq <- seq(0.0001, 1, by=0.0001)
    # 自由度n-1の累積確率密度（偏差平方和/母分散の累積確率密度）
    q_chisq <- qchisq(s_seq, n-1)
    # 母分散の累積確率密度（上のq_chisqと偏差平方和から計算）
    sig <- ss/q_chisq
    # 母分散（母分散の平方根）の累積確率密度をプロットしたもの
    plot(sig ^ 0.5, s_seq, type="l", xlab="母標準偏差", ylab="累積確率密度（を1から引いたもの）")
  })
  
  output$dist_psd <- renderPlot({
    # サンプル数
    n = 10
    # 標本標準偏差
    s = 2
    # 偏差平方和（分散 × 自由度）
    ss <- s ^ 2 * (n-1)
    # 確率を示す数値
    s_seq <- seq(0.0001, 1, by=0.0001)
    # 自由度n-1の累積確率密度（偏差平方和/母分散の累積確率密度）
    q_chisq <- qchisq(s_seq, n-1)
    # 母分散の累積確率密度（上のq_chisqと偏差平方和から計算）
    sig <- ss/q_chisq
    # 累積確率密度を微分してマイナスを付けたもの
    plot(((sig[-1] + sig[-length(sig)])/2)^0.5, -diff(s_seq)/diff(sig), type="l", xlab="母標準偏差", ylab="確率密度関数（のようなもの）")
  })
    
  output$dist_psd_point <- renderPlot({
    # サンプル数
    n = 10
    # 標本標準偏差
    s = 2
    # 偏差平方和（分散 × 自由度）
    ss <- s ^ 2 * (n-1)
    # 確率を示す数値
    s_seq <- seq(0.0001, 1, by=0.0001)
    # 自由度n-1の累積確率密度（偏差平方和/母分散の累積確率密度）
    q_chisq <- qchisq(s_seq, n-1)
    # 母分散の累積確率密度（上のq_chisqと偏差平方和から計算）
    sig <- ss/q_chisq
    # 累積確率密度を微分してマイナスを付けたもの
    plot(x=(((sig[-1] + sig[-length(sig)])/2)^0.5), y=(-diff(s_seq)/diff(sig)), xlab="母標準偏差", ylab="確率密度関数（のようなもの）", cex=0.5)
  })
  
  output$dist_psd_dist <- renderPlot({
    # サンプル数
    n = 10
    # 標本標準偏差
    s = 2
    # 偏差平方和（分散 × 自由度）
    ss <- s ^ 2 * (n-1)
    # 確率を示す数値
    s_seq <- seq(0.0001, 1, by=0.0001)
    # 自由度n-1の累積確率密度（偏差平方和/母分散の累積確率密度）
    q_chisq <- qchisq(s_seq, n-1)
    # 母分散の累積確率密度（上のq_chisqと偏差平方和から計算）
    sig <- ss/q_chisq
    # 累積確率密度を微分してマイナスを付けたもの
    plot(x=(((sig[-1] + sig[-length(sig)])/2)^0.5)[-1], y=-diff(((sig[-1] + sig[-length(sig)])/2)^0.5), xlab="母標準偏差", ylab="母標準偏差の横の幅", type="l")
  })  
 
  output$dist_psd_dist2 <- renderPlot({
    # サンプル数
    n = 10
    # 標本標準偏差
    s = 2
    # 偏差平方和（分散 × 自由度）
    ss <- s ^ 2 * (n-1)
    # 確率を示す数値
    s_seq <- seq(0.0001, 1, by=0.0001)
    # 自由度n-1の累積確率密度（偏差平方和/母分散の累積確率密度）
    q_chisq <- qchisq(s_seq, n-1)
    # 母分散の累積確率密度（上のq_chisqと偏差平方和から計算）
    sig <- ss/q_chisq
    # 累積確率密度を微分してマイナスを付けたもの
    plot(x=(((sig[-1] + sig[-length(sig)])/2)^0.5)[-1], y=-diff(((sig[-1] + sig[-length(sig)])/2)^0.5), xlab="母標準偏差", ylab="母標準偏差の横の幅", ylim=c(0, 0.01), type="l")
  })   
  
  output$dist_psd_r_dist <- renderPlot({
    # サンプル数
    n = 10
    # 標本標準偏差
    s = 2
    # 偏差平方和（分散 × 自由度）
    ss <- s ^ 2 * (n-1)
    # 確率を示す数値
    s_seq <- seq(0.0001, 1, by=0.0001)
    # 自由度n-1の累積確率密度（偏差平方和/母分散の累積確率密度）
    q_chisq <- qchisq(s_seq, n-1)
    # 母分散の累積確率密度（上のq_chisqと偏差平方和から計算）
    sig <- ss/q_chisq
    
    temp <- calc_psd(10, 2)
    samplep <- sample_psd(temp, 9000)
    
    hist(samplep, xlim=c(0, 7), xlab="", ylab="")
    par(new=T)
    plot(((sig[-1] + sig[-length(sig)])/2)^0.5, -diff(s_seq)/diff(sig), type="l", xlim=c(0, 7), xlab="", ylab="")
  })   
  
}
