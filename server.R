function(input, output, session) {
  
  mm_3lot <- reactive({return(mean(c(input$lot1mean, input$lot2mean, input$lot3mean)))})
  svar_3lot <- reactive({return(calc_var(input$lot1sd, input$lot2sd, input$lot3sd, input$n_sample))})
  ssd_3lot <- reactive({return((svar_3lot() ^ 0.5))})
  ms_3lot <- reactive({return(ssd_3lot() / input$n_sample ^ 0.5)})
  sixsigma <- reactive({return((ssd_3lot() * 3))})
  m_seq <- reactive({return(m_seq_fn(mm_3lot(), sixsigma()))})
  s_seq <- reactive({return(s_seq_fn(sixsigma()))})
  
  
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
    
    output$sddist_plot <- renderPlot(
      df_sddist_plot(s_seq(), svar_3lot())
    )
    
    output$sddist_90 <- renderText(
      paste0(
        "90%のロットでは標準偏差は<span style=\"color:#99D6D1;font-weight:bold;\">", 
        round(qchisq(0.05, svar_3lot())^0.5, digits = 2), "～", round(qchisq(0.95, svar_3lot())^0.5, digits = 2), 
        "</span>%の範囲に含まれます。"
      )
    )
    
    output$sddist_95 <- renderText(
      paste0(
        "95%のロットでは標準偏差は<span style=\"color:#56bcb3;font-weight:bold;\">", 
        round(qchisq(0.025, svar_3lot())^0.5, digits = 2), "～", round(qchisq(0.975, svar_3lot())^0.5, digits = 2), 
        "</span>%の範囲に含まれます。"
      )
    )
    
    output$sddist_99 <- renderText(
      paste0(
        "99%のロットでは標準偏差は<span style=\"color:#2B5D59;font-weight:bold;\">", 
        round(qchisq(0.005, svar_3lot())^0.5, digits = 2), "～", round(qchisq(0.995, svar_3lot())^0.5, digits = 2), 
        "</span>%の範囲に含まれます。"
      )
    )
  })
  
  observeEvent(input$runSim, {
    sim_u <- sim_uniform(mm_3lot(), ms_3lot(), svar_3lot())
    
    output$sim_3lot <- renderDT(
      sim_u[[1]] %>% round(digits=1), 
      rownames=FALSE, 
      options = list(dom="t", autoWidth = F)
    )
    
    output$sim_3lot_summary <- renderDT(
      sim_u[[2]], 
      rownames=FALSE, 
      options = list(dom="t", autoWidth = F),
      colnames=c("ロット名", "平均値", "標準偏差", "最大値", "最小値", "3σ上限", "3σ加減", "判定値")
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
    cc <- control_chart_fn(mm_3lot(), ms_3lot(), svar_3lot())
    output$cchart <- renderPlot(cc[[2]])
    output$ACchart <- renderPlot(cc[[3]])
  })
  
}
