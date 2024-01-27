navbarPage(
  theme = shinytheme("united"),  
  # Application title
  "ARIMAシミュレーター", 
  
  tabPanel(
    "Simulation",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          fluidPage(
            column(6, 
              numericInput("ar_d", label="AR（自己回帰）の次数", value=0, min=0, max=15, step=1),
              numericInput("ma_d", label="MA（移動平均）の次数", value=0, min=0, max=15, step=1),
              textInput("intr_v", label="切片項", value = 0),
            ),
          
          column(6, 
             numericInput("i_d", label="I（和分）の次数", value=0, min=0, max=15, step=1),
             textInput("sd_v", label="標準偏差", value = 1),
             numericInput("n_d", label="時系列の長さ", value=100, min=100, max=1500, step=1),
             br(),
             br(),
             actionButton("runculc", label="実行"),  
          )
        ),
        tags$footer(
        br(),
        br(),
        tags$a(href="https://github.com/sb8001at/ARIMAsim", "sb8001at/ARIMAsim"),
        tags$a(href = "https://github.com/sb8001at/ARIMAsim", icon("github")),
        br(),
        br(),
        tags$a(href = "https://creativecommons.org/licenses/by-nc-sa/4.0/deed.ja", tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/1/12/Cc-by-nc-sa_icon.svg", width="10%")),
        br(),
        tags$a(href = "https://creativecommons.org/licenses/by-nc-sa/4.0/deed.ja", "クリエイティブ・コモンズ CC BY-NC-SA"),
        tags$p("に従い，複製、頒布、展示、実演を行うにあたり、著作権者の表示を要求し、非営利目的での利用に限定し、作品を改変・変形・加工してできた作品についても、元になった作品と同じライセンスを継承させた上で頒布を認めます。"),
        br(),
        tags$p("Rから以下のコードで実行すると、ローカルPCで動かすことができます。"),
        tags$p(code("if(require(shiny)){install.packages(\"shiny\")};runGitHub(\"ARIMAsim\", \"sb8001at\")"))
        )
      ),
          
        mainPanel(
          tabsetPanel(
            tabPanel("グラフ", plotOutput("tsplot")),
            tabPanel(
              "統計",
              br(),
              p("ar関数の結果"),
              verbatimTextOutput("ar"),
              br(),
              p("auto.arima関数の結果"),
              verbatimTextOutput("autoarima")
            ),
            tabPanel(
              "単位根検定",
              br(),
              p("ADF検定（棄却された場合は単位根がない）"),
              verbatimTextOutput("adf"),
              br(),
              p("KPSS検定"),
              p("（test-statisticとcritical valuesを比較し、test-statisticの方が大きい場合は単位根がある）"),
              verbatimTextOutput("kpss")
            ),
            tabPanel(
              "設定値",
              br(),
              p("ARの係数（前から1次、2次…）"),
              verbatimTextOutput("ar_p"),
              br(),
              p("MAの係数（前から1次、2次…）"),
              verbatimTextOutput("ma_p")
            )
          )
    
        )
      )
    )
  ),
  tabPanel(
    "説明",
    h2("このアプリケーションについて"),
    p("このShinyアプリは、ARIMAモデルのイメージを深めることを目的とした、ARIMAのシミュレーターです。"),
    h2("はじめに"),
    p("ARIMAは、自己回帰和分移動平均（Autoregressive Integrated Moving Average）モデルと呼ばれる時系列解析のモデルの一つで、
      自己回帰（AR）、移動平均（MA）を組み合わせたモデルに、和分（差分が定常過程となるもの）を組み合わせたモデルです。"),
    p("時系列の教科書を読むと、まずAR、次にMA、ARとMAを組み合わせたARMA、そしてARIMAモデルが必ず出てきます。
      詳しいAR、MA、ARMA、ARIMAモデルの説明は教科書や他のウェブサイトにたくさん記載があるため、割愛します。"),
    p("このARIMAモデル、AR成分、和分、MA成分の次数をそれぞれp、q、rとして、ARIMA(p,q,r)といった形で表現するのが一般的です。
      AICを用いたモデル選択で時系列データを分析し、p、q、rを求めます。"),
    p("ただ、このp、q、rが求まったときに、時系列としてどういう形なのか、求まった値にどれぐらい意味があるのか、
      統計の素人にはどうも理解が難しく、イメージしにくいものです。"),
    p("少しでもARIMAモデルをイメージしやすくするため、Shinyを用いたアプリケーションを作成しました。"),
    h2("シミュレーションについて"),
    p("シミュレーションには、Rのarima.sim関数を用いています。このarima.sim関数は、モデル（ARIMA(p,q,r)）と、AR、MAの係数をそれぞれ
      指定すれば、そのモデル・係数に対応した時系列を生成してくれる関数です。このアプリケーションでは、モデルだけ指定し、AR、MAの
      係数は乱数（-1～1の一様乱数）で生成しています。"),
    p("ただし、このarima.sim関数では、定常性を持たないAR、MAの係数を取ることができないようになっています。ARの係数は以下の関数の定義に従い
      設定しています。"),
    p(code("if (p) {
        minroots <- min(Mod(polyroot(c(1, -model$ar))))
        if (minroots <= 1) 
            stop(\"'ar' part of model is not stationary\")
    }")),
    p("maの係数は係数の合計が1を超えないように設定しています。ARもMAも、係数が1を超えてもおかしくはないのですが、定常性をコントロールするのが
      難しいため、あまり大きな係数を取らないようにしています。"),
    h2("入力について"),
    p("入力はSimulationタブの左側で行います。ARの次数、和分、MAの次数、時系列の長さは整数で設定します。標準偏差と切片項は数値を入力します。
      切片項は時系列を全体的に上下させる項、標準偏差はばらつきの大きさを指定している項目です。前者はarima.sim関数の返り値に足す数値、
      後者はarima.simの引数となります。標準偏差と切片項には数値以外を入力することができますが、切片項は0、標準偏差は1に変換されます。"),
    p("値を入力した後で、「実行」ボタンを押すと、計算が行われます。右のパネルの「グラフ」にはシミュレーションした時系列と自己相関・偏自己相関、
      「統計」にはar関数（ARモデルと次数を推定する関数）とauto.arima関数（ARIMAモデルの次数・係数を計算する関数）の結果、「単位根検定」には
      ADR検定とKPSS検定の結果、設定値にはarima.sim関数に与えたARとMAの係数をそれぞれ示します。"),
    h2("感想"),
    p("計算してみるとわかりますが、auto.arima関数はデータ数が少ないとAR、MAの次数が少なくてもあまり正確な計算結果を示してくれません。
      和分がなくても単位根ありになる場合もあり、シミュレーションしてみてもARIMAは難しい、というのが感想です。")
  )
)
