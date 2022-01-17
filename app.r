# update:2022.01.17
library(shiny)
library(shinydashboard)
library(plotly)

theme_basic <- function() {
  fill.colour <- "white"
  theme_classic() +
    theme(plot.background    = element_rect(fill = fill.colour)) +
    theme(panel.background   = element_rect(fill = fill.colour)) +
    theme(panel.grid.major   = element_line(colour = "grey95")) +
    theme(panel.grid.major.x = element_blank()) +
    theme(axis.line          = element_blank()) +
    theme(axis.ticks.x       = element_blank()) +
    theme(axis.ticks.y       = element_line(colour = "grey95")) +
    theme(text               = element_text(colour = "grey20"))
}
#------------ ui-app.R ------------ ---
#------------ ui-header & sidebar -----
header <- dashboardHeader(title = "Probability")
sidebar <- dashboardSidebar(width = NULL,disable = FALSE,
                            sidebarMenu(
                              menuItem("Summary", tabName = "Page0", icon = icon("list")),
                              menuItem("Basic Probability", tabName = "Page1", icon = icon("list")),
                              menuItem("Discrate", tabName = "Page2", icon = icon("chart-bar")),
                              menuItem("Continuous", tabName = "Page3", icon = icon("chart-area"))
                              # menuItem("About",tabName = "final", icon = icon("address-card"))
                              # ref of icon : https://fontawesome.com/v5.15/icons 
                            )) # dashboardSidebar
#------------ ui-Body content -----
body <- dashboardBody(
  tags$head(tags$style(HTML('
                            /* logo */
                            .skin-blue  .main-header .logo {
                            background-color: #204969;
                            font-family: "monospace", Times, "Times New Roman", serif;
                            font-weight: bold;
                            font-size: 24px;
                            }
                            /* logo when hovered */
                            .skin-blue  .main-header .logo:hover {
                            background-color: #FFB6C1;
                            }
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #f4b943;
                            }
                            /* main sidebar */
                            .skin-blue .main-sidebar {
                            background-color: #204969;
                            font-size: 16px; 
                            }
                            /* body */
                            .content-wrapper, .right-side {
                            background-color: #fff7f7;
                            }
                            /* boxboxbox */
                            ') #HTML
  ) # tags$style
  ), #tags$head
  tabItems(
    #-------------- Page0 ------------- 
    tabItem(tabName = "Page0",
            # Boxes need to be put in a row (or column)
            fluidRow(
              box(title = "Definitions",width = 12,
                  solidHeader = FALSE, 
                  collapsible = FALSE,
                  uiOutput("summary")
              ) # box
            ) # fluidRow
    ), # tabItem
    #-------------- Page1 ------------- 
    tabItem(tabName = "Page1",
            fluidRow(
              box(title = "Flipping a coin",
                  "A classic example of a probabilistic experiment is a fair coin toss, 
                  in which the two possible outcomes are heads or tails.",br(),
                  "In this case,The probability of flipping a head or a tail is 1/2."),#box
              box(title = "Rolling a dice",
                  "Dice probabilities refer to calculating the probabilities of events related to a single or multiple rolls of a fair die. 
                  In a fair die, each side is equally likely to appear in any single roll.
                  The probability of rolling a dice is 1/6.")#box
              ),#fluidRow
            fluidRow(
              box(
                width = 6,
                collapsible = FALSE,
                sidebarPanel(
                  actionButton("action_coin", "Flip the coin")
                  # textOutput("text_coin")
                  ),# sidebarPanel
                mainPanel(
                  uiOutput("coin")
                )# mainPanel
              ), #box
              box(
                width = 6,
                collapsible = FALSE,
                sidebarPanel(
                  actionButton("action_dice", "Roll the dice")
                  # textOutput("text_dice")
                  ),#sidebarPanel
                mainPanel(
                uiOutput("dice")
                )# mainPanel
              )#box
            ),#fluidRow
            fluidRow(
              ## 投擲硬幣透過按鈕隨機投擲1次和100次
              box(width = 6,
                  collapsible = FALSE,
                  sidebarPanel(
                    
                    # sliderInput 在server有對應的程式碼
                     sliderInput("n_coin","times",
                                 value = 1,
                                 min = 1,
                                 max = 100)
                    # "times",
                    # hr(),#換行
                    # actionButton("action1", "Flip the coin"),
                    # hr(),
                    # actionButton("action2", "Flip 100 times")
                    ), # sidebarPanel
                  mainPanel(plotOutput("plot_coin"))
                  ),# box
              # -------------------- plot_dice ----
              ## 投擲骰子
              box(width = 6,
                  collapsible = FALSE,
                  sidebarPanel(width = 4,
                               
                               sliderInput("n_dice","times",
                                           value = 1,
                                           min = 1,
                                           max = 1000)# sliderInput
                  ),# sidebarPanel
                  mainPanel(plotOutput("plot_dice"))
              )# box
            )#fluidRow
            ## 呈現反應投擲硬幣的表格，尚未連結成功
            # ,
            # fluidRow(
            #   box(title = "coin", width = 6,
            #       collapsible = FALSE,
            #       tableOutput("data_coin")
            #   )
            # )
            
    ),
    #-------------- Page2 ------------- 
    tabItem(tabName = "Page2",
            # #----------- radioButtons ----------------------
            ## 按鈕反應機率分配的定義，但目前有summary總表呈現，因此暫時不需要
            # fluidRow(
            #   box(title = "Discrate",width = 4,
            #       status = "warning",
            #       solidHeader = FALSE, 
            #       collapsible = FALSE,
            #       "Choose one of the following major discrete distributions.",
            #       br(),
            #       radioButtons("dist1", "Distribution type:",
            #                    c("Binomial","Negative Binomial",
            #                      "Geometric","Poisson"))
            #   ),
            #   box(title = "Definitions",width = 8,
            #       status = "warning",
            #       solidHeader = FALSE, 
            #       collapsible = FALSE,
            #       uiOutput("txt1")
            #       # htmlOutput("txt1")
            #       # verbatimTextOutput("txt1")
            #   )
            # ),
            #----------- bin ------------------------------------
            fluidRow(
              box(title = "Binomial", width = 6,
                  status = "warning",
                  collapsible = TRUE,
                  sidebarPanel(width = 4,
                               sliderInput("n","number",
                                           value = 10,
                                           min = 10,
                                           max = 100),
                               sliderInput("p",
                                           "Probability",
                                           value = 0.6,
                                           min = 0,
                                           max = 1)
                  ),
                  mainPanel(plotOutput("plot_bin1"))
              ),
              #----------- nb ------------------------------------
              box(title = "Negative Binomial",width = 6,
                  status = "warning",
                  solidHeader = FALSE, 
                  collapsible = TRUE,
                  sidebarPanel(width = 4,
                               sliderInput("r","number",
                                           value = 10,
                                           min = 1,
                                           max = 100),
                               sliderInput("p_nb","Probability",
                                           value = 0.6,
                                           min = 0,
                                           max = 1)),
                  mainPanel(plotOutput("plot_nb1")))
            ),
            #----------- geo ------------------------------------
            fluidRow(
              box(title = "Geometric",width = 6,
                  status = "warning", solidHeader = FALSE, 
                  collapsible = TRUE,
                  sidebarPanel(width = 4, 
                               sliderInput("p_geo","Probability",
                                           value = 0.6,
                                           min = 0.1,
                                           max = 1)
                  ),
                  mainPanel(plotOutput("plot_geo1"))
              ),
              #----------- Poi ------------------------------------
              box(title = "Poisson",width = 6,
                  status = "warning", solidHeader = FALSE, 
                  collapsible = TRUE,
                  sidebarPanel(width = 4, 
                               sliderInput("lambda","lambda",
                                           value = 2,
                                           min = 0.1,
                                           max = 50)
                  ),
                  mainPanel(plotOutput("plot_poi1"))
              )
            )
    ),
    #-------------- Page3 ------------- 
    tabItem(tabName = "Page3",
            ## 按鈕反應機率分配的定義，但目前有summary總表呈現，因此暫時不需要
            # fluidRow(
            #   box(title = "Continuous",width = 4,
            #       solidHeader = FALSE, 
            #       collapsible = FALSE,
            #       "Choose one of the following major continuous distributions.",
            #       br(),
            #       radioButtons("dist2", "Distribution type:",
            #                    c("Uniform","Exponential","Normal"))
            #       ),
            #   box(title = "Definitions",width = 8,solidHeader = FALSE, 
            #       collapsible = FALSE,
            #       uiOutput("txt2")
            #   )
            # ),
            #----------- unif ------------------------------------
            fluidRow(
              box(title = "Uniform", width = 6,
                  collapsible = TRUE,
                  sidebarPanel(width = 4,
                               # numericInput("min", "Minimum", 2),
                               # numericInput("max", "Maximum", 5)
                               sliderInput("unifRange", "Range",
                                           min = 0, max = 20,
                                           value = c(2, 5))
                  ),
                  mainPanel(plotOutput("plot_unif1"))
              ),
              
              
              
              
              #----------- Exponential  ------------------------------------
              box(title = "Exponential ",width = 6,
                  solidHeader = FALSE, 
                  collapsible = TRUE,
                  sidebarPanel(width = 4,
                               sliderInput("lambda2","lambda",
                                           value = 2,
                                           min = 0.1,
                                           max = 5,
                                           step = 0.1)
                  ),
                  mainPanel(plotOutput("plot_exp1")))
            ),#fluidRow
            #----------- Normal ------------------------------------
            fluidRow(
              box(title = "Normal", width = 6,
                  collapsible = TRUE,
                  sidebarPanel(width = 4, 
                               sliderInput("mu","mu",
                                           value = 175,
                                           min = 0,
                                           max = 200,
                                           step = 0.1),
                               sliderInput("sigma","sigma",
                                           value = 6,
                                           min = 0.1,
                                           max = 50,
                                           step = 0.1)
                  ),
                  mainPanel(plotOutput("plot_norm1"))
              )# box
            )# fluidRow
    ),
    #-------------- final ------------- 
    tabItem(tabName = "final",
            # fluidRow(
            #   column(width = 4,
            #          box(
            #            title = "Updata:2022/01/07", width = NULL, solidHeader = TRUE, status = "warning"
            #          )
            #   )
            # ),
            fluidRow(
              infoBox("Updata", "2022/01/07", icon = icon("edit"), fill = FALSE,color = "light-blue")
            ) # fluidRow
    ) #tabItem
  ) # tabItems
  ) # dashboardBody
#------------ ui -----
ui <- dashboardPage(header, sidebar, body)

#------------ server -----
server <- function(input, output, session) {
  #------------ txt0 -----
  output$summary <- renderUI({
    withMathJax(includeHTML("dis_formula\\Summary.html"))
  })

  #------------ coin png ------------
  v1 <- reactiveValues(times = NULL)
  observeEvent(input$action_coin, { 
    v1$times <- trunc(runif(1, min = 1, max = 3))
  })
  output$coin <- renderUI({ # renderImage({
    if (is.null(v1$times))return(tags$img(src="coinflip.gif", height = '200px'))
    if (v1$times == 1)
      return(
        tags$img(src="coin1.png", height = '200px')
      )
    else 
        tags$img(src="coin2.png", height = '200px')
    
  })
  # output$text_coin <- renderText({
  #   if (is.null(v1$times)) return("Outcome：")
  #   if (v1$times == 1) return("Outcome：Head")
  #   else 
  #     return("Outcome：Tail")
  #   })
  #------------ dice png ------------
  v2 <- reactiveValues(times = NULL)
  observeEvent(input$action_dice, { 
    v2$times <- trunc(runif(1, min = 1, max = 7))
  })
  output$dice <- renderUI({ # renderImage({
    if (is.null(v2$times))return(tags$img(src="dice-roll.gif", height = '200px'))
    if(v2$times == 1)return(tags$img(src="dice1.png", height = '200px'))
    if(v2$times == 2)return(tags$img(src="dice2.png", height = '200px'))
    if(v2$times == 3)return(tags$img(src="dice3.png", height = '200px'))
    if(v2$times == 4)return(tags$img(src="dice4.png", height = '200px'))
    if(v2$times == 5)return(tags$img(src="dice5.png", height = '200px'))
    if(v2$times == 6)return(tags$img(src="dice6.png", height = '200px'))
  }) 
  # output$text_dice <- renderText({
  #   if (is.null(v2$times)) return("Outcome：")
  #   if (v2$times == 1) return("Outcome：1")
  #   if (v2$times == 2) return("Outcome：2")
  #   if (v2$times == 3) return("Outcome：3")
  #   if (v2$times == 4) return("Outcome：4")
  #   if (v2$times == 5) return("Outcome：5")
  #   if (v2$times == 6) return("Outcome：6")
  # })
  
  #------------ coin plot ------------
  # https://ithelp.ithome.com.tw/articles/10196754
  ## 反應隨機抽取1次和100次的結果
  v <- reactiveValues(times = NULL)
  observeEvent(input$action1, {
    v$times <- trunc(runif(1, min = 1, max = 3))
  })
  observeEvent(input$action2, {
    v$times <- trunc(runif(100, min = 1, max = 3))
  })  
  
  # output$plot_coin <- renderPlot({
    
  #  ## 反應隨機抽取1次和100次的結果
  #  if (is.null(v$times)) return()
  #  # count <- table(trunc(runif(1, min = 1, max = 3)))
  #  count <- table(v$times)
  #  sums <- integer(2)
  #  if(nrow(count)>1){
  #    sums <- count
  #  }else{
  #    ifelse(names(count)=="1",
  #           sums[1] <- as.numeric(count),
  #           sums[2] <- as.numeric(count))
  #  }
  #  
  #  dice <- data.frame(number = c("Heads","Tails"), 
  #                     count = sums) %>%
  #    mutate(p = (count / sum(count)), theoretical = (1/2))          
  #  
  #  
  #  title <- paste("Probability Distribution of coin")
  #  ggplot(dice) + theme_basic() + 
  #    geom_bar(aes(x = number, y = p),
  #             stat = "identity", position = "dodge",
  #             fill = c("#a2d2ff","#bee1e6"))+
  #    ggtitle(title)+
  #    geom_hline(yintercept=1/2, linetype="dashed", color = "red")+
  #    # scale_x_continuous(labels = 1:2, breaks = 1:2)+
  #    scale_y_continuous(limits = c(0, max(dice$p)+0.05),
  #                       breaks = seq(0, max(dice$p)+0.05, by = 0.1)) 
  #})
  
  ## 配合sliderInput的結果
  output$plot_coin <- renderPlot({
    sums <- integer(2)
    for (i in 1:input$n_coin) {
      outcome <- trunc(runif(1, min = 1, max = 3))
      # count result
      sums[outcome] <- sums[outcome] + 1
    }
    # prepare a data frame for graphing
    dice <- data.frame(number = c("Heads","Tails"), count = sums[1:2]) %>%
      mutate(p = (count / sum(count)), theoretical = (1/2))          
    
    
    title <- paste("Probability Distribution of coin")
    ggplot(dice) + theme_basic() + 
      geom_bar(aes(x = number, y = p),
               stat = "identity", position = "dodge",
               fill = c("#a2d2ff","#bee1e6"))+
      ggtitle(title)+
      # scale_x_continuous(labels = 1:2, breaks = 1:2)+
      scale_y_continuous(limits = c(0, max(dice$p)+0.05),
                         breaks = seq(0, max(dice$p)+0.05, by = 0.1)) + 
      geom_hline(yintercept=1/2, linetype="dashed", color = "red")
  })
  
  
  ## https://stackoverflow.com/questions/61287106/how-to-create-a-table-in-a-reactive-object-in-shiny
  
  
  ## 動態呈現表格，需再嘗試
  # output$data_coin <- renderTable({
  #   tableData$d1
  # })
  
  #------------ dice ------------
  output$plot_dice <- renderPlot({
    sums <- integer(6)
    for (i in 1:input$n_dice) {
      # "roll" number between 1 and 6; add together
      outcome <- trunc(runif(1, min = 1, max = 7))
      # count result
      sums[outcome] <- sums[outcome] + 1
    }
    # prepare a data frame for graphing
    dice <- data.frame(number = 1:6, count = sums[1:6]) %>%
      mutate(p = (count / sum(count)), theoretical = (1/6))          
    
    
    title <- paste("Probability Distribution of dice")
    ggplot(dice) + theme_basic() + 
      geom_bar(aes(x = number, y = p),
               stat = "identity", position = "dodge",
               fill = c("#cdb4db","#ffc8dd","#ffafcc","#bde0fe","#a2d2ff","#bee1e6"))+
      ggtitle(title)+
      scale_x_continuous(labels = 1:6, breaks = 1:6)+
      scale_y_continuous(limits = c(0, max(dice$p)),
                         breaks = seq(0, max(dice$p), by = 0.05))+ 
      geom_hline(yintercept=1/6, linetype="dashed", color = "red")
  })
  #------------ button -------
  ## ## 以下為反應分配按鈕，程式碼過長，所以以includeHTM將資料放置資料夾呈現
  ## n  <- 0
  ## observe({
  ##   dist1 <- input$dist1
  ##   n <- n+1
  ##   if (!is.null(dist1)) {
  ##     if (dist1=='dummy')  showActions ()
  ##     else {
  ##       if (dist1=='Binomial')
  ##         output$txt1  <- renderUI({
  ##           withMathJax(paste(input$dist1,"\n",
  ##                 "$$f(x)=\\binom{n}{x}p^{x}(1-p)^{n-x}$$","\n",
  ##                 "$$\\text{E}(X)=np$$","\n",
  ##                 "$$\\text{Var}(X)=np(1-p)$$"))
  ##           
  ##         })
  ##       if (dist1=='Negative Binomial')  
  ##         output$txt1  <- renderUI({
  ##           withMathJax(paste(input$dist1,"\n",
  ##                             "$$f(x)=\\binom{x+r-1}{r-1}p^{r}(1-p)^{x}$$","\n",
  ##                             "$$\\text{E}(X)=\\frac{rp}{1-p}$$","\n",
  ##                             "$$\\text{Var}(X)=\\frac{rp}{(1-p)^2}$$"))
  ##           
  ##         })
  ##       if (dist1=='Geometric')  
  ##         output$txt1  <- renderText (paste (input$dist1, "action C"))
  ##       if (dist1=='Poisson')  
  ##         output$txt1  <- renderText (paste (input$dist1, "action C"))
  ##     }
  ##   } 
  ## })
  
  #------------ txt1 -----
  output$txt1 <- renderUI({
    ## 會無法出現表格
    ## withMathJax(includeMarkdown(paste0("C:\\Users\\user\\R\\R_2021\\prob\\dis_formula\\",input$dist1,".md")))
    # withMathJax(includeHTML(paste0("C:\\Users\\user\\R\\R_2021\\prob\\dis_formula\\",input$dist1,".html")))
    withMathJax(includeHTML(paste0("dis_formula\\",input$dist1,".html")))
  })
  #------------ txt2 -----
  output$txt2 <- renderUI({
    withMathJax(includeHTML(paste0("dis_formula\\",input$dist2,".html")))
  })
  #------------ Bin(n,p) #####
  output$plot_bin1 <- renderPlot({
    x <- seq(from = 0,to = input$n,by = 1) #成功次數
    plot(x,dbinom(x,size = input$n, prob=input$p), # 
         pch=16,type="h", #type='l'
         main = paste("Binomial ( n=",input$n,", p=",input$p,")"),
         ylab='Probability',
         xlab ='x',
         lwd=4,
         col="deepskyblue")
  })
  #------------ NB(r,p) #####
  output$plot_nb1 <- renderPlot({
    x <- seq(from = 0,to = input$r,by = 1) #成功次數
    plot(x,dbinom(x,size = input$r, prob=input$p_nb), # 
         pch=16,type="h", 
         main = paste("Negative Binomial ( r=",input$r,", p=",input$p_nb,")"),
         ylab='Probability',
         xlab ='x',
         lwd=4,
         col="deepskyblue")
  })
  #------------ geo(p) #####
  output$plot_geo1 <- renderPlot({
    x <- seq(from = 0,to = 30,by = 1)
    plot(x,dgeom(x, prob=input$p_geo), # 
         type = 'h', 
         main = paste("Geometric ( p=",input$p_geo,")"),
         ylab = 'Probability',
         xlab = 'x',
         lwd=4,
         col="deepskyblue")
  })
  #------------ Poisson (λ) #####
  output$plot_poi1 <- renderPlot({
    x <- seq(from = 0,to = 30,by = 1)
    plot(x,dpois(x, lambda = input$lambda), # 
         type = 'h', #type='o'
         main = paste("Poisson ( λ=",input$lambda,")"),
         ylab = 'Probability',
         xlab = 'x',
         lwd=4,
         col="deepskyblue")
  })
  #------------ unif (a,b) #####
  output$plot_unif1 <- renderPlot({
    
    ## 文字框輸入數值
    # x <- seq(from = input$min-0.5,to = input$max+0.5, length=100)
    # plot(x,dunif(x,input$min,input$max),
    #      type = 'l',pch=16,
    #      main = paste("Unif ( a=",input$min,",","b=",input$max,")"),
    #      ylab = 'density',
    #      xlab = 'x',
    #      col="deepskyblue",
    #      lwd="4")
    
    ## 用區間方式拖拉數值
    x <- seq(from = min(input$unifRange)-3,to = max(input$unifRange)+3, length=100)
    plot(x,dunif(x,min(input$unifRange),max(input$unifRange)),
         type = 'l',pch=16,
         main = paste("Unif ( a=",min(input$unifRange),",","b=",max(input$unifRange),")"),
         ylab = 'density',
         xlab = 'x',
         col="deepskyblue",
         lwd="4")
  })
  #------------ exp (λ) ------------
  output$plot_exp1 <- renderPlot({
    
    x <- seq(from = min(rexp(100,input$lambda2)),to = max(rexp(100,input$lambda2)), by=0.01)
    plot(x,dexp(x,input$lambda2),
         type = 'l',pch=16,
         main = paste("Exp ( λ=",input$lambda2,")"),
         ylab = 'density',
         xlab = 'x',
         col="deepskyblue",
         lwd="4")
  })
  #------------ N(μ,σ) #####
  output$plot_norm1 <- renderPlot({
    x <- seq(from = min(rnorm(100,input$mu,input$sigma)), to = max(rnorm(100,input$mu,input$sigma)), by = 0.1)
    plot(x, dnorm(x,input$mu,input$sigma), type = "l", 
         ylab = "density",
         main = paste("Normal ( μ=",input$mu,",σ=",input$sigma,")"),
         xlab = 'x',
         col="deepskyblue",
         lwd="4")
  })
}
shinyApp(ui, server)
