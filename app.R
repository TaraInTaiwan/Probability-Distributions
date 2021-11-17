library(shiny)
ui <- fluidPage(
    titlePanel("Distribution"),
    tabsetPanel(type = "pills",
                tabPanel("Binomial",
                         sidebarLayout(
                             sidebarPanel(width = 4,
                                          sliderInput("n","number",
                                                      value = 1,
                                                      min = 1,
                                                      max = 100),
                                          sliderInput("p",
                                                      "Probability",
                                                      value = 0.6,
                                                      min = 0,
                                                      max = 1)
                                          ),
                             mainPanel(
                                 fluidRow(column(12, plotOutput("plot_bin1"))
                                          )
                                    )
                                )
                         ),
                tabPanel("Negative Binomial",
                         sidebarLayout(
                             sidebarPanel(width = 4,
                                          sliderInput("r","number",
                                                      value = 1,
                                                      min = 1,
                                              max = 100),
                                          sliderInput("p_nb","Probability",
                                                      value = 0.6,
                                                      min = 0,
                                                      max = 1)
                                          ),
                              mainPanel(
                                  fluidRow(
                                      column(12, plotOutput("plot_nb1"))
                                      )
                                  )
                              )
                         ),
                tabPanel("Geometric",
                         sidebarLayout(
                             sidebarPanel(width = 4, 
                                          sliderInput("p_geo","Probability",
                                                      value = 0.6,
                                                      min = 0.0001,
                                                      max = 1)
                                          ),
                             mainPanel(
                                 fluidRow(
                                     column(12, plotOutput("plot_geo1"))
                                     )
                                 )
                             )
                         ),
                tabPanel("Poisson",
                         sidebarLayout(
                             sidebarPanel(width = 4, 
                                          sliderInput("lambda","lambda",
                                                      value = 2,
                                                      min = 0.0001,
                                                      max = 50)
                             ),
                             mainPanel(
                                 fluidRow(
                                     column(12, plotOutput("plot_poi1"))
                                     )
                                 )
                             )
                         ),
                tabPanel("Uniform",
                         sidebarLayout(
                             sidebarPanel(width = 4, 
                                          numericInput("min", "Minimum", 2),
                                          numericInput("max", "Maximum", 5)
                                          # sliderInput("a","a",
                                          #             value = 2,
                                          #             min = -10,
                                          #             max = 10),
                                          # sliderInput("b","b",
                                          #             value = 5,
                                          #             min = -10,
                                          #             max = 10)
                             ),
                             mainPanel(
                                 fluidRow(
                                     column(12, plotOutput("plot_unif1"))
                                     )
                                 )
                             )
                         ),
                tabPanel("Exponential ",
                         sidebarLayout(
                             sidebarPanel(width = 4, 
                                          # numericInput("lambda2", "lambda", 2)
                                          sliderInput("lambda2","lambda",
                                                      value = 2,
                                                      min = 0,
                                                      max = 10,
                                                      step = 0.01)
                             ),
                             mainPanel(
                                 fluidRow(
                                     column(12, plotOutput("plot_exp1"))
                                     )
                                 )
                             )
                         )
                )
    )
server <- function(input, output) {
    
    output$plot_bin1 <- renderPlot({
        x <- seq(from = 0,to = input$n,by = 1) #成功次數
        plot(x,dbinom(x,size = input$n, prob=input$p), # 
             pch=16,type="h", #type='l'
             main = paste("Binomial ( n=",input$n,", p=",input$p,")"),
             ylab='Probability',
             xlab ='x',
             lwd=3)
    })
    
    ##### NB(r,p) #####
    output$plot_nb1 <- renderPlot({
        x <- seq(from = 0,to = input$r,by = 1) #成功次數
        plot(x,dbinom(x,size = input$r, prob=input$p_nb), # 
             pch=16,type="h", 
             main = paste("Negative Binomial ( r=",input$r,", p=",input$p_nb,")"),
             ylab='Probability',
             xlab ='x',
             lwd=3)
    })
    ##### geo(p) #####
    output$plot_geo1 <- renderPlot({
        x <- seq(from = 0,to = 30,by = 1)
        plot(x,dgeom(x, prob=input$p_geo), # 
             type = 'h', 
             main = paste("Geometric ( p=",input$p_geo,")"),
             ylab = 'Probability',
             xlab = 'x',
             lwd=3)
    })
    ##### Poisson (λ) #####
    output$plot_poi1 <- renderPlot({
        x <- seq(from = 0,to = 30,by = 1)
        plot(x,dpois(x, lambda = input$lambda), # 
             type = 'h', #type='o'
             main = paste("Poisson ( λ=",input$lambda,")"),
             ylab = 'Probability',
             xlab = 'x',
             lwd=3)
    })
    ##### unif (a,b) #####
    output$plot_unif1 <- renderPlot({
        
        x <- seq(from = input$min-0.5,to = input$max+0.5, length=100)
        plot(x,dunif(x,input$min,input$max),
             type = 'l',pch=16,
             main = paste("Unif ( a=",input$min,",","b=",input$max,")"),
             ylab = 'Probability',
             xlab = 'x',
             lwd=3)
    })
    ##### exp (λ) #####
    output$plot_exp1 <- renderPlot({
        
        x <- seq(from = min(input$lambda2-0.5,0),to = input$lambda2+3, by=0.01)
        plot(x,dexp(x,input$lambda2),
             type = 'l',pch=16,
             main = paste("Exp ( λ=",input$lambda2,")"),
             ylab = 'Probability',
             xlab = 'x',
             lwd=3)
    })

}
shinyApp(ui = ui, server = server)
