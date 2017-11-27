library(shiny)
library(ggplot2)
library(reshape2)
library(matrixStats)
library(markdown)
setwd("~/Desktop/term2/shiny/gg")

ui <-  shinyUI(navbarPage("My App",
                          tabPanel("Instructions", fluidPage(
                            uiOutput('instructions')
                          )),
                          tabPanel("App", fluidPage(
  titlePanel("Law of iterated logarithm"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribution",
                  list("Normal" = "normal",
                       "Bernoulli" = "bernoulli",
                       "Poisson" = "poisson"
                  )
      ),  
      conditionalPanel(
        condition = "input.dist== 'normal'",
        numericInput("mean", "mean:", min=0, max=400, value=0, step=0.05),
        numericInput("sd", "sd: ", min=0.1, max=20, value=1, step=0.05)
      ),
      conditionalPanel(
        condition = "input.dist == 'bernoulli'",
        numericInput("p", "p: ", min=0.05, max=1, value=0.5, step=0.05)
      ),
      conditionalPanel(
        condition = "input.dist == 'poisson'",
        numericInput("lambda", "lambda: ", min=1, max=10, value=5, step=0.5)),
      checkboxInput("fix_x", "Fix x-axis in histogram")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput('plot1'),
      plotOutput('plot2'),
      plotOutput('plot3'),
      plotOutput('plot4'),
      plotOutput('plot5'),
      plotOutput('plot6'),
      plotOutput('plot7'),
      plotOutput('plot8')
    )
  )
)
)
)
)




server <- function(input, output) {
  DistX <- reactive( input$dist )
  nX <- reactive( input$nX )
  paramsX <- reactive(  {
    switch(DistX(),
           "normal" = list(mean=input$mean, sd=input$sd),
           "bernoulli" = list(size=1,prob=input$p),
           "poisson" = list(lambda=input$lambda)
    )} )
  rdistX <- reactive(  {
    switch(DistX(),
           "normal" = "rnorm",
           "bernoulli" = "rbinom",
           "poisson" = "rpois"
    )} )
  mean.dist=reactive({
    switch(DistX(),
           "normal" = input$mean,
           "bernoulli" = input$p,
           "poisson" = input$lambda
    )})
  sd.dist=reactive({
    switch(DistX(),
           "normal" = input$sd,
           "bernoulli" = sqrt(input$p*(1-input$p)),
           "poisson" = sqrt(input$lambda)
    )})
  sampleDistX=reactive({
    res = do.call(rdistX(), c(10000*200, paramsX()))
    res = matrix(res,nrow=10000)
    res
  })
  dat=reactive({
    (sampleDistX()-mean.dist())/sd.dist()
    })
  
  data_sn=reactive({
    colCumsums(dat())
  })
 
  sn_df=reactive({
    df = data_sn()
    df = data.frame(df, n = 1:nrow(df))
    long = melt(df,id='n', value.name = "sn")
    long$sn_n = long$sn / long$n
    long$sn_sqrtn = long$sn / sqrt(long$n)
    long$loglog = pmax(1e-7, log(log(long$n)))
    long$sn_loglog = long$sn / sqrt(long$n * long$loglog)
    
    # subset data
    long = long[ (long$n %% 10 == 0),]
    long
  })
  sn_last_n = reactive({
    long = sn_df()
    long = long[ long$n == max(long$n), ]
    long
  })
  
  gg = reactive({
    ggplot(data=sn_df(), aes(x=n, group = variable)) + 
      geom_line(alpha=0.05)
  })
  
  hist_lims = reactive({
    if (input$fix_x) {
      data = sn_last_n()
      lim = range(c(data$sn_n, data$sn_sqrtn, 
                    data$sn_loglog))
    } else {
      lim = NULL
    }
    lim
  })
  
  run_hist = function(data, xlim, ...) {
    if (is.null(xlim)) {
      hist(data, ...)
    } else {
      hist(data, xlim = xlim, ...)
    }
  }
  
    output$plot1=renderPlot({
    gg() + aes(y = sn)
  })
  output$plot2=renderPlot({
    data = sn_last_n()
    hist(data$sn,main='Histogram of sn')
  })
  
  ############
  # S_n / n plots
  ############
  output$plot3=renderPlot({
    gg() + aes(y = sn_n)
  })
  output$plot4=renderPlot({
    data = sn_last_n()
    run_hist(data$sn_n,main='Histogram of sn/n', xlim = hist_lims())    
         })
  
  ############
  # S_n / sqrt(n) plots
  ############
  output$plot5=renderPlot({
    gg() + aes(y = sn_sqrtn)
  })
  output$plot6=renderPlot({
    data = sn_last_n()
    run_hist(data$sn_sqrtn,main='Histogram of sn/sqrt(n)', xlim = hist_lims())    
  })
  ############
  # S_n / sqrt(n loglog(n)) plots
  ############
  output$plot7=renderPlot({
    gg() + aes(y = sn_loglog)
  })
  output$plot8=renderPlot({
    data = sn_last_n()
    run_hist(data$sn_loglog,main='Histogram of sn/sqrt(nloglogn)', xlim = hist_lims())    
  })
  
  output$instructions <- renderUI({
    includeMarkdown('info.md')
  })
}
shinyApp(ui = ui, server = server)
