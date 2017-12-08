library(shiny)
library(ggplot2)
library(reshape2)
library(matrixStats)
library(markdown)

ui <-  shinyUI(navbarPage("Law of the Iterated Logarithm",
                          tabPanel("Descriptions", fluidPage(
                            uiOutput('descriptions')
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
                                checkboxInput("fix_x", "Fix x-axis in histogram", value = TRUE),
                                actionButton('go','Go')
                              ),
                              # Show a plot of the generated distribution
                              mainPanel(
                                
                                withMathJax(h6('$$\\text{The simulation generates 200 iid random variables with 10,000 replicats.}$$')),
                                withMathJax(h6('$$\\text{The sum of the random variables } S_n~\\text{are calculated,and}~ S_n~ \\text{are dependent for i =1,2,..n.}$$')),
                                withMathJax(h6('$$S_n\\text{,}~S_n/n\\text{,}~ S_n/\\sqrt{n}~\\text{and}~ S_n/\\sqrt{n \\log\\log(n)}~\\text{are plotted.}$$')),
                                withMathJax(h6('$$\\text{The histograms shows the corresponding distribution of the last replicate.}$$')),
                                plotOutput('plot3', click = "plot_click"),
                                verbatimTextOutput("click_ids"),
                                plotOutput('plot4'),
                                plotOutput('plot5'),
                                plotOutput('plot6'),
                                # withMathJax(h2( "$$\\text{Plot of Sum divided by }\\sqrt{n \\log\\log(n)}$$")),
                                plotOutput('plot7'),
                                plotOutput('plot8'),
                                #h2("Plot of Sum of n variables"),
                                plotOutput('plot1'),
                                plotOutput('plot2')
                              )
                            )
                          )
                          )
)
)




server <- function(input, output) {
  
  output$click_ids <- renderPrint({
     cat("The closest n is:\n")
     str(input$plot_click$x)
   })
   
  DistX <- reactive( input$dist )
  nX <- eventReactive (input$go,{input$nX })
  
  paramsX <- eventReactive (input$go,{
    switch(DistX(),
           "normal" = list(mean=input$mean, sd=input$sd),
           "bernoulli" = list(size=1,prob=input$p),
           "poisson" = list(lambda=input$lambda)
    )} )
  rdistX <- eventReactive (input$go,{
    switch(DistX(),
           "normal" = "rnorm",
           "bernoulli" = "rbinom",
           "poisson" = "rpois"
    )} )
  mean.dist=eventReactive (input$go,{
    switch(DistX(),
           "normal" = input$mean,
           "bernoulli" = input$p,
           "poisson" = input$lambda
    )})
  sd.dist=eventReactive (input$go,{
    switch(DistX(),
           "normal" = input$sd,
           "bernoulli" = sqrt(input$p*(1-input$p)),
           "poisson" = sqrt(input$lambda)
    )})
  sampleDistX=eventReactive (input$go,{
    res = do.call(rdistX(), c(10000*200, paramsX()))
    res = matrix(res,nrow=10000)
    res
  })
  dat=eventReactive (input$go,{
    (sampleDistX()-mean.dist())/sd.dist()
  })
  
  data_sn=eventReactive (input$go,{
    colCumsums(dat())
  })
  
  sn_df=eventReactive (input$go,{
    df = data_sn()
    df = data.frame(df, n = 1:nrow(df))
    long = melt(df,id='n', value.name = "sn")
    long$sn_n = long$sn / long$n
    long$sn_sqrtn = long$sn / sqrt(long$n)
    long$loglog = pmax(1e-7, log(log(long$n)))
    long$sn_loglog = long$sn / sqrt(long$n * long$loglog)
    
    # subset data
    long = long[ (long$n %% 50 == 0),]
    long
  })
  # OLD_sn_last_n = eventReactive (input$go,{
  #   long = sn_df()
  #   long = long[ long$n == max(long$n), ]
  #   long
  # })
  
  sn_last_n = eventReactive (input$go,{
    long = sn_df()
    clicked_n = input$plot_click$x
    if (is.null(clicked_n)) {
      clicked_n = max(long$n)
    } else {
      clicked_n = ceiling(clicked_n / 50) * 50
    }
    long = long[ long$n == clicked_n, ]
    long
  })
  
  gg = eventReactive (input$go,{
    ggplot(data=sn_df(), aes(x=n, group = variable)) + 
      geom_line(alpha=0.1,color='hotpink') +
      theme(axis.title=element_text(size=30),
            axis.text=element_text(size=21,face="bold"),
            title=element_text(size=25)) 
    
  })
  
  hist_lims = eventReactive (input$go,{
    if (input$fix_x) {
      data = sn_last_n()
      lim = range(c(data$sn_n, data$sn_sqrtn, 
                    data$sn_loglog))
      lim = range(floor(lim), ceiling(lim))
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
    gg() + aes(y = sn) + xlab('n') + ylab('Sn')+ ggtitle('Sum of n variables')
  })
  
  output$plot2=renderPlot({
    data = sn_last_n()
    un = unique(data$n)
    hist(data$sn,
         main = paste0('Histogram of Sn for n = ', un),
         xlab='Sn',ylab='Frequency',
         cex.lab=2, cex.axis=2, cex.main=2, 
         cex.sub=2,col='lightblue')
  })
  
  ############
  # S_n / n plots
  ############
  
  observeEvent(input$go,{
    output$plot3=renderPlot({
      gg() + aes(y = sn_n)+xlab('n')+ylab('Sn/n')+ ggtitle('Plot of Sum divided by n')
    })
  })
  
  # observeEvent(input$go,{
    output$plot4=renderPlot({
      data = sn_last_n()
      un = unique(data$n)
      run_hist(data$sn_n, 
               main = paste0('Histogram of Sn/n n = ', un),
               xlim = hist_lims(),xlab='Sn',
               ylab='Frequency',cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,col='lightblue')
    })
  # })
  ############
  # S_n / sqrt(n) plots
  ############
  
  output$plot5=renderPlot({
    gg() + aes(y = sn_sqrtn)+xlab('n')+ylab(expression(Sn/sqrt(n)))+ggtitle(expression(Plot~of~Sum~divided~by~sqrt(n))) +
      geom_hline(yintercept =c(-3,3),color='blue',alpha=0.5)
  }) 
  
  output$plot6=renderPlot({
    data = sn_last_n()
    run_hist(data$sn_sqrtn,main=expression(Histogram~of~sn/sqrt(n)), xlim = hist_lims(),
             xlab='Sn',ylab='Frequency',cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,col='lightblue') 
  })
  
  ############
  # S_n / sqrt(n loglog(n)) plots
  ############
  output$plot7=renderPlot({
    gg() + aes(y = sn_loglog)+xlab('n')+
      ggtitle(expression(Plot~of~Sum~divided~by~sqrt(nloglogn)))+
      ylab(expression(Sn/sqrt(nloglogn)))+
      geom_hline(yintercept =c(-sqrt(2),sqrt(2)),color='blue',alpha=0.5)
  })
  
  output$plot8=renderPlot({
    data = sn_last_n()
    
    run_hist(data$sn_loglog,main=expression(Histogram~of~Sn/sqrt(nloglogn)), xlim = hist_lims(),
             xlab='Sn',ylab='Frequency',cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,col='lightblue')  
    
  })
  
  output$descriptions <- renderUI({
    withMathJax(includeMarkdown('info.md'))
  })
}
shinyApp(ui = ui, server = server)
