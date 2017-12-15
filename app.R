library(shiny)
library(ggplot2)
library(reshape2)
library(matrixStats)
library(markdown)
library(plotly)
library(crosstalk)
library(tidyr)
library(dplyr)
library(gridExtra)
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
                                withMathJax(h6('$$\\text{The histograms shows the corresponding distribution of the last replicate by default, which can be changed by users.}$$')),
                                plotOutput('plot1', click = "plot_click1",height = 2000),
                                br(),
                                verbatimTextOutput('plot1_txt'),
                                br(),
                                br(),
                                plotOutput('plot2'),
                               br(),
                               verbatimTextOutput('plot2_txt'),
                               br(),
                               br(),
                               plotOutput('plot3'),
                               br(),
                               verbatimTextOutput('plot3_txt'),
                               br(),
                               br(),
                               plotOutput('plot4'),
                               br(),
                               verbatimTextOutput('plot4_txt'),
                               br(),
                               br(),
                               plotOutput('plot5'),
                               br(),
                               verbatimTextOutput('plot5_txt'),
                               br(),
                               br(),
                               plotOutput('with'),
                               br(),
                               verbatimTextOutput('with_txt'),
                               br(),
                               br(),
                               plotOutput('sta'),
                               br(),
                               verbatimTextOutput('sta_txt'),
                               br(),
                               br()
                              )
                            )
                          )
                          )
)
)


server <- function(input, output) {
  
  
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
    long$log_out = abs(long$sn_loglog) > sqrt(2)
    
    # subset data
    long = long[ (long$n %% 50 == 0),]
    long 

  })
  
  
  within= reactive({
    long = sn_df()
    dat=long %>%
      group_by(n) %>%
      summarize(pct = mean(!log_out))
    dat
  })

  stats = reactive({
    long = sn_df()
    longer = long %>%
      select(-sn,-loglog) %>%
      gather(type, value = value, sn_n, sn_sqrtn, sn_loglog)
    dat =  longer %>%
    group_by(n) %>%
    summarize(
      mean = mean(value),
      min = min(value),
      q25 = quantile(value, probs = 0.25),
      q50 = median(value),
      q75 = quantile(value, probs = 0.75),
      max = max(value)
    ) %>%
    gather(type, value, -n)
   dat
  })


  sn_last_n = reactive ({
    long = sn_df()
    clicked_n = input$plot_click1$x
    if (is.null(clicked_n)) {
      clicked_n = max(long$n)
    } else {
      clicked_n = ceiling(clicked_n*10000 / 50) * 50
    }
    long = long[ long$n == clicked_n, ]
    long
  })
  
  
  

  gg = eventReactive (input$go,{
    ggplot(data=sn_df(), aes(x=n, group = variable)) + 
      geom_line(alpha=0.5,color='lightblue') +
      # facet_wrap(~ type)+
      # facet_wrap(~ type, ncol = 1)+
      theme(axis.title=element_text(size=25),
            axis.text=element_text(size=21),
            title=element_text(size=25)) 
  })

  
  hist_lims = reactive ({
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
  

  ############
  # S_n / n plots
  ############

observeEvent(input$go,{
 output$plot1=renderPlot({
   g1=gg() + aes(y = sn_n)+ylab('Sn/n')+xlab('Sample size n')+
    ggtitle('Plot of Sum divided by n')
   #xlab('Sample size n')+
   g2=gg() + aes(y = sn_sqrtn)+ylab(expression(Sn/sqrt(n)))+ggtitle(expression(Plot~of~Sum~divided~by~sqrt(n))) + xlab('Sample size n')+
     geom_hline(yintercept =c(-3,3),color='blue',alpha=0.5)
   g3=gg() + aes(y = sn_loglog)+
     ggtitle(expression(Plot~of~Sum~divided~by~sqrt(nloglogn)))+xlab('Sample size n')+
     ylab(expression(Sn/sqrt(nloglogn)))+
     geom_hline(yintercept =c(-sqrt(2),sqrt(2)),color='blue',alpha=0.5)
   g4=  gg() + aes(y = sn)  + ylab('Sn')+ ggtitle('Sum of n variables')+xlab('Sample size n')
   grid.arrange(g1, g2,g3,g4,ncol=1 )},height = 2000)
  })


  observeEvent(input$go,{
    output$plot1_txt=renderPrint({
      cat('The plot shows the sum Sn divided by n,divided by √n,divided by √loglog(n) of the 200 replicates. 

Sn/n would be close to 0 as n gets larger. By the law of large numers we have Sn/n → 0 almost surely.

Sn/√n is a continous distirbution and lie roughly between -3 and 3. By the central limit theorem we have Sn/√n converges in distribution to a standard normal random variable.

Sn/√loglog(n) would oscillate between ±√2.')
    })
  })
  
  output$plot2=renderPlot({
    data = sn_last_n()
    un = unique(data$n)
    run_hist(data$sn_n,
             main = paste0('Histogram of Sn/n at n = ', un),
             xlim = hist_lims(),xlab='Sn/n',
             ylab='Frequency',cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,col='lightblue')
  })
  observeEvent(input$go,{
    output$plot2_txt=renderPrint({
      cat('The histogram shows the Sum Sn divided by n at n = ')
      clicked_n = input$plot_click1$x
      if (is.null(clicked_n)) {
        clicked_n = 10000
      } else {
        clicked_n = ceiling(clicked_n*10000 / 50) * 50
      }
      cat(clicked_n)
      cat(', which would be approximate normal distribution as n gets larger.')
    })
  })
 
  ############
  # S_n / sqrt(n) plots
  ############

  output$plot3=renderPlot({
    data = sn_last_n()
    un = unique(data$n)
    run_hist(data$sn_sqrtn,
             main = paste0('Histogram of Sn/√n at n = ', un),
             xlim = hist_lims(),
             xlab='Sn/√n',ylab='Frequency',cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,col='lightblue')
  })
  observeEvent(input$go,{
    output$plot3_txt=renderPrint({
      cat('The histogram shows the Sum Sn divided by √n at n = ')
      clicked_n = input$plot_click1$x
      if (is.null(clicked_n)) {
        clicked_n = 10000
      } else {
        clicked_n = ceiling(clicked_n*10000 / 50) * 50
      }
      cat(clicked_n)      
      cat(',which would be approximate normal distribution as n gets larger.')
    })
  })

  output$plot4=renderPlot({
    data = sn_last_n()
    un = unique(data$n)
    run_hist(data$sn_loglog,
             main = paste0('Histogram of Sn/√loglog(n) at n = ', un),
             xlim = hist_lims(),
             xlab='Sn/√loglog(n)',ylab='Frequency',cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,col='lightblue')
    })
  observeEvent(input$go,{
    output$plot4_txt=renderPrint({
      cat('The histogram shows the Sum Sn divided by √loglog(n) at n = ')
      clicked_n = input$plot_click1$x
      if (is.null(clicked_n)) {
        clicked_n = 10000
      } else {
        clicked_n = ceiling(clicked_n*10000 / 50) * 50
      }
      cat(clicked_n)
      cat(', which would be approximate normal distribution as n gets larger.')
    })
  })
  
  output$plot5=renderPlot({
    data = sn_last_n()
    un = unique(data$n)
    hist(data$sn,
         main = paste0('Histogram of Sn at n = ', un),
         xlab='Sn',ylab='Frequency',
         cex.lab=2, cex.axis=2, cex.main=2,
         cex.sub=2,col='lightblue')
  })
  observeEvent(input$go,{
    output$plot5_txt=renderPrint({
      cat('The histogram shows sum Sn at n = ')
      clicked_n = input$plot_click1$x
      if (is.null(clicked_n)) {
        clicked_n = 10000
      } else {
        clicked_n = ceiling(clicked_n*10000 / 50) * 50
      }
      cat(clicked_n)
      cat(', which would be approximate normal distribution as n gets larger.')
    })
  })
  
  observeEvent(input$go,{
    output$with=renderPlot({
      dat=within()
      ggplot(data=dat,aes(x=n, y=pct))+
           ylim(c(0.90, 1))+
      xlab('Sample size n')+ylab('Proportion')+
        ggtitle('Proportion of Sn/sqrt(nloglogn) within √2 boundary')+
      theme(axis.title=element_text(size=30),
            axis.text=element_text(size=21,face="bold"),
            title=element_text(size=25))+
        geom_line()+
      geom_hline(yintercept = 1)    
      })
  })
  
  observeEvent(input$go,{
    output$with_txt=renderPrint({
      cat('The plot shows the proportion of Sn/√loglog(n) that are bounded within ±√2.')
    })
  })


  

  observeEvent(input$go,{
    output$sta=renderPlot({
      dat=stats()
      g = dat %>%
        ggplot(aes(x = n, y = value, colour = type)) +
        ggtitle('Quantiles of Sn/√n')+
        xlab('Sample size n')+
        theme(axis.title=element_text(size=30),
              axis.text=element_text(size=21,face="bold"),
              title=element_text(size=25))+
        geom_line()
      g

    })
  })
  observeEvent(input$go,{
    output$sta_txt=renderPrint({
      cat('The plot shows distribution of the max,75%, 50%, 25% quantiles and min of Sn/√n.')
    })
  })
  
  output$descriptions <- renderUI({
    withMathJax(includeMarkdown('info.md'))
  })
}

shinyApp(ui = ui, server = server)
