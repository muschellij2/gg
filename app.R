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
library(grDevices)
ui <-  shinyUI(navbarPage("Law of Iterated Logarithm",
                          tabPanel("Descriptions", fluidPage(
                            uiOutput('descriptions')
                          )),
                          tabPanel("App", fluidPage(
                            tags$head(
                              tags$style(HTML(" @import url('https://fonts.googleapis.com/css?family=Roboto:400,700');
                                                                      h1{
                                                                      font-family: 'Roboto', cursive;
                                                                      font-weight: 500;
                                                                      line-height: 1.5;
                                                                      color: #2A9FD6;
                                                                      }
                                                                      h2{
                                                                      font-family: 'Roboto', cursive;
                                                                      font-weight: 500;
                                                                      line-height: 1.5;
                                                                      color: #2A9FD6;
                                                                      }
                                                                   h3{
                                                       font-family: 'Roboto', cursive;
                                                                      font-weight: 500;
                                                                      line-height: 1.5;
                                                                      color: #2A9FD6;
                                                                      }
                                                                      figure {
                                                                          margin: 0 0 1rem;
                                                                      }
                                                                      
                                                                      # img {
                                                                      # vertical-align: middle;
                                                                      # border-style: none;
                                                                      # }
                                                                      "))
                            ),
                            headerPanel("Law of Iterated Logarithm"),
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Create random variable by setting distribution and parameter values"),
                                selectInput("dist", "Distribution",
                                            list("Normal" = "normal",
                                                 "Bernoulli" = "bernoulli",
                                                 "Poisson" = "poisson"
                                            )
                                ),
                                conditionalPanel(
                                  condition = "input.dist== 'normal'",
                                  numericInput("mean", "Mean:", value=0, step=0.05),
                                  numericInput("sd", "Standard Deviation: ", min=0.1, value=1, step=0.05)
                                ),
                                conditionalPanel(
                                  condition = "input.dist == 'bernoulli'",
                                  numericInput("p", "p: ", min=0.01, max=1, value=0.5, step=0.01)
                                ),
                                conditionalPanel(
                                  condition = "input.dist == 'poisson'",
                                  numericInput("lambda", "lambda: ", min=1, value=5, step=0.5)),
                                numericInput("rep", "Number of replicates:", value=200, min = 1),
                                
                                actionButton('go','Go'),
                                checkboxInput("fix_x", "Fix x-axis in histogram", value = TRUE),
                                
                                helpText("Choose the number of single replicate"),
                                numericInput("nX", "Track single replicate:", value=1, min = 1)
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                h1('Instruction'), 
                                h4("The APP aims to demonstrate the Law of the Iterated Logarithm. Users can select Normal, Bernoulli, Poisson distributions and set up corresponding parameters for random variable generation."),
                                tags$head(tags$style("
                                                     h4{
                                                       font-family: 'Roboto', cursive;
                                                       font-weight: 500;
                                                       line-height: 1.5;
                                                       color: black;
                                                     }"
                                )
                                ),
                                h4('The simulation generates independent and identically ditributed random variables with user-defined number of replicates.The sum of the random variables Sn are calculated,and Sn are dependent for i =1,2,..n.') ,
                                h2("Comparison of Sn/n, Sn/√n, Sn/√(nloglog(n)) plots"),
                                plotlyOutput('plot1',height = 600),br(),br(),
                                verbatimTextOutput('plot1_txt'),br(),br(),
                                h3('The histograms shows the corresponding frequency of n=10000 by default. Users can change it by clicking the Sn plots.'),
                                fluidRow(
                                  splitLayout(cellWidths = c("50%", "50%"), plotOutput('plot4'), plotOutput('plot2')),
                                  splitLayout(cellWidths = c("50%", "50%"), plotOutput('plot3'), plotOutput('plot5'))
                                ),
                                
                                verbatimTextOutput('plot4_txt'),br(),br(),
                                
                                h2('Comparison of Sn/n, Sn/√n and Sn/√(nloglog(n)) plots for single replicate'), 
                                plotlyOutput('track'),br(),br(),
                                verbatimTextOutput('track_txt'),br(),br(),
                                h2('Explore the boundary of Law of Iterated Logarithm'), 
                                plotOutput('with'),
                                verbatimTextOutput('with_txt'),br(),br(),
                                plotOutput('first_time'),br(),br(),
                                verbatimTextOutput('first_time_txt'),br(),br(),
                                h3('With simulations, we can find out that the as n gets infinity, Sn/√(nloglog(n)) would oscillates between ±√2, by the law of iterated logarithm')
                              )
                            )
                          )
                          )
)
)


server <- function(input, output) {
  DistX <- reactive( input$dist)
  n.rep= eventReactive (input$go,{input$rep},ignoreNULL = FALSE)
  n.X=reactive (input$nX)
  paramsX <- eventReactive (input$go,{
    switch(DistX(),
           "normal" = list(mean=input$mean, sd=input$sd),
           "bernoulli" = list(size=1,prob=input$p),
           "poisson" = list(lambda=input$lambda)
    )},ignoreNULL = FALSE)
  rdistX <- eventReactive (input$go,{
    switch(DistX(),
           "normal" = "rnorm",
           "bernoulli" = "rbinom",
           "poisson" = "rpois"
    )},ignoreNULL = FALSE)
  mean.dist=eventReactive (input$go,{
    switch(DistX(),
           "normal" = input$mean,
           "bernoulli" = input$p,
           "poisson" = input$lambda
    )},ignoreNULL = FALSE)
  sd.dist=eventReactive (input$go,{
    switch(DistX(),
           "normal" = input$sd,
           "bernoulli" = sqrt(input$p*(1-input$p)),
           "poisson" = sqrt(input$lambda)
    )},ignoreNULL = FALSE)
  sampleDistX=reactive ({
    res = do.call(rdistX(), c(10000*n.rep(), paramsX()))
    res = matrix(res,nrow=10000)
    res
  })
  dat=reactive({
    (sampleDistX()-colMeans(sampleDistX()))/apply(sampleDistX(),2,sd)
  })
  
  data_sn=reactive({
    colCumsums(dat())
  })
  # observe({
  # if (is.null(input$go)){
  
  
  sn_df_all=reactive({
    if (is.null(input$go)){
      load(file='sn_df_all.rda')
      sn_df_all
    } else{
      df = data_sn()
      df = data.frame(df, n = 1:nrow(df))
      long = melt(df,id='n', value.name = "sn")
      long$sn_n = long$sn / long$n
      long$sn_sqrtn = long$sn / sqrt(long$n)
      long$loglog = pmax(1e-7, log(log(long$n)))
      long$sqrtlog=sqrt(long$loglog)
      long$sn_loglog = long$sn / sqrt(long$n * long$loglog)
      long$log_out = abs(long$sn_loglog) > sqrt(2)
      names(long)[names(long) == 'variable'] <- 'replicate'
      long
    }
  })
  sn_df=reactive({
    if (is.null(input$go)){
      load(file='sn_df.rda')
      sn_df
    }else{
      long=sn_df_all()
      long = long[ (long$n %% 50 == 0),]
      long
    }
  })
  # 
  df_long =reactive ({
    if (is.null(input$go)){
      load(file='df_long.rda')
      df_long
    }else{
      long=sn_df()
      longer = long %>%
        select(-sn, -loglog) %>%
        gather(type, value = value, sn_n, sn_sqrtn, sn_loglog)
      longer
    }
  })
  
  time = reactive ({
    if (is.null(input$go)){
      load(file='time.rda')
      time
    }else{
      long = sn_df_all()
      log=long[c('replicate','sn_loglog')]
      t= sapply(unique(log$replicate),function(i) min(which(abs(log[log$replicate==i,]$sn_loglog)<sqrt(2))))
      t
    }
  })
  
  within= reactive({
    if (is.null(input$go)){
      load(file='within.rda')
      within
    }else{
      long = sn_df()
      dat=long %>%
        group_by(n) %>%
        summarize(pct = mean(!log_out))
      dat
    }
  })
  # line= reactive({
  #   if (is.null(input$go)){
  #     load(file='/Users/alice/Documents/term2/shiny/gg/line.rda')
  #     line
  #   }else{
  #  line = data.frame(type=c( "sn_n","sn_sqrtn","sn_loglog"),upper=c(0,3,sqrt(2)),lower=c(0,-3,-sqrt(2)))
  #   }
  # })
  
  #   }else{
  
  # 
  #   sn_df_all=reactive({
  #     df = data_sn()
  #     df = data.frame(df, n = 1:nrow(df))
  #     long = melt(df,id='n', value.name = "sn")
  #     long$sn_n = long$sn / long$n
  #     long$sn_sqrtn = long$sn / sqrt(long$n)
  #     long$loglog = pmax(1e-7, log(log(long$n)))
  #     long$sqrtlog=sqrt(long$loglog)
  #     long$sn_loglog = long$sn / sqrt(long$n * long$loglog)
  #     long$log_out = abs(long$sn_loglog) > sqrt(2)
  #     names(long)[names(long) == 'variable'] <- 'replicate'
  #     long
  #   })
  
  # sn_df=reactive({
  #   # subset data
  #   long=sn_df_all()
  #   long = long[ (long$n %% 50 == 0),]
  #   long
  # })
  
  # time = reactive ({
  #   long = sn_df_all()
  #   log=long[c('replicate','sn_loglog')]
  #   t= sapply(unique(log$replicate),function(i) min(which(abs(log[log$replicate==i,]$sn_loglog)<sqrt(2))))
  #   t
  # })
  
  maxtime=reactive ({
    t=time()
    max(t)
  })
  
  # df_long =reactive ({
  #   long=sn_df()
  #   longer = long %>%
  #    select(-sn, -loglog) %>%
  #    gather(type, value = value, sn_n, sn_sqrtn, sn_loglog)
  #   longer
  #   })
  # 
  df_longer=reactive ({
    longer=df_long()
    shared_longer=SharedData$new(longer)
    shared_longer
  })
  
  # within= reactive({
  #   long = sn_df()
  #   dat=long %>%
  #     group_by(n) %>%
  #     summarize(pct = mean(!log_out))
  #   dat
  # })
  
  line = data.frame(type=c( "sn_n","sn_sqrtn","sn_loglog"),upper=c(0,3,sqrt(2)),lower=c(0,-3,-sqrt(2)))
  #}
  
  
  sn_last_n = reactive ({
    long = sn_df()
    clicked_n = event_data("plotly_click")$x
    if (is.null(clicked_n)) {
      clicked_n = max(long$n)
    }else if (clicked_n>10000) {
      clicked_n = max(long$n)
    }else if (clicked_n<0) {
      clicked_n = min(long$n) 
    }else {
      clicked_n = ceiling(clicked_n / 50) * 50
    }
    long = long[ long$n == clicked_n, ]
    sn_last_n= long
  })
  
  n.X = reactive ({
    clicked_n = event_data("plotly_click")$x
    if (is.null(clicked_n)) {
      number=1
    }else{
      number=round((event_data("plotly_click")$pointNumber-event_data("plotly_click")$x/50)/200,0)
    }
    number
  })
  
  track_single = reactive({
    long = sn_df_all()
    long = long[-c(1,2),]
    n.X=n.X()
    longer = long[long$replicate == paste0('X',n.X), ]
    longer = longer %>%
      select(-sn) %>%
      gather(type, value = value, sn_n, sn_sqrtn, sn_loglog,sqrtlog)
    longer
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
  # 
  ############
  # plot1
  ############
  output$plot1=renderPlotly({
    shared_longer=df_longer()
    labels <- c(sn_loglog = 'Sn√(nloglogn) -> [ -√2, √2] ', sn_n = " CLT: Sn/n -> 0",sn_sqrtn='LLN: Sn/√(n) -> N(0,1)')
    gfac=ggplot(shared_longer, aes(n, value,group = replicate)) + 
      geom_line(alpha=0.5,color='lightblue') + 
      facet_wrap(~type, scales = "free_x", ncol = 1,labeller = labeller(type=labels))+
      geom_hline(data=line, aes(yintercept=upper),linetype="dashed", size=0.5, colour="red")+
      geom_hline(data=line, aes(yintercept=lower),linetype="dashed", size=0.5, colour="red")+
      theme(axis.title=element_text(size=15),
            axis.text=element_text(size=10),
            title=element_text(size=23),
            strip.text = element_text(size = 15))+
      xlab('Sample size n')+
      xlim(1,10000)
    p=gfac+scale_y_continuous(labels=function(x) sprintf("%.2f", x),
                              breaks = sort(c(ggplot_build(gfac)$layout$panel_ranges[[1]]$y.major_source, 
                                              line$upper,line$lower)))
    p
  })
  
  
  
  output$plot1_txt=renderPrint({
    cat('The plot shows the sum Sn divided by √nloglog(n), divided by n,divided by √n of the replicates. 

Sn/√nloglog(n) would oscillate between ±√2.

Sn/n would be close to 0 as n gets larger. By the law of large number we have Sn/n → 0 almost surely.

Sn/√n is a continuous distribution and lie roughly between -3 and 3. By the central limit theorem we have Sn/√n converges in distribution to a standard normal random variable.')
  })
  
  
  
  output$track=renderPlotly({
    dat=track_single()
    g=ggplot(dat,aes(x = n, y = value, colour = factor(type,labels=c("Sn/√(nloglog(n))", "Sn/n", "Sn/√n","√loglog")))) +
      ylab(label="value") +
      xlab("Sample size n")+
      ggtitle(paste0('Single replicate',n.X()))+
      theme(axis.title=element_text(size=15),
            axis.text=element_text(size=13),
            title=element_text(size=18),
            strip.text = element_text(size = 15),
            legend.text = element_text(size = 16),
            legend.position=c(0.9,0.9))+
      theme(legend.title=element_blank())+
      scale_x_continuous(breaks=seq(0, 10000, 1000), limits=c(0,10000))+
      scale_y_continuous(breaks=seq(-3, 3, 0.5), limits=c(-3,3))+
      scale_fill_manual(values = colorRampPalette(brewer.pal(4, "Accent"))(4)) +
      #scale_color_manual(value=1:4)+
      geom_line()
    g
  })
  
  output$track_txt=renderPrint({
    cat(paste0('The plot shows the Sum Sn divided by n, √n and √{nloglog(n)} for the replicate',n.X(),'. For a single replicate, the plot of Sn/n is almost constant at 0; Sn/√n and Sn/√(nloglog(n)) has similar trend. Both of them oscillate when the sample size is small. As the sample size gets larger, they become more stable and Sn/√(nloglog(n)) would be closer to 0.'))
  })
  output$plot2=renderPlot({
    data = sn_last_n()
    un = unique(data$n)
    
    run_hist(data$sn_n,
             main = paste0('Sn/n at n = ', un),
             xlim = hist_lims(),xlab='Sn/n',
             ylab='Frequency',cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,col='lightblue',
             breaks = seq(min(data$sn_n), max(data$sn_n),
                          length.out = 10))
  })
  
  output$plot3=renderPlot({
    data = sn_last_n()
    un = unique(data$n)
    
    run_hist(data$sn_sqrtn,
             main = paste0('Sn/√n at n = ', un),
             xlim = hist_lims(),
             xlab='Sn/√n',ylab='Frequency',cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,col='lightblue',
             breaks = seq(min(data$sn_sqrtn), max(data$sn_sqrtn),
                          length.out = 10))
  })
  
  output$plot4=renderPlot({
    data = sn_last_n()
    un = unique(data$n)
    run_hist(data$sn_loglog,
             main = paste0('Sn/√{nloglog(n)} at n = ', un),
             xlim = hist_lims(),
             xlab='Sn/√{nloglog(n)}',ylab='Frequency',cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,col='lightblue',
             breaks = seq(min(data$sn_loglog), max(data$sn_loglog),
                          length.out = 10))
  })
  
  
  
  output$plot5=renderPlot({
    data = sn_last_n()
    un = unique(data$n)
    hist(data$sn,
         main = paste0('Sn at n = ', un),
         xlab='Sn',ylab='Frequency',
         cex.lab=2, cex.axis=2, cex.main=2,
         cex.sub=2,col='lightblue',
         breaks = seq(min(data$sn), max(data$sn),
                      length.out = 10))
  })
  
  output$plot4_txt=renderPrint({
    data = sn_last_n()
    un = unique(data$n)
    cat(paste0('The histograms shows Sn/√{nloglog(n)}, Sn/n, Sn/√n, Sn at n = ',un,
               ', which would be approximate normal distribution as n gets larger.'))
  })
  
  
  
  output$with=renderPlot({
    dat=within()
    ggplot(data=dat,aes(x=n, y=pct))+
      ylim(c(0.90, 1))+
      xlab('Sample size n')+ylab('Proportion')+
      ggtitle('Proportion of Sn/√{nloglog(n)} within √2 boundary')+
      theme(axis.title=element_text(size=15),
            axis.text=element_text(size=15,face="bold"),
            title=element_text(size=23))+
      geom_line()+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_hline(yintercept = 1)
  })
  
  
  output$with_txt=renderPrint({
    cat('The plot shows the proportion of Sn/√{nloglog(n)} that are bounded within ±√2. The proportion oscillate at small sample size and become more stable as sample size gets larger. The reason why the proportion does not reach 1 is that there might be some errors in the simulation.')
  })
  
  
  output$first_time=renderPlot({
    data=time()
    hist(data,
         main = 'First time Sn/√{nloglog(n)} hits ±√2 boundary',
         xlab='Sample size first crossing boundary',ylab='Frequency',
         cex.lab=2, cex.axis=2, cex.main=2,
         cex.sub=2,col='blue',
         breaks=30)
  })
  
  
  
  output$first_time_txt=renderPrint({
    cat(paste0("The plot shows the distribution of the first time Sn/√{nloglog(n)} hits ±√2 boundary. Most of the repeats hit the boundary at the first 10 samples. And all of the repeats would hit the ±√2 boundary before samples size become ",maxtime(),". In theory, the expected value of the first the statistic hits the boundary is infinity, and the same is true for the maxmium value"))})
  
  
  output$descriptions <- renderUI({
    withMathJax(includeMarkdown('info.md'))
  })
}

shinyApp(ui = ui, server = server)
