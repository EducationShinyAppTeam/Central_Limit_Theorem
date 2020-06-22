library(shiny)
library(shinydashboard)
library(plotrix)
library(ggplot2)
library(reshape2)
library(scales)
library(stats)
library(Rlab)
library(dplyr)
library(formattable)
library(discrimARTs)
library(truncnorm)
library(shinyWidgets)

shinyServer(function(session, input, output) {
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      type = NULL,
      closeOnClickOutside = TRUE,
      text = "Population graph is used to see the overall population density.
              Pick a population type and use the sliders to see how the population histogram and sample size affect the sampling distribution of the sample average."
      
    )
  })
  #Go Button
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "largeNumber")
  })
  #list all input value 
  observeEvent({
    # choose population type
    input$popDist
    
    # Left skewed
    input$leftskew
    input$leftsize
    input$leftreps
    
    # Right skewed
    input$rightskew
    input$rightsize
    input$rightreps
    
    # Symmetric
    input$inverse
    input$symsize
    input$symreps
    
    # Bimodal
    input$prop
    input$bisize
    input$bireps
    
    # Accident Rate
    input$poissonMean
    input$poreps
    input$posize
    
    
    # Astrugluas
    input$asreps
    input$assize
    
    #ipodshuffle
    input$ptype
    input$s1
    input$s2
    input$s3
    input$s4
    input$ipodreps
    input$ipodsize
  },
  {
    ###################################################################
    ## Left skewed
    ####################################################################
    
    # Population of left skewed
    output$plotleft1 <- renderPlot({
      # plot(seq(5,0,-.001), dgamma(seq(0,5,.001), input$leftskew, input$leftskew),
      #      main="Population Graph", col="red", xlab="value", ylab="density", lwd = 1)
      curve(dgamma(-x, shape = input$leftskew, beta = 1),
            main="Population Graph", col="red", xlab="value", ylab="density",lwd = 5,
            cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
            xlim = c(input$leftskew-9*sqrt(input$leftskew), 0))
    })
    
    # Matrix for first ten reps of data
    firsttenData1 <- reactive(
      matrix(-rgamma(n = 10*input$leftsize, 
                              input$leftskew, beta = 1), 
                       nrow = 10, ncol= input$leftsize))
    
    # Write the mean of first ten data into vector
    firstten1 <- reactive({
      matrix <- firsttenData1()
      matrix.means <- matrix(0, nrow = 10, ncol = input$leftsize)
      for(i in 1:10){
        for(j in 1:input$leftsize){
          matrix.means[i,j] = mean(matrix[i, 1:j])
        }
      }

      tenmeans = as.vector(matrix.means)
      return(tenmeans)
    })
    
    # Merge the first ten means with the rest of data
    data1 <- reactive({
      datameans = firstten1()
      for(i in 1:(input$leftreps-10)){
        datameans = append(datameans, mean(-rgamma(n = input$leftsize, shape = input$leftskew, 
                                                        beta = 1)))
      }
      return(datameans)
    })
    
    # One Sample Histogram
    output$plotleft2 <- renderPlot({
      matrix <- firsttenData1()
      input$new1
      hist(matrix[sample(10, 1, replace = FALSE),],
           freq = FALSE, main="Histogram of values in a single sample", col="lightblue",
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           xlab = "individual value")
    })
    
    # All Sample Histogram
    output$plotleft3<-renderPlot({
      vector <- data1()
      tmphist <- hist(vector, plot = FALSE)
      highestCount <- max(tmphist$density)
      tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
      if(input$leftreps <= 100){
        hist(vector, main="All Samples Histogram", col="lightblue", breaks= input$leftreps,
             freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        curve(dnorm(x, mean = mean(vector), sd = sd(vector)),col="blue", lwd=3, add=TRUE)
      }
      else{
        hist(vector, main="All Samples Histogram", col="lightblue", breaks= 100,
             freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        curve(dnorm(x, mean = mean(vector), sd = sd(vector)),col="blue", lwd=3, add=TRUE)
      }
      
    })
    
    
    ###################################################################
    ## Right skewed
    ####################################################################
    
    # Population of right skewed
    output$plotright1<-renderPlot({
      # plot(seq(0,5,.001),dgamma(seq(0,5,.001),input$rightskew, input$rightskew),
      #      main="Population Graph", col="red", xlab="value", ylab="density")
      curve(dgamma(x, shape = input$rightskew, beta = 1),
            main="Population Graph", col="red", xlab="value", ylab="density",lwd = 5,
            cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
            xlim = c(0, input$rightskew+9*sqrt(input$rightskew)))
    })
    
    
    # Matrix for first ten reps of data
    firsttenData2 <- reactive(
      matrix(rgamma(n = 10*input$rightsize, 
                     input$rightskew, beta = 1), 
             nrow = 10, ncol= input$rightsize))
    
    # Write the mean of first ten data into vector
    firstten2 <- reactive({
      matrix <- firsttenData2()
      matrix.means <- matrix(0, nrow = 10, ncol = input$rightsize)
      for(i in 1:10){
        for(j in 1:input$rightsize){
          matrix.means[i,j] = mean(matrix[i, 1:j])
        }
      }
      
      tenmeans = as.vector(matrix.means)
      return(tenmeans)
    })
    
    # Merge the first ten means with the rest of data
    data2 <- reactive({
      datameans = firstten2()
      for(i in 1:(input$rightreps-10)){
        datameans = append(datameans, mean(rgamma(n = input$rightsize, shape = input$rightskew, 
                                                   beta = 1)))
      }
      return(datameans)
    })
    

    # One Sample Histogram
    output$plotright2 <- renderPlot({
      matrix <- firsttenData2()
      input$new2
      hist(matrix[sample(10, 1, replace = FALSE),],
           main="Histogram of values in a single sample", col="lightblue",
           freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           xlab = "individual value")
    })
    
    # All Sample Histogram 
    output$plotright3<-renderPlot({
      vector <- data2()
      tmphist <- hist(vector, plot = FALSE)
      highestCount <- max(tmphist$density)
      tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
      if(input$rightreps <= 80){
        hist(vector, main="All Samples Histogram", col="lightblue", breaks = input$rightreps,
             freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        curve(dnorm(x, mean = mean(vector), sd = sd(vector)),col="blue", lwd=3, add=TRUE)  
      }
      else{
        hist(vector, main="All Samples Histogram", col="lightblue", breaks = 80,
             freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        curve(dnorm(x, mean = mean(vector), sd = sd(vector)),col="blue", lwd=3, add=TRUE)
      }
      
      
    })
      
    ###################################################################
    ## Symmetric skewed
    ####################################################################
    
    # Population of Symmetric skewed
    output$plotsymmetric1 <- renderPlot({
      
      x <- seq(0, 1, length = 100)
      dens <- dbeta(x, shape1 = input$inverse, shape2 = input$inverse)
      
      # Dealing with peakness = 1 special case
      if(input$inverse == 1){
        plot(x, dens, type = "l", yaxs = "i", xaxs = "i", xlim=c(-0.03,1.03),
             cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "value", ylab = "density", main = "Population Graph",
             col = "red", lwd = 5)
        segments(0,0,0,1, col = "red",lwd = 5)
        segments(1,0,1,1, col = "red",lwd = 5)
        segments(0,1,1,1, col = "red", lwd=5)
        
      }else{
        plot(x, dens, type = "l", yaxs = "i", xaxs = "i", xlim=c(-0.01,1.01),
             cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "value", ylab = "density", main = "Population Graph",
             col = "red", lwd = 5)
        lines(x, dens, col = "red")
      }
    })
    
    
    # Matrix for first ten reps of data
    firsttenData3 <- reactive(
      matrix(rbeta(10*input$symsize, 
                   shape1 = input$inverse, shape2 = input$inverse), 
             nrow = 10, ncol = input$symsize))
    
    
    
    # Write the mean of first ten data into vector
    firstten3 <- reactive({
      matrix <- firsttenData3()
      matrix.means <- matrix(0, nrow = 10, ncol = input$symsize)
      for(i in 1:10){
        for(j in 1:input$symsize){
          matrix.means[i,j] = mean(matrix[i, 1:j])
        }
      }
      
      tenmeans = as.vector(matrix.means)
      return(tenmeans)
    })
    
    # Merge the first ten means with the rest of data
    data3 <- reactive({
      datameans = firstten3()
      for(i in 1:(input$symreps-10)){
        datameans = append(datameans, mean(rbeta(n=input$symsize, 
                                                  shape1 = input$inverse, shape2 = input$inverse)))
      }
      return(datameans)
    })
    

    
    # One Sample Histogram
    output$plotsymmetric2 <- renderPlot({
      matrix <- firsttenData3()
      input$new3
      hist(matrix[sample(10, 1, replace = FALSE),],
           freq = FALSE, main="Histogram of values in a single sample", col="lightblue",
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           xlab = "individual value")
    })
    
    # All Sample Histogram
    output$plotsymmetric3 <- renderPlot({
      vector <- data3()
      tmphist <- hist(vector, plot = FALSE)
      highestCount <- max(tmphist$density)
      tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
      if(input$symreps <= 80){
        hist(vector, main="All Samples Histogram", col="lightblue", breaks = input$symreps,
             freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        curve(dnorm(x, mean = mean(vector), sd = sd(vector)),col="blue", lwd=3, add=TRUE)
      }
      else{
        hist(vector, main="All Samples Histogram", col="lightblue",breaks = 80,
             freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        curve(dnorm(x, mean = mean(vector), sd = sd(vector)),col="blue", lwd=3, add=TRUE)
      }
      
    })
    
    
    ###################################################################
    ## Bimodal
    ####################################################################
    # Population for biomodel
    output$plotbiomodel1 <- renderPlot({
      t <- 0.0001
      y <- seq(0+t, 5, t)
      z <- seq(5-t, 0, -t)
      
      x <- seq(0, 5, by=0.005)
      leftdraw <- dgamma(z, input$leftskew, beta=1)
      rightdraw <- dgamma(y, input$rightskew, beta=1)
      Z <- input$prop*leftdraw + (1-input$prop)*rightdraw

      
      plot(y, Z, type="l", yaxs="i", xaxs="i",
           xlab="value", ylab="density", main="Population Graph", 
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           col="red", lwd=5)
      lines(y, Z, type="l", col="red", xlab="",ylab="")
    })
    
    
    # Matrix for first ten reps of data
    firsttenData4 <- reactive(
      matrix(mix.synthetic.facing.gamma(N = 10*input$bisize, mix.prob = 1-input$prop,
                                               lower = 0, upper = 6, shape1=input$leftskew, scale1=1, 
                                               shape2=input$rightskew, scale2=1),
                    nrow = 10, ncol = input$bisize))
      
    
    
    
    # Write the mean of first ten data into vector
    firstten4 <- reactive({
      matrix <- firsttenData4()
      matrix.means <- matrix(0, nrow = 10, ncol = input$bisize)
      for(i in 1:10){
        for(j in 1:input$bisize){
          matrix.means[i,j] = mean(matrix[i, 1:j])
        }
      }
      
      tenmeans = as.vector(matrix.means)
      return(tenmeans)
    })
    
    # Merge the first ten means with the rest of data
    data4 <- reactive({
      datameans = firstten4()
      for(i in 1:(input$bireps-10)){
        datameans = append(datameans, mean(mix.synthetic.facing.gamma(N = input$bisize, mix.prob = 1-input$prop,
                                                                      lower = 0, upper = 6, shape1=input$leftskew, scale1=1, 
                                                                      shape2=input$rightskew, scale2=1)))
      }
      return(datameans)
    })
    
    
    # One Sample Histogram
    output$plotbiomodel2 <- renderPlot({
      matrix <- firsttenData4()
      input$new4
      hist(matrix[sample(10, 1, replace = FALSE),],
           freq = FALSE, main="Histogram of values in a single sample", col="lightblue",
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           xlab = "individual value")
    })
    
    # All Sample Histogram
    output$plotbiomodel3<-renderPlot({
      vector <- data4()
      tmphist <- hist(vector, plot = FALSE)
      highestCount <- max(tmphist$density)
      tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
      if(input$bireps <= 80){
        hist(vector, main="All Samples Histogram", col="lightblue", breaks = input$bireps,
             freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        curve(dnorm(x, mean = mean(vector), sd = sd(vector)),col="blue", lwd=3, add=TRUE)  
      }
      else{
        hist(vector, main="All Samples Histogram", col="lightblue", breaks = 80,
             freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        curve(dnorm(x, mean = mean(vector), sd = sd(vector)),col="blue", lwd=3, add=TRUE) 
      }
      
    })
    
  
    
    ###################################################################
    ## Accident Rate
    ####################################################################
    
    # Population of poisson
    output$poissonpop <- renderPlot({
      N <- 10000
      x <- rpois(N, input$poissonmean)
      hist(x, 
           xlim=c(min(x),max(x)), probability = T, nclass = max(x)-min(x)+1, 
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           col='lightblue', xlab = "# of accidents", ylab = "probability",
           main='Population Graph')
    })
    
    
    # Matrix for first ten reps of data
    firsttenData5 <- reactive(
      matrix(rpois(10*input$posize,
                   input$poissonmean), 
             nrow = 10, ncol = input$posize))
    
    
    # Write the mean of first ten data into vector
    firstten5 <- reactive({
      matrix <- firsttenData5()
      matrix.means <- matrix(0, nrow = 10, ncol = input$posize)
      for(i in 1:10){
        for(j in 1:input$posize){
          matrix.means[i,j] = mean(matrix[i, 1:j])
        }
      }
      
      tenmeans = as.vector(matrix.means)
      return(tenmeans)
    })
    
    # Merge the first ten means with the rest of data
    data5 <- reactive({
      datameans = firstten5()
      for(i in 1:(input$poreps-10)){
        datameans = append(datameans, mean(rpois(input$posize,
                                                 input$poissonmean)))
      }
      return(datameans)
    })
    
    
    
    # One Sample Histogram
    output$plotpoisson1 <- renderPlot({
      matrix <- firsttenData5()
      input$new5
      hist(matrix[sample(10, 1, replace = FALSE),],
           freq = FALSE, main="Histogram of values in a single sample", col="lightblue",
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           xlab = "individual value")
    })
    
    # All Sample Histogram
    output$plotpoisson2 <-renderPlot({
      vector <- data5()
      tmphist <- hist(vector, plot = FALSE)
      highestCount <- max(tmphist$density)
      tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
      if(input$poreps <= 80){
        hist(vector, main="All Samples Histogram", col="lightblue", breaks = input$poreps,
             freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        curve(dnorm(x, mean = mean(vector), sd = sd(vector)),col="blue", lwd=3, add=TRUE)  
      }
      else{
        hist(vector, main="All Samples Histogram", col="lightblue", breaks = 80,
             freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        curve(dnorm(x, mean = mean(vector), sd = sd(vector)),col="blue", lwd=3, add=TRUE)
      }
      
    })
    
    
    ###################################################################
    ## Astrugluas
    ####################################################################
    
    # die results
    die<-reactive({
      die<-c(rep(1,1),rep(3,4),rep(4,4),rep(6,1))
    })
    
    # Population of Astragalus
    output$pop <- renderPlot({
      a = min(die())
      b = max(die())
      foo <- hist(x = die()+0.001,
                  breaks=b-a,
                  probability = T,
                  xaxt="n",
                  cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
                  col='lightblue', xlab = "# on roll of Astragalus",
                  ylab = "probability",
                  main="Population Graph")
      axis(side=1, at=foo$mids,labels=seq(a,b))
      
    })
    
   
    # Matrix for first ten reps of data
    firsttenData6 <- reactive(
      matrix(sample(die(), 10*input$assize,
                    replace = TRUE), 
             nrow = 10, ncol = input$assize))
    
    
    # Write the mean of first ten data into vector
    firstten6 <- reactive({
      matrix <- firsttenData6()
      matrix.means <- matrix(0, nrow = 10, ncol = input$assize)
      for(i in 1:10){
        for(j in 1:input$assize){
          matrix.means[i,j] = mean(matrix[i, 1:j])
        }
      }
      
      tenmeans = as.vector(matrix.means)
      return(tenmeans)
    })
    
    # Merge the first ten means with the rest of data
    data6 <- reactive({
      datameans = firstten6()
      for(i in 1:(input$asreps-10)){
        datameans = append(datameans, mean(sample(die(), input$assize,
                                                  replace = TRUE)))
      }
      return(datameans)
    })
    
    
    
    
    # One Sample Histogram
    output$line2 <- renderPlot({
      matrix <- firsttenData6()
      input$new6
      hist(matrix[sample(10, 1, replace = FALSE),],
           freq = FALSE, main="Histogram of values in a single sample", col="lightblue",
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           xlab = "individual value")
    })
    
    # All Sample Histogram
    output$line1 <-renderPlot({
      vector <- data6()
      tmphist <- hist(vector, plot = FALSE)
      highestCount <- max(tmphist$density)
      tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
      if(input$asreps <= 80){
        hist(vector, main="All Samples Histogram", col="lightblue", breaks = input$asreps,
             freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        curve(dnorm(x,  mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE)  
      }
      else{
        hist(vector, main="All Samples Histogram", col="lightblue", breaks = 80,
             freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        curve(dnorm(x,  mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE)
      }
      
    })
    
    
    ###################################################################
    ## IPOD SHUFFLE
    ####################################################################
    
    #Population and Sum for IPOD
    
    # set up songs from four types  
    songs <- reactive({
      songs <- c(rep(input$s1), 
                 rep(input$s2),
                 rep(input$s3),
                 rep(input$s4)
      )
    })
    
    # average songs in the IPOD
    avg_songs<-reactive({
      mean(songs())
    })
    
    
    # Jazz percent
    output$Jazz_percent<-renderPrint({
      cat(round(input$s1/sum(songs()),digits = 2))
    })
    
    # Rock percent 
    output$Rock_percent<-renderPrint({
      cat(round(input$s2/sum(songs()),digits = 2))
    })
    
    # Country percent 
    output$Country_percent<-renderPrint({
      cat(round(input$s3/sum(songs()),digits = 2))
    })
    
    # Hip-pop percent
    output$Hiphop_percent<-renderPrint({
      cat(round(input$s4/sum(songs()),digits = 2))
    })
    
    ############################################
    # Plot with bar plot with 4 categories songs 
    
    # Jazz population plot
    output$Plot1 <- renderPlot({
      pjazz <- input$s1/sum(songs())
      count <- c(pjazz*input$ipodsize, (1-pjazz)*input$ipodsize)
      barplot(count, main="Population Graph", xlab="Jazz vs Other music"
              ,ylab="probability",col='lightblue',
              cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              names.arg = c("Jazz","Other music"))
      # n <- input$ipodsize
      # x <- seq(0, n, by = 1)
      # plot (x, dbinom(x, n, pjazz, log = FALSE), type = "l", xlab = "values",ylab = "density",
      #       main = "Population Graph",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #       col="red", lwd=5)
    })
    
    # Rock population plot
    output$Plot2 <- renderPlot({
      prock <- input$s2/sum(songs())
      count <- c(prock*input$ipodsize, (1-prock)*input$ipodsize)
      barplot(count, main="Population Graph", xlab="Rock vs Other music"
              ,ylab="probability",col='lightblue',
              cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              names.arg = c("Rock","Other music"))
      # n <- input$ipodsize
      # x <- seq(0, n, by = 1)
      # plot (x, dbinom(x, n, prock, log = FALSE), type = "l", xlab = "values",ylab = "density",
      #       main = "Population Graph",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #       col="red", lwd=5)
    })
    
    # Country population plot
    output$Plot3 <- renderPlot({
      pcountry <- input$s3/sum(songs())
      count <- c(pcountry*input$ipodsize, (1-pcountry)*input$ipodsize)
      barplot(count, main="Population Graph", xlab="Country vs Other music"
              ,ylab="probability",col='lightblue',
              cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              names.arg = c("Country","Other music"))
      # n <- input$ipodsize
      # x <- seq(0, n, by = 1)
      # plot (x, dbinom(x, n, pcountry, log = FALSE), type = "l", xlab = "values",ylab = "density",
      #       main = "Population Graph",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #       col="red", lwd=5)
    })
    
    #Hip-pop population plot
    output$Plot4 <- renderPlot({
      phiphop <- input$s4/sum(songs())
      count <- c(phiphop*input$ipodsize, (1-phiphop)*input$ipodsize)
      barplot(count, main="Population Graph", xlab="Hip-hop vs Other music"
              ,ylab="probability",col='lightblue',
              cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              names.arg = c("Hip-hop","Other music"))
      # n <- input$ipodsize
      # x <- seq(0, n, by = 1)
      # plot (x, dbinom(x, n, phiphop, log = FALSE), type = "l", xlab = "values",ylab = "density",
      #       main = "Population Graph",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #       col="red", lwd=5)
    })
    
    ############################################
    # Average Plot with 4 categories songs
    
    
    # Matrix of Songs from 4 types
    
    ### Jazz
    # Matrix for first ten reps of data
    firsttenDataJazz <- reactive(
      matrix(rbinom(10*input$ipodsize, size= 1, prob=input$s1/sum(songs())),
             nrow = 10, ncol = input$ipodsize))
    
    
    # Write the mean of first ten data into vector
    firsttenJazz <- reactive({
      matrix <- firsttenDataJazz()
      matrix.means <- matrix(0, nrow = 10, ncol = input$ipodsize) #matrix with all zeroes
      for(i in 1:10){
        for(j in 1:input$ipodsize){
          matrix.means[i,j] = mean(matrix[i, 1:j])
        }
      }
      
      tenmeans = as.vector(matrix.means)
      return(tenmeans)
    })
    
    # Merge the first ten means with the rest of data
    Jazzdata <- reactive({
      datameans = firsttenJazz()
      for(i in 1:(input$ipodreps-10)){
        datameans = append(datameans, mean(rbinom(input$ipodsize, size= 1, prob=input$s1/sum(songs()))))
      }
      return(datameans)
    })
    
    ### Rock
    # Matrix for first ten reps of data
    firsttenDataRock <- reactive(
      matrix(rbinom(10*input$ipodsize, size= 1, prob=input$s2/sum(songs())),
             nrow = 10, ncol = input$ipodsize))
    
    
    # Write the mean of first ten data into vector
    firsttenRock <- reactive({
      matrix <- firsttenDataRock()
      matrix.means <- matrix(0, nrow = 10, ncol = input$ipodsize)
      for(i in 1:10){
        for(j in 1:input$ipodsize){
          matrix.means[i,j] = mean(matrix[i, 1:j])
        }
      }
      
      tenmeans = as.vector(matrix.means)
      return(tenmeans)
    })
    
    # Merge the first ten means with the rest of data
    Rockdata <- reactive({
      datameans = firsttenRock()
      for(i in 1:(input$ipodreps-10)){
        datameans = append(datameans, mean(rbinom(input$ipodsize, size= 1, prob=input$s2/sum(songs()))))
      }
      return(datameans)
    })
    
    
    ### Country
    # Matrix for first ten reps of data
    firsttenDataCountry <- reactive(
      matrix(rbinom(10*input$ipodsize, size= 1, prob=input$s3/sum(songs())),
             nrow = 10, ncol = input$ipodsize))
    
    
    # Write the mean of first ten data into vector
    firsttenCountry <- reactive({
      matrix <- firsttenDataCountry()
      matrix.means <- matrix(0, nrow = 10, ncol = input$ipodsize)
      for(i in 1:10){
        for(j in 1:input$ipodsize){
          matrix.means[i,j] = mean(matrix[i, 1:j])
        }
      }
      
      tenmeans = as.vector(matrix.means)
      return(tenmeans)
    })
    
    # Merge the first ten means with the rest of data
    Countrydata <- reactive({
      datameans = firsttenCountry()
      for(i in 1:(input$ipodreps-10)){
        datameans = append(datameans, mean(rbinom(input$ipodsize, size= 1, prob=input$s3/sum(songs()))))
      }
      return(datameans)
    })
    
    ### Hiphop
    # Matrix for first ten reps of data
    firsttenDataHiphop <- reactive(
      matrix(rbinom(10*input$ipodsize, size= 1, prob=input$s4/sum(songs())),
             nrow = 10, ncol = input$ipodsize))
    
    
    # Write the mean of first ten data into vector
    firsttenHiphop <- reactive({
      matrix <- firsttenDataHiphop()
      matrix.means <- matrix(0, nrow = 10, ncol = input$ipodsize)
      for(i in 1:10){
        for(j in 1:input$ipodsize){
          matrix.means[i,j] = mean(matrix[i, 1:j])
        }
      }
      
      tenmeans = as.vector(matrix.means)
      return(tenmeans)
    })
    
    # Merge the first ten means with the rest of data
    Hiphopdata <- reactive({
      datameans = firsttenHiphop()
      for(i in 1:(input$ipodreps-10)){
        datameans = append(datameans, mean(rbinom(input$ipodsize, size= 1, prob=input$s4/sum(songs()))))
      }
      return(datameans)
    })
    
    
    # JAZZ
    # One Sample Barplot
    output$Plot01 <- renderPlot({
        matrix <- firsttenDataJazz()
        input$new7
        count <- c(mean(matrix[sample(10, 1, replace = FALSE),]),1-mean(matrix[sample(10, 1, replace = FALSE),]))
        barplot(count, main="Histogram of values in a single sample",
                xlab="Jazz vs Other music", ylab="probability",col='lightblue',
                cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
                names.arg = c("Jazz","Other music"))
    })
  
    
    # Rock 
    # One Sample Barplot
    output$Plot02<- renderPlot({
      matrix <- firsttenDataRock()
      input$new7
      count <- c(1-mean(matrix[sample(10, 1, replace = FALSE),]),mean(matrix[sample(10, 1, replace = FALSE),]))
      barplot(count, main="Histogram of values in a single sample",
              xlab="Rock vs Other music", ylab="probability",col='lightblue',
              cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              names.arg = c("Rock","Other music"))
    })
    
    # Country Average Plot
    output$Plot03<- renderPlot({
      matrix <- firsttenDataCountry()
      input$new7
      count <- c(1-mean(matrix[sample(10, 1, replace = FALSE),]),mean(matrix[sample(10, 1, replace = FALSE),]))
      barplot(count, main="Histogram of values in a single sample",
              xlab="Country vs Other music", ylab="probability",col='lightblue',
              cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              names.arg = c("Country","Other music"))
    })
    
    # Hip-hop Average Plot
    output$Plot04<- renderPlot({
      matrix <-  firsttenDataHiphop()
      input$new7
      count <- c(1-mean(matrix[sample(10, 1, replace = FALSE),]),mean(matrix[sample(10, 1, replace = FALSE),]))
      barplot(count, main="Histogram of values in a single sample",
              xlab="Hip-hop vs Other music", ylab="probability",col='lightblue',
              cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              names.arg = c("Hiphop","Other music"))
    })
    
    
    ############################################
    # Sum Plot with 4 categories songs 
    
    # JAZZ
    output$Plot10<- renderPlot({
      if(input$s1 == 0){
        vector <-Jazzdata()
        if(input$ipodreps <= 80){
          hist(vector, main="All Samples Histogram", col="lightblue", breaks = input$ipodreps,
               freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               xlab = "sample average")
          curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
        }
        else{
          hist(vector, main="All Samples Histogram", col="lightblue", breaks = 80,
               freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               xlab = "sample average")
          curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
        }
        
      }
      else{
        # vector <-Jazzdata()
        # tmphist <- hist(vector, plot = FALSE)
        # highestCount <- max(tmphist$density)
        # tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
        n <- input$ipodsize
        # x <- seq(0, n, by = 1)
        vector <- rbinom(input$ipodreps*n, size= n, prob=input$s1/sum(songs()))
        vector_mean <- c()
        for(i in 1:input$ipodreps*n){
          vector_mean <- append(vector_mean, vector[i]/n)
        }
        hist(vector_mean,main="All Samples Histogram", col="lightblue",#breaks = input$ipodsize,
             #cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             prob = TRUE,xlab = "sample average")
        curve(dnorm(x, mean(vector_mean), sd(vector_mean)), col="blue", lwd=3,add = TRUE)
        # hist(vector, main="All Samples Histogram", col="lightblue", breaks = 100,
        #      freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
        #      xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        # curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE)
        
        
      }
    })
    
    
    
    # Rock SUM PLOT 
    output$Plot20<- renderPlot({
      if(input$s2 == 0){
        vector <-Rockdata()
        if(input$ipodreps <= 80){
          hist(vector, main="All Samples Histogram", col="lightblue", breaks = input$ipodreps,
               freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               xlab = "sample average")
          curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
        }
        else{
          hist(vector, main="All Samples Histogram", col="lightblue", breaks = 80,
               freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               xlab = "sample average")
          curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
        }
        
      }
      # else{
      #   vector <-Rockdata()
      #   tmphist <- hist(vector, plot = FALSE)
      #   highestCount <- max(tmphist$density)
      #   tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
      #   if(input$ipodreps <= 100){
      #     hist(vector, main="All Samples Histogram", col="lightblue", breaks = input$ipodreps,
      #          freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #          xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
      #     curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
      #   }
      #   else{
      #     hist(vector, main="All Samples Histogram", col="lightblue", breaks = 100,
      #          freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #          xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
      #     curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
      #   }
      #   
      # }
      else{
        # vector <-Jazzdata()
        # tmphist <- hist(vector, plot = FALSE)
        # highestCount <- max(tmphist$density)
        # tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
        n <- input$ipodsize
        # x <- seq(0, n, by = 1)
        vector <- rbinom(input$ipodreps*n, size= n, prob=input$s2/sum(songs()))
        vector_mean <- c()
        for(i in 1:input$ipodreps*n){
          vector_mean <- append(vector_mean, vector[i]/n)
        }
        hist(vector_mean,main="All Samples Histogram", col="lightblue",# breaks = input$ipodreps,
             #cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             prob = TRUE,xlab = "sample average")
        curve(dnorm(x, mean(vector_mean), sd(vector_mean)), col="blue", lwd=3,add = TRUE)
        # hist(vector, main="All Samples Histogram", col="lightblue", breaks = 100,
        #      freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
        #      xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        # curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE)
      }
    })
    
    # Country SUM PLOT 
    output$Plot30<- renderPlot({
      if(input$s3==0){
        vector <- Countrydata()
        if(input$ipodreps <= 80){
          hist(vector, main="All Samples Histogram", col="lightblue", breaks = input$ipodreps,
               freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               xlab = "sample average")
          curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
        }
        else{
          hist(vector, main="All Samples Histogram", col="lightblue", breaks = 80,
               freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               xlab = "sample average")
          curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
        }
        
      }
      # else{
      #   vector <- Countrydata()
      #   tmphist <- hist(vector, plot = FALSE)
      #   highestCount <- max(tmphist$density)
      #   tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
      #   if(input$ipodreps <= 100){
      #     hist(vector, main="All Samples Histogram", col="lightblue", breaks = input$ipodreps,
      #          freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #          xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
      #     curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
      #   }
      #   else{
      #     hist(vector, main="All Samples Histogram", col="lightblue", breaks = 100,
      #          freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #          xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
      #     curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
      #   }
      # }
      else{
        # vector <-Jazzdata()
        # tmphist <- hist(vector, plot = FALSE)
        # highestCount <- max(tmphist$density)
        # tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
        n <- input$ipodsize
        # x <- seq(0, n, by = 1)
        vector <- rbinom(input$ipodreps*n, size= n, prob=input$s3/sum(songs()))
        vector_mean <- c()
        for(i in 1:input$ipodreps*n){
          vector_mean <- append(vector_mean, vector[i]/n)
        }
        hist(vector_mean,main="All Samples Histogram", col="lightblue",# breaks = input$ipodreps,
             #cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             prob = TRUE,xlab = "sample average")
        curve(dnorm(x, mean(vector_mean), sd(vector_mean)), col="blue", lwd=3,add = TRUE)
        # hist(vector, main="All Samples Histogram", col="lightblue", breaks = 100,
        #      freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
        #      xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        # curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE)
      }
    })
    
    # Hip_Hop SUM PLOT 
    output$Plot40<- renderPlot({
      if(input$s4==0){
        vector <- Hiphopdata()
        if(input$ipodreps <= 80){
          hist(vector, main="All Samples Histogram", col="lightblue", breaks = input$ipodreps,
               freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               xlab = "sample average")
          curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
        }
        else{
          hist(vector, main="All Samples Histogram", col="lightblue", breaks = 80,
               freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               xlab = "sample average")
          curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
        }
      }
      # else{
      #   vector <- Hiphopdata()
      #   tmphist <- hist(vector, plot = FALSE)
      #   highestCount <- max(tmphist$density)
      #   tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
      #   if(input$ipodreps <= 100){
      #     hist(vector, main="All Samples Histogram", col="lightblue", breaks = input$ipodreps,
      #          freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #          xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
      #     curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE)  
      #   }
      #   else{
      #     hist(vector, main="All Samples Histogram", col="lightblue", breaks = 100,
      #          freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #          xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
      #     curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE) 
      #   }
      # }
      else{
        # vector <-Jazzdata()
        # tmphist <- hist(vector, plot = FALSE)
        # highestCount <- max(tmphist$density)
        # tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))

        n <- input$ipodsize
        # x <- seq(0, n, by = 1)
        vector <- rbinom(input$ipodreps*n, size= n, prob=input$s4/sum(songs()))
        vector_mean <- c()
        for(i in 1:input$ipodreps*n){
          vector_mean <- append(vector_mean, vector[i]/n)
        }
        hist(vector_mean,main="All Samples Histogram", col="lightblue",# breaks = input$ipodreps,
             #cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             prob = TRUE,xlab = "sample average")
        curve(dnorm(x, mean(vector_mean), sd(vector_mean)), col="blue", lwd=3,add = TRUE)
        # hist(vector, main="All Samples Histogram", col="lightblue", breaks = 100,
        #      freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
        #      xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
        # curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE)

      }
    })
    
  })
})
