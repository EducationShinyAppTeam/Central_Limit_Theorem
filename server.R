library(shiny)
library(shinydashboard)
library(ggplot2)
library(stats)
library(Rlab)
library(dplyr)
library(shinyWidgets)

shinyServer(function(session, input, output) {
  # i button in corner
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions",
      type = NULL,
      closeOnClickOutside = TRUE,
      text = "Population graph is used to see the overall population density.
              Pick a population type and use the sliders to see how the
              population histogram and sample size affect the sampling
              distribution of the Sample mean."
    )
  })

  #Go Button
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "largeNumber")
  })

  # Function to create Histogram of a Single Samples
  # Inputs: matrix that gives values to plot, number of bins to use
  # Output: ggplot of Histogram of a Single Sample
  singleSample <- function(matrix, reps, bw = 1){
    ggplot(data=data.frame(gg = matrix[sample(min(50, reps), 1, replace = FALSE),]),aes(x=gg)) +
      geom_histogram(aes(y =..density..),
                     binwidth = bw,
                     fill = "lightblue",
                     col = "black",
      ) + xlab("Individual value") +
      ylab("Density") +
      ggtitle("Histogram of a Single Sample") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black")
      )
  }

  # Function to create all sample histograms
  # Inputs: vector (or similar object) that gives values to plot, number of bins to use
  # Output: ggplot of histogram with overlayed density
  allSample <- function(vector, reps){
    ggplot(data=data.frame(gg = vector),aes(x=gg)) +
      geom_histogram(aes(y =..density..),
                     bins = reps,
                     fill = "lightblue",
                     col = "black") +
      xlab("Sample mean") +
      ylab("Density") +
      ggtitle("All Samples Histogram") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black")) +
      stat_function(fun = dnorm,
                    args = list(mean = mean(vector), sd = sd(vector)),
                    color = "blue",
                    lwd = 1)
  }


  # Function to create density plots for each group
  # Inputs: Dataframe consisting of columns x and y to define axes, limits for x axis in form c(lower, upper), optional path for symmetric case
  # Output: ggplot of density
  makeDensityPlot <- function(data, xlims, path=0){
    plot <- ggplot2::ggplot(aes(x=x, y=y), data= data) +
      geom_path(color="#0072B2", size=1.5) +
      xlim(xlims) +
      xlab("Value") +
      ylab("Density") +
      ggtitle("Population Graph")+
      theme(axis.text = element_text(size=18),
            plot.title = element_text(size=18, face="bold"),
            axis.title = element_text(size=18),
            panel.background = element_rect(fill = "white", color="black")
      )
    # For case in symmetric where path is 1 causing "box" shape
    if(path ==1){
      plot <- plot+
        geom_segment(aes(x=0, y=0, xend=0, yend=1), color="#0072B2", size=1.5)+
        geom_segment(aes(x=1, y=0, xend=1, yend=1), color="#0072B2", size=1.5)
    }
    plot
  }

  #Make the make bar plot function
  makeBarPlot <-
    function(xlab, ggtitle2, data, levels = as.character(data$x), color = "#0072B2", color2 = "#0072B2") {
      plot <-
        ggplot(aes(x = factor(x, levels = levels), y = y), data = data) +
        geom_bar(stat = "identity",
                 fill = color,
                 col = color2) +
        ylim(c(0, max(data$y) + .1 * max(data$y))) +
        xlab(xlab) +
        ylab("Probability") +
        ggtitle(ggtitle2) +
        theme(
          axis.text = element_text(size = 18),
          plot.title = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 18),
          panel.background = element_rect(fill = "white", color = "black")
        ) +
        scale_x_discrete(drop = FALSE)

      plot
    }


  ###################################################################
  ## Left skewed
  ####################################################################
  leftSkew <- reactive({11-10*input$leftskew})

  # Population of left skewed
  output$plotleft1 <- renderPlot({
    # Define parameters for density plot
    x <- seq((leftSkew()) - 9 * sqrt((leftSkew())),0, length = 100)
    y <- dgamma(-x, shape = (leftSkew()), beta = 1)
    data <- data.frame(x=x, y=y)

    # Make Density Plot
    makeDensityPlot(data=data, xlims = c((leftSkew()) - 9 * sqrt((leftSkew())), 0))
  })

  # Matrix for first 50 reps of data
  firstfifData1 <- reactive(matrix(
    -rgamma(
      n = 50 * input$leftsize,
      11 - 10 * input$leftskew,
      beta = 1
    ),
    nrow = 50,
    ncol = input$leftsize
  ))

  # Write the mean of first 50 data into vector
  firstfif1 <- reactive({
    matrix <- firstfifData1()
    matrix.means <- matrix(0, nrow = 50, ncol = input$leftsize)
    for (i in 1:50) {
      for (j in 1:input$leftsize) {
        matrix.means[i, j] = mean(matrix[i, 1:j])
      }
    }
    fifmeans = as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first 50 means with the rest of data
  data1 <- reactive({
    datameans = firstfif1()
    for (i in 1:(input$leftreps - 50)) {
      datameans = append(datameans, mean(
        -rgamma(
          n = input$leftsize,
          shape =  11 - 10 * input$leftskew,
          beta = 1
        )
      ))
    }
    return(datameans)
  })

  # One Sample Histogram
  output$plotleft2 <- renderPlot({
    matrix <- firstfifData1()
    input$new
    singleSample(matrix, input$leftreps)
  })

  # All Sample Histogram
  output$plotleft3 <- renderPlot({
    vector <- data1()
    allSample(vector, min(100, input$leftreps))
  })


  ###################################################################
  ## Right skewed
  ####################################################################
  rightSkew <- reactive({11-10*input$rightskew})
  # Population of right skewed
  output$plotright1 <- renderPlot({
    # Define parameters for density plot
    x <- seq(0, (rightSkew()) + 9 * sqrt(rightSkew()), length = 100)
    y <- dgamma(x, shape = (rightSkew()), beta = 1)
    data <- data.frame(x=x, y=y)

    # Make the density plot
    makeDensityPlot(data=data, xlims = c(0, (rightSkew()) + 9 * sqrt((rightSkew()))))
  })

  # Matrix for first 50 reps of data
  firstfifData2 <- reactive(matrix(
    rgamma(
      n = 50 * input$rightsize,
      11 - 10 * input$rightskew,
      beta = 1
    ),
    nrow = 50,
    ncol = input$rightsize
  ))

  # Write the mean of first 50 data into vector
  firstfif2 <- reactive({
    matrix <- firstfifData2()
    matrix.means <- matrix(0, nrow = 50, ncol = input$rightsize)
    for (i in 1:50) {
      for (j in 1:input$rightsize) {
        matrix.means[i, j] = mean(matrix[i, 1:j])
      }
    }

    fifmeans = as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first 50 means with the rest of data
  data2 <- reactive({
    datameans = firstfif2()
    for (i in 1:(input$rightreps - 50)) {
      datameans = append(datameans, mean(
        rgamma(
          n = input$rightsize,
          shape = 11 - 10 * input$rightskew,
          beta = 1
        )
      ))
    }
    return(datameans)
  })


  # One Sample Histogram
  output$plotright2 <- renderPlot({
    matrix <- firstfifData2()
    input$new
    singleSample(matrix, input$rightreps)
  })

  # All Sample Histogram
  output$plotright3 <- renderPlot({
    vector <- data2()
    allSample(vector, min(80, input$rightreps))
  })

  ###################################################################
  ## Symmetric skewed
  ####################################################################
  inverse <- reactive({
    round(14.6 * input$inverse ^ 3 - 5.7 * input$inverse ^ 2 + input$inverse + .1,
          3)
  })

  # Population of Symmetric skewed
  output$plotsymmetric1 <- renderCachedPlot({
    x <- seq(0, 1, length = 100)
    dens <-
      dbeta(x,
            shape1 = inverse(),
            shape2 = inverse())
    data <- data.frame(x = x, y = dens)

    # Make density plot separated by case where the peakedness is exactly 1 (causes a "box" shape)
    makeDensityPlot(data = data,
                    xlims = c(-0.03, 1.03),
                    path = inverse())
  },
  cacheKeyExpr = {
    list(input$symsize, input$inverse)
  })


  # Matrix for first 50 reps of data
  firstfifData3 <- reactive(matrix(
    rbeta(
      50 * input$symsize,
      shape1 = inverse(),
      shape2 = inverse()
    ),
    nrow = 50,
    ncol = input$symsize
  ))



  # Write the mean of first 50 data into vector
  firstfif3 <- reactive({
    matrix <- firstfifData3()
    matrix.means <- matrix(0, nrow = 50, ncol = input$symsize)
    for (i in 1:50) {
      for (j in 1:input$symsize) {
        matrix.means[i, j] = mean(matrix[i, 1:j])
      }
    }

    fifmeans = as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first 50 means with the rest of data
  data3 <- reactive({
    datameans = firstfif3()
    for (i in 1:(input$symreps - 50)) {
      datameans = append(datameans, mean(
        rbeta(
          n = input$symsize,
          shape1 = inverse(),
          shape2 = inverse()
        )
      ))
    }
    return(datameans)
  })



  # One Sample Histogram
  output$plotsymmetric2 <- renderPlot({
    matrix <- firstfifData3()
    input$new
    singleSample(matrix, input$symreps, bw = .1)
  })

  # All Sample Histogram
  output$plotsymmetric3 <- renderPlot({
    vector <- data3()
    allSample(vector, min(80, input$symreps))
  })


  ###################################################################
  ## Bimodal
  ####################################################################

  # Change: Added reactive to go from percent to proportion
  prop <- reactive({
    input$prop / 100
  })

  # Population for bimodal
  output$plotbimodal1 <- renderPlot({
    t <- 1 / (input$bisize * input$bireps)
    y <- seq(0, 1, t)
    z <- seq(1, 0, -t)
    leftdraw <- dbeta(z, 4, 14) * .2
    rightdraw <- dbeta(y, 4, 14) * .2
    data <-
      data.frame(x = seq(0, 5, t * 5),
                 y = prop() * leftdraw + (1 - prop()) * rightdraw)

    # Make the density plot
    makeDensityPlot(data = data, xlims = c(0, 5))
  })


  # Matrix for first fif reps of data
  firstfifData4 <- reactive({
    rand <- sample(
      x = c(0, 1),
      size = input$bisize * 50,
      replace = TRUE,
      prob = c(1 - prop(), prop()) # Change: switch input$prop for prop() and swapped order (because was wrong in my code)
    )

    # Number of elements sampled from the right distribution (represented by 1)
    rights <- sum(rand)

    # Number of elements sampled from left distribution (represented by 0)
    lefts <- input$bisize * 10 - rights

    leftbetas <- rbeta(lefts, 4, 14) * 5 # Samples left distribution
    rightbetas <- 5 - rbeta(rights, 4, 14) * 5 # Samples right distribution

    # Loop to assign values from gamma distributions to rand
    rightIndex <- 1
    leftIndex <- 1
    for (x in 1:length(rand)) {
      if (rand[x] == 0) {
        rand[x] <- leftbetas[leftIndex]
        leftIndex <- leftIndex + 1
      }
      else{
        rand[x] <- rightbetas[rightIndex]
        rightIndex <- rightIndex + 1
      }
    }
    matrix(rand, nrow = 50)
  })


  # Matrix for first fif reps of data
  firstfifData4 <- reactive({
    rand <- sample(
      x = c(0, 1),
      size = input$bisize * 50,
      replace = TRUE,
      prob = c(1 - prop(), prop())
    )
    # Number of elements sampled from right distribution (represented by 1)
    rights <- sum(rand)

    # Number of elements sampled from left distribution (represented by 0)
    lefts <- input$bisize * 50 - rights

    leftbetas <- rbeta(lefts, 4, 14) * 5 # Samples left distribution
    rightbetas <- 5 - rbeta(rights, 4, 14) * 5 # Samples right distribution

    # Loop to assign values from gamma distributions to rand
    rightIndex <- 1
    leftIndex <- 1
    for (x in 1:length(rand)) {
      if (rand[x] == 0) {
        rand[x] <- leftbetas[leftIndex]
        leftIndex <- leftIndex + 1
      }
      else{
        rand[x] <- rightbetas[rightIndex]
        rightIndex <- rightIndex + 1
      }
    }
    matrix(rand, nrow = 50)
  })


  # Write the mean of first fif data into vector
  firstfif4 <- reactive({
    matrix <- firstfifData4()
    matrix.means <- matrix(0, nrow = 50, ncol = input$bisize)
    for (i in 1:50) {
      for (j in 1:input$bisize) {
        matrix.means[i, j] = mean(matrix[i, 1:j])
      }
    }

    fifmeans = as.vector(matrix.means)
    return(fifmeans)
  })


  # Merge the first fif means with the rest of data
  data4 <- reactive({
    datameans = firstfif4()
    for (i in 1:(input$bireps - 50)) {
      # Random vector of 0s and 1s to determine which distribution each element samples from
      rand <- sample(
        x = c(0, 1),
        size = input$bisize,
        replace = TRUE,
        prob = c(1 - prop(), prop())
      )

      # Number of elements sampled from the right distribution (represented by 1)
      rights <- sum(rand)

      # Number of elements sampled from left distribution (represented by 0)
      lefts <- input$bisize - rights

      leftbetas <- rbeta(lefts, 4, 14) * 5 # Samples left distribution
      rightbetas <- 5 - rbeta(rights, 4, 14) * 5 # Samples right distribution

      # Loop to assign values from gamma distributions to rand
      rightIndex <- 1
      leftIndex <- 1
      for (x in 1:length(rand)) {
        if (rand[x] == 0) {
          rand[x] <- leftbetas[leftIndex]
          leftIndex <- leftIndex + 1
        }
        else{
          rand[x] <- rightbetas[rightIndex]
          rightIndex <- rightIndex + 1
        }
      }

      datameans <- append(datameans, mean(rand))
    }
    return(datameans)
  })


  # One Sample Histogram
  output$plotbimodal2 <- renderPlot({
    matrix <- firstfifData4()
    input$new
    singleSample(matrix, input$bireps, bw = .5)
  })

  # All Sample Histogram
  output$plotbimodal3 <- renderPlot({
    vector <- data4()
    allSample(vector, min(80, input$bireps))
  })


  ###################################################################
  ## Accident Rate
  ####################################################################

  output$poissonpop <- renderCachedPlot({
    data <- data.frame(x = 0:ceiling(2 * input$poissonmean + 5)) # More x's than necessary
    data$y <- (input$poissonmean ^ data$x) * exp(-input$poissonmean) / factorial(data$x) # Get y vals for x's
    data <- rbind(data[1:2,], filter(data[-c(1, 2), ], y > .0005)) # Filter based on probability

    makeBarPlot(xlab = "Number of accidents", ggtitle="Population Graph",data = data)
  },
  cacheKeyExpr = {
    list(input$poissonmean)
  })



  # Matrix for first 50 reps of data
  firstfifData5 <- reactive(matrix(
    rpois(50 * input$posize,
          input$poissonmean),
    nrow = 50,
    ncol = input$posize
  ))


  # Write the mean of first 50 data into vector
  firstfif5 <- reactive({
    matrix <- firstfifData5()
    matrix.means <- matrix(0, nrow = 50, ncol = input$posize)
    for (i in 1:50) {
      for (j in 1:input$posize) {
        matrix.means[i, j] = mean(matrix[i, 1:j])
      }
    }

    fifmeans = as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first 50 means with the rest of data
  data5 <- reactive({
    datameans = firstfif5()
    for (i in 1:(input$poreps - 50)) {
      datameans = append(datameans, mean(rpois(input$posize,
                                               input$poissonmean)))
    }
    return(datameans)
  })


  # One Sample Histogram
  output$plotpoisson1 <- renderPlot({
    matrix <- firstfifData5()
    input$new
    singleSample(matrix, input$poreps)
  })

  # All Sample Histogram
  output$plotpoisson2 <- renderPlot({
    vector <- data5()
    allSample(vector, min(50, input$poreps))
  })


  ###################################################################
  ## Astrugluas
  ####################################################################

  # die results
  die <- reactive({
    die <- c(rep(1, 1), rep(3, 4), rep(4, 4), rep(6, 1))
  })

  # Population of Astragalus
  output$pop <- renderPlot({
    data <- data.frame(x=c(1,3,4,6), y=c(.1,.4,.4,.1))
    makeBarPlot(xlab= "Number on roll of astragalus", ggtitle2="Population Graph",data= data, levels=1:6)
  })


  # All Sample Histogram
  output$line1 <- renderPlot({
    N <- input$asreps
    res <- vector("numeric",N)
    res
    for(i in 1:input$asreps){
      numbers=sample(x=c(1,3,4,6),size=input$assize,replace=TRUE,prob=c(.1,.4,.4,.1))
      c1=sum(numbers==1)/input$assize
      c2=sum(numbers==3)/input$assize
      c3=sum(numbers==4)/input$assize
      c4=sum(numbers==6)/input$assize
      c5=c1+c2*3+c3*4+c4*6
      res[i] <- c5
    }

    res <- data.frame(gg = res)
    x <- seq(1,6,length.out=50)
    curv <- with(res,data.frame(x=x,y=dnorm(x, mean(gg), sd(gg))))

    p <- ggplot(data=res,aes(x=gg)) +
      geom_histogram(aes(y =..density..),
                     binwidth = 0.1,
                     fill = "lightblue",
                     col = "black",
      ) + xlab("Sample mean")+
      ggtitle("All Samples Histogram") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black")
      )+geom_line(data = curv, aes(x = x, y = y), color = "blue",lwd = 1)
    p
  })

  # One Sample Histogram
  output$line2 <- renderPlot({
    input$new
    numbers= sample(x=c(1,3,4,6),size=input$assize,replace=TRUE,prob=c(.1,.4,.4,.1))
    c1=sum(numbers==1)/input$assize
    c2=sum(numbers==3)/input$assize
    c3=sum(numbers==4)/input$assize
    c4=sum(numbers==6)/input$assize
    firstfifData6=data.frame(x=c(1,3,4,6), y=c(c1,c2,c3,c4))
    makeBarPlot(xlab= "Individual value",
                ggtitle2="Histogram of a Single Sample",
                data = firstfifData6,
                levels=1:6, color = "lightblue", color2 = "black")
  })

  ###################################################################
  ## Playlist SHUFFLE
  ####################################################################

  # set up songs from four types
  songs <- reactive({
    songs <- c(rep(input$s1),
               rep(input$s2),
               rep(input$s3),
               rep(input$s4))
  })

  # Reactive expression to get the number of songs of the chosen type
  nSongs <- reactive({
    if(input$ptype=="Jazz"){
      nSongs <- input$s1
    }
    else if(input$ptype=="Rock"){
      nSongs <- input$s2
    }
    else if(input$ptype=="Country"){
      nSongs <- input$s3
    }
    else{
      nSongs <- input$s4
    }
  })

  # average songs in the Playlist
  avg_songs <- reactive({
    mean(songs())
  })


  # Jazz percent
  output$Jazz_percent <- renderPrint({
    cat(round(input$s1 / sum(songs()), digits = 2))
  })

  # Rock percent
  output$Rock_percent <- renderPrint({
    cat(round(input$s2 / sum(songs()), digits = 2))
  })

  # Country percent
  output$Country_percent <- renderPrint({
    cat(round(input$s3 / sum(songs()), digits = 2))
  })

  # Hip-pop percent
  output$Hiphop_percent <- renderPrint({
    cat(round(input$s4 / sum(songs()), digits = 2))
  })

  ############################################
  # Plot with bar plot with 4 categories songs

  # Jazz population plot
  output$playlistPlot <- renderPlot({
    p <- nSongs() / sum(songs())
    data <- data.frame(x = c("Other music (0)", paste(input$ptype,"(1)")), y=c(1-p, p))
    data$x <- factor(data$x, levels=data$x) # Done to force sorted order for bars

    # Make bar plot
    makeBarPlot(xlab= "Genre", data= data, ggtitle2 = "Population Graph")
  })


  ############################################
  # Average Plot with 4 categories songs


  # Matrix of Songs from 4 types

  ### Jazz
  # Matrix for first 50 reps of data
  firstfifDataJazz <- reactive(matrix(
    rbinom(
      50 * input$Playlistsize,
      size = 1,
      prob = input$s1 / sum(songs())
    ),
    nrow = 50,
    ncol = input$Playlistsize
  ))


  # Write the mean of first fif data into vector
  firstfifJazz <- reactive({
    matrix <- firstfifDataJazz()
    matrix.means <-
      matrix(0, nrow = 50, ncol = input$Playlistsize) #matrix with all zeroes
    for (i in 1:50) {
      for (j in 1:input$Playlistsize) {
        matrix.means[i, j] = mean(matrix[i, 1:j])
      }
    }
    fifmeans = as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first fif means with the rest of data
  Jazzdata <- reactive({
    datameans = firstfifJazz()
    for (i in 1:(input$Playlistreps - 50)) {
      datameans = append(datameans, mean(rbinom(
        input$Playlistsize,
        size = 1,
        prob = input$s1 / sum(songs())
      )))
    }
    return(datameans)
  })

  ### Rock
  # Matrix for first fif reps of data
  firstfifDataRock <- reactive(matrix(
    rbinom(
      50 * input$Playlistsize,
      size = 1,
      prob = input$s2 / sum(songs())
    ),
    nrow = 50,
    ncol = input$Playlistsize
  ))


  # Write the mean of first fif data into vector
  firstfifRock <- reactive({
    matrix <- firstfifDataRock()
    matrix.means <- matrix(0, nrow = 50, ncol = input$Playlistsize)
    for (i in 1:50) {
      for (j in 1:input$Playlistsize) {
        matrix.means[i, j] = mean(matrix[i, 1:j])
      }
    }

    fifmeans = as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first fif means with the rest of data
  Rockdata <- reactive({
    datameans = firstfifRock()
    for (i in 1:(input$Playlistreps - 50)) {
      datameans = append(datameans, mean(rbinom(
        input$Playlistsize,
        size = 1,
        prob = input$s2 / sum(songs())
      )))
    }
    return(datameans)
  })


  ### Country
  # Matrix for first fif reps of data
  firstfifDataCountry <- reactive(matrix(
    rbinom(
      50 * input$Playlistsize,
      size = 1,
      prob = input$s3 / sum(songs())
    ),
    nrow = 50,
    ncol = input$Playlistsize
  ))


  # Write the mean of first fif data into vector
  firstfifCountry <- reactive({
    matrix <- firstfifDataCountry()
    matrix.means <- matrix(0, nrow = 50, ncol = input$Playlistsize)
    for (i in 1:50) {
      for (j in 1:input$Playlistsize) {
        matrix.means[i, j] = mean(matrix[i, 1:j])
      }
    }

    fifmeans = as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first fif means with the rest of data
  Countrydata <- reactive({
    datameans = firstfifCountry()
    for (i in 1:(input$Playlistreps - 50)) {
      datameans = append(datameans, mean(rbinom(
        input$Playlistsize,
        size = 1,
        prob = input$s3 / sum(songs())
      )))
    }
    return(datameans)
  })

  ### Hiphop
  # Matrix for first fif reps of data
  firstfifDataHiphop <- reactive(matrix(
    rbinom(
      50 * input$Playlistsize,
      size = 1,
      prob = input$s4 / sum(songs())
    ),
    nrow = 50,
    ncol = input$Playlistsize
  ))


  # Write the mean of first fif data into vector
  firstfifHiphop <- reactive({
    matrix <- firstfifDataHiphop()
    matrix.means <- matrix(0, nrow = 50, ncol = input$Playlistsize)
    for (i in 1:50) {
      for (j in 1:input$Playlistsize) {
        matrix.means[i, j] = mean(matrix[i, 1:j])
      }
    }

    fifmeans = as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first fif means with the rest of data
  Hiphopdata <- reactive({
    datameans = firstfifHiphop()
    for (i in 1:(input$Playlistreps - 50)) {
      datameans = append(datameans, mean(rbinom(
        input$Playlistsize,
        size = 1,
        prob = input$s4 / sum(songs())
      )))
    }
    return(datameans)
  })

  firstFif <- reactive({
    if(input$ptype=="Jazz"){
      firstfifDataJazz()
    }
    else if(input$ptype=="Rock"){
      firstfifDataRock()
    }
    else if(input$ptype=="Country"){
      firstfifDataCountry()
    }
    else{
      firstfifDataHiphop()
    }
  })

  output$playlistSampleMean <- renderPlot({
    p <- mean(firstFif()[sample(min(50, input$Playlistreps), 1, replace = FALSE),])
    input$new
    data <- data.frame(x = c("Other music (0)", paste(input$ptype,"(1)")), y=c(1-p, p))
    data$x <- factor(data$x, levels=data$x) # Done to force sorted order for bars

    # Make bar plot
    makeBarPlot(xlab= "Genre", data= data, ggtitle2 = "Histogram of a Single Sample", color = "lightblue", color2 = "black")
  })

  ############################################
  # Sum Plot with 4 categories songs

  # Make the sum plot for playlist
  makePlaylistSumPlot <- function(s, data){
    if (s == 0) {
      vector <- data
      allSample(vector, min(80, input$Playlistreps))
    }
    else{
      n <- input$Playlistsize
      vector <-
        rbinom(input$Playlistreps * n,
               size = n,
               prob = s / sum(songs()))
      vector_mean <- c()
      for (i in 1:input$Playlistreps * n) {
        vector_mean <- append(vector_mean, vector[i] / n)
      }
      allSample(vector_mean, 10)
    }
  }

  # All sample playlist plot
  output$allSamplePlaylist <- renderPlot({
    if(input$ptype== 'Jazz'){
      makePlaylistSumPlot(input$s1, Jazzdata())
    }
    else if(input$ptype== 'Rock'){
      makePlaylistSumPlot(input$s2, Rockdata())
    }
    else if(input$ptype== 'Country'){
      makePlaylistSumPlot(input$s3, Countrydata())
    }
    else{
      makePlaylistSumPlot(input$s4, Hiphopdata())
    }
  })

})