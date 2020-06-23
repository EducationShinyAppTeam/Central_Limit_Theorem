library(shiny)
library(shinydashboard)
library(ggplot2)
library(stats)
library(Rlab)
library(dplyr)
library(shinyWidgets)

shinyServer(function(session, input, output) {
  observeEvent(input$info, {
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



  # Function to create density plots for each group
  # Inputs: Dataframe consisting of columns x and y to define axes, limits for x axis in form c(lower, upper), optional path for symmetric case
  # Output: ggplot of density
  makeDensityPlot <- function(data, xlims, path = 0) {
    plot <- ggplot2::ggplot(aes(x = x, y = y), data = data) +
      geom_path(color = "red", size = 1.5) +
      xlim(xlims) +
      xlab("value") +
      ylab("density") +
      ggtitle("Population Graph") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black")
      )
    # For case in symmetric where path is 1 causing "box" shape
    if (path == 1) {
      plot <- plot +
        geom_segment(aes(
          x = 0,
          y = 0,
          xend = 0,
          yend = 1
        ),
        color = "#0072B2",
        size = 1.5) +
        geom_segment(aes(
          x = 1,
          y = 0,
          xend = 1,
          yend = 1
        ),
        color = "#0072B2",
        size = 1.5)
    }
    plot
  }
  #Make the make bar plot function
  makeBarPlot <- function(xlab, data, levels = as.character(data$x)) {
    plot <- ggplot(aes(x = factor(x, levels = levels), y = y), data = data) +
      geom_bar(stat = "identity",
               fill = "lightblue",
               col = "black") +
      ylim(c(0, max(data$y) + .1 * max(data$y))) +
      xlab(xlab) +
      ylab("Probability") +
      ggtitle("Population Graph") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black")
      ) +
      scale_x_discrete(drop = FALSE)

    plot
  }
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
      curve(
        dgamma(-x, shape = input$leftskew, beta = 1),
        main = "Population Graph",
        col = "red",
        xlab = "value",
        ylab = "density",
        lwd = 5,
        cex.lab = 1.5,
        cex.axis = 1.5,
        cex.main = 1.5,
        cex.sub = 1.5,
        xlim = c(input$leftskew - 9 * sqrt(input$leftskew), 0)
      )
    })

    # Matrix for first 50 reps of data
    firstfifData1 <- reactive(matrix(
      -rgamma(
        n = 50 * input$leftsize,
        input$leftskew,
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
            shape = input$leftskew,
            beta = 1
          )
        ))
      }
      return(datameans)
    })

    # One Sample Histogram
    output$plotleft2 <- renderPlot({
      matrix <- firstfifData1()
      input$new1
      if (input$leftreps <= 50) {
        hist(
          matrix[sample(input$leftreps, 1, replace = FALSE), ],
          freq = FALSE,
          main = "Histogram of values in a single sample",
          col = "lightblue",
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.25,
          cex.sub = 1.5,
          xlab = "individual value"
        )
      }
      else{
        hist(
          matrix[sample(50, 1, replace = FALSE), ],
          freq = FALSE,
          main = "Histogram of values in a single sample",
          col = "lightblue",
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.25,
          cex.sub = 1.5,
          xlab = "individual value"
        )
      }

    })

    # All Sample Histogram
    output$plotleft3 <- renderPlot({
      vector <- data1()
      tmphist <- hist(vector, plot = FALSE)
      highestCount <- max(tmphist$density)
      tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
      if (input$leftreps <= 100) {
        hist(
          vector,
          main = "All Samples Histogram",
          col = "lightblue",
          breaks = input$leftreps,
          freq = FALSE,
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.5,
          cex.sub = 1.5,
          xlab = "sample average",
          ylim = c(0, max(tmp, highestCount) + 0.25)
        )
        curve(
          dnorm(x, mean = mean(vector), sd = sd(vector)),
          col = "blue",
          lwd = 3,
          add = TRUE
        )
      }
      else{
        hist(
          vector,
          main = "All Samples Histogram",
          col = "lightblue",
          breaks = 100,
          freq = FALSE,
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.5,
          cex.sub = 1.5,
          xlab = "sample average",
          ylim = c(0, max(tmp, highestCount) + 0.25)
        )
        curve(
          dnorm(x, mean = mean(vector), sd = sd(vector)),
          col = "blue",
          lwd = 3,
          add = TRUE
        )
      }

    })


    ###################################################################
    ## Right skewed
    ####################################################################

    # Population of right skewed
    output$plotright1 <- renderPlot({
      # plot(seq(0,5,.001),dgamma(seq(0,5,.001),input$rightskew, input$rightskew),
      #      main="Population Graph", col="red", xlab="value", ylab="density")
      curve(
        dgamma(x, shape = input$rightskew, beta = 1),
        main = "Population Graph",
        col = "red",
        xlab = "value",
        ylab = "density",
        lwd = 5,
        cex.lab = 1.5,
        cex.axis = 1.5,
        cex.main = 1.5,
        cex.sub = 1.5,
        xlim = c(0, input$rightskew + 9 * sqrt(input$rightskew))
      )
    })


    # Matrix for first 50 reps of data
    firstfifData2 <- reactive(matrix(
      rgamma(
        n = 50 * input$rightsize,
        input$rightskew,
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
            shape = input$rightskew,
            beta = 1
          )
        ))
      }
      return(datameans)
    })


    # One Sample Histogram
    output$plotright2 <- renderPlot({
      matrix <- firstfifData2()
      input$new2
      if (input$rightreps <= 50) {
        hist(
          matrix[sample(input$rightreps, 1, replace = FALSE), ],
          freq = FALSE,
          main = "Histogram of values in a single sample",
          col = "lightblue",
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.25,
          cex.sub = 1.5,
          xlab = "individual value"
        )
      }
      else{
        hist(
          matrix[sample(50, 1, replace = FALSE), ],
          freq = FALSE,
          main = "Histogram of values in a single sample",
          col = "lightblue",
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.25,
          cex.sub = 1.5,
          xlab = "individual value"
        )
      }

    })

    # All Sample Histogram
    output$plotright3 <- renderPlot({
      vector <- data2()
      tmphist <- hist(vector, plot = FALSE)
      highestCount <- max(tmphist$density)
      tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
      if (input$rightreps <= 80) {
        hist(
          vector,
          main = "All Samples Histogram",
          col = "lightblue",
          breaks = input$rightreps,
          freq = FALSE,
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.5,
          cex.sub = 1.5,
          xlab = "sample average",
          ylim = c(0, max(tmp, highestCount) + 0.25)
        )
        curve(
          dnorm(x, mean = mean(vector), sd = sd(vector)),
          col = "blue",
          lwd = 3,
          add = TRUE
        )
      }
      else{
        hist(
          vector,
          main = "All Samples Histogram",
          col = "lightblue",
          breaks = 80,
          freq = FALSE,
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.5,
          cex.sub = 1.5,
          xlab = "sample average",
          ylim = c(0, max(tmp, highestCount) + 0.25)
        )
        curve(
          dnorm(x, mean = mean(vector), sd = sd(vector)),
          col = "blue",
          lwd = 3,
          add = TRUE
        )
      }


    })

    ###################################################################
    ## Symmetric skewed
    ####################################################################

    # Population of Symmetric skewed
    output$plotsymmetric1 <- renderPlot({
      x <- seq(0, 1, length = 100)
      dens <-
        dbeta(x,
              shape1 = input$inverse,
              shape2 = input$inverse)

      # Dealing with peakness = 1 special case
      if (input$inverse == 1) {
        plot(
          x,
          dens,
          type = "l",
          yaxs = "i",
          xaxs = "i",
          xlim = c(-0.03, 1.03),
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.5,
          cex.sub = 1.5,
          xlab = "value",
          ylab = "density",
          main = "Population Graph",
          col = "red",
          lwd = 5
        )
        segments(0, 0, 0, 1, col = "red", lwd = 5)
        segments(1, 0, 1, 1, col = "red", lwd = 5)
        segments(0, 1, 1, 1, col = "red", lwd = 5)

      } else{
        plot(
          x,
          dens,
          type = "l",
          yaxs = "i",
          xaxs = "i",
          xlim = c(-0.01, 1.01),
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.5,
          cex.sub = 1.5,
          xlab = "value",
          ylab = "density",
          main = "Population Graph",
          col = "red",
          lwd = 5
        )
        lines(x, dens, col = "red")
      }
    })


    # Matrix for first 50 reps of data
    firstfifData3 <- reactive(matrix(
      rbeta(
        50 * input$symsize,
        shape1 = input$inverse,
        shape2 = input$inverse
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
            shape1 = input$inverse,
            shape2 = input$inverse
          )
        ))
      }
      return(datameans)
    })



    # One Sample Histogram
    output$plotsymmetric2 <- renderPlot({
      matrix <- firstfifData3()
      input$new3
      if (input$symreps <= 50) {
        hist(
          matrix[sample(input$symreps, 1, replace = FALSE), ],
          freq = FALSE,
          main = "Histogram of values in a single sample",
          col = "lightblue",
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.25,
          cex.sub = 1.5,
          xlab = "individual value"
        )
      }
      else{
        hist(
          matrix[sample(50, 1, replace = FALSE), ],
          freq = FALSE,
          main = "Histogram of values in a single sample",
          col = "lightblue",
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.25,
          cex.sub = 1.5,
          xlab = "individual value"
        )
      }

    })

    # All Sample Histogram
    output$plotsymmetric3 <- renderPlot({
      vector <- data3()
      tmphist <- hist(vector, plot = FALSE)
      highestCount <- max(tmphist$density)
      tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
      if (input$symreps <= 80) {
        hist(
          vector,
          main = "All Samples Histogram",
          col = "lightblue",
          breaks = input$symreps,
          freq = FALSE,
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.5,
          cex.sub = 1.5,
          xlab = "sample average",
          ylim = c(0, max(tmp, highestCount) + 1.7)
        )
        curve(
          dnorm(x, mean = mean(vector), sd = sd(vector)),
          col = "blue",
          lwd = 3,
          add = TRUE
        )
      }
      else{
        hist(
          vector,
          main = "All Samples Histogram",
          col = "lightblue",
          breaks = 80,
          freq = FALSE,
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.5,
          cex.sub = 1.5,
          xlab = "sample average",
          ylim = c(0, max(tmp, highestCount) + 1.7)
        )
        curve(
          dnorm(x, mean = mean(vector), sd = sd(vector)),
          col = "blue",
          lwd = 3,
          add = TRUE
        )
      }

    })


    ###################################################################
    ## Bimodal
    ####################################################################

    # Population for biomodel
    output$plotbiomodel1 <- renderPlot({
      # t <- 0.0001
      # y <- seq(0+t, 5, t)
      # z <- seq(5-t, 0, -t)
      #
      # x <- seq(0, 5, by=0.005)
      # leftdraw <- dgamma(z, input$leftskew, beta=1)
      # rightdraw <- dgamma(y, input$rightskew, beta=1)
      # Z <- input$prop*leftdraw + (1-input$prop)*rightdraw
      t <- 5 / (input$bisize * input$bireps)
      y <- seq(0, 5, t)
      z <- seq(5, 0, -t)
      leftdraw <- dgamma(z, 1.2, beta = 1)
      rightdraw <- dgamma(y, 1.2, beta = 1)
      data <-
        data.frame(x = seq(0, 5, t),
                   y = input$prop * leftdraw + (1 - input$prop) * rightdraw)

      # Make the density plot
      makeDensityPlot(data = data, xlims = c(0, 5))

      # plot(y, Z, type="l", yaxs="i", xaxs="i",
      #      xlab="value", ylab="density", main="Population Graph",
      #      cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      #      col="red", lwd=5)
      # lines(y, Z, type="l", col="red", xlab="",ylab="")
    })


    # Matrix for first 50 reps of data
    firstfifData4 <- reactive({
      rand <- sample(
        x = c(0, 1),
        size = input$bisize * 50,
        replace = TRUE,
        prob = c(input$prop, 1 - input$prop)
      )

      rights <-
        sum(rand) # Number of elements sampled from the right distribution (represented by 1)
      lefts <-
        input$bisize * 10 - rights # Number of elements sampled from left distribution (represented by 0)
      leftGammas <-
        rgamma(lefts, 1.25, beta = 1) # Samples left distribution
      rightGammas <-
        5 - rgamma(rights, 1.25, beta = 1) # Samples right distribution

      # Loop to assign values from gamma distributions to rand
      rightIndex <- 1
      leftIndex <- 1
      for (x in 1:length(rand)) {
        if (rand[x] == 0) {
          rand[x] <- leftGammas[leftIndex]
          leftIndex <- leftIndex + 1
        }
        else{
          rand[x] <- rightGammas[rightIndex]
          rightIndex <- rightIndex + 1
        }
      }
      matrix(rand, nrow = 50
             # mix.synthetic.facing.gamma(N = 10*input$bisize, mix.prob = 1-input$prop,
             #                                        lower = 0, upper = 6, shape1=input$leftskew, scale1=1,
             #                                        shape2=input$rightskew, scale2=1),
             #             nrow = 10, ncol = input$bisize
    )})





      # Matrix for first 50 reps of data
      firstfifData4 <- reactive({
        rand <- sample(
          x = c(0, 1),
          size = input$bisize * 50,
          replace = TRUE,
          prob = c(input$prop, 1 - input$prop)
        )

        rights <-
          sum(rand) # Number of elements sampled from the right distribution (represented by 1)
        lefts <-
          input$bisize * 50 - rights # Number of elements sampled from left distribution (represented by 0)
        leftGammas <-
          rgamma(lefts, 1.25, beta = 1) # Samples left distribution
        rightGammas <-
          5 - rgamma(rights, 1.25, beta = 1) # Samples right distribution

        # Loop to assign values from gamma distributions to rand
        rightIndex <- 1
        leftIndex <- 1
        for (x in 1:length(rand)) {
          if (rand[x] == 0) {
            rand[x] <- leftGammas[leftIndex]
            leftIndex <- leftIndex + 1
          }
          else{
            rand[x] <- rightGammas[rightIndex]
            rightIndex <- rightIndex + 1
          }
        }
        matrix(rand, nrow = 50
               # mix.synthetic.facing.gamma(N = 10*input$bisize, mix.prob = 1-input$prop,
               #                                        lower = 0, upper = 6, shape1=input$leftskew, scale1=1,
               #                                        shape2=input$rightskew, scale2=1),
               #             nrow = 10, ncol = input$bisize
      )})




        # Write the mean of first 50 data into vector
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


        # Merge the first 50 means with the rest of data
        data4 <- reactive({
          datameans = firstfif4()
          for (i in 1:(input$bireps - 50)) {
            # Random vector of 0s and 1s to determine which distribution each element samples from
            rand <- sample(
              x = c(0, 1),
              size = input$bisize,
              replace = TRUE,
              prob = c(input$prop, 1 - input$prop)
            )

            rights <-
              sum(rand) # Number of elements sampled from the right distribution (represented by 1)
            lefts <-
              input$bisize - rights # Number of elements sampled from left distribution (represented by 0)
            leftGammas <-
              rgamma(lefts, 1.25, beta = 1) # Samples left distribution
            rightGammas <-
              5 - rgamma(rights, 1.25, beta = 1) # Samples right distribution

            # Loop to assign values from gamma distributions to rand
            rightIndex <- 1
            leftIndex <- 1
            for (x in 1:length(rand)) {
              if (rand[x] == 0) {
                rand[x] <- leftGammas[leftIndex]
                leftIndex <- leftIndex + 1
              }
              else{
                rand[x] <- rightGammas[rightIndex]
                rightIndex <- rightIndex + 1
              }
            }

            datameans <- append(datameans, mean(rand))
            #datameans = append(datameans, mean(mix.synthetic.facing.gamma(N = input$bisize, mix.prob = 1-input$prop,
            #                                                              lower = 0, upper = 6, shape1=input$leftskew, scale1=1,
            #                                                              shape2=input$rightskew, scale2=1)))
          }
          return(datameans)
        })


        # One Sample Histogram
        output$plotbiomodel2 <- renderPlot({
          matrix <- firstfifData4()
          input$new4
          if (input$bireps <= 50) {
            hist(
              matrix[sample(input$bireps, 1, replace = FALSE), ],
              freq = FALSE,
              main = "Histogram of values in a single sample",
              col = "lightblue",
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              xlab = "individual value"
            )
          }
          else{
            hist(
              matrix[sample(50, 1, replace = FALSE), ],
              freq = FALSE,
              main = "Histogram of values in a single sample",
              col = "lightblue",
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              xlab = "individual value"
            )
          }
        })

        # All Sample Histogram
        output$plotbiomodel3 <- renderPlot({
          vector <- data4()
          tmphist <- hist(vector, plot = FALSE)
          highestCount <- max(tmphist$density)
          tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
          if (input$bireps <= 80) {
            hist(
              vector,
              main = "All Samples Histogram",
              col = "lightblue",
              breaks = input$bireps,
              freq = FALSE,
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.5,
              cex.sub = 1.5,
              xlab = "sample average",
              ylim = c(0, max(tmp, highestCount) + 0.25)
            )
            curve(
              dnorm(x, mean = mean(vector), sd = sd(vector)),
              col = "blue",
              lwd = 3,
              add = TRUE
            )
          }
          else{
            hist(
              vector,
              main = "All Samples Histogram",
              col = "lightblue",
              breaks = 80,
              freq = FALSE,
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.5,
              cex.sub = 1.5,
              xlab = "sample average",
              ylim = c(0, max(tmp, highestCount) + 0.25)
            )
            curve(
              dnorm(x, mean = mean(vector), sd = sd(vector)),
              col = "blue",
              lwd = 3,
              add = TRUE
            )
          }

        })


















        ###################################################################
        ## Accident Rate
        ####################################################################

        # Population of poisson
        #output$poissonpop <- renderPlot({
        #N <- 10000
        #x <- rpois(N, input$poissonmean)
        #hist(x,
        #xlim=c(min(x),max(x)), probability = T, nclass = max(x)-min(x)+1,
        #cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
        #col='lightblue', xlab = "# of accidents", ylab = "probability",
        #main='Population Graph')
        #})
        output$poissonpop <- renderCachedPlot({
          data <-
            data.frame(x = 0:ceiling(2 * input$poissonmean + 5)) # More x's than necessary
          data$y <-
            (input$poissonmean ^ data$x) * exp(-input$poissonmean) / factorial(data$x) # Get y vals for x's
          data <-
            rbind(data[1:2, ], filter(data[-c(1, 2),], y > .0005)) # Filter based on probability
          makeBarPlot(xlab = "Number of accidents", data = data)
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
          input$new5
          if (input$poreps <= 50) {
            hist(
              matrix[sample(input$poreps, 1, replace = FALSE), ],
              freq = FALSE,
              main = "Histogram of values in a single sample",
              col = "lightblue",
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              xlab = "individual value"
            )
          }
          else{
            hist(
              matrix[sample(50, 1, replace = FALSE), ],
              freq = FALSE,
              main = "Histogram of values in a single sample",
              col = "lightblue",
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              xlab = "individual value"
            )
          }

        })
        # All Sample Histogram
        output$plotpoisson2 <- renderPlot({
          vector <- data5()
          tmphist <- hist(vector, plot = FALSE)
          highestCount <- max(tmphist$density)
          tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
          if (input$poreps <= 80) {
            hist(
              vector,
              main = "All Samples Histogram",
              col = "lightblue",
              breaks = input$poreps,
              freq = FALSE,
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.5,
              cex.sub = 1.5,
              xlab = "sample average",
              ylim = c(0, max(tmp, highestCount) + 3)
            )
            curve(
              dnorm(x, mean = mean(vector), sd = sd(vector)),
              col = "blue",
              lwd = 3,
              add = TRUE
            )
          }
          else{
            hist(
              vector,
              main = "All Samples Histogram",
              col = "lightblue",
              breaks = 80,
              freq = FALSE,
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.5,
              cex.sub = 1.5,
              xlab = "sample average",
              ylim = c(0, max(tmp, highestCount) + 3)
            )
            curve(
              dnorm(x, mean = mean(vector), sd = sd(vector)),
              col = "blue",
              lwd = 3,
              add = TRUE
            )
          }

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
          a = min(die())
          b = max(die())
          foo <- hist(
            x = die() + 0.001,
            breaks = b - a,
            probability = T,
            xaxt = "n",
            cex.lab = 1.5,
            cex.axis = 1.5,
            cex.main = 1.5,
            cex.sub = 1.5,
            col = 'lightblue',
            xlab = "# on roll of Astragalus",
            ylab = "probability",
            main = "Population Graph"
          )
          axis(side = 1,
               at = foo$mids,
               labels = seq(a, b))

        })



        # Matrix for first 50 reps of data
        firstfifData6 <- reactive(matrix(
          sample(die(), 50 * input$assize,
                 replace = TRUE),
          nrow = 50,
          ncol = input$assize
        ))


        # Write the mean of first 50 data into vector
        firstfif6 <- reactive({
          matrix <- firstfifData6()
          matrix.means <- matrix(0, nrow = 50, ncol = input$assize)
          for (i in 1:50) {
            for (j in 1:input$assize) {
              matrix.means[i, j] = mean(matrix[i, 1:j])
            }
          }

          fifmeans = as.vector(matrix.means)
          return(fifmeans)
        })

        # Merge the first 50 means with the rest of data
        data6 <- reactive({
          datameans = firstfif6()
          for (i in 1:(input$asreps - 50)) {
            datameans = append(datameans, mean(sample(die(), input$assize,
                                                      replace = TRUE)))
          }
          return(datameans)
        })



        # One Sample Histogram
        output$line2 <- renderPlot({
          matrix <- firstfifData6()
          input$new6
          if (input$asreps <= 50) {
            hist(
              matrix[sample(input$asreps, 1, replace = FALSE), ],
              freq = FALSE,
              main = "Histogram of values in a single sample",
              col = "lightblue",
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              xlab = "individual value"
            )
          }
          else{
            hist(
              matrix[sample(50, 1, replace = FALSE), ],
              freq = FALSE,
              main = "Histogram of values in a single sample",
              col = "lightblue",
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              xlab = "individual value"
            )
          }

        })
        # All Sample Histogram
        output$line1 <- renderPlot({
          vector <- data6()
          tmphist <- hist(vector, plot = FALSE)
          highestCount <- max(tmphist$density)
          tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
          if (input$asreps <= 80) {
            hist(
              vector,
              main = "All Samples Histogram",
              col = "lightblue",
              breaks = input$asreps,
              freq = FALSE,
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.5,
              cex.sub = 1.5,
              xlab = "sample average",
              ylim = c(0, max(tmp, highestCount) + 2)
            )
            curve(
              dnorm(x,  mean = mean(vector), sd = sd(vector)),
              col = "blue",
              lwd = 3,
              add = TRUE
            )
          }
          else{
            hist(
              vector,
              main = "All Samples Histogram",
              col = "lightblue",
              breaks = 80,
              freq = FALSE,
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.5,
              cex.sub = 1.5,
              xlab = "sample average",
              ylim = c(0, max(tmp, highestCount) + 2)
            )
            curve(
              dnorm(x,  mean = mean(vector), sd = sd(vector)),
              col = "blue",
              lwd = 3,
              add = TRUE
            )
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
                     rep(input$s4))
        })

        # average songs in the IPOD
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
        output$Plot1 <- renderPlot({
          pjazz <- input$s1 / sum(songs())
          count <- c(pjazz * input$ipodsize, (1 - pjazz) * input$ipodsize)
          barplot(
            count,
            main = "Population Graph",
            xlab = "Jazz vs Other music"
            ,
            ylab = "probability",
            col = 'lightblue',
            cex.lab = 1.5,
            cex.axis = 1.5,
            cex.main = 1.5,
            cex.sub = 1.5,
            names.arg = c("Jazz", "Other music")
          )
          # n <- input$ipodsize
          # x <- seq(0, n, by = 1)
          # plot (x, dbinom(x, n, pjazz, log = FALSE), type = "l", xlab = "values",ylab = "density",
          #       main = "Population Graph",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
          #       col="red", lwd=5)
        })

        # Rock population plot
        output$Plot2 <- renderPlot({
          prock <- input$s2 / sum(songs())
          count <- c(prock * input$ipodsize, (1 - prock) * input$ipodsize)
          barplot(
            count,
            main = "Population Graph",
            xlab = "Rock vs Other music"
            ,
            ylab = "probability",
            col = 'lightblue',
            cex.lab = 1.5,
            cex.axis = 1.5,
            cex.main = 1.5,
            cex.sub = 1.5,
            names.arg = c("Rock", "Other music")
          )
          # n <- input$ipodsize
          # x <- seq(0, n, by = 1)
          # plot (x, dbinom(x, n, prock, log = FALSE), type = "l", xlab = "values",ylab = "density",
          #       main = "Population Graph",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
          #       col="red", lwd=5)
        })

        # Country population plot
        output$Plot3 <- renderPlot({
          pcountry <- input$s3 / sum(songs())
          count <-
            c(pcountry * input$ipodsize, (1 - pcountry) * input$ipodsize)
          barplot(
            count,
            main = "Population Graph",
            xlab = "Country vs Other music"
            ,
            ylab = "probability",
            col = 'lightblue',
            cex.lab = 1.5,
            cex.axis = 1.5,
            cex.main = 1.5,
            cex.sub = 1.5,
            names.arg = c("Country", "Other music")
          )
          # n <- input$ipodsize
          # x <- seq(0, n, by = 1)
          # plot (x, dbinom(x, n, pcountry, log = FALSE), type = "l", xlab = "values",ylab = "density",
          #       main = "Population Graph",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
          #       col="red", lwd=5)
        })

        #Hip-pop population plot
        output$Plot4 <- renderPlot({
          phiphop <- input$s4 / sum(songs())
          count <- c(phiphop * input$ipodsize, (1 - phiphop) * input$ipodsize)
          barplot(
            count,
            main = "Population Graph",
            xlab = "Hip-hop vs Other music"
            ,
            ylab = "probability",
            col = 'lightblue',
            cex.lab = 1.5,
            cex.axis = 1.5,
            cex.main = 1.5,
            cex.sub = 1.5,
            names.arg = c("Hip-hop", "Other music")
          )
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
        # Matrix for first 50 reps of data
        firstfifDataJazz <- reactive(matrix(
          rbinom(
            50 * input$ipodsize,
            size = 1,
            prob = input$s1 / sum(songs())
          ),
          nrow = 50,
          ncol = input$ipodsize
        ))


        # Write the mean of first fif data into vector
        firstfifJazz <- reactive({
          matrix <- firstfifDataJazz()
          matrix.means <-
            matrix(0, nrow = 50, ncol = input$ipodsize) #matrix with all zeroes
          for (i in 1:50) {
            for (j in 1:input$ipodsize) {
              matrix.means[i, j] = mean(matrix[i, 1:j])
            }
          }

          fifmeans = as.vector(matrix.means)
          return(fifmeans)
        })

        # Merge the first fif means with the rest of data
        Jazzdata <- reactive({
          datameans = firstfifJazz()
          for (i in 1:(input$ipodreps - 50)) {
            datameans = append(datameans, mean(rbinom(
              input$ipodsize,
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
            50 * input$ipodsize,
            size = 1,
            prob = input$s2 / sum(songs())
          ),
          nrow = 50,
          ncol = input$ipodsize
        ))


        # Write the mean of first fif data into vector
        firstfifRock <- reactive({
          matrix <- firstfifDataRock()
          matrix.means <- matrix(0, nrow = 50, ncol = input$ipodsize)
          for (i in 1:50) {
            for (j in 1:input$ipodsize) {
              matrix.means[i, j] = mean(matrix[i, 1:j])
            }
          }

          fifmeans = as.vector(matrix.means)
          return(fifmeans)
        })

        # Merge the first fif means with the rest of data
        Rockdata <- reactive({
          datameans = firstfifRock()
          for (i in 1:(input$ipodreps - 50)) {
            datameans = append(datameans, mean(rbinom(
              input$ipodsize,
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
            50 * input$ipodsize,
            size = 1,
            prob = input$s3 / sum(songs())
          ),
          nrow = 50,
          ncol = input$ipodsize
        ))


        # Write the mean of first fif data into vector
        firstfifCountry <- reactive({
          matrix <- firstfifDataCountry()
          matrix.means <- matrix(0, nrow = 50, ncol = input$ipodsize)
          for (i in 1:50) {
            for (j in 1:input$ipodsize) {
              matrix.means[i, j] = mean(matrix[i, 1:j])
            }
          }

          fifmeans = as.vector(matrix.means)
          return(fifmeans)
        })

        # Merge the first fif means with the rest of data
        Countrydata <- reactive({
          datameans = firstfifCountry()
          for (i in 1:(input$ipodreps - 50)) {
            datameans = append(datameans, mean(rbinom(
              input$ipodsize,
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
            50 * input$ipodsize,
            size = 1,
            prob = input$s4 / sum(songs())
          ),
          nrow = 50,
          ncol = input$ipodsize
        ))


        # Write the mean of first fif data into vector
        firstfifHiphop <- reactive({
          matrix <- firstfifDataHiphop()
          matrix.means <- matrix(0, nrow = 50, ncol = input$ipodsize)
          for (i in 1:50) {
            for (j in 1:input$ipodsize) {
              matrix.means[i, j] = mean(matrix[i, 1:j])
            }
          }

          fifmeans = as.vector(matrix.means)
          return(fifmeans)
        })

        # Merge the first fif means with the rest of data
        Hiphopdata <- reactive({
          datameans = firstfifHiphop()
          for (i in 1:(input$ipodreps - 50)) {
            datameans = append(datameans, mean(rbinom(
              input$ipodsize,
              size = 1,
              prob = input$s4 / sum(songs())
            )))
          }
          return(datameans)
        })


        # JAZZ
        # One Sample Barplot
        output$Plot01 <- renderPlot({
          matrix <- firstfifDataJazz()
          input$new7
          if (input$ipodreps <= 50) {
            count <-
              c(mean(matrix[sample(input$ipodreps, 1, replace = FALSE), ]), 1 - mean(matrix[sample(input$ipodreps, 1, replace = FALSE), ]))
            barplot(
              count,
              main = "Histogram of values in a single sample",
              xlab = "Jazz vs Other music",
              ylab = "probability",
              col = 'lightblue',
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              names.arg = c("Jazz", "Other music")
            )
          }
          else{
            count <-
              c(mean(matrix[sample(50, 1, replace = FALSE), ]), 1 - mean(matrix[sample(10, 1, replace = FALSE), ]))
            barplot(
              count,
              main = "Histogram of values in a single sample",
              xlab = "Jazz vs Other music",
              ylab = "probability",
              col = 'lightblue',
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              names.arg = c("Jazz", "Other music")
            )
          }

        })


        # Rock
        # One Sample Barplot
        output$Plot02 <- renderPlot({
          matrix <- firstfifDataRock()
          input$new7
          if (input$ipodreps <= 50) {
            count <-
              c(1 - mean(matrix[sample(input$ipodreps, 1, replace = FALSE), ]), mean(matrix[sample(input$ipodreps, 1, replace = FALSE), ]))
            barplot(
              count,
              main = "Histogram of values in a single sample",
              xlab = "Rock vs Other music",
              ylab = "probability",
              col = 'lightblue',
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              names.arg = c("Rock", "Other music")
            )
          }
          else{
            count <-
              c(1 - mean(matrix[sample(50, 1, replace = FALSE), ]), mean(matrix[sample(10, 1, replace = FALSE), ]))
            barplot(
              count,
              main = "Histogram of values in a single sample",
              xlab = "Rock vs Other music",
              ylab = "probability",
              col = 'lightblue',
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              names.arg = c("Rock", "Other music")
            )
          }

        })

        # Country Average Plot
        output$Plot03 <- renderPlot({
          matrix <- firstfifDataCountry()
          input$new7
          if (input$ipodreps <= 50) {
            count <-
              c(1 - mean(matrix[sample(input$ipodreps, 1, replace = FALSE), ]), mean(matrix[sample(input$ipodreps, 1, replace = FALSE), ]))
            barplot(
              count,
              main = "Histogram of values in a single sample",
              xlab = "Country vs Other music",
              ylab = "probability",
              col = 'lightblue',
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              names.arg = c("Country", "Other music")
            )
          }
          else{
            count <-
              c(1 - mean(matrix[sample(50, 1, replace = FALSE), ]), mean(matrix[sample(10, 1, replace = FALSE), ]))
            barplot(
              count,
              main = "Histogram of values in a single sample",
              xlab = "Country vs Other music",
              ylab = "probability",
              col = 'lightblue',
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              names.arg = c("Country", "Other music")
            )
          }

        })


        # Hip-hop Average Plot
        output$Plot04 <- renderPlot({
          matrix <-  firstfifDataHiphop()
          input$new7
          if (input$ipodreps <= 50) {
            count <-
              c(1 - mean(matrix[sample(input$ipodreps, 1, replace = FALSE), ]), mean(matrix[sample(input$ipodreps, 1, replace = FALSE), ]))
            barplot(
              count,
              main = "Histogram of values in a single sample",
              xlab = "Hip-hop vs Other music",
              ylab = "probability",
              col = 'lightblue',
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              names.arg = c("Hip-hop", "Other music")
            )
          }
          else{
            count <-
              c(1 - mean(matrix[sample(50, 1, replace = FALSE), ]), mean(matrix[sample(10, 1, replace = FALSE), ]))
            barplot(
              count,
              main = "Histogram of values in a single sample",
              xlab = "Hip-hop vs Other music",
              ylab = "probability",
              col = 'lightblue',
              cex.lab = 1.5,
              cex.axis = 1.5,
              cex.main = 1.25,
              cex.sub = 1.5,
              names.arg = c("Hip-hop", "Other music")
            )
          }

        })



        ############################################
        # Sum Plot with 4 categories songs

        # JAZZ
        output$Plot10 <- renderPlot({
          if (input$s1 == 0) {
            vector <- Jazzdata()
            if (input$ipodreps <= 80) {
              hist(
                vector,
                main = "All Samples Histogram",
                col = "lightblue",
                breaks = input$ipodreps,
                freq = FALSE,
                cex.lab = 1.5,
                cex.axis = 1.5,
                cex.main = 1.5,
                cex.sub = 1.5,
                xlab = "sample average"
              )
              curve(
                dnorm(x, mean = mean(vector), sd = sd(vector)),
                col = "blue",
                lwd = 3,
                add = TRUE
              )
            }
            else{
              hist(
                vector,
                main = "All Samples Histogram",
                col = "lightblue",
                breaks = 80,
                freq = FALSE,
                cex.lab = 1.5,
                cex.axis = 1.5,
                cex.main = 1.5,
                cex.sub = 1.5,
                xlab = "sample average"
              )
              curve(
                dnorm(x, mean = mean(vector), sd = sd(vector)),
                col = "blue",
                lwd = 3,
                add = TRUE
              )
            }

          }
          else{
            # vector <-Jazzdata()
            # tmphist <- hist(vector, plot = FALSE)
            # highestCount <- max(tmphist$density)
            # tmp <- dnorm(vector, mean = mean(vector), sd = sd(vector))
            n <- input$ipodsize
            # x <- seq(0, n, by = 1)
            vector <-
              rbinom(input$ipodreps * n,
                     size = n,
                     prob = input$s1 / sum(songs()))
            vector_mean <- c()
            for (i in 1:input$ipodreps * n) {
              vector_mean <- append(vector_mean, vector[i] / n)
            }
            hist(
              vector_mean,
              main = "All Samples Histogram",
              col = "lightblue",
              #breaks = input$ipodsize,
              #cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              prob = TRUE,
              xlab = "sample average"
            )
            curve(
              dnorm(x, mean(vector_mean), sd(vector_mean)),
              col = "blue",
              lwd = 3,
              add = TRUE
            )
            # hist(vector, main="All Samples Histogram", col="lightblue", breaks = 100,
            #      freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
            #      xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
            # curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE)


          }
        })



        # Rock SUM PLOT
        output$Plot20 <- renderPlot({
          if (input$s2 == 0) {
            vector <- Rockdata()
            if (input$ipodreps <= 80) {
              hist(
                vector,
                main = "All Samples Histogram",
                col = "lightblue",
                breaks = input$ipodreps,
                freq = FALSE,
                cex.lab = 1.5,
                cex.axis = 1.5,
                cex.main = 1.5,
                cex.sub = 1.5,
                xlab = "sample average"
              )
              curve(
                dnorm(x, mean = mean(vector), sd = sd(vector)),
                col = "blue",
                lwd = 3,
                add = TRUE
              )
            }
            else{
              hist(
                vector,
                main = "All Samples Histogram",
                col = "lightblue",
                breaks = 80,
                freq = FALSE,
                cex.lab = 1.5,
                cex.axis = 1.5,
                cex.main = 1.5,
                cex.sub = 1.5,
                xlab = "sample average"
              )
              curve(
                dnorm(x, mean = mean(vector), sd = sd(vector)),
                col = "blue",
                lwd = 3,
                add = TRUE
              )
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
            vector <-
              rbinom(input$ipodreps * n,
                     size = n,
                     prob = input$s2 / sum(songs()))
            vector_mean <- c()
            for (i in 1:input$ipodreps * n) {
              vector_mean <- append(vector_mean, vector[i] / n)
            }
            hist(
              vector_mean,
              main = "All Samples Histogram",
              col = "lightblue",
              # breaks = input$ipodreps,
              #cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              prob = TRUE,
              xlab = "sample average"
            )
            curve(
              dnorm(x, mean(vector_mean), sd(vector_mean)),
              col = "blue",
              lwd = 3,
              add = TRUE
            )
            # hist(vector, main="All Samples Histogram", col="lightblue", breaks = 100,
            #      freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
            #      xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
            # curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE)
          }
        })

        # Country SUM PLOT
        output$Plot30 <- renderPlot({
          if (input$s3 == 0) {
            vector <- Countrydata()
            if (input$ipodreps <= 80) {
              hist(
                vector,
                main = "All Samples Histogram",
                col = "lightblue",
                breaks = input$ipodreps,
                freq = FALSE,
                cex.lab = 1.5,
                cex.axis = 1.5,
                cex.main = 1.5,
                cex.sub = 1.5,
                xlab = "sample average"
              )
              curve(
                dnorm(x, mean = mean(vector), sd = sd(vector)),
                col = "blue",
                lwd = 3,
                add = TRUE
              )
            }
            else{
              hist(
                vector,
                main = "All Samples Histogram",
                col = "lightblue",
                breaks = 80,
                freq = FALSE,
                cex.lab = 1.5,
                cex.axis = 1.5,
                cex.main = 1.5,
                cex.sub = 1.5,
                xlab = "sample average"
              )
              curve(
                dnorm(x, mean = mean(vector), sd = sd(vector)),
                col = "blue",
                lwd = 3,
                add = TRUE
              )
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
            vector <-
              rbinom(input$ipodreps * n,
                     size = n,
                     prob = input$s3 / sum(songs()))
            vector_mean <- c()
            for (i in 1:input$ipodreps * n) {
              vector_mean <- append(vector_mean, vector[i] / n)
            }
            hist(
              vector_mean,
              main = "All Samples Histogram",
              col = "lightblue",
              # breaks = input$ipodreps,
              #cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              prob = TRUE,
              xlab = "sample average"
            )
            curve(
              dnorm(x, mean(vector_mean), sd(vector_mean)),
              col = "blue",
              lwd = 3,
              add = TRUE
            )
            # hist(vector, main="All Samples Histogram", col="lightblue", breaks = 100,
            #      freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
            #      xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
            # curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE)
          }
        })

        # Hip_Hop SUM PLOT
        output$Plot40 <- renderPlot({
          if (input$s4 == 0) {
            vector <- Hiphopdata()
            if (input$ipodreps <= 80) {
              hist(
                vector,
                main = "All Samples Histogram",
                col = "lightblue",
                breaks = input$ipodreps,
                freq = FALSE,
                cex.lab = 1.5,
                cex.axis = 1.5,
                cex.main = 1.5,
                cex.sub = 1.5,
                xlab = "sample average"
              )
              curve(
                dnorm(x, mean = mean(vector), sd = sd(vector)),
                col = "blue",
                lwd = 3,
                add = TRUE
              )
            }
            else{
              hist(
                vector,
                main = "All Samples Histogram",
                col = "lightblue",
                breaks = 80,
                freq = FALSE,
                cex.lab = 1.5,
                cex.axis = 1.5,
                cex.main = 1.5,
                cex.sub = 1.5,
                xlab = "sample average"
              )
              curve(
                dnorm(x, mean = mean(vector), sd = sd(vector)),
                col = "blue",
                lwd = 3,
                add = TRUE
              )
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
            vector <-
              rbinom(input$ipodreps * n,
                     size = n,
                     prob = input$s4 / sum(songs()))
            vector_mean <- c()
            for (i in 1:input$ipodreps * n) {
              vector_mean <- append(vector_mean, vector[i] / n)
            }
            hist(
              vector_mean,
              main = "All Samples Histogram",
              col = "lightblue",
              # breaks = input$ipodreps,
              #cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
              prob = TRUE,
              xlab = "sample average"
            )
            curve(
              dnorm(x, mean(vector_mean), sd(vector_mean)),
              col = "blue",
              lwd = 3,
              add = TRUE
            )
            # hist(vector, main="All Samples Histogram", col="lightblue", breaks = 100,
            #      freq = FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
            #      xlab = "sample average",ylim = c(0, max(tmp, highestCount)))
            # curve(dnorm(x, mean = mean(vector), sd = sd(vector)), col="blue", lwd=3, add=TRUE)

          }
        })

  })
})
