# Load Packages
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(Rlab)
library(stats)
library(boastUtils)

# App Meta Data----------------------------------------------------------------
APP_TITLE <<- "Central Limit Theorem"
APP_DESCP <<- paste(
  "This app allows the user to examine the sampling distribution of the mean",
  "for samples from a variety of continuous and discrete populations.."
)
# End App Meta Data------------------------------------------------------------

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = "Central Limit Theorem",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        tags$a(
          target = "_blank", icon("comments"),
          href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Central_Limit_Theorem"
        )
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          href = "https://shinyapps.science.psu.edu/",
          icon("home")
        )
      )
    ),
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
        menuItem("Explore", icon = icon("wpexplorer"), tabName = "largeNumber"),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          ## Overview ----
          tabName = "Overview",
          fluidPage(
            h1("Central Limit Theorem"),
            p("This app is designed to examine the Central Limit Theorem under
            different population distributions and sample sizes."),
            p("The Central Limit Theorem tells us when the sample size increases,
            the all sample histogram will become normal distribution."),
            br(),
            h2("Instructions"),
            tags$ol(
              tags$li(
                "Pick a population from one of the continuous types (left-skewed,
              right-skewed, symmetric, or bimodal) or one of the discrete
              examples (rolls of an astragalus, random songs from an Playlist
              shuffle, or accident occurrence)."
              ),
              tags$li("Use the sliders to adjust the parameters of the population
                    model you chosen."),
              tags$li("Use the sliders to decide the sample size (n) you want for
                    each sample and how many reps you will repeat the process of
                    taking samples from the population (calculating
                    \\(\\overline{x}\\) for each)."),
              tags$li("Use the button to see one of the individual samples chosen."),
              tags$li("Observe the histograms of one sample and for
                    \\(\\overline{x}\\) from all samples."),
            ),
            div(
              style = "text-align: center",
              bsButton(
                inputId = "go",
                label = "Go!",
                icon = icon("bolt"),
                size = "large"
              )
            ),
            br(),
            br(),
            h2("Acknowledgements"),
            p("This app was developed and coded by Yingjie (Chelsea) Wang with the
             help of Yubaihe Zhou. Selection of population densities was originally
             coded by Caihui Xiao. This application was modified by Zhiruo Wang
            in 2019 and by Jiawei Wu and Leah Hunt in 2020."),
            div(class = "updated", "Last Update: 09/18/2020 by LMH.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "Prerequisites",
          h2("Prerequisites"),
          p(
            "In order to get the most out of this app, please review the
            following information that will be used in the app."
          ),
          tags$ul(
            tags$li(
              "This app uses four continuous distributions: right (positive)
                   skewed, left (negative) skewed, symmetric, and bimodal. While
                  in depth understanding of these distributions is not required,
                  you may wish to review this ",
              tags$a(
                href = "https://online.stat.psu.edu/stat100/lesson/3/3.2#graphshapes",
                "Stat 100 Table of Graph Shapes"
              ),
              ".",
            ),
            tags$li("One of the distributions is based upon rolls of an astragalus.
                  The astragalus (ankle or heel bone) of animals were used in
                  ancient times as a forerunner of modern dice. When a sheep
                  astragalus is thrown into the air it can land on one of four
                  sides, which were associated with the numbers 1, 3, 4, and 6.
                  Two sides (the 3 and the 4) are wider and each come up about
                  40% of the time, while the narrower sides (the 1 and the 6)
                  each come up about 10% of the time. An image of an astralagus
                  is shown below.")
          ),
          div(
            style = "text-align: center;",
            tags$figure(
              img(
                src = "astragalus.jpg",
                alt = "Picture of an astragalus",
                width = "50%",
                style = "text-align: center;"
              ),
              tags$figcaption("Image by Yaan, 2007.")
            )
          )
        ),
        # Explore Law of Large Numbers Tab ----
        tabItem(
          tabName = "largeNumber",
          h2("Central Limit Theorem"),
          p("In this section, you will have the chance to explore the Central
            Limit Theorem. To do so, first choose a population to sample from,
            a number of samples to take, and how many times to repeat the sampling
            process (i.e., 'reps'). Then observe the histogram of values in a
            single sample (these are data values) and the histogram of the values
            of the ", tags$em("sample arithemetic mean"), "from all of the
            repetitions to see how the normal approximation compares to this
            estimate of the sampling distribution."),
          hr(),
          h3("Population"),
          ### Population Picker ----
          sidebarLayout(
            sidebarPanel(
              width = 6,
              fluidRow(
                column(
                  width = 6,
                  selectInput(
                    inputId = "popDist",
                    label = "Population Type",
                    list(
                      "Left-skewed" = "leftskewed",
                      "Right-skewed" = "rightskewed",
                      "Symmetric" = "symmetric",
                      "Bimodal" = "bimodal",
                      "Astragalus (Bone Die)" =
                        "astragalus",
                      "Playlist" =
                        "Playlistshuffle",
                      "Accident Rate" = "poisson"
                    )
                  ),
                  conditionalPanel(
                    condition = "input.popDist=='leftskewed'",
                    sliderInput(
                      inputId = "leftskew",
                      label = "Skewness",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      step = 0.01,
                      ticks = FALSE
                    ),
                    div(style = "position: absolute; left: 0.5em; top: 9em", "min"),
                    div(style = "position: absolute; right: 0.5em; top: 9em", "max"),
                  ),
                  conditionalPanel(
                    condition = "input.popDist=='rightskewed'",
                    sliderInput(
                      inputId = "rightskew",
                      label = "Skewness",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      step = 0.01,
                      ticks = FALSE
                    ),
                    div(style = "position: absolute; left: 0.5em; top: 9em", "min"),
                    div(style = "position: absolute; right: 0.5em; top: 9em", "max")
                  ),
                  conditionalPanel(
                    condition = "input.popDist=='symmetric'",
                    sliderInput(
                      inputId = "inverse",
                      label = "Peakedness",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      step = 0.01,
                      ticks = FALSE
                    ),
                    div(style = "position: absolute; left: 0.5em; top: 9em", "U"),
                    div(style = "position: absolute; left: 0.5em; top: 10em", "Shaped"),
                    div(style = "position: absolute; right: 0.5em; top: 9em", "Bell"),
                    div(style = "position: absolute; right: 0.5em; top: 10em", "Shaped"),
                  ),
                  conditionalPanel(
                    condition = "input.popDist=='bimodal'",
                    sliderInput(
                      inputId = "prop",
                      label = "Percent under right mode",
                      min = 10,
                      max = 90,
                      value = 50,
                      ticks = FALSE,
                      post = "%"
                    )
                  ),
                  conditionalPanel(
                    condition = "input.popDist=='poisson'",
                    sliderInput(
                      inputId = "poissonmean",
                      label = "Mean",
                      min = 0.1,
                      max = 10,
                      value = 1,
                      step = 0.1
                    ),
                    conditionalPanel(
                      condition = "input.poissonmean==0",
                      "Note: When the mean is set to 0, the number of accidents is always 0, so the variance is 0."
                    )
                  ),
                  # Playlist shuffle
                  conditionalPanel(
                    condition = "input.popDist == 'Playlistshuffle'",
                    p("Number of songs:"),
                    numericInput(
                      inputId = "s1",
                      label = "Jazz",
                      value = 1,
                      min = 0,
                      max = 100,
                      step = 1,
                      width = "50%"
                    ),
                    numericInput(
                      inputId = "s2",
                      label = "Rock",
                      value = 1,
                      min = 0,
                      max = 200,
                      step = 1,
                      width = "50%"
                    ),
                    numericInput(
                      inputId = "s3",
                      label = "Country",
                      value = 1,
                      min = 0,
                      max = 200,
                      step = 1,
                      width = "50%"
                    ),
                    numericInput(
                      inputId = "s4",
                      label = "Hip-hop",
                      value = 1,
                      min = 0,
                      max = 200,
                      step = 1,
                      width = "50%"
                    )
                  )
                ),
                column(
                  6,
                  # left skewed
                  conditionalPanel(
                    condition = "input.popDist == 'leftskewed'",
                    sliderInput(
                      inputId = "leftsize",
                      label = "Sample size (n)",
                      min = 1,
                      max = 50,
                      value = 10
                    ),
                    sliderInput(
                      inputId = "leftreps",
                      label = "Number of reps",
                      min = 1,
                      max = 5000,
                      value = 1000
                    )
                  ),
                  conditionalPanel(
                    condition = "input.popDist == 'rightskewed'",
                    sliderInput(
                      inputId = "rightsize",
                      label = "Sample size (n)",
                      min = 1,
                      max = 50,
                      value = 10
                    ),
                    sliderInput(
                      inputId = "rightreps",
                      label = "Number of reps",
                      min = 1,
                      max = 5000,
                      value = 1000
                    )
                  ),
                  conditionalPanel(
                    condition = "input.popDist == 'symmetric'",
                    sliderInput(
                      inputId = "symsize",
                      label = "Sample size (n)",
                      min = 1,
                      max = 50,
                      value = 10
                    ),
                    sliderInput(
                      inputId = "symreps",
                      label = "Number of reps",
                      min = 1,
                      max = 5000,
                      value = 1000
                    )
                  ),
                  conditionalPanel(
                    condition = "input.popDist == 'astragalus'",
                    sliderInput(
                      inputId = "assize",
                      label = "Sample size (n)",
                      min = 1,
                      max = 50,
                      value = 10
                    ),
                    sliderInput(
                      inputId = "asreps",
                      label = "Number of reps",
                      min = 1,
                      max = 5000,
                      value = 1000
                    )
                  ),
                  conditionalPanel(
                    condition = "input.popDist == 'bimodal'",
                    sliderInput(
                      inputId = "bisize",
                      label = "Sample size (n)",
                      min = 1,
                      max = 50,
                      value = 10
                    ),
                    # choose the number of sample means
                    sliderInput(
                      inputId = "bireps",
                      label = "Number of reps",
                      min = 1,
                      max = 5000,
                      value = 1000
                    )
                  ),
                  conditionalPanel(
                    condition = "input.popDist == 'poisson'",
                    sliderInput(
                      inputId = "posize",
                      label = "Sample size (n)",
                      min = 1,
                      max = 50,
                      value = 10
                    ),
                    # choose the number of sample means
                    sliderInput(
                      inputId = "poreps",
                      label = "Number of reps",
                      min = 1,
                      max = 5000,
                      value = 1000
                    )
                  ),
                  conditionalPanel(
                    condition = "input.popDist == 'Playlistshuffle'",
                    sliderInput(
                      inputId = "Playlistsize",
                      label = "Sample size (n)",
                      min = 1,
                      max = 50,
                      value = 10
                    ),
                    # choose the number of sample means
                    sliderInput(
                      inputId = "Playlistreps",
                      label = "Number of reps",
                      min = 1,
                      max = 5000,
                      value = 1000
                    ),
                    radioButtons(
                      inputId = "ptype",
                      label = "Genre to track:",
                      list(
                        "Jazz",
                        "Rock",
                        "Country",
                        "Hip-hop"
                      ),
                      selected = "Jazz"
                    )
                  )
                )
              )
            ),
            mainPanel(
              width = 6,
              conditionalPanel(
                condition = "input.popDist == 'leftskewed'",
                plotOutput("plotleft1"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotleft1').setAttribute('aria-label',
                  `The population graph shows the density curve for a process
                  which has a left (or negative) skewness, making a long tail on
                  the left. You can control the amount of skewness with the
                  skewness slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'rightskewed'",
                plotOutput("plotright1"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotright1').setAttribute('aria-label',
                  `The population graph shows the density curve for a process
                  which has a right (or positive) skewness, making a long tail on
                  the right. You can control the amount of skewness with the
                  skewness slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'symmetric'",
                plotOutput("plotsymmetric1"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotsymmetric1').setAttribute('aria-label',
                  `The population graph shows the density curve for a process
                  which is symmetric. You can make the process generally produce
                  values at the two extremes (a u-shape), produce them uniformly
                  (rectangular) or concentrate values in the middle (bell-shaped).`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'astragalus'",
                plotOutput("pop"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('pop').setAttribute('aria-label',
                  `The population graph shows the probabilities for getting each
                  particular outcome when tossign the astragalus. A 1 and 6 each
                  occur 10% of the time, while 3 and 4 both occur 40% of the time.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'bimodal'",
                plotOutput("plotbimodal1"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotbimodal1').setAttribute('aria-label',
                  `The population graph shows the density curve for a process
                  which is bimodal. You may adjust what percentage occurs under
                  the right most mode with the slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'poisson'",
                plotOutput("poissonpop"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('poissonpop').setAttribute('aria-label',
                  `The population graph shows the probabilities for the number
                  of accidents which occur. You may control the mean (the unit
                   rate of accidents) with the mean slider.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'Playlistshuffle'",
                plotOutput("playlistPlot"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('playlistPlot').setAttribute('aria-label',
                  `The population graph shows the probability for listening to a
                  song that is in the genre you're opted to track. You may set
                  how many songs of each genre there are with the jazz, rock,
                  country, and hip-hop inputs. You may then choose which genre
                  to track with the genre to track input.`)
                  })"
                ))
              )
            )
          ),
          br(),
          hr(),
          h3("Samples"),
          ### Sample Results ----
          fluidRow(
            column(
              width = 6,
              conditionalPanel(
                condition = "input.popDist == 'leftskewed'",
                plotOutput("plotleft2"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotleft2').setAttribute('aria-label',
                  `This histogram displays one sample of values randomly selected
                  from the population. You may change the size of the sample using
                  the sample size (n) slider. Clicking the Show a new sample
                  button will generate a new sample.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'rightskewed'",
                plotOutput("plotright2"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotright2').setAttribute('aria-label',
                  `This histogram displays one sample of values randomly selected
                  from the population. You may change the size of the sample using
                  the sample size (n) slider. Clicking the Show a new sample
                  button will generate a new sample.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'symmetric'",
                plotOutput("plotsymmetric2"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotsymmetric2').setAttribute('aria-label',
                  `This histogram displays one sample of values randomly selected
                  from the population. You may change the size of the sample using
                  the sample size (n) slider. Clicking the Show a new sample
                  button will generate a new sample.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'astragalus'",
                plotOutput("line2"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotline2').setAttribute('aria-label',
                  `This histogram displays one sample of values randomly selected
                  from the population. You may change the size of the sample using
                  the sample size (n) slider. Clicking the Show a new sample
                  button will generate a new sample.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'bimodal'",
                plotOutput("plotbimodal2"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotbimodal2').setAttribute('aria-label',
                  `This histogram displays one sample of values randomly selected
                  from the population. You may change the size of the sample using
                  the sample size (n) slider. Clicking the Show a new sample
                  button will generate a new sample.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist == 'poisson'",
                plotOutput("plotpoisson1"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotpoisson1').setAttribute('aria-label',
                  `This histogram displays one sample of values randomly selected
                  from the population. You may change the size of the sample using
                  the sample size (n) slider. Clicking the Show a new sample
                  button will generate a new sample.`)
                  })"
                ))
              ),
              conditionalPanel(
                condition = "input.popDist =='Playlistshuffle'",
                plotOutput("playlistSampleMean"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('playlistSampleMean').setAttribute('aria-label',
                  `This histogram displays one sample of values randomly selected
                  from the population. You may change the size of the sample using
                  the sample size (n) slider. Clicking the Show a new sample
                  button will generate a new sample.`)
                  })"
                ))
              )
            ),
            column(
              width = 6,
              conditionalPanel(
                condition = "input.popDist == 'leftskewed'",
                plotOutput("plotleft3"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotleft3').setAttribute('aria-label',
                  `This histogram displays the collection of values of the sample
                  arithmetic mean when you repeatedly draw new samples. Each
                  sample has the same sample size which you may set using the
                  sample size (n) slider. The number of reps slider sets how many
                  times you repeated draw samples and calculate the value of the
                  sample arithmetic mean.`)
                  })"
                ))
              ),
              bsPopover(
                id = "plotleft3",
                title = "All Samples Histogram",
                content = "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                trigger = "hover",
                placement = "top"
              ),
              conditionalPanel(
                condition = "input.popDist == 'rightskewed'",
                plotOutput("plotright3"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotright3').setAttribute('aria-label',
                  `This histogram displays the collection of values of the sample
                  arithmetic mean when you repeatedly draw new samples. Each
                  sample has the same sample size which you may set using the
                  sample size (n) slider. The number of reps slider sets how many
                  times you repeated draw samples and calculate the value of the
                  sample arithmetic mean.`)
                  })"
                ))
              ),
              bsPopover(
                id = "plotright3",
                title = "All Samples Histogram",
                content = "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                trigger = "hover",
                placement = "top"
              ),
              conditionalPanel(
                condition = "input.popDist == 'symmetric'",
                plotOutput("plotsymmetric3"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotsymmetric3').setAttribute('aria-label',
                  `This histogram displays the collection of values of the sample
                  arithmetic mean when you repeatedly draw new samples. Each
                  sample has the same sample size which you may set using the
                  sample size (n) slider. The number of reps slider sets how many
                  times you repeated draw samples and calculate the value of the
                  sample arithmetic mean.`)
                  })"
                ))
              ),
              bsPopover(
                id = "plotsymmetric3",
                title = "All Samples Histogram",
                content = "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                trigger = "hover",
                placement = "top"
              ),
              conditionalPanel(
                condition = "input.popDist == 'astragalus'",
                plotOutput("line1"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotline1').setAttribute('aria-label',
                  `This histogram displays the collection of values of the sample
                  arithmetic mean when you repeatedly draw new samples. Each
                  sample has the same sample size which you may set using the
                  sample size (n) slider. The number of reps slider sets how many
                  times you repeated draw samples and calculate the value of the
                  sample arithmetic mean.`)
                  })"
                ))
              ),
              bsPopover(
                id = "line1",
                title = "All Samples Histogram",
                content = "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                trigger = "hover",
                placement = "top"
              ),
              conditionalPanel(
                condition = "input.popDist == 'bimodal'",
                plotOutput("plotbimodal3"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotbimodal3').setAttribute('aria-label',
                  `This histogram displays the collection of values of the sample
                  arithmetic mean when you repeatedly draw new samples. Each
                  sample has the same sample size which you may set using the
                  sample size (n) slider. The number of reps slider sets how many
                  times you repeated draw samples and calculate the value of the
                  sample arithmetic mean.`)
                  })"
                ))
              ),
              bsPopover(
                id = "plotbimodal3",
                title = "All Samples Histogram",
                content = "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                trigger = "hover",
                placement = "top"
              ),
              conditionalPanel(
                condition = "input.popDist == 'poisson'",
                plotOutput("plotpoisson2"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('plotpoisson2').setAttribute('aria-label',
                  `This histogram displays the collection of values of the sample
                  arithmetic mean when you repeatedly draw new samples. Each
                  sample has the same sample size which you may set using the
                  sample size (n) slider. The number of reps slider sets how many
                  times you repeated draw samples and calculate the value of the
                  sample arithmetic mean.`)
                  })"
                ))
              ),
              bsPopover(
                id = "plotpoisson2",
                title = "All Samples Histogram",
                content = "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                trigger = "hover",
                placement = "top"
              ),
              conditionalPanel(
                condition = "input.popDist =='Playlistshuffle'",
                plotOutput("allSamplePlaylist"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('allSamplePlaylist').setAttribute('aria-label',
                  `This histogram displays the collection of values of the sample
                  arithmetic mean when you repeatedly draw new samples. Each
                  sample has the same sample size which you may set using the
                  sample size (n) slider. The number of reps slider sets how many
                  times you repeated draw samples and calculate the value of the
                  sample arithmetic mean.`)
                  })"
                )),
                bsPopover(
                  id = "allSamplePlaylist",
                  title = "All Samples Histogram",
                  content = "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                  trigger = "hover",
                  placement = "top"
                )
              )
            )
          ),
          bsButton(
            inputId = "new",
            label = "Show a new sample",
            icon = icon("retweet"),
            size = "large"
          )
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J.,Allaire, JJ , Xie, Y. and McPherson, J. (2020).
            shiny: Web Application Framework for R. R package version 1.4.0.2.
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Ribeiro, B.B. (2018). shinydashboard: Create Dashboards
            with 'Shiny'. R package version 0.7.1.
            Available from https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Bailey, D. (2015). shinyBS: Twitter Bootstrap Components for Shiny.
            R package version 0.61. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            " Perrier, V., Meyer, F. and Granjon, D. (2020). shinyWidgets: Custom
            Inputs Widgets for Shiny. R package version 0.5.2.
            Available from https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Penn State University. 3.2 - Graphs: Displaying Measurement Data:
            STAT 100. Penn State: Statistics Online Courses. Available from
            https://online.stat.psu.edu/stat100/lesson/3/3.2."
          ),
          p(
            class = "hangingindent",
            " Wickham, H. ggplot2: Elegant Graphics for Data Analysis.
            Springer-Verlag New York, 2016."
          ),
          p(
            class = "hangingindent",
            "Boos, D.D. and Nychka, D. (2012). Rlab: Functions and Datasets
            Required for ST370 class. R package version 2.15.1.
            Available from https://CRAN.R-project.org/package=Rlab"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, H., Henry, L. and Müller, K. (2020). dplyr:
            A Grammar of Data Manipulation. R package version 0.8.5.
            Available from https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Yaan. (2007). Shagai. Wikimedia.
            Available from https://commons.wikimedia.org/wiki/File:Shagai.jpg."
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(session, input, output) {
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

  # Go Button
  observeEvent(input$go, {
    updateTabItems(session, "pages", "largeNumber")
  })

  # Function to create Histogram of a Single Samples
  # Inputs: matrix that gives values to plot, number of bins to use
  # Output: ggplot of Histogram of a Single Sample
  singleSample <- function(matrix, reps, bw = 1) {
    ggplot(data = data.frame(gg = matrix[sample(min(50, reps), 1, replace = FALSE), ]), aes(x = gg)) +
      geom_histogram(aes(y = ..density..),
        binwidth = bw,
        fill = "lightblue",
        col = "black",
      ) +
      xlab("Individual value") +
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
  allSample <- function(vector, reps) {
    ggplot(data = data.frame(gg = vector), aes(x = gg)) +
      geom_histogram(aes(y = ..density..),
        bins = reps,
        fill = "lightblue",
        col = "black"
      ) +
      xlab("Sample mean") +
      ylab("Density") +
      ggtitle("All Samples Histogram") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black")
      ) +
      stat_function(
        fun = dnorm,
        args = list(mean = mean(vector), sd = sd(vector)),
        color = "blue",
        lwd = 1
      )
  }


  # Function to create density plots for each group
  # Inputs: Dataframe consisting of columns x and y to define axes, limits for x axis in form c(lower, upper), optional path for symmetric case
  # Output: ggplot of density
  makeDensityPlot <- function(data, xlims, path = 0) {
    plot <- ggplot2::ggplot(aes(x = x, y = y), data = data) +
      geom_path(color = "#0072B2", size = 1.5) +
      xlim(xlims) +
      xlab("Value") +
      ylab("Density") +
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
        geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1), color = "#0072B2", size = 1.5) +
        geom_segment(aes(x = 1, y = 0, xend = 1, yend = 1), color = "#0072B2", size = 1.5)
    }
    plot
  }

  # Make the make bar plot function
  makeBarPlot <-
    function(xlab, ggtitle2, data, levels = as.character(data$x), color = "#0072B2", color2 = "#0072B2") {
      plot <-
        ggplot(aes(x = factor(x, levels = levels), y = y), data = data) +
        geom_bar(
          stat = "identity",
          fill = color,
          col = color2
        ) +
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
  leftSkew <- reactive({
    11 - 10 * input$leftskew
  })

  # Population of left skewed
  output$plotleft1 <- renderPlot({
    # Define parameters for density plot
    x <- seq((leftSkew()) - 9 * sqrt((leftSkew())), 0, length = 100)
    y <- dgamma(-x, shape = (leftSkew()), beta = 1)
    data <- data.frame(x = x, y = y)

    # Make Density Plot
    makeDensityPlot(data = data, xlims = c((leftSkew()) - 9 * sqrt((leftSkew())), 0))
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
        matrix.means[i, j] <- mean(matrix[i, 1:j])
      }
    }
    fifmeans <- as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first 50 means with the rest of data
  data1 <- reactive({
    datameans <- firstfif1()
    for (i in 1:(input$leftreps - 50)) {
      datameans <- append(datameans, mean(
        -rgamma(
          n = input$leftsize,
          shape = 11 - 10 * input$leftskew,
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
  rightSkew <- reactive({
    11 - 10 * input$rightskew
  })
  # Population of right skewed
  output$plotright1 <- renderPlot({
    # Define parameters for density plot
    x <- seq(0, (rightSkew()) + 9 * sqrt(rightSkew()), length = 100)
    y <- dgamma(x, shape = (rightSkew()), beta = 1)
    data <- data.frame(x = x, y = y)

    # Make the density plot
    makeDensityPlot(data = data, xlims = c(0, (rightSkew()) + 9 * sqrt((rightSkew()))))
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
        matrix.means[i, j] <- mean(matrix[i, 1:j])
      }
    }

    fifmeans <- as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first 50 means with the rest of data
  data2 <- reactive({
    datameans <- firstfif2()
    for (i in 1:(input$rightreps - 50)) {
      datameans <- append(datameans, mean(
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
    round(
      14.6 * input$inverse^3 - 5.7 * input$inverse^2 + input$inverse + .1,
      3
    )
  })

  # Population of Symmetric skewed
  output$plotsymmetric1 <- renderCachedPlot(
    {
      x <- seq(0, 1, length = 100)
      dens <-
        dbeta(x,
          shape1 = inverse(),
          shape2 = inverse()
        )
      data <- data.frame(x = x, y = dens)

      # Make density plot separated by case where the peakedness is exactly 1 (causes a "box" shape)
      makeDensityPlot(
        data = data,
        xlims = c(-0.03, 1.03),
        path = inverse()
      )
    },
    cacheKeyExpr = {
      list(input$symsize, input$inverse)
    }
  )

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
        matrix.means[i, j] <- mean(matrix[i, 1:j])
      }
    }

    fifmeans <- as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first 50 means with the rest of data
  data3 <- reactive({
    datameans <- firstfif3()
    for (i in 1:(input$symreps - 50)) {
      datameans <- append(datameans, mean(
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
      data.frame(
        x = seq(0, 5, t * 5),
        y = prop() * leftdraw + (1 - prop()) * rightdraw
      )

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
      else {
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
      else {
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
        matrix.means[i, j] <- mean(matrix[i, 1:j])
      }
    }

    fifmeans <- as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first fif means with the rest of data
  data4 <- reactive({
    datameans <- firstfif4()
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
        else {
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
  
  output$poissonpop <- renderCachedPlot(
    {
      data <- data.frame(x = 0:ceiling(2 * input$poissonmean + 5)) # More x's than necessary
      data$y <- (input$poissonmean^data$x) * exp(-input$poissonmean) / factorial(data$x) # Get y vals for x's
      data <- rbind(data[1:2, ], filter(data[-c(1, 2), ], y > .0005)) # Filter based on probability

      makeBarPlot(xlab = "Number of accidents", ggtitle = "Population Graph", data = data)
    },
    cacheKeyExpr = {
      list(input$poissonmean)
    }
  )

  # Matrix for first 50 reps of data
  firstfifData5 <- reactive(matrix(
    rpois(
      50 * input$posize,
      input$poissonmean
    ),
    nrow = 50,
    ncol = input$posize
  ))

  # Write the mean of first 50 data into vector
  firstfif5 <- reactive({
    matrix <- firstfifData5()
    matrix.means <- matrix(0, nrow = 50, ncol = input$posize)
    for (i in 1:50) {
      for (j in 1:input$posize) {
        matrix.means[i, j] <- mean(matrix[i, 1:j])
      }
    }

    fifmeans <- as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first 50 means with the rest of data
  data5 <- reactive({
    datameans <- firstfif5()
    for (i in 1:(input$poreps - 50)) {
      datameans <- append(datameans, mean(rpois(
        input$posize,
        input$poissonmean
      )))
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
    data <- data.frame(x = c(1, 3, 4, 6), y = c(.1, .4, .4, .1))
    makeBarPlot(xlab = "Number on roll of astragalus", ggtitle2 = "Population Graph", data = data, levels = 1:6)
  })

  # All Sample Histogram
  output$line1 <- renderPlot({
    N <- input$asreps
    res <- vector("numeric", N)
    res
    for (i in 1:input$asreps) {
      numbers <- sample(x = c(1, 3, 4, 6), size = input$assize, replace = TRUE, prob = c(.1, .4, .4, .1))
      c1 <- sum(numbers == 1) / input$assize
      c2 <- sum(numbers == 3) / input$assize
      c3 <- sum(numbers == 4) / input$assize
      c4 <- sum(numbers == 6) / input$assize
      c5 <- c1 + c2 * 3 + c3 * 4 + c4 * 6
      res[i] <- c5
    }

    res <- data.frame(gg = res)
    x <- seq(1, 6, length.out = 50)
    curv <- with(res, data.frame(x = x, y = dnorm(x, mean(gg), sd(gg))))

    p <- ggplot(data = res, aes(x = gg)) +
      geom_histogram(aes(y = ..density..),
        binwidth = 0.1,
        fill = "lightblue",
        col = "black",
      ) +
      xlab("Sample mean") +
      ggtitle("All Samples Histogram") +
      theme(
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = "black")
      ) +
      geom_line(data = curv, aes(x = x, y = y), color = "blue", lwd = 1)
    p
  })

  # One Sample Histogram
  output$line2 <- renderPlot({
    input$new
    numbers <- sample(x = c(1, 3, 4, 6), size = input$assize, replace = TRUE, prob = c(.1, .4, .4, .1))
    c1 <- sum(numbers == 1) / input$assize
    c2 <- sum(numbers == 3) / input$assize
    c3 <- sum(numbers == 4) / input$assize
    c4 <- sum(numbers == 6) / input$assize
    firstfifData6 <- data.frame(x = c(1, 3, 4, 6), y = c(c1, c2, c3, c4))
    makeBarPlot(
      xlab = "Individual value",
      ggtitle2 = "Histogram of a Single Sample",
      data = firstfifData6,
      levels = 1:6, color = "lightblue", color2 = "black"
    )
  })

  ###################################################################
  ## Playlist SHUFFLE
  ####################################################################

  # set up songs from four types
  songs <- reactive({
    songs <- c(
      rep(input$s1),
      rep(input$s2),
      rep(input$s3),
      rep(input$s4)
    )
  })

  # Reactive expression to get the number of songs of the chosen type
  nSongs <- reactive({
    if (input$ptype == "Jazz") {
      nSongs <- input$s1
    } else if (input$ptype == "Rock") {
      nSongs <- input$s2
    } else if (input$ptype == "Country") {
      nSongs <- input$s3
    } else {
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
  ############################################
  
  # Jazz population plot
  output$playlistPlot <- renderPlot({
    p <- nSongs() / sum(songs())
    data <- data.frame(x = c("Other music (0)", paste(input$ptype, "(1)")), y = c(1 - p, p))
    data$x <- factor(data$x, levels = data$x) # Done to force sorted order for bars

    # Make bar plot
    makeBarPlot(xlab = "Genre", data = data, ggtitle2 = "Population Graph")
  })


  ############################################
  # Average Plot with 4 categories songs
  ############################################

  # Matrix of Songs from 4 types

  ############################################
  ## Jazz
  ############################################
  
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
      matrix(0, nrow = 50, ncol = input$Playlistsize) # matrix with all zeroes
    for (i in 1:50) {
      for (j in 1:input$Playlistsize) {
        matrix.means[i, j] <- mean(matrix[i, 1:j])
      }
    }
    fifmeans <- as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first fif means with the rest of data
  Jazzdata <- reactive({
    datameans <- firstfifJazz()
    for (i in 1:(input$Playlistreps - 50)) {
      datameans <- append(datameans, mean(rbinom(
        input$Playlistsize,
        size = 1,
        prob = input$s1 / sum(songs())
      )))
    }
    return(datameans)
  })

  ############################################
  ## Rock
  ############################################
  
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
        matrix.means[i, j] <- mean(matrix[i, 1:j])
      }
    }

    fifmeans <- as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first fif means with the rest of data
  Rockdata <- reactive({
    datameans <- firstfifRock()
    for (i in 1:(input$Playlistreps - 50)) {
      datameans <- append(datameans, mean(rbinom(
        input$Playlistsize,
        size = 1,
        prob = input$s2 / sum(songs())
      )))
    }
    return(datameans)
  })

  ############################################
  ## Country
  ############################################
  
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
        matrix.means[i, j] <- mean(matrix[i, 1:j])
      }
    }

    fifmeans <- as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first fif means with the rest of data
  Countrydata <- reactive({
    datameans <- firstfifCountry()
    for (i in 1:(input$Playlistreps - 50)) {
      datameans <- append(datameans, mean(rbinom(
        input$Playlistsize,
        size = 1,
        prob = input$s3 / sum(songs())
      )))
    }
    return(datameans)
  })

  ############################################
  ## Hiphop
  ############################################
  
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
        matrix.means[i, j] <- mean(matrix[i, 1:j])
      }
    }

    fifmeans <- as.vector(matrix.means)
    return(fifmeans)
  })

  # Merge the first fif means with the rest of data
  Hiphopdata <- reactive({
    datameans <- firstfifHiphop()
    for (i in 1:(input$Playlistreps - 50)) {
      datameans <- append(datameans, mean(rbinom(
        input$Playlistsize,
        size = 1,
        prob = input$s4 / sum(songs())
      )))
    }
    return(datameans)
  })

  firstFif <- reactive({
    if (input$ptype == "Jazz") {
      firstfifDataJazz()
    } else if (input$ptype == "Rock") {
      firstfifDataRock()
    } else if (input$ptype == "Country") {
      firstfifDataCountry()
    } else {
      firstfifDataHiphop()
    }
  })

  output$playlistSampleMean <- renderPlot({
    p <- mean(firstFif()[sample(min(50, input$Playlistreps), 1, replace = FALSE), ])
    input$new
    data <- data.frame(x = c("Other music (0)", paste(input$ptype, "(1)")), y = c(1 - p, p))
    data$x <- factor(data$x, levels = data$x) # Done to force sorted order for bars

    # Make bar plot
    makeBarPlot(xlab = "Genre", data = data, ggtitle2 = "Histogram of a Single Sample", color = "lightblue", color2 = "black")
  })

  ############################################
  ## Sum Plot with 4 categories songs
  ############################################
  
  # Make the sum plot for playlist
  makePlaylistSumPlot <- function(s, data) {
    if (s == 0) {
      vector <- data
      allSample(vector, min(80, input$Playlistreps))
    } else {
      n <- input$Playlistsize
      vector <-
        rbinom(input$Playlistreps * n,
          size = n,
          prob = s / sum(songs())
        )
      vector_mean <- c()
      for (i in 1:input$Playlistreps * n) {
        vector_mean <- append(vector_mean, vector[i] / n)
      }
      allSample(vector_mean, 10)
    }
  }

  # All sample playlist plot
  output$allSamplePlaylist <- renderPlot({
    if (input$ptype == "Jazz") {
      makePlaylistSumPlot(input$s1, Jazzdata())
    }
    else if (input$ptype == "Rock") {
      makePlaylistSumPlot(input$s2, Rockdata())
    }
    else if (input$ptype == "Country") {
      makePlaylistSumPlot(input$s3, Countrydata())
    }
    else {
      makePlaylistSumPlot(input$s4, Hiphopdata())
    }
  })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
