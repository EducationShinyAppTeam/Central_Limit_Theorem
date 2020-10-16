library(shiny)
library(shinydashboard)
library(shinyBS)


# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = 'Central Limit Theorem',
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Central_Limit_Theorem"
        )
      ),
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
        menuItem("Explore",icon = icon("wpexplorer"),tabName = "largeNumber"),
        menuItem("References", tabName = "References", icon = icon("leanpub")
        )),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet",
                  type = "text/css",
                  href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
      ),
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
              shuffle, or accident occurrence)."),
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
            in 2019 and by Jiawei Wu and Leah Hunt in 2020."
            ),
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
            tags$li("This app uses four continuous distributions: right (positive)
                   skewed, left (negative) skewed, symmetric, and bimodal. While
                  in depth understanding of these distributions is not required,
                  you may wish to review this ",
                    tags$a(href="https://online.stat.psu.edu/stat100/lesson/3/3.2#graphshapes",
                           "Stat 100 Table of Graph Shapes"),
                    ".",),
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
              img(src = "astragalus.jpg",
                  alt = "Picture of an astragalus",
                  width = "50%",
                  style = "text-align: center;"),
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
            estimate of the sampling distribution."
          ),
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
                  #Playlist shuffle
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
                      width = '50%'
                    ),
                    numericInput(
                      inputId = "s2",
                      label = "Rock",
                      value = 1,
                      min = 0,
                      max = 200,
                      step = 1,
                      width = '50%'
                    ),
                    numericInput(
                      inputId = "s3",
                      label = "Country",
                      value = 1,
                      min = 0,
                      max = 200,
                      step = 1,
                      width = '50%'
                    ),
                    numericInput(
                      inputId = "s4",
                      label = "Hip-hop",
                      value = 1,
                      min = 0,
                      max = 200,
                      step = 1,
                      width = '50%'
                    )
                  )
                ),
                column(
                  6,
                  #left skewed
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
                    #choose the number of sample means
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
                    #choose the number of sample means
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
                    #choose the number of sample means
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
                      list("Jazz",
                           "Rock",
                           "Country",
                           "Hip-hop"),
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
                plotOutput('plotleft1'),
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
                plotOutput('plotright1'),
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
                plotOutput('plotsymmetric1'),
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
                plotOutput('plotbimodal1'),
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
                plotOutput('poissonpop'),
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
                plotOutput('plotleft2'),
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
                plotOutput('plotright2'),
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
                plotOutput('plotsymmetric2'),
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
                plotOutput('plotbimodal2'),
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
                plotOutput('plotpoisson1'),
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
                plotOutput('plotleft3'),
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
                plotOutput('plotright3'),
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
                plotOutput('plotsymmetric3'),
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
                plotOutput('plotbimodal3'),
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
                plotOutput('plotpoisson2'),
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