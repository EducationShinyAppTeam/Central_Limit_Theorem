library(shiny)
library(shinydashboard)
library(plotrix)
library(shinythemes)
library(shinyBS)
library(shinyWidgets)



# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = 'Central Limit Theorem',
    titleWidth=250,
    tags$li(class = "dropdown", actionLink("info", icon("info"))),
    tags$li(class = "dropdown",
            tags$a(href='https://github.com/EducationShinyAppTeam/BOAST',
                   icon("github"))),
    tags$li(class = "dropdown",
            tags$a(href='https://shinyapps.science.psu.edu/',
                   icon("home")))
  ),
  dashboardSidebar(
    width=250,
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
      tags$link(rel = "stylesheet", type = "text/css", href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
    ),
    tabItems(
      tabItem(
        tabName = "Overview",
        # tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
        fluidPage(
          
          h1("Central Limit Theorem"),
          p(
            "This app is designed to examine the Central Limit Theorem under different population distributions and sample sizes."
            
          ),
          p("The Central Limit Theorem tells us when the sample size increases, the all sample histogram will become normal distribution."),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li(
              "Pick a population from one of the continuous types (left-skewed; right-skewed; symmetric; or bimodal) or one of the
                                 discrete examples (rolls of an astragalus; random songs from an iPod shuffle; or accident occurrence)."
            ),
            
            
            tags$li(
              "Use the sliders to adjust the parameters of the population model you chosen."
            ),
            
            
            tags$li(
              "Use the sliders to decide the sample size (n) you want for each sample and how many reps you will repeat the process
                                 of taking samples from the population (calculating \\(\\bar{x}\\) for each)."
            ),
            
            
            tags$li("Use the button to see one of the individual samples chossen."),
            
            
            tags$li(
              "Observe the histograms of one sample and for \\(\\bar{x}\\) from all samples."
            ),
          ),
          div(style = "text-align: center",
              bsButton("go", "Go!", icon("bolt"), class =
                         "circle grow")),
          br(),
          h2("Acknowledgements:"),
          p(
            "This app was developed and coded by Yingjie (Chelsea) Wang with the help of Yubaihe Zhou. Selection of population densities was originally coded by Caihui Xiao.
            This application was modified by Zhiruo Wang in 2019 and by Jiawei Wu in 2020."
          ),
          
          div(class = "updated", "Last Update: 06/22/2020 by WJW.")
          
        )
      ),
      
      #### Set up the Prerequisites Page
      tabItem(
        tabName = "Prerequisites",
        h2("Prerequisites"),
        
        p(
          "In order to get the most out of this app, please review the
            following information that will be used in the app."
        ),
        
        
        #https://online.stat.psu.edu/stat100/lesson/3/3.2
        #"https://online.stat.psu.edu/stat200/lesson/2/2.2/2.2.3"
        
        tags$ul(
          tags$li("This app uses four distributions: right (positive) skewed, left (negative) skewed, symmetric, and bimodal.
          While in depth understanding of these distributions is not required, you may wish to review this ",
                  tags$a(href="https://online.stat.psu.edu/stat100/lesson/3/3.2#graphshapes", "Stat 100 Table of Graph Shapes"),
                  ".",),
          tags$li("One of the distributions is based upon rolls of an astragalus. The astragalus (ankle or heel bone) of animals were used in ancient times as a forerunner of modern dice. 
                  When a sheep astragalus is thrown into the air it can land on one of four sides, which were associated with the numbers 
                  1, 3, 4, and 6. Two sides (the 3 and the 4) are wider and each come up about 40% of the time, while the narrower sides 
                  (the 1 and the 6) each come up about 10% of the time. An image of an astralagus is shown below.")
        ),
        HTML('<center><figure><img src="astragalus.jpg" alt="Picture of an astragalus" width="600"><figcaption>image by Yaan, 2007</figcaption></figure></center>'),
      ),
      
      
      
      
      
      # Explore Law of Large Numbers Tab
      tabItem(
        tabName = "largeNumber",
        
        tags$head(tags$style(
          HTML("input[type=\"number\"] {width: 60px;}")
        )),
        h2("Central Limit Theorem"),
        p("In this section, you will have the chance to explore the Central Limit Theorem. 
          To do so, first choose a population to sample from, a number of samples to take, 
          and repetitions. Then observe the graphs of Histogram of values in a single sample
          and all sample histogram to see the CLT."),
        
        sidebarLayout(
          sidebarPanel(
            width=6,
            fluidRow(
              column(
                6,
                selectInput(
                  "popDist",
                  "Population Type",
                  list(
                    "Left-skewed" = "leftskewed",
                    "Right-skewed" = "rightskewed",
                    "Symmetric" = "symmetric",
                    "Bimodal" = "bimodal",
                    "Astragalus" =
                      "astragalus",
                    "IPod Shuffle" =
                      "ipodshuffle",
                    "Accident Rate" = "poisson"
                  )
                ),
                
                conditionalPanel(
                  condition = "input.popDist=='leftskewed'",
                  sliderInput(
                    "leftskew",
                    " Skewness:",
                    min = 0,
                    max = 1,
                    value = 0.5,
                    step = 0.01,
                    ticks = FALSE
                  ),
                  #fluidRow(
                  #column(width=8,"min"),
                  #column(width=9, ""),
                  #column(width=2,"max")),
                  div(style = "position: absolute; left: 0.5em; top: 9em", "min"),
                  div(style = "position: absolute; right: 0.5em; top: 9em", "max"),
                  br(),
                  br(),
                  actionButton("new1", "show one of the samples", icon("retweet"), style =
                                 "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                conditionalPanel(
                  condition = "input.popDist=='rightskewed'",
                  sliderInput(
                    "rightskew",
                    " Skewness:",
                    min = 0,
                    max = 1,
                    value = 0.5,
                    step = 0.01,
                    ticks = FALSE
                  ),
                  #fluidRow(
                  #column(width=8,"min"),
                  #column(width=9, ""),
                  #column(width=2,"max")),
                  div(style = "position: absolute; left: 0.5em; top: 9em", "min"),
                  div(style = "position: absolute; right: 0.5em; top: 9em", "max"),
                  br(),
                  br(),
                  actionButton("new2", "show one of the samples", icon("retweet"), style =
                                 "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                conditionalPanel(
                  condition = "input.popDist=='symmetric'",
                  sliderInput(
                    "inverse",
                    " Peakedness:",
                    min = 0,
                    max = 1,
                    value = 0.5,
                    step = 0.01,
                    ticks = FALSE
                  ),
                  #fluidRow(
                  #column(width=8,"U-shaped"),
                  #column(width=9, ""),
                  #column(width=2,"Bell-shaped")),
                  div(style = "position: absolute; left: 0.5em; top: 9em", "U"),
                  div(style = "position: absolute; left: 0.5em; top: 10em", "Shaped"),
                  div(style = "position: absolute; right: 0.5em; top: 9em", "Bell"),
                  div(style = "position: absolute; right: 0.5em; top: 10em", "Shaped"),
                  br(),
                  br(),
                  br(),
                  actionButton("new3", "show one of the samples", icon("retweet"), style =
                                 "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                conditionalPanel(
                  condition = "input.popDist=='bimodal'",
                  
                  sliderInput(
                    "prop",
                    "% under right mode:",
                    min = 10,
                    max = 90,
                    value = 50,
                    ticks = FALSE,
                    post = "%"
                  ),
                  
                  
                  #fluidRow(
                  #column(width=8,"min"),
                  #column(width=9, ""),
                  #column(width=2,"max")),
                  
                  
                  actionButton("new4", "show one of the samples", icon("retweet"), style =
                                 "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                conditionalPanel(
                  condition = "input.popDist=='poisson'",
                  
                  sliderInput(
                    "poissonmean",
                    "Mean:",
                    min = 0.1,
                    max = 10,
                    value = 1,
                    step = 0.1
                  ),
                  conditionalPanel(
                    condition = "input.poissonmean==0",
                    "Note: When the mean is set to 0, the number of accidents is always 0, so the variance is 0."
                  ),
                  
                  actionButton("new5", "show one of the samples", icon("retweet"), style =
                                 "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                
                conditionalPanel(
                  condition = "input.popDist=='astragalus'",
                  
                  #sliderInput("poissonmean","Mean:", min = 0.1, max = 10, value = 1, step= 0.1),
                  
                  actionButton("new6", "show one of the samples", icon("retweet"), style =
                                 "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                
                #iPod shuffle
                conditionalPanel(
                  condition = "input.popDist == 'ipodshuffle'",
                  column(
                    width = 7,
                    offset = 0,
                    radioButtons(
                      "ptype",
                      "Category to follow:",
                      list("Jazz",
                           "Rock",
                           "Country",
                           "Hip-hop"),
                      selected = "Jazz"
                    ),
                    
                    column(
                      4,
                      offset = 0,
                      
                      numericInput(
                        "s1",
                        "Jazz",
                        1,
                        min = 0,
                        max = 100,
                        step = 1,
                        width = '10%'
                      ),
                      numericInput(
                        "s2",
                        "Rock",
                        1,
                        min = 0,
                        max = 200,
                        step = 1
                      ),
                      numericInput(
                        "s3",
                        "Country",
                        1,
                        min = 0,
                        max = 200,
                        step = 1
                      ),
                      numericInput(
                        "s4",
                        "Hip-hop",
                        1,
                        min = 0,
                        max = 200,
                        step = 1
                      )
                    )
                  ),
                  column(
                    width = 5,
                    helpText('P of Jazz'),
                    verbatimTextOutput("Jazz_percent"),
                    helpText('P of Rock '),
                    verbatimTextOutput("Rock_percent")
                  ),
                  column(
                    width = 5,
                    helpText('P of Country'),
                    verbatimTextOutput("Country_percent"),
                    helpText('P of Hip-hop'),
                    verbatimTextOutput("Hiphop_percent")
                  )
                )
                
              ),
              column(
                6,
                #left skewed
                conditionalPanel(
                  condition = "input.popDist == 'leftskewed'",
                  sliderInput(
                    "leftsize",
                    "sample size (n)",
                    min = 1,
                    max = 50,
                    value = 10
                  ),
                  sliderInput(
                    "leftreps",
                    "# of reps",
                    min = 1,
                    max = 5000,
                    value = 1000
                  )
                  
                ),
                conditionalPanel(
                  condition = "input.popDist == 'rightskewed'",
                  sliderInput(
                    "rightsize",
                    "sample size (n)",
                    min = 1,
                    max = 50,
                    value = 10
                  ),
                  
                  sliderInput(
                    "rightreps",
                    "# of reps",
                    min = 1,
                    max = 5000,
                    value = 1000
                  )
                ),
                
                conditionalPanel(
                  condition = "input.popDist == 'symmetric'",
                  
                  sliderInput(
                    "symsize",
                    "sample size (n)",
                    min = 1,
                    max = 50,
                    value = 10
                  ),
                  
                  sliderInput(
                    "symreps",
                    "# of reps",
                    min = 1,
                    max = 5000,
                    value = 1000
                  )
                  
                ),
                conditionalPanel(
                  condition = "input.popDist == 'astragalus'",
                  sliderInput(
                    "assize",
                    "sample size (n)",
                    min = 1,
                    max = 50,
                    value = 10
                  ),
                  
                  sliderInput(
                    "asreps",
                    "# of reps",
                    min = 1,
                    max = 5000,
                    value = 1000
                  )
                  
                ),
                conditionalPanel(
                  condition = "input.popDist == 'bimodal'",
                  
                  sliderInput(
                    "bisize",
                    "sample size (n)",
                    min = 1,
                    max = 50,
                    value = 10
                  ),
                  #choose the number of sample means
                  sliderInput(
                    "bireps",
                    "# of reps",
                    min = 1,
                    max = 5000,
                    value = 1000
                  )
                  
                ),
                conditionalPanel(
                  condition = "input.popDist == 'poisson'",
                  sliderInput(
                    "posize",
                    "sample size (n)",
                    min = 1,
                    max = 50,
                    value = 10
                  ),
                  #choose the number of sample means
                  sliderInput(
                    "poreps",
                    "# of reps",
                    min = 1,
                    max = 5000,
                    value = 1000
                  )
                  
                ),
                conditionalPanel(
                  condition = "input.popDist == 'ipodshuffle'",
                  sliderInput(
                    "ipodsize",
                    "sample size (n)",
                    min = 1,
                    max = 50,
                    value = 10
                  ),
                  #choose the number of sample means
                  sliderInput(
                    "ipodreps",
                    "# of reps",
                    min = 1,
                    max = 5000,
                    value = 1000
                  ),
                  actionButton("new7", "show one of the samples", icon("retweet"), style =
                                 "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                )
              )
            )
          ),
          mainPanel(
            width=6,
            conditionalPanel(condition = "input.popDist == 'leftskewed'",
                             plotOutput('plotleft1')),
            
            conditionalPanel(condition = "input.popDist == 'rightskewed'",
                             plotOutput('plotright1')),
            conditionalPanel(condition = "input.popDist == 'symmetric'",
                             plotOutput('plotsymmetric1')),
            conditionalPanel(condition = "input.popDist == 'astragalus'",
                             plotOutput("pop")),
            conditionalPanel(condition = "input.popDist == 'bimodal'",
                             plotOutput('plotbiomodel1')),
            conditionalPanel(condition = "input.popDist == 'poisson'",
                             plotOutput('poissonpop')),
            conditionalPanel(
              condition = "input.popDist == 'ipodshuffle'",
              conditionalPanel(condition =
                                 "input.ptype== 'Jazz'",
                               plotOutput("Plot1")),
              conditionalPanel(condition =
                                 "input.ptype == 'Rock'",
                               plotOutput('Plot2')),
              conditionalPanel(condition =
                                 "input.ptype == 'Country'",
                               plotOutput('Plot3')),
              conditionalPanel(condition =
                                 "input.ptype == 'Hip-hop'",
                               plotOutput('Plot4'))
              
            )
          )
        ),
        br(),
        fluidRow(
          column(
            6,
            conditionalPanel(condition = "input.popDist == 'leftskewed'",
                             plotOutput('plotleft2')),
            conditionalPanel(condition = "input.popDist == 'rightskewed'",
                             plotOutput('plotright2')),
            conditionalPanel(condition = "input.popDist == 'symmetric'",
                             plotOutput('plotsymmetric2')),
            conditionalPanel(condition = "input.popDist == 'astragalus'",
                             plotOutput("line2")),
            conditionalPanel(condition = "input.popDist == 'bimodal'",
                             plotOutput('plotbiomodel2')),
            conditionalPanel(condition = "input.popDist == 'poisson'",
                             plotOutput('plotpoisson1')),
            conditionalPanel(
              condition = "input.popDist =='ipodshuffle'",
              conditionalPanel(condition = "input.ptype== 'Jazz'",
                               plotOutput("Plot01")),
              conditionalPanel(condition = "input.ptype=='Rock' ",
                               plotOutput("Plot02")),
              conditionalPanel(condition = "input.ptype=='Country'",
                               plotOutput("Plot03")),
              conditionalPanel(condition = "input.ptype=='Hip-hop'",
                               plotOutput("Plot04"))
            )
          ),
          column(
            6,
            conditionalPanel(condition = "input.popDist == 'leftskewed'",
                             plotOutput('plotleft3')),
            bsPopover(
              "plotleft3",
              "All Samples Histogram",
              "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
              trigger = "hover",
              placement = "top"
            ),
            conditionalPanel(condition = "input.popDist == 'rightskewed'",
                             plotOutput('plotright3')),
            bsPopover(
              "plotright3",
              "All Samples Histogram",
              "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
              trigger = "hover",
              placement = "top"
            ),
            conditionalPanel(condition = "input.popDist == 'symmetric'",
                             plotOutput('plotsymmetric3')),
            bsPopover(
              "plotsymmetric3",
              "All Samples Histogram",
              "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
              trigger = "hover",
              placement = "top"
            ),
            conditionalPanel(condition = "input.popDist == 'astragalus'",
                             plotOutput("line1")),
            bsPopover(
              "line1",
              "All Samples Histogram",
              "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
              trigger = "hover",
              placement = "top"
            ),
            conditionalPanel(condition = "input.popDist == 'bimodal'",
                             plotOutput('plotbiomodel3')),
            bsPopover(
              "plotbiomodel3",
              "All Samples Histogram",
              "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
              trigger = "hover",
              placement = "top"
            ),
            conditionalPanel(condition = "input.popDist == 'poisson'",
                             plotOutput('plotpoisson2')),
            bsPopover(
              "plotpoisson2",
              "All Samples Histogram",
              "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
              trigger = "hover",
              placement = "top"
            ),
            conditionalPanel(
              condition = "input.popDist =='ipodshuffle'",
              conditionalPanel(condition = "input.ptype== 'Jazz'",
                               plotOutput("Plot10")),
              bsPopover(
                "Plot10",
                "All Samples Histogram",
                "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                trigger = "hover",
                placement = "top"
              ),
              conditionalPanel(condition = "input.ptype=='Rock' ",
                               plotOutput("Plot20")),
              bsPopover(
                "Plot20",
                "All Samples Histogram",
                "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                trigger = "hover",
                placement = "top"
              ),
              conditionalPanel(condition = "input.ptype=='Country'",
                               plotOutput("Plot30")),
              bsPopover(
                "Plot30",
                "All Samples Histogram",
                "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                trigger = "hover",
                placement = "top"
              ),
              conditionalPanel(
                condition = "input.ptype=='Hip-hop'",
                plotOutput("Plot40"),
                bsPopover(
                  "Plot40",
                  "All Samples Histogram",
                  "Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                  trigger = "hover",
                  placement = "top"
                )
              )
              
            )
            
            
          )
        )
      ),
      #### Set up the References Page
      tabItem(
        tabName = "References",
        withMathJax(),
        h2("References"),
        p(
          class = "hangingindent",
          "Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework for R. R
  package version 1.4.0.2. https://CRAN.R-project.org/package=shiny"
        ),
        p(
          class = "hangingindent",
          "Winston Chang and Barbara Borges Ribeiro (2018). shinydashboard: Create Dashboards with 'Shiny'. R package version 0.7.1.
  https://CRAN.R-project.org/package=shinydashboard"
        ),
        p(
          class = "hangingindent",
          "Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12."
        ),
        p(
          class = "hangingindent",
          "Winston Chang (2018). shinythemes: Themes for Shiny. R package version 1.1.2.
  https://CRAN.R-project.org/package=shinythemes"
        ),
        p(
          class = "hangingindent",
          "DEric Bailey (2015). shinyBS: Twitter Bootstrap Components for Shiny. R package version 0.61.
  https://CRAN.R-project.org/package=shinyBS"
        ),
        p(
          class = "hangingindent",
          " Victor Perrier, Fanny Meyer and David Granjon (2020). shinyWidgets: Custom Inputs Widgets for Shiny. R package version
  0.5.2. https://CRAN.R-project.org/package=shinyWidgets"
        ),
        p(
          class = "hangingindent",
          "Penn State University. 3.2 - Graphs: Displaying Measurement Data: STAT 100.
          Penn State: Statistics Online Courses. Available at https://online.stat.psu.edu/stat100/lesson/3/3.2."
        ),
        p(
          class = "hangingindent",
          " H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016."
        )
        ,
        
        p(
          class = "hangingindent",
          "Hadley Wickham (2007). Reshaping Data with the reshape Package. Journal of Statistical Software, 21(12), 1-20. URL
  http://www.jstatsoft.org/v21/i12/."
        ),
        
        p(
          class = "hangingindent",
          " Hadley Wickham and Dana Seidel (2020). scales: Scale Functions for Visualization. R package version 1.1.1.
  https://CRAN.R-project.org/package=scales"
        ),
        p(
          class = "hangingindent",
          "R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
  Vienna, Austria. URL https://www.R-project.org/."
        ),
        p(
          class = "hangingindent",
          "Dennis D. Boos and Douglas Nychka (2012). Rlab: Functions and Datasets Required for ST370 class. R package version 2.15.1.
  https://CRAN.R-project.org/package=Rlab"
        ),
        p(
          class = "hangingindent",
          "Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2020). dplyr: A Grammar of Data Manipulation. R package
  version 0.8.5. https://CRAN.R-project.org/package=dplyr"
        ),
        p(
          class = "hangingindent",
          "Kun Ren and Kenton Russell (2016). formattable: Create 'Formattable' Data Structures. R package version 0.2.0.1.
  https://CRAN.R-project.org/package=formattable"
        ),
        p(
          class = "hangingindent",
          "Yaan. (2007). Shagai. Wikimedia. Available at https://commons.wikimedia.org/wiki/File:Shagai.jpg."
        )
        
      )
    )
  )
)
)