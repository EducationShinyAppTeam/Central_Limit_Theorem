library(shiny)
library(shinydashboard)
library(plotrix)
library(shinythemes)
library(shinyBS)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(skin = "blue",
                dashboardHeader(title = 'Central Limit Theorem',
                                tags$li(class = "dropdown",tags$a(href='http://stat.psu.edu/',icon("home"))),
                                tags$li(class = "dropdown",actionLink("info",icon("info",class = "myClass")))),
                dashboardSidebar(
                  sidebarMenu(
                    id = "tabs",
                    menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
                    menuItem("Central Limit Theorem", icon = icon("wpexplorer"), tabName = "largeNumber")
                  )
                ),
                dashboardBody(
                  tags$head( 
                    tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css"),
                    tags$style(HTML('#go{color:white;background-color: #337ab7}'))
                  ),
                  tabItems(
                    tabItem(tabName = "Overview",
                           # tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
                            br(),br(),br(),
                            fluidPage(
                              withMathJax(),
                              h3(tags$b("About:")),
                              h4("This app is designed to examine the Central Limit Theorem under different population distributions and sample sizes."),
                              h3(tags$b("Instructions:")),
                              h4(tags$li("Pick a population from one of the continuous types (left-skewed; right-skewed; symmetric; or bimodal) or one of the 
                                 discrete examples (rolls of an astragalus; random songs from an iPod shuffle; or accident occurrence).")),
                              h4(tags$li("Use the sliders to adjust the parameters of the population model you chosen.")),
                              h4(tags$li("Use the sliders to decide the sample size (n) you want for each sample and how many reps you will repeat the process 
                                 of taking samples from the population (calculating \\(\\bar{x}\\) for each).")),
                              h4(tags$li("Use the button to see one of the individual samples chossen.")),
                              h4(tags$li("Observe the histograms of one sample and for \\(\\bar{x}\\) from all samples.")),
                              br(),
                              div(style = "text-align: center",
                                  bsButton("go", "Explore", icon("bolt"), class="circle grow")),
                              h3(tags$b("Acknowledgements:")),
                              h4("This app was developed and coded by Yingjie (Chelsea) Wang with the help of Yubaihe Zhou. Selection of population densities was originally coded by Caihui Xiao."),
                              h4("This application was modified by Zhiruo Wang.")
                                 )),
                    
                    tabItem(tabName = "largeNumber",
                            tags$head(
                              tags$style(HTML("
                                              input[type=\"number\"] {
                                              width: 60px;
                                              }
                                              ")),
                              tags$style(HTML(
                                '.popover-title{
                                    color: #FFFFFF;
                                    background-color: #337ab7 }'
                              ))
                              ),
                            h4(
                              wellPanel(
                                # div(style="display: inline-block;vertical-align:top;",
                                #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                                # ),
                                # div(style="display: inline-block;vertical-align:top;",
                                #     circleButton("info",icon = icon("info"), status = "myClass",size = "xs")
                                # ),
                                fluidRow(
                                  column(3,
                                         selectInput("popDist", "Population Type",
                                                     list( "Left-skewed" = "leftskewed",
                                                           "Right-skewed" = "rightskewed",
                                                           "Symmetric" = "symmetric",
                                                           "Bimodal" = "bimodal",
                                                           "Astragalus"="astragalus",
                                                           "IPod Shuffle"="ipodshuffle",
                                                           "Accident Rate"= "poisson"
                                                     )
                                         ),
                                         
                                         conditionalPanel(
                                           condition="input.popDist=='leftskewed'",
                                           sliderInput("leftskew", " Skewness:",min = 1, max = 10, value = 1, step= 0.1),
                                           
                                           actionButton("new1", "show one of the samples",icon("retweet"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                         ),
                                         conditionalPanel(
                                           condition="input.popDist=='rightskewed'",
                                           sliderInput("rightskew", "Skewness:",min = 1, max = 10, value = 1, step= 0.1),
                                           
                                           actionButton("new2", "show one of the samples",icon("retweet"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                         ),
                                         conditionalPanel(
                                           condition="input.popDist=='symmetric'",
                                           sliderInput("inverse","Peakedness:", min = 0.5, max = 10, value = 1, step= 0.1),
                                           
                                           actionButton("new3", "show one of the samples",icon("retweet"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                         ),
                                         conditionalPanel(
                                           condition="input.popDist=='bimodal'",
                                           
                                           sliderInput("prop","% under right mode:",min = 0,max = 1,value = 0.2),
                                           
                                           actionButton("new4", "show one of the samples",icon("retweet"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                         ),
                                         conditionalPanel(
                                           condition="input.popDist=='poisson'",
                                           
                                           sliderInput("poissonmean","Mean:", min = 0.1, max = 10, value = 1, step= 0.1),
                                           
                                           actionButton("new5", "show one of the samples",icon("retweet"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                         ),
                                         
                                         # conditionalPanel(
                                         #   condition="input.popDist=='astragalus'",
                                         #   
                                         #   sliderInput("poissonmean","Mean:", min = 0.1, max = 10, value = 1, step= 0.1),
                                         #   
                                         #   actionButton("new6", "show one of the samples",icon("retweet"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                         # ),
                                         
                                         #iPod shuffle 
                                         conditionalPanel(
                                           condition="input.popDist == 'ipodshuffle'",
                                           column(width = 7, offset = 0, 
                                                  radioButtons(
                                                    "ptype", "Category to follow:",
                                                    list("Jazz",
                                                         "Rock",
                                                         "Country",
                                                         "Hip-hop"),
                                                    selected ="Jazz"),
                                                  
                                                  column(4,offset = 0, 
                                                         
                                                         numericInput("s1", "Jazz", 1,
                                                                      min = 0, max = 100, step = 1, width = '10%'),
                                                         numericInput("s2", "Rock", 1,
                                                                      min = 0, max = 200, step = 1),
                                                         numericInput("s3", "Country", 1,
                                                                      min = 0, max = 200, step = 1),
                                                         numericInput("s4", "Hip-hop", 1,
                                                                      min = 0, max = 200, step = 1))
                                           ),
                                           column(width = 5,
                                                  helpText('P of Jazz'),
                                                  verbatimTextOutput("Jazz_percent"),
                                                  helpText('P of Rock '),
                                                  verbatimTextOutput("Rock_percent")),
                                           column(width = 5,
                                                  helpText('P of Country'),
                                                  verbatimTextOutput("Country_percent"),
                                                  helpText('P of Hip-hop'),
                                                  verbatimTextOutput("Hiphop_percent"))
                                         )
                                         
                                  ),
                                  column(3,
                                         #left skewed
                                         conditionalPanel(
                                           condition ="input.popDist == 'leftskewed'", 
                                           sliderInput("leftsize",
                                                       "sample size (n)",
                                                       min = 1,
                                                       max = 50,
                                                       value = 10),
                                           sliderInput("leftreps",
                                                       "# of reps",
                                                       min = 1,
                                                       max = 5000,
                                                       value = 1000)
                                           
                                         ),
                                         conditionalPanel(
                                           condition = "input.popDist == 'rightskewed'", 
                                           sliderInput("rightsize",
                                                       "sample size (n)",
                                                       min = 1,
                                                       max = 50,
                                                       value = 10),
                                           
                                           sliderInput("rightreps",
                                                       "# of reps",
                                                       min = 1,
                                                       max = 5000,
                                                       value = 1000)
                                         ),
                                         
                                         conditionalPanel(
                                           condition= "input.popDist == 'symmetric'",
   
                                           sliderInput("symsize",
                                                       "sample size (n)",
                                                       min = 1,
                                                       max = 50,
                                                       value = 10),
                                           
                                           sliderInput("symreps",
                                                       "# of reps",
                                                       min = 1,
                                                       max = 5000,
                                                       value = 1000)
                                           
                                         ),
                                         conditionalPanel(
                                           condition= "input.popDist == 'astragalus'",
                                           sliderInput("assize",
                                                       "sample size (n)",
                                                       min = 1,
                                                       max = 50,
                                                       value = 10),
                                           
                                           sliderInput("asreps",
                                                       "# of reps",
                                                       min = 1,
                                                       max = 5000,
                                                       value = 1000)
                                         
                                         ),
                                         conditionalPanel(
                                           condition= "input.popDist == 'bimodal'",

                                           sliderInput("bisize",
                                                       "sample size (n)",
                                                       min = 1,
                                                       max = 50,
                                                       value = 10),
                                           #choose the number of sample means
                                           sliderInput("bireps",
                                                       "# of reps",
                                                       min = 1,
                                                       max = 5000,
                                                       value = 1000)
                                           
                                         ),
                                         conditionalPanel(
                                           condition= "input.popDist == 'poisson'", 
                                           sliderInput("posize",
                                                       "sample size (n)",
                                                       min = 1,
                                                       max = 50,
                                                       value = 10),
                                           #choose the number of sample means
                                           sliderInput("poreps",
                                                       "# of reps",
                                                       min = 1,
                                                       max = 5000,
                                                       value = 1000)
                                         
                                         ),
                                         conditionalPanel(
                                           condition= "input.popDist == 'ipodshuffle'",
                                           sliderInput("ipodsize",
                                                       "sample size (n)",
                                                       min = 1,
                                                       max = 50,
                                                       value = 10),
                                           #choose the number of sample means
                                           sliderInput("ipodreps",
                                                       "# of reps",
                                                       min = 1,
                                                       max = 5000,
                                                       value = 1000),
                                           actionButton("new7", "show one of the samples",icon("retweet"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                         )
                                  ),
                                  column(6,
                                         conditionalPanel(
                                           condition ="input.popDist == 'leftskewed'", 
                                           plotOutput('plotleft1')),
                                         
                                         conditionalPanel(
                                           condition = "input.popDist == 'rightskewed'", 
                                           plotOutput('plotright1')),
                                         conditionalPanel(
                                           condition= "input.popDist == 'symmetric'",
                                           plotOutput('plotsymmetric1')),
                                         conditionalPanel(
                                           condition= "input.popDist == 'astragalus'",
                                           plotOutput("pop")),
                                         conditionalPanel(
                                           condition= "input.popDist == 'bimodal'",
                                           plotOutput('plotbiomodel1')),
                                         conditionalPanel(
                                           condition= "input.popDist == 'poisson'", 
                                           plotOutput('poissonpop')),
                                         conditionalPanel(
                                           condition="input.popDist == 'ipodshuffle'",
                                           conditionalPanel(
                                             condition="input.ptype== 'Jazz'",
                                             plotOutput("Plot1")),
                                           conditionalPanel(
                                             condition="input.ptype == 'Rock'",
                                             plotOutput('Plot2')),
                                           conditionalPanel(
                                             condition="input.ptype == 'Country'",
                                             plotOutput('Plot3')),
                                           conditionalPanel(
                                             condition="input.ptype == 'Hip-hop'",
                                             plotOutput('Plot4'))
                                           
                                         )
                                  )
                                )
                                
                              )
                            ),
                            br(),
                            fluidRow(
                              column(6,
                                     conditionalPanel(
                                       condition ="input.popDist == 'leftskewed'", 
                                       plotOutput('plotleft2')),
                                     conditionalPanel(
                                       condition = "input.popDist == 'rightskewed'", 
                                       plotOutput('plotright2')),
                                     conditionalPanel(
                                       condition= "input.popDist == 'symmetric'",
                                       plotOutput('plotsymmetric2')),
                                     conditionalPanel(
                                       condition= "input.popDist == 'astragalus'",
                                       plotOutput("line2")),
                                     conditionalPanel(
                                       condition= "input.popDist == 'bimodal'",
                                       plotOutput('plotbiomodel2')),
                                     conditionalPanel(
                                       condition= "input.popDist == 'poisson'", 
                                       plotOutput('plotpoisson1')),
                                     conditionalPanel(
                                       condition="input.popDist =='ipodshuffle'",
                                       conditionalPanel(
                                         condition="input.ptype== 'Jazz'",
                                         plotOutput("Plot01")),
                                       conditionalPanel(
                                         condition="input.ptype=='Rock' ",
                                         plotOutput("Plot02")),
                                       conditionalPanel(
                                         condition="input.ptype=='Country'",
                                         plotOutput("Plot03")),
                                       conditionalPanel(
                                         condition="input.ptype=='Hip-hop'",
                                         plotOutput("Plot04"))
                                     )
                              ),
                              column(6,
                                     conditionalPanel(
                                       condition ="input.popDist == 'leftskewed'", 
                                       plotOutput('plotleft3')),
                                       bsPopover("plotleft3","All Samples Histogram","Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                                               trigger="hover",placement="top"),
                                     conditionalPanel(
                                       condition = "input.popDist == 'rightskewed'", 
                                       plotOutput('plotright3')),
                                       bsPopover("plotright3","All Samples Histogram","Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                                               trigger="hover",placement="top"),
                                     conditionalPanel(
                                       condition= "input.popDist == 'symmetric'",
                                       plotOutput('plotsymmetric3')),
                                       bsPopover("plotsymmetric3","All Samples Histogram","Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                                               trigger="hover",placement="top"),
                                     conditionalPanel(
                                       condition= "input.popDist == 'astragalus'",
                                       plotOutput("line1")),
                                       bsPopover("line1","All Samples Histogram","Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                                               trigger="hover",placement="top"),
                                     conditionalPanel(
                                       condition= "input.popDist == 'bimodal'",
                                       plotOutput('plotbiomodel3')),
                                       bsPopover("plotbiomodel3","All Samples Histogram","Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                                               trigger="hover",placement="top"),
                                     conditionalPanel(
                                       condition= "input.popDist == 'poisson'", 
                                       plotOutput('plotpoisson2')),
                                       bsPopover("plotpoisson2","All Samples Histogram","Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                                               trigger="hover",placement="top"),
                                     conditionalPanel(
                                       condition="input.popDist =='ipodshuffle'",
                                       conditionalPanel(
                                         condition="input.ptype== 'Jazz'",
                                         plotOutput("Plot10")),
                                         bsPopover("Plot10","All Samples Histogram","Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                                                 trigger="hover",placement="top"),
                                       conditionalPanel(
                                         condition="input.ptype=='Rock' ",
                                         plotOutput("Plot20")),
                                         bsPopover("Plot20","All Samples Histogram","Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                                                 trigger="hover",placement="top"),
                                       conditionalPanel(
                                         condition="input.ptype=='Country'",
                                         plotOutput("Plot30")),
                                         bsPopover("Plot30","All Samples Histogram","Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                                                 trigger="hover",placement="top"),
                                       conditionalPanel(
                                         condition="input.ptype=='Hip-hop'",
                                         plotOutput("Plot40"),
                                         bsPopover("Plot40","All Samples Histogram","Histogram of x-bar values from each rep. The blue line shows the normal approximation.",
                                                 trigger="hover",placement="top")
                                         )
                                       ))))))))
