shinyUI(fluidPage(
  titlePanel(title="Astronomy Calculator"),
  tabsetPanel(tabPanel("Calculation",
                       sidebarLayout(
                         sidebarPanel(
                           fluidRow(
                             actionButton(inputId="submitCalc", label="Recalculate", icon("random")),
                             h4("Set Variables:"),
                             column(6,
                                    textInput(inputId="calcH0", label="H0", value="70.0"),
                                    textInput(inputId="calcOmegaM", label="OmegaM", value="0.3")
                             ),
                             column(5,
                                    textInput(inputId="calcOmegaL", label="OmegaL", value="1-OmegaM"),
                                    textInput(inputId="calcz", label="z", value="3")
                             )
                           )
                         ),
                         mainPanel(
                            uiOutput("calcOut")
                         ) 
                       )
    ),
    tabPanel("Plot",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   actionButton(inputId="submitPlot", label="Replot", icon("random")),
                   h4("Set Variables:"),
                   column(6,
                          textInput(inputId="plotH0", label="H0", value="70.0"),
                          textInput(inputId="plotOmegaM", label="OmegaM", value="0.3")
                   ),
                   column(5,
                          textInput(inputId="plotOmegaL", label="OmegaL", value="1-OmegaM")
                   )
                 ),
                 fluidRow(
                   h4("Set Plot Options:"),
                   column(6,
                          textInput(inputId="plotStart", label="z Start", value="0"),
                          textInput(inputId="plotRes", label="z Resolution", value="1000")
                   ),
                   column(5,
                          textInput(inputId="plotEnd", label="z End", value="1"),
                          checkboxInput("plotLogY", label = "Log y axis", value = FALSE)
                   )
                 ),
                 fluidRow(
                   selectInput("plotAxis", label="x Axis", choices = list("z"="z",
                                                                          "Travel Time (yr)"="TravelTime"
                   ),selected="z")
                  ),
                 fluidRow(
                   h4("Custom Plot:"),
                   column(6,
                          selectInput(inputId="customXAxis", label="x Axis", choices = list("z"=1,
                                                                                            "Travel Time"=2,
                                                                                            "Comoving Radial Distance LoS"=3,
                                                                                            "Comoving Radial Distance Tran"=4,
                                                                                            "Luminosity Distance"=5,
                                                                                            "DistMod"=6,
                                                                                            "Angular Size Distance"=7,
                                                                                            "Angular Size"=8,
                                                                                            "Comoving Volume"=9,
                                                                                            "Universe Age at z"=10
                          ),selected=2),
                          checkboxInput("customLogX", label = "Log x axis", value = FALSE)
                   ),
                   column(5,
                          selectInput(inputId="customYAxis", label="y Axis", choices = list("z"=1,
                                                                                            "Travel Time"=2,
                                                                                            "Comoving Radial Distance LoS"=3,
                                                                                            "Comoving Radial Distance Tran"=4,
                                                                                            "Luminosity Distance"=5,
                                                                                            "DistMod"=6,
                                                                                            "Angular Size Distance"=7,
                                                                                            "Angular Size"=8,
                                                                                            "Comoving Volume"=9,
                                                                                            "Universe Age at z"=10
                          ),selected=8),
                          checkboxInput("customLogY", label = "Log y axis", value = FALSE)
                   )
                 ),
                 fluidRow(
                   h4("Save Data:"),
                   column(6,
                          downloadButton("saveData_txt",label="Download as .txt")
                   ),
                   column(5,
                          downloadButton("saveData_csv",label="Download as .csv")
                   )
                 )
               ),
               mainPanel(
                            plotOutput("plotDistOut"),
                            plotOutput("customPlotOut"),
                            textInput(inputId="customYValue", label="y at an x of:", value="0.5"),
                            uiOutput("customYValueOut")
                )
            )
    ),
    tabPanel("Info",
             h3("About"),
             p(span("Welcome to ICRAR's Astronomy Calculator!", style="color:#08c"),
               "This calculator is written in the programming language R, and uses the library Shiny to provide
                   the interface."),
             br(),
             h4("Useage"),
             p("Fill in the variables under", strong("Set Variables"), "and click the", span("Recalculate", style='color:#04c'),
               "button to calculate variables at a certain redshift. The results appear under the", span("Calculation", style="color:#08c"), "tab.",
               "The string '1-OmegaM' may be used in the OmegaL field, setting OmegaL to 1-OmegaM for all calculations.",
               "To produce plots of the variables across a redshift range, fill in the attributes of the plots first under", strong("Set Plot Options"),
               ", then click click ", span("Recalculate.", style='color:#04c'), "The plots will appear under the ", span("Plot", style="color:#08c"), "tab."),
             br(),
             h4("Acknowledgements"),
             p("The calculations are based on", a("D. W. Hogg et all 1999 (arXiv 9905116)", href="http://arxiv.org/abs/astro-ph/9905116", target="_blank"))
    )
  )
))