shinyUI(fluidPage(
  titlePanel(title="Astronomy Calculator"),
  tabsetPanel(tabPanel("Calculation",
                       sidebarLayout(
                         sidebarPanel(
                           fluidRow(
                             actionButton(inputId="submitCalc", label="Calculate", icon("random")),
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
                   actionButton(inputId="submitPlot", label="Plot", icon("random")),
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
                   h4("Plot Options:"),
                   column(6,
                          textInput(inputId="plotStart", label="z Start", value="0"),
                          textInput(inputId="plotRes", label="z Resolution", value="1000")
                   ),
                   column(5,
                          textInput(inputId="plotEnd", label="z End", value="1")
                   )
                 ),
                 fluidRow(
                   selectInput("plotAxis", label="x Axis", choices = list("z"=1,
                                                                          "Travel Time"=2
                   ),selected=1),
                   checkboxInput("plotLogY", label = "Log y axis", value = FALSE)
                 ),
                 fluidRow(
                   h4("Custom Plot:"),
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
                   checkboxInput("customLogX", label = "Log x axis", value = FALSE),
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
                 ),
                 fluidRow(
                   h4("Save Data:"),
                   downloadButton("saveData_txt",label="Download as .txt"),
                   downloadButton("saveData_csv",label="Download as .csv")
                 )
               ),
               mainPanel(
                            plotOutput("plotDistOut"),
                            plotOutput("customPlotOut"),
                            textInput(inputId="customYValue", label="At an x of:", value="0.5"),
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
             h4("Calculation", style='color:#08c'),
             p(
               "In this tab, fill in the variables under", strong("Set Variables"), "and click the", actionButton(inputId="dud", label="Calculate", icon("random")),
               "button to calculate variables at a certain redshift. Type '1-OmegaM' into the OmegaL field to set OmegaL to", span("1-OmegaM", style="text-decoration:underline;"),
               "for all calculations."
             ),
             br(),
             h4("Plot", style='color:#08c'),
             p(
               "In this tab, fill in the attributes under", strong("Set Variables"), "and", strong("Plot Options"), ", and click",
               actionButton(inputId="dud", label="Plot", icon("random")), " to produce plots across a redshift range.",
               "Type '1-OmegaM' into the OmegaL field to set OmegaL to", span("1-OmegaM", style="text-decoration:underline;"),
               "for all calculations. Some of the plot options are as follows:"
             ),
             p(strong("z Start"), "- The minimum redshift for the plots."),
             p(strong("z End"), "- The maximum redshift for the plots."),
             p(strong("z Resolution"), "- The number of points to plot between", strong("z Start"), "and", strong("z End.")),
             p(
               "The resulting data can be saved using the options under", strong("Save Data."),
               "The second plot is a custom plot, and can be modified using the options under", strong("Custom Plot."),
               "Through linear interpolation between points, the y value at any point along the custom graph may be found.",
               "To do this, input an x value into the box below the custom graph.",
               "The input may be in the form '2e3', etc."
               ),
             br(),
             h4("Acknowledgements"),
             p(a("D. W. Hogg et all 1999 (arXiv 9905116)", href="http://arxiv.org/abs/astro-ph/9905116", target="_blank")),
             p(a("Wright E.L., 2006, PASP, 118, 1711", href="http://arxiv.org/abs/astro-ph/0609593", target="_blank")),
             br(),
             div("Written by Joseph Dunne (2015)", style="font-size:10px; color:grey;", align="center")
    )
  )
))