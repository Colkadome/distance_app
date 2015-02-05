source("table.R")
shinyUI(fluidPage(
    
    tags$head(
        tags$script(src = "google-code-prettify/run_prettify.js")
    ),
    
    titlePanel(title="Cosmology Calculator"),
    tabsetPanel(tabPanel("Calculation",
                         sidebarLayout(
                             sidebarPanel(
                                 actionButton(inputId="submitCalc", label=span("Calculate"), icon("random")),
                                 h4("Set Variables:"),
                                 fluidRow(
                                     column(6,
                                            textInput(inputId="calcz", label="z", value="1"),
                                            textInput(inputId="calcOmegaM", label="OmegaM", value="0.3")
                                     ),
                                     column(6,
                                            textInput(inputId="calcH0", label="H0", value="70.0"),
                                            textInput(inputId="calcOmegaL", label="OmegaL", value="1-OmegaM")
                                     )
                                 ),
                                 fluidRow(
                                     h4("Custom Calc:"),
                                     selectInput(inputId="custom_calcAxis", label="Variable", choices = list(
                                         "a"="a",
                                         "Comoving Radial Distance LoS"="CoDist",
                                         "Luminosity Distance"="LumDist",
                                         "Comoving Radial Distance Tran"="CoDistTran",
                                         "Distance Modulus"="DistMod",
                                         "Comoving Volume"="CoVol",
                                         "Universe Age at z"="UniAgeAtz",
                                         "Look-back time to z"="TravelTime",
                                         "H"="H",
                                         "OmegaM"="OmegaM",
                                         "OmegaL"="OmegaL",
                                         "OmegaK"="OmegaK",
                                         "Growth Factor"="Factor",
                                         "Growth Rate"="Rate",
                                         "Universe Critical Mass Density"="RhoCrit"
                                     ),selected="CoVol"),
                                     textInput(inputId="custom_calcValue", label=uiOutput("custom_calcUnit"), value="")
                                     
                                 )
                             ),
                             mainPanel(
                                 uiOutput("calcOut")
                             )
                         ),
                         br(),br(),br()
    ),
    tabPanel("Plot",
             sidebarLayout(
                 sidebarPanel(
                     fluidRow(
                         actionButton(inputId="submitPlot", label="Plot", icon("random")),
                         h4("Set Variables:"),
                         textInput(inputId="plotH0", label="H0", value="70.0")
                     ),
                     fluidRow(
                         column(6,
                                textInput(inputId="plotOmegaM", label="OmegaM", value="0.3")
                         ),
                         column(6,
                                textInput(inputId="plotOmegaL", label="OmegaL", value="1-OmegaM")
                         )
                     ),
                     h4("Plot Options:"),
                     fluidRow(
                         column(6,
                                textInput(inputId="plotStart", label="z Start", value="0"),
                                textInput(inputId="plotRes", label="z Resolution", value="1000")
                         ),
                         column(6,
                                textInput(inputId="plotEnd", label="z End", value="1")
                         )
                     ),
                     fluidRow(
                         selectInput("plotAxis", label="x Axis", choices = list("z"="z",
                                                                                "Look-back time to z"="TravelTime"
                         ),selected="z"),
                         checkboxInput("plotLogY", label = "Log y axis", value = FALSE)
                     ),
                     fluidRow(
                         h4("Custom Plot:"),
                         selectInput(inputId="customXAxis", label="x Axis", choices = list("z"="z",
                                                                                           "a"="a",
                                                                                           "Comoving Radial Distance LoS"="CoDist",
                                                                                           "Luminosity Distance"="LumDist",
                                                                                           "Comoving Radial Distance Tran"="CoDistTran",
                                                                                           "Distance Modulus"="DistMod",
                                                                                           "Comoving Volume"="CoVol",
                                                                                           "Universe Age at z"="UniAgeAtz",
                                                                                           "Look-back time to z"="TravelTime",
                                                                                           "H"="H",
                                                                                           "OmegaM"="OmegaM",
                                                                                           "OmegaL"="OmegaL",
                                                                                           "OmegaK"="OmegaK",
                                                                                           "Growth Factor"="Factor",
                                                                                           "Growth Rate"="Rate",
                                                                                           "Universe Critical Mass Density"="RhoCrit"
                         ),selected="TravelTime"),
                         checkboxInput("customLogX", label = "Log x axis", value = FALSE),
                         selectInput(inputId="customYAxis", label="y Axis", choices = list("z"="z",
                                                                                           "a"="a",
                                                                                           "Comoving Radial Distance LoS"="CoDist",
                                                                                           "Luminosity Distance"="LumDist",
                                                                                           "Angular Size Distance"="AngDist",
                                                                                           "Comoving Radial Distance Tran"="CoDistTran",
                                                                                           "Distance Modulus"="DistMod",
                                                                                           "Angular Size"="AngSize",
                                                                                           "Comoving Volume"="CoVol",
                                                                                           "Universe Age at z"="UniAgeAtz",
                                                                                           "Look-back time to z"="TravelTime",
                                                                                           "H"="H",
                                                                                           "OmegaM"="OmegaM",
                                                                                           "OmegaL"="OmegaL",
                                                                                           "OmegaK"="OmegaK",
                                                                                           "Growth Factor"="Factor",
                                                                                           "Growth Rate"="Rate",
                                                                                           "Universe Critical Mass Density"="RhoCrit"
                         ),selected="CoVol"),
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
                     plotOutput("customPlotOut")
                 )
             ),
             br(),br(),br()
    ),
    tabPanel("Survey Design",
             sidebarLayout(
                 sidebarPanel(
                     fluidRow(
                         actionButton(inputId="sky_submit", label=span("Calculate"), icon("random")),
                         h4("Set Variables:")
                     ),
                     fluidRow(
                         column(6,
                                textInput(inputId="sky_area", label="Area", value="59.97867933223287")
                         ),
                         column(6,
                                selectInput(inputId="sky_areaUnit", label="Unit", choices = list("deg²"="deg2",
                                                                                                 "amin²"="amin2",
                                                                                                 "asec²"="asec2",
                                                                                                 "sr"="sr"
                                ),selected="deg2")
                         )
                     ),
                     fluidRow(
                         textInput(inputId="sky_H0", label="H0", value="70.0")
                     ),
                     fluidRow(
                         column(6,
                                textInput(inputId="sky_OmegaM", label="OmegaM", value="0.3"),
                                textInput(inputId="sky_minz", label="min z", value="0")
                         ),
                         column(6,
                                textInput(inputId="sky_OmegaL", label="OmegaL", value="1-OmegaM"),
                                textInput(inputId="sky_maxz", label="max z", value="0.5")
                         )
                     ),
                     fluidRow(
                         h4("Find Area (optional):"),
                         actionButton(inputId="sky_setArea", icon("arrow-up")),
                         br(), br()
                     ),
                     fluidRow(
                         column(6,
                                textInput(inputId="sky_long1", label="Longitude 1 (deg)", value="129"),
                                textInput(inputId="sky_lat1", label="Latitude 1 (deg)", value="-2")
                         ),
                         column(6,
                                textInput(inputId="sky_long2", label="Longitude 2 (deg)", value="141"),
                                textInput(inputId="sky_lat2", label="Latitude 2 (deg)", value="3")
                         )
                     )
                 ),
                 mainPanel(
                     h4("Result:"),
                     uiOutput("sky_out")
                 ) 
             ),
             br(),br(),br()
    ),
    tabPanel("Info",
             h3("About"),
             p(span("Welcome to ICRAR's Cosmology Calculator!", style="color:#08c"),
               "This calculator was written by", strong("Joseph Dunne"), "and", strong("Aaron Robotham"), "in the programming language R, and uses the library Shiny to provide
                   the interface."),
             br(),
             h4("Calculation", style='color:#08c'),
             p(
                 "This tab is used to calculate various distance parameters."
             ),
             p(
                 "To use this tab, fill in the variables under", strong("Set Variables"), "and click the", actionButton(inputId="dud", label="Calculate", icon("random")),
                 "button to calculate variables at a certain redshift. Type '1-OmegaM' into the OmegaL field to set OmegaL to", span("1-OmegaM", style="text-decoration:underline;"),
                 "for all calculations."
             ),
             p(
                 "Under", strong("Custom Calc,"), "the calculation may be done using a chosen variable from the", span("Variable", style="text-decoration:underline;"), "menu.",
                 "When the", span("Value", style="text-decoration:underline;"), "box contains a value, the custom calculation will be used next time the", actionButton(inputId="dud", label="Calculate", icon("random")),
                 "button is clicked."
             ),
             br(),
             h4("Plot", style='color:#08c'),
             p(
                 "This tab is used to plot various parameters."
             ),
             p(
                 "To use this tab, fill in the variables under", strong("Set Variables"), "and", strong("Plot Options,"), "and click",
                 actionButton(inputId="dud", label="Plot", icon("random")), " to produce plots across a redshift range.",
                 "The first plot is a distance plot, and the second plot is a custom plot which may be modified using the options under", strong("Custom Plot."),
                 "Type '1-OmegaM' into the OmegaL field to set OmegaL to", span("1-OmegaM", style="text-decoration:underline;"),
                 "for all calculations. Some of the plot options are as follows:"
             ),
             p(strong("z Start"), "- The starting redshift for the plots."),
             p(strong("z End"), "- The finishing redshift for the plots."),
             p(strong("z Resolution"), "- The number of points to plot between", strong("z Start"), "and", strong("z End.")),
             p(
                 "The resulting data can be saved using the options under", strong("Save Data.")
             ),
             br(),
             h4("Survey Design", style='color:#08c'),
             p(
                 "This tab is used to find the Comoving Volume of an area in the sky."
             ),
             p(
                 "To use this tab, fill in the variables under", strong("Set Variables"), "and click the", actionButton(inputId="dud", label="Calculate", icon("random")),
                 "button to calculate the Comoving Volume. If the area is unknown, an extra option under", strong("Find Area (optional)"), "can be used the find the area of the sky given the latitude and longitude.",
                 "The Area field is then updated by clicking the", actionButton(inputId="dud", icon("arrow-up")), "button."
             ),
             br(),
             h4("References"),
             p(a("D. W. Hogg et all 1999 (arXiv 9905116)", href="http://arxiv.org/abs/astro-ph/9905116", target="_blank")),
             p(a("Wright E.L., 2006, PASP, 118, 1711", href="http://adsabs.harvard.edu/abs/2006PASP..118.1711W", target="_blank")),
             p(a("Hamilton A.J.S., 2001, MNRAS 322 419", href="http://adsabs.harvard.edu/abs/2001MNRAS.322..419H", target="_blank")),
             p(a("Lahav O., et al., 1991, MNRAS, 251, 136", href="http://adsabs.harvard.edu/abs/1991MNRAS.251..128L", target="_blank")),
             br(),br(),br()
             
    ),
    tabPanel("R Code",
             p("Basic cosmological calculator R code used server-side to generate outputs."),
             p("Written by Aaron Robotham (see", span("Info", style='color:#08c'), "tab for references)."),
             HTML("<pre class='prettyprint lang-r' style='padding:5px'>function (z = 1, H0 = 100, OmegaM = 0.3, OmegaL = 1 - OmegaM, age = FALSE) {
    OmegaK = 1 - OmegaM - OmegaL
    Einv = function(z, OmegaM, OmegaL, OmegaK) {
        1/sqrt(OmegaM * (1 + z)^3 + OmegaK * (1 + z)^2 + OmegaL)
    }
    if (age) {
        Einvz = function(z, OmegaM, OmegaL, OmegaK) {
            1/(sqrt(OmegaM * (1 + z)^3 + OmegaK * (1 + z)^2 + OmegaL) * (1 + z))
        }
    }
    temp = function(z, H0, OmegaM, OmegaL, OmegaK) {
        HubDist = (299792.458/H0)
        CoDist = HubDist * integrate(Einv, 0, z, OmegaM = OmegaM,
            OmegaL = OmegaL, OmegaK = OmegaK, subdivisions = 1000)$value
        if (OmegaK == 0) {
            CoDistTran = CoDist
            CoVol = ((4/3) * pi * CoDist^3)/1e+09
        }
        else {
            if (OmegaK > 0) {
                CoDistTran = HubDist * (1/sqrt(OmegaK)) *
                    sinh(sqrt(OmegaK) * CoDist/HubDist)
                CoVol = ((4 * pi * HubDist^3/(2 * OmegaK)) *
                    ((CoDistTran/HubDist) * sqrt(1 + OmegaK * (CoDistTran/HubDist)^2) -
                    (1/sqrt(abs(OmegaK))) * asinh(sqrt(abs(OmegaK)) *
                    (CoDistTran/HubDist))))/1e+09
            }
            if (OmegaK < 0) {
                CoDistTran = HubDist * (1/sqrt(abs(OmegaK))) *
                    sin(sqrt(abs(OmegaK)) * CoDist/HubDist)
                CoVol = ((4 * pi * HubDist^3/(2 * OmegaK)) *
                    ((CoDistTran/HubDist) * sqrt(1 + OmegaK * (CoDistTran/HubDist)^2) -
                    (1/sqrt(abs(OmegaK))) * asin(sqrt(abs(OmegaK)) *
                    (CoDistTran/HubDist))))/1e+09s
            }
        }
        a = 1/(1 + z)
        LumDist = (1 + z) * CoDistTran
        AngDist = CoDistTran/(1 + z)
        DistMod = 5 * log10(LumDist) + 25
        AngSize = AngDist * (pi/(180 * 60 * 60)) * 1000
        if (age) {
            HT = (3.08568025e+19/(H0 * 31556926))/1e+09
            UniAge = HT * integrate(Einvz, 0, Inf, OmegaM = OmegaM,
                OmegaL = OmegaL, OmegaK = OmegaK, subdivisions = 1000)$value
            zAge = HT * integrate(Einvz, 0, z, OmegaM = OmegaM,
                OmegaL = OmegaL, OmegaK = OmegaK, subdivisions = 1000)$value
        }
        if (age) {
            return = c(z = z, a = a, CoDist = CoDist, LumDist = LumDist,
                AngDist = AngDist, CoDistTran = CoDistTran, DistMod = DistMod,
                AngSize = AngSize, CoVol = CoVol, HubTime = HT,
                UniAgeNow = UniAge, UniAgeAtz = UniAge - zAge,
                TravelTime = zAge)
        }
        else {
            return = c(z = z, a = a, CoDist = CoDist, LumDist = LumDist,
                AngDist = AngDist, CoDistTran = CoDistTran, DistMod = DistMod,
                AngSize = AngSize, CoVol = CoVol)
        }
    }
    return(as.data.frame(t(Vectorize(temp)(z = z, H0 = H0, OmegaM = OmegaM, OmegaL = OmegaL, OmegaK = OmegaK))))
}</pre>"
             )
    )
    )
))