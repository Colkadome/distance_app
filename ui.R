source("table.R")
shinyUI(fluidPage(
    
    tags$head(
        tags$title("Cosmology Calculator"),
        tags$link(rel="shortcut icon", href="favicon.ico"),
#         tags$style(HTML("
# 
#                         ")),
        tags$script(src = "google-code-prettify/run_prettify.js")
    ),

    # title #
    #########

    fluidRow(
        h1("Cosmology Calculator", style="vertical-align:middle;color:white;margin:30px;"),
        style = "margin-bottom:10px;background: #000 url('space.gif') repeat 0 0;"
    ),
    
    # The Calculation tab #
    #######################
    tabsetPanel(tabPanel("Calculation",
                         sidebarLayout(
                             sidebarPanel(
                                 actionButton(inputId="submitCalc", label=span("Calculate"), icon("random")),
                                 h4("Set Variables:"),
                                 fluidRow(
                                     column(6,
                                            textInput(inputId="calcz", label="z", value="1")
                                            ),
                                     column(6,
                                            numericInput("calcSigFigs",label="Sig Figs",value=6,min=1,max=15)
                                            )
                                     ),
                                 tags$hr(),
                                 fluidRow(
                                     column(6,
                                            selectInput(inputId="calcDefaults", label="Reference Set", choices = names(defaultParams))
                                     ),
                                     column(6,
                                            textInput(inputId="calcH0", label="H0", value="70.0")
                                     )
                                 ),
                                 fluidRow(
                                     column(6,
                                            textInput(inputId="calcOmegaM", label="OmegaM", value="0.3")
                                     ),
                                     column(6,
                                            textInput(inputId="calcOmegaL", label="OmegaL", value="1-OmegaM")
                                     )
                                 ),
                                 h4("Custom Calc:"),
                                 textInput(inputId="custom_calcValue", label=uiOutput("custom_calcUnit"), value=""),
                                 selectInput(inputId="custom_calcAxis", label="Variable", choices = {l<-list();
                                                                                                     for(i in 1:length(lookUpTable)) {
                                                                                                         if(lookUpTable[[i]]$val != 'z' && lookUpTable[[i]]$val != 'AngSize' && lookUpTable[[i]]$val != 'AngDist' && lookUpTable[[i]]$val != 'HubTime' && lookUpTable[[i]]$val != 'UniAgeNow')
                                                                                                             l[[lookUpTable[[i]]$label]]<-lookUpTable[[i]]$val
                                                                                                     };l},
                                             selected="TravelTime")
                             ),
                             mainPanel(
                                 fluidRow(column(6,
                                                 uiOutput("calcOut_1")
                                                 ),
                                          column(6,
                                                 uiOutput("calcOut_2")
                                                 )
                                          )
                             )
                         )
    ),
    # The Plot tab #
    ################
    tabPanel("Plot",
             sidebarLayout(
                 sidebarPanel(
                     actionButton(inputId="submitPlot", label="Plot", icon("random")),
                     h4("Set Variables:"),
                     fluidRow(
                         column(6,
                                selectInput(inputId="plotDefaults", label="Reference Set", choices = names(defaultParams))
                         ),
                         column(6,
                                textInput(inputId="plotH0", label="H0", value="70.0")
                         )
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
                                textInput(inputId="plotStart", label="z Start", value="0")
                         ),
                         column(6,
                                textInput(inputId="plotEnd", label="z End", value="1")
                         )
                     ),
                     fluidRow(
                         column(6,
                                numericInput(inputId="plotRes", label="z Resolution", value=100, min=10, max=1000)
                         ),
                         column(6,
                                selectInput("plotAxis", label="x Axis", choices = {l <- list();
                                                                                   l[[lookUpTable$z$label]] <- lookUpTable$z$val;
                                                                                   l[[lookUpTable$TravelTime$label]] <- lookUpTable$TravelTime$val;
                                                                                   l;
                                },selected="z")
                         )
                     ),
                     fluidRow(
                         column(6,
                                checkboxInput("plotLogX", label = "Log x axis", value = FALSE)
                         ),
                         column(6,
                                checkboxInput("plotLogY", label = "Log y axis", value = FALSE)
                         )
                     ),
                     h4("Custom Plot:"),
                     selectInput(inputId="customXAxis", label="x Axis", choices = {l<-list();
                                                                                   for(i in 1:length(lookUpTable)) {
                                                                                       if(lookUpTable[[i]]$val != 'HubTime' && lookUpTable[[i]]$val != 'UniAgeNow')
                                                                                           l[[lookUpTable[[i]]$label]]<-lookUpTable[[i]]$val
                                                                                   };l},
                                 selected="TravelTime"),
                     fluidRow(
                         column(6,
                                checkboxInput("customLogX", label = "Log x axis", value = FALSE)
                         ),
                         column(6,
                                checkboxInput("customFlipX", label = "Flip x axis", value = FALSE)
                         )
                     ),
                     selectInput(inputId="customYAxis", label="y Axis", choices = {l<-list();
                                                                                   for(i in 1:length(lookUpTable)) {
                                                                                       if(lookUpTable[[i]]$val != 'HubTime' && lookUpTable[[i]]$val != 'UniAgeNow')
                                                                                           l[[lookUpTable[[i]]$label]]<-lookUpTable[[i]]$val
                                                                                   };l},
                                 selected="CoVol"),
                     fluidRow(
                         column(6,
                                checkboxInput("customLogY", label = "Log y axis", value = FALSE)
                         ),
                         column(6,
                                checkboxInput("customFlipY", label = "Flip y axis", value = FALSE)
                         )
                     ),
                     h4("Save Data:"),
                     downloadButton("saveData_txt",label="Download as .txt"),
                     downloadButton("saveData_csv",label="Download as .csv")
                 ),
                 mainPanel(
                     plotOutput("plotDistOut"),
                     plotOutput("customPlotOut")
                 )
             )
    ),
    # The Survey Design tab #
    #########################
    tabPanel("Survey Design",
             sidebarLayout(
                 sidebarPanel(
                     fluidRow(
                         column(6,
                                actionButton(inputId="sky_submit", label=span("Calculate"), icon("random"))
                         ),
                         column(6,
                                numericInput("sky_SigFigs",label="Sig Figs",value=6,min=1,max=15)
                         )
                     ),
                     h4("Set Variables:"),
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
                         column(6,
                                textInput(inputId="sky_minz", label="min z", value="0")
                         ),
                         column(6,
                                textInput(inputId="sky_maxz", label="max z", value="0.5")
                         )
                     ),
                     tags$hr(),
                     fluidRow(
                         column(6,
                                selectInput(inputId="sky_Defaults", label="Reference Set", choices = names(defaultParams))
                         ),
                         column(6,
                                textInput(inputId="sky_H0", label="H0", value="70.0")
                         )
                     ),
                     fluidRow(
                         column(6,
                                textInput(inputId="sky_OmegaM", label="OmegaM", value="0.3")
                         ),
                         column(6,
                                textInput(inputId="sky_OmegaL", label="OmegaL", value="1-OmegaM")
                         )
                     ),
                     h4("Find Area (optional):"),
                     actionButton(inputId="sky_setArea", icon("arrow-up")),
                     br(), br(),
                     fluidRow(
                         column(6,
                                textInput(inputId="sky_lat1", label="Latitude 1 (deg)", value="-2")
                         ),
                         column(6,
                                textInput(inputId="sky_lat2", label="Latitude 2 (deg)", value="3")
                         )
                     ),
                     fluidRow(
                         column(6,
                                textInput(inputId="sky_long1", label="Longitude 1 (deg)", value="129")
                         ),
                         column(6,
                                textInput(inputId="sky_long2", label="Longitude 2 (deg)", value="141")
                         )
                     )
                 ),
                 mainPanel(
                     h4("Result:"),
                     uiOutput("sky_out")
                 ) 
             )
    ),
    # The Info tab #
    ################
    tabPanel("Info",
             div(
                 h3("About"),
                 p(span("Welcome to ICRAR's Cosmology Calculator!", style="color:#08c"),
                   "This calculator was written by", strong("Joseph Dunne"), "and", strong("Aaron Robotham"), "in the programming language R, and uses the library Shiny to provide the interface. The functions are available as part of Aaron Robotham's R", a("celestial", href="https://github.com/asgr/celestial", target="_blank"), "package."),
                 br(),
                 h4("Calculation", style='color:#08c'),
                 p(
                     "This tab is used to calculate various distance parameters."
                 ),
                 p(
                     "To use this tab, fill in the variables under", strong("Set Variables"), "and click the", actionButton(inputId="dud", label="Calculate", icon("random")),
                     "button to calculate variables at a certain redshift. Type '1-OmegaM' into the OmegaL field to set OmegaL to", span("1-OmegaM", style="text-decoration:underline;"),
                     "for all calculations.",
                     "The", strong("Reference Set"), "box lets you set the variables (H0, OmegaM and OmegaL) to a reference set (e.g. Planck, WMAP)."
                 ),
                 p(
                     "Under", strong("Custom Calc,"), "the calculation may be done using a chosen variable from the", span("Variable", style="text-decoration:underline;"), "menu.",
                     "When the", span("Value", style="text-decoration:underline;"), "box contains a value, the custom calculation will be used next time the", actionButton(inputId="dud", label="Calculate", icon("random")),
                     "button is clicked.",
                     "Mappings are not availabe for AngDist (angular diameter distance) or AngSize (physical projected size) because the solutions are ambiguous."
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
                     "for all calculations.",
                     "The", strong("Reference Set"), "box lets you set the variables (H0, OmegaM and OmegaL) to a reference set (e.g. Planck, WMAP).",
                     "Some of the plot options are as follows:"
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
                     "button to calculate the Comoving Volume.",
                     "The", strong("Reference Set"), "box lets you set the variables (H0, OmegaM and OmegaL) to a reference set (e.g. Planck, WMAP).",
                     "If the area is unknown, an extra option under", strong("Find Area (optional)"), "can be used the find the area of the sky given the latitude and longitude.",
                     "The Area field is then updated by clicking the", actionButton(inputId="dud", icon("arrow-up")), "button."
                 ),
                 br(),
                 fluidRow(
                     column(5,
                            h4("CosmoCalc References"),
                            p(a("Hamilton A. J. S., 2001, MNRAS, 322, 419", href="http://adsabs.harvard.edu/abs/2001MNRAS.322..419H", target="_blank")),
                            p(a("Hogg D. W., et al., 1999 (arXiv 9905116)", href="http://arxiv.org/abs/astro-ph/9905116", target="_blank")),
                            p(a("Lahav O., et al., 1991, MNRAS, 251, 136", href="http://adsabs.harvard.edu/abs/1991MNRAS.251..128L", target="_blank")),
                            p(a("Wright E. L., 2006, PASP, 118, 1711", href="http://adsabs.harvard.edu/abs/2006PASP..118.1711W", target="_blank")),
                            br()
                            ),
                     column(7,
                            h4("Reference Sets"),
                            HTML("<table class='table table-condensed'><tbody>
                                <tr>
                                <td>Planck</th>
                                <td><a href='http://arxiv.org/abs/1303.5076' target='_blank'>Planck Collaboration, 2014, A&A, 571, 16 (arXiv:1303.5076)</a></th>
                                </tr>
                                <tr>
                                <td>WMAP9</th>
                                <td><a href='http://arxiv.org/abs/1212.5226' target='_blank'>Hinshaw G., et al., 2013, ApJS, 208, 19 (arXiv:1212.5226)</a></th>
                                </tr>
                                <td>WMAP7</th>
                                <td><a href='http://arxiv.org/abs/1001.4538' target='_blank'>Komatsu E., et al., 2010, ApJS, 192, 18 (arXiv:1001.4538)</a></th>
                                </tr>
                                <td>WMAP5</th>
                                <td><a href='http://arxiv.org/abs/0803.0547' target='_blank'>Dunkley J., et al., 2009, ApJS, 180, 306 (arXiv:0803.0547)</a></th>
                                </tr>
                                <td>WMAP3</th>
                                <td><a href='http://arxiv.org/abs/astro-ph/0603449' target='_blank'>Spergel D. N., et al., 2007, ApJS, 170, 377 (arXiv:astro-ph/0603449)</a></th>
                                </tr>
                                <td>WMAP1</th>
                                <td><a href='http://arxiv.org/abs/astro-ph/0302209' target='_blank'>Spergel D. N., et al., 2003, ApJS, 148, 175 (arXiv:astro-ph/0302209)</a></th>
                                </tr>
                                <td>Millennium</th>
                                <td><a href='http://arxiv.org/abs/astro-ph/0504097' target='_blank'>Springel V., et al., 2005, Nature, 435, 629 (arXiv:astro-ph/0504097)</a></th>
                                </tr>
                                <td>GiggleZ</th>
                                <td><a href='http://arxiv.org/abs/1407.0390' target='_blank'>Poole G. B., et al., 201?, MNRAS, ?, ? (arXiv:1407.0390)</a></th>
                                </tr>
                                </tbody></table>")
                            )
                    ),
             class="container-fluid")
    ),
    # The R Code tab #
    ##################
    tabPanel("R Code",
             p("Basic cosmological distance calculator R code used server-side to generate outputs."),
             p("Written by Aaron Robotham as part of the R",a("celestial", href="https://github.com/asgr/celestial", target="_blank"), "package (see", span("Info", style='color:#08c'), "tab for references)."),
             strong("cosdist"),
             HTML('<pre class="prettyprint lang-r" style="padding:5px">
function (z = 1, H0 = 100, OmegaM = 0.3, OmegaL = 1 - OmegaM, 
    age = FALSE, error = FALSE) 
{
    if (!all(is.finite(z))) {
        stop("All z must be finite and numeric")
    }
    if (!all(z >= 0)) {
        stop("All z must be > -1")
    }
    OmegaK = 1 - OmegaM - OmegaL
    Einv = function(z, OmegaM, OmegaL, OmegaK) {
        1/sqrt(OmegaM * (1 + z)^3 + OmegaK * (1 + z)^2 + OmegaL)
    }
    if (age) {
        Einvz = function(z, OmegaM, OmegaL, OmegaK) {
            1/(sqrt(OmegaM * (1 + z)^3 + OmegaK * (1 + z)^2 + 
                OmegaL) * (1 + z))
        }
    }
    temp = function(z, H0, OmegaM, OmegaL, OmegaK) {
        HubDist = (299792.458/H0)
        temp = integrate(Einv, 0, z, OmegaM = OmegaM, OmegaL = OmegaL, 
            OmegaK = OmegaK, subdivisions = 1000L)
        CoDist = HubDist * temp$value
        if (error) {
            if (z > 0) {
                RelError = abs(temp$abs.error/temp$value)
            }
            else {
                RelError = 0
            }
        }
        if (OmegaK == 0) {
            CoDistTran = CoDist
            CoVol = ((4/3) * pi * CoDist^3)/1e+09
        }
        else {
            if (OmegaK > 0) {
                CoDistTran = HubDist * (1/sqrt(OmegaK)) * sinh(sqrt(OmegaK) * 
                    CoDist/HubDist)
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
                    (CoDistTran/HubDist))))/1e+09
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
                OmegaL = OmegaL, OmegaK = OmegaK, subdivisions = 1000L)$value
            zAge = HT * integrate(Einvz, 0, z, OmegaM = OmegaM, 
                OmegaL = OmegaL, OmegaK = OmegaK, subdivisions = 1000L)$value
        }
        if (error) {
            if (age) {
                return = c(z = z, a = a, CoDist = CoDist, LumDist = LumDist, 
                    AngDist = AngDist, CoDistTran = CoDistTran, 
                    DistMod = DistMod, AngSize = AngSize, CoVol = CoVol, 
                    HubTime = HT, UniAgeNow = UniAge, UniAgeAtz = UniAge - 
                    zAge, TravelTime = zAge, RelError = RelError)
            }
            else {
                return = c(z = z, a = a, CoDist = CoDist, LumDist = LumDist, 
                    AngDist = AngDist, CoDistTran = CoDistTran, 
                    DistMod = DistMod, AngSize = AngSize, CoVol = CoVol, 
                    RelError = RelError)
            }
        }
        else {
            if (age) {
                return = c(z = z, a = a, CoDist = CoDist, LumDist = LumDist, 
                    AngDist = AngDist, CoDistTran = CoDistTran, 
                    DistMod = DistMod, AngSize = AngSize, CoVol = CoVol, 
                    HubTime = HT, UniAgeNow = UniAge, UniAgeAtz = UniAge - 
                    zAge, TravelTime = zAge)
            }
            else {
                return = c(z = z, a = a, CoDist = CoDist, LumDist = LumDist, 
                    AngDist = AngDist, CoDistTran = CoDistTran, 
                    DistMod = DistMod, AngSize = AngSize, CoVol = CoVol)
            }
        }
    }
    return(as.data.frame(t(Vectorize(temp)(z = z, H0 = H0, OmegaM = OmegaM, 
        OmegaL = OmegaL, OmegaK = OmegaK))))
}</pre>'
             ),
             strong("cosgrow"),
             HTML('<pre class="prettyprint lang-r" style="padding:5px">
                  function (z = 1, H0 = 100, OmegaM = 0.3, OmegaL = 1 - OmegaM) 
{
    if (!all(is.finite(z))) {
        stop("All z must be finite and numeric")
    }
    if (!all(z >= 0)) {
        stop("All z must be > -1")
    }
    OmegaK = 1 - OmegaM - OmegaL
    temp = function(z, H0, OmegaM, OmegaL, OmegaK) {
        OmegaSum = OmegaM * (1 + z)^3 + OmegaK * (1 + z)^2 + 
            OmegaL
        Hz = H0 * sqrt(OmegaSum)
        OmegaMAtz = (OmegaM * (1 + z)^3)/OmegaSum
        OmegaLAtz = OmegaL/OmegaSum
        OmegaKAtz = (OmegaK * (1 + z)^2)/OmegaSum
        OmegaK = 1 - OmegaM - OmegaL
        Einva3 = function(a, OmegaM, OmegaL, OmegaK) {
            1/(a^3 * (sqrt(OmegaM * a^(-3) + OmegaK * a^(-2) + 
                OmegaL))^3)
        }
        Factor = (5 * OmegaM/2) * (Hz/H0) * (1 + z) * integrate(Einva3, 
            0, 1/(1 + z), OmegaM = OmegaM, OmegaL = OmegaL, OmegaK = OmegaK, 
            subdivisions = 1000L)$value
        Rate = -1 - OmegaMAtz/2 + OmegaLAtz + (5 * OmegaMAtz)/(2 * 
            Factor)
        G = 6.67384e-11
        Hub2 = H0 * sqrt(OmegaM * (1 + z)^3 + OmegaK * (1 + z)^2 + 
            OmegaL)
        km2m = 1000
        Mpc2m = 3.08567758e+22
        Msol2kg = 1.9891e+30
        RhoCrit = (3 * Hub2)/(8 * pi * G) * (km2m^2) * Mpc2m/Msol2kg
        return = c(z = z, a = 1/(1 + z), H = Hz, OmegaM = OmegaMAtz, 
            OmegaL = OmegaLAtz, OmegaK = OmegaKAtz, Factor = Factor, 
            Rate = Rate, RhoCrit = RhoCrit)
    }
    return(as.data.frame(t(Vectorize(temp)(z = z, H0 = H0, OmegaM = OmegaM, 
    OmegaL = OmegaL, OmegaK = OmegaK))))
}</pre>'
             )
    )
    ),
    br(),
    div(
        span(a("ICRAR", href="http://www.icrar.org/", target="_blank"), "2015, written by Joseph Dunne, Aaron Robotham", style="color:grey;font-size:12px;"),
        style="text-align:center;margin-bottom:3px;"
    )
))