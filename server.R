require("celestial")
require("magicaxis")
source("table.R")

shinyServer(function(input, output, clientData, session) {
    
    # Helper Function to get OmegaL #
    #################################
    getOmegaL <- function(OmegaM, OmegaL_string) {
        if(gsub(" ", "", tolower(OmegaL_string), fixed = TRUE)=="1-omegam") {
            OmegaL <- 1 - OmegaM
        }
        else {
            OmegaL <- as.numeric(OmegaL_string)
        }
        return (OmegaL)
    }
    
    # The Cosmo Calc #
    ##################
    calcResult <- reactive ({
        
        # make output reactive to 'Calculate' button
        input$submitCalc
        
        # get variables from input
        z <- isolate(as.numeric(input$calcz))
        H0 <- isolate(as.numeric(input$calcH0))
        OmegaM <- isolate(as.numeric(input$calcOmegaM))
        OmegaL <- getOmegaL(OmegaM, isolate(input$calcOmegaL))
        
        # if the custom field is filled, use the custom calculation
        if(nchar(isolate(input$custom_calcValue)) > 0) {
            # get the custom variable/value
            axis <- lookUpTable[[isolate(input$custom_calcAxis)]]
            axisValue <- as.numeric(isolate(input$custom_calcValue))
            # unit conversion
            if(axis$val=='RhoCrit'){axisValue=axisValue*1e10}
            
            # get custom results
            r <- cosmapval(axisValue, axis$val, H0, OmegaM, OmegaL, zrange=c(-0.99,100), res=12, iter=12, out='cos')
            updateTextInput(session, "calcz", value = r$z)
            r <- cbind(r, cosgrow(r$z, H0, OmegaM, OmegaL))
        }
        else {
            # get normal results
            r <- cosdist(z, H0, OmegaM, OmegaL, age=T, error=T)
            r <- cbind(r, cosgrow(z, H0, OmegaM, OmegaL))
        }
        
        # unit conversion
        r$RhoCrit <- r$RhoCrit/1e10
        
        return (r)
    })
    
    # The Calculation Output (column 1) #
    #####################################
    output$calcOut_1 <- renderUI ({
        
        # get calculation result + sigFigs (reactive!)
        r <- calcResult()
        sigF <- input$calcSigFigs
        t <- lookUpTable
        
        # build output based on the results (r) and the lookUpTable (t)
        list(
            HTML("<h4>Results :</h4>"),
            HTML("<p>The redshift <b>z</b> is <span style='color:#08c;'>", signif(r$z,digits=sigF), "</span></p>"),
            HTML("<p>The expansion factor <b>a</b> is <span style='color:#08c;'>", signif(r$a,digits=sigF), "</span></p>"),
            HTML("<p>The <b>Relative Error</b> is <span style='color:#08c;'>±", signif(r$RelError,digits=sigF), "</span></p>"),
            HTML("<br/>"),
            HTML("<h4>Distances :</h4>"),
            HTML("<p>The <b>", t$CoDist$label, "</b> to z is <span style='color:#08c;'>", signif(r$CoDist,digits=sigF), "</span> ", t$CoDist$unit_html,"</p>"),
            HTML("<p>The <b>", t$LumDist$label, "</b> to z is <span style='color:#08c;'>", signif(r$LumDist,digits=sigF), "</span> ", t$LumDist$unit_html,"</p>"),
            HTML("<p>The <b>", t$AngDist$label, "</b> to z is <span style='color:#08c;'>", signif(r$AngDist,digits=sigF), "</span> ", t$AngDist$unit_html,"</p>"),
            HTML("<p>The <b>", t$CoDistTran$label, "</b> to z is <span style='color:#08c;'>", signif(r$CoDistTran,digits=sigF), "</span> ", t$CoDistTran$unit_html,"</p>"),
            HTML("<p>The <b>", t$DistMod$label, "</b> to z is <span style='color:#08c;'>", signif(r$DistMod,digits=sigF), "</span> ", t$DistMod$unit_html,"</p>"),
            HTML("<p>The <b>", t$AngSize$label, "</b> at z is <span style='color:#08c;'>", signif(r$AngSize,digits=sigF), "</span> ", t$AngSize$unit_html,"</p>"),
            HTML("<p>The <b>", t$CoVol$label, "</b> to z is <span style='color:#08c;'>", signif(r$CoVol,digits=sigF), "</span> ", t$CoVol$unit_html,"</p>"),
            HTML("<br/>")
        )
    })
    
    # The Calculation Output (column 2) #
    #####################################
    output$calcOut_2 <- renderUI ({
        
        # get calculation result + sigFigs (reactive!)
        r <- calcResult()
        sigF <- input$calcSigFigs
        t <- lookUpTable
        
        # build output based on the results (r) and the lookUpTable (t)
        list(
            HTML("<h4>z dependent times :</h4>"),
            HTML("<p>The <b>", t$UniAgeAtz$label, "</b> at z is <span style='color:#08c;'>", signif(r$UniAgeAtz,digits=sigF), "</span> ", t$UniAgeAtz$unit_html,"</p>"),
            HTML("<p>The <b>", t$TravelTime$label, "</b> at z is <span style='color:#08c;'>", signif(r$TravelTime,digits=sigF), "</span> ", t$TravelTime$unit_html,"</p>"),
            HTML("<br/>"),
            HTML("<h4>z independent times :</h4>"),
            HTML("<p>The <b>", t$HubTime$label, "</b> is <span style='color:#08c;'>", signif(r$HubTime,digits=sigF), "</span> ", t$HubTime$unit_html, "</p>"),
            HTML("<p>The <b>", t$UniAgeNow$label, "</b> is <span style='color:#08c;'>", signif(r$UniAgeNow,digits=sigF), "</span> ", t$UniAgeNow$unit_html, "</p>"),
            HTML("<br/>"),
            HTML("<h4>Structural evolution properties :</h4>"),
            HTML("<p>Hubble's constant <b>H</b> at z is <span style='color:#08c;'>", signif(r$H,digits=sigF), "</span> ", t$H$unit_html, "</p>"),
            HTML("<p><b>", t$OmegaM$label, "</b> at z is <span style='color:#08c;'>", signif(r$OmegaM,digits=sigF), "</span> ", t$OmegaM$unit_html, "</p>"),
            HTML("<p><b>", t$OmegaL$label, "</b> at z is <span style='color:#08c;'>", signif(r$OmegaL,digits=sigF), "</span> ", t$OmegaL$unit_html, "</p>"),
            HTML("<p><b>", t$OmegaK$label, "</b> at z is <span style='color:#08c;'>", signif(r$OmegaK,digits=sigF), "</span> ", t$OmegaK$unit_html, "</p>"),
            HTML("<p>The <b>", t$Factor$label, "</b> to z is <span style='color:#08c;'>", signif(r$Factor,digits=sigF), "</span> ", t$Factor$unit_html, "</p>"),
            HTML("<p>The <b>", t$Rate$label, "</b> at z is <span style='color:#08c;'>", signif(r$Rate,digits=sigF), "</span> ", t$Rate$unit_html, "</p>"),
            HTML("<p>The <b>", t$RhoCrit$label, "</b> at z is <span style='color:#08c;'>", signif(r$RhoCrit,digits=sigF), "</span> ", t$RhoCrit$unit_html, "</p>")
        )
    })
    
    # The custom calc label #
    #########################
    output$custom_calcUnit <- renderUI({
        
        # construct the label
        var <- lookUpTable[[input$custom_calcAxis]]
        str <- paste("Value", var$unit_html)
        
        # if the input box has something, make text green
        if(nchar(input$custom_calcValue) > 0) {
            HTML("<span style='color:green;'>", str, "</span>")
        }
        else {
            HTML("<span>", str, "</span>")
        }
    })
    
    # The custom calc defaults #
    ############################
    
    calcLastAction <- reactiveValues(last = "none")
    
    # Changes selection field based on text
    observe({
        H0 <- as.numeric(input$calcH0)
        OmegaM <- as.numeric(input$calcOmegaM)
        OmegaL <- getOmegaL(OmegaM, input$calcOmegaL)
        
        # if the text entry is by the user, check if it matches any of the default values.
        if(isolate(calcLastAction$last) != "updateText") {
            if(is.na(H0) || is.na(OmegaM) || is.na(OmegaL)) {
                updateSelectInput(session, "calcDefaults", selected = "Custom")
                return()
            }
            for(n in names(defaultParams)) {
                if(n != "Custom") {
                    l <- defaultParams[[n]]
                    if(H0 == l$H0 && OmegaM == l$OmegaM && OmegaL == l$OmegaL) {
                        calcLastAction$last <- "updateSelect"
                        updateSelectInput(session, "calcDefaults", selected = n)
                        return()
                    }
                }
            }
            updateSelectInput(session, "calcDefaults", selected = "Custom")
        }
        else {
            calcLastAction$last <- "none"
        }
    })
    
    # Changes text based on selection filed
    observe ({
        selected <- input$calcDefaults
        # if the selection is by the user, update the text input fields
        if(selected != "Custom" && isolate(calcLastAction$last) != "updateSelect") {
            calcLastAction$last <- "updateText"
            updateTextInput(session, "calcH0", value = defaultParams[[selected]]$H0)
            updateTextInput(session, "calcOmegaM", value = defaultParams[[selected]]$OmegaM)
            updateTextInput(session, "calcOmegaL", value = defaultParams[[selected]]$OmegaL)
        }
        else {
            calcLastAction$last <- "none"
        }
    })
    
    # The calculated plot result #
    ##############################
    plotResult <- reactive({
        
        # get variables from inputs
        H0 <- as.numeric(input$plotH0)
        OmegaM <- as.numeric(input$plotOmegaM)
        OmegaL <- getOmegaL(OmegaM, input$plotOmegaL)
        
        # get the z points using input
        start <- as.numeric(input$plotStart)
        end <- as.numeric(input$plotEnd)
        res <- input$plotRes
        if(res<10){res=10; updateNumericInput(session, "plotRes", value=10)}
        else if(res>1000){res=1000; updateNumericInput(session, "plotRes", value=1000)}
        z <- seq(start, end, (end-start)/res)
        
        # get results
        r <- cosdist(z, H0, OmegaM, OmegaL, TRUE)
        r <- cbind(r, cosgrow(z, H0, OmegaM, OmegaL))
        # unit conversion
        r$RhoCrit <- r$RhoCrit/1e10
        
        return (r)
    })
    
    # Plot save buttons #
    #####################
    output$saveData_txt <- downloadHandler(
        filename = function() { paste("plot", "txt", sep=".") },
        content = function(file) { write.table(plotResult(), file, sep=" ", row.names=FALSE) }
    )
    output$saveData_csv <- downloadHandler(
        filename = function() { paste("plot", "csv", sep=".") },
        content = function(file) { write.table(plotResult(), file, sep=",", row.names=FALSE) }
    )
    
    # Distance Plot Output #
    ########################
    output$plotDistOut <- renderPlot({
        
        # make plot reactive to the submitPlot button
        input$submitPlot
        
        # get plot results + plot parameters
        r <- isolate(plotResult())
        xAxis <- lookUpTable[[input$plotAxis]]
        useLogX <- input$plotLogX
        useLogY <- input$plotLogY
        
        # find the min and max distances to scale Y axis correctly
        all <- c(r$LumDist, r$CoDistTran, r$CoDist, r$AngDist)
        ymin <- min(all)
        ymax <- max(all)
        
        # set the 'log' attribute in magplot
        log <- ''
        if(useLogX) {
            log <- 'x'
        }
        if(useLogY) {
            log <- paste0(log, 'y')
            ymin <- min(all[all>0])
        }
        
        # format the x axis label
        xlab_str <- paste0('"',xAxis$label,'"')
        if(nchar(xAxis$unit_r)>0) xlab_str <- paste0(xlab_str, "~", xAxis$unit_r)
        
        # plot results
        magplot(x=r[[xAxis$val]], y=r$LumDist, ylim=c(ymin, ymax), main=paste("Distance vs ", xAxis$label), xlab=parse(text=xlab_str), ylab="Distance (Mpc)", type="l", col='red',log=log)
        lines(x=r[[xAxis$val]], y=r$CoDistTran, type='l', lty=2, col='black')
        lines(x=r[[xAxis$val]], y=r$CoDist, type='l', col='black')
        lines(x=r[[xAxis$val]], y=r$AngDist, type='l', col='blue')
        legend("topleft",bty='n',legend=c('Co Dist (LoS)','Co Dist (tran)','Lum Dist','Ang Dist'),col=c('black','black','red','blue'),lty=c(1,2,1,1))
    })
    
    # Custom Plot output #
    ######################
    output$customPlotOut <- renderPlot({
        
        # make plot reactive to the submitPlot button
        input$submitPlot
        
        # get plot results + plot attributes
        r <- isolate(plotResult())
        xAxis <- lookUpTable[[input$customXAxis]]
        yAxis <- lookUpTable[[input$customYAxis]]
        useLogX <- input$customLogX
        useLogY <- input$customLogY
        flipX <- input$customFlipX
        flipY <- input$customFlipY
        
        # set range
        xRange <- range(r[[xAxis$val]], finite=T)
        yRange <- range(r[[yAxis$val]], finite=T)
        
        # set the 'log' attribute in magplot
        log <- ''
        if(useLogX) {
            log <- 'x'
            xRange <- range(r[[xAxis$val]][c(r[[xAxis$val]]>0)])
        }
        if(useLogY) {
            log <- paste0(log, 'y')
            yRange <- range(r[[yAxis$val]][c(r[[yAxis$val]]>0)])
        }
        
        # check if axes flipped
        if(input$customFlipX) {
            xRange <- rev(xRange)
        }
        if(input$customFlipY) {
            yRange <- rev(yRange)
        }
        # format xlab and ylab
        xlab_str <- paste0('"',xAxis$label,'"')
        if(nchar(xAxis$unit_r)>0) xlab_str <- paste0(xlab_str, "~", xAxis$unit_r)
        ylab_str <- paste0('"',yAxis$label,'"')
        if(nchar(yAxis$unit_r)>0) ylab_str <- paste0(ylab_str, "~", yAxis$unit_r)
        
        # plot results
        magplot(x=r[[xAxis$val]], y=r[[yAxis$val]], xlim=xRange, ylim=yRange,
                main=paste0(yAxis$label, " vs ", xAxis$label),
                xlab=parse(text=xlab_str), ylab=parse(text=ylab_str),type="l",
                col='black',log=log)
    })
    
    # The plot defaults #
    #####################
    
    plotLastAction <- reactiveValues(last = "none")
    
    # Changes selection field based on text
    observe({
        H0 <- as.numeric(input$plotH0)
        OmegaM <- as.numeric(input$plotOmegaM)
        OmegaL <- getOmegaL(OmegaM, input$plotOmegaL)
        
        # if the text entry is by the user, check if it matches any of the default values.
        if(isolate(plotLastAction$last) != "updateText") {
            if(is.na(H0) || is.na(OmegaM) || is.na(OmegaL)) {
                updateSelectInput(session, "plotDefaults", selected = "Custom")
                return()
            }
            for(n in names(defaultParams)) {
                if(n != "Custom") {
                    l <- defaultParams[[n]]
                    if(H0 == l$H0 && OmegaM == l$OmegaM && OmegaL == l$OmegaL) {
                        plotLastAction$last <- "updateSelect"
                        updateSelectInput(session, "plotDefaults", selected = n)
                        return()
                    }
                }
            }
            updateSelectInput(session, "plotDefaults", selected = "Custom")
        }
        else {
            plotLastAction$last <- "none"
        }
    })
    
    # Changes text based on selection filed
    observe ({
        selected <- input$plotDefaults
        # if the selection is by the user, update the text input fields
        if(selected != "Custom" && isolate(plotLastAction$last) != "updateSelect") {
            plotLastAction$last <- "updateText"
            updateTextInput(session, "plotH0", value = defaultParams[[selected]]$H0)
            updateTextInput(session, "plotOmegaM", value = defaultParams[[selected]]$OmegaM)
            updateTextInput(session, "plotOmegaL", value = defaultParams[[selected]]$OmegaL)
        }
        else {
            plotLastAction$last <- "none"
        }
    })
    
    # Survey Design "Find Area (optional)" section #
    ################################################
    observe ({
        
        # check if the up button is pressed
        if(input$sky_setArea > 0) {
            
            # get variables from inputs
            long1 <- as.numeric(isolate(input$sky_long1))
            long2 <- as.numeric(isolate(input$sky_long2))
            lat1 <- as.numeric(isolate(input$sky_lat1))
            lat2 <- as.numeric(isolate(input$sky_lat2))
            
            # Allow reverse longitude/latitude
            if(long1 > long2) {
                temp <- long1
                long1 <- long2
                long2 <- temp
            }
            if(lat1 > lat2) {
                temp <- lat1
                lat1 <- lat2
                lat2 <- temp
            }
            
            # get result
            a <- skyarea(long = c(long1,long2), lat = c(lat1,lat2), inunit = "deg", outunit = "deg2")
            
            # update main inputs with result
            updateTextInput(session, "sky_area", value = as.numeric(a[1]))
            updateSelectInput(session, "sky_areaUnit", selected = "deg2")
        }
    })
    
    # Survey Design Result #
    ########################
    skyResult <- reactive ({
        
        # make reactive to 'Calculate' button
        input$sky_submit
        
        # get variables from inputs
        area <- as.numeric(isolate(input$sky_area))
        H0 <- as.numeric(isolate(input$sky_H0))
        OmegaM <- as.numeric(isolate(input$sky_OmegaM))
        if(gsub(" ", "", tolower(isolate(input$sky_OmegaL)), fixed = TRUE)=="1-omegam") {
            OmegaL <- 1 - OmegaM
        }
        else {
            OmegaL <- as.numeric(isolate(input$sky_OmegaL))
        }
        unit <- isolate(input$sky_areaUnit)
        minz <- as.numeric(isolate(input$sky_minz))
        maxz <- as.numeric(isolate(input$sky_maxz))
        
        # get result
        s <- cosvol(area, maxz, minz, H0, OmegaM, OmegaL, unit)
        
        return(s)
    })
    
    # Survey Design Output #
    ########################
    output$sky_out <- renderUI ({
        
        # get results + sig figs
        s <- skyResult()
        sigF <- input$sky_SigFigs
        
        # output result
        HTML("<p>The <b>Comoving Volume</b> is <span style='color:#08c;'>", signif(s,digits=sigF), "</span> (Gpc<sup>3</sup>)</p>")
    })
    
    # Survey Design defaults #
    ##########################
    
    sky_LastAction <- reactiveValues(last = "none")
    
    # Changes selection field based on text
    observe({
        H0 <- as.numeric(input$sky_H0)
        OmegaM <- as.numeric(input$sky_OmegaM)
        OmegaL <- getOmegaL(OmegaM, input$sky_OmegaL)
        
        # if the text entry is by the user, check if it matches any of the default values.
        if(isolate(sky_LastAction$last) != "updateText") {
            if(is.na(H0) || is.na(OmegaM) || is.na(OmegaL)) {
                updateSelectInput(session, "sky_Defaults", selected = "Custom")
                return()
            }
            for(n in names(defaultParams)) {
                if(n != "Custom") {
                    l <- defaultParams[[n]]
                    if(H0 == l$H0 && OmegaM == l$OmegaM && OmegaL == l$OmegaL) {
                        sky_LastAction$last <- "updateSelect"
                        updateSelectInput(session, "sky_Defaults", selected = n)
                        return()
                    }
                }
            }
            updateSelectInput(session, "sky_Defaults", selected = "Custom")
        }
        else {
            sky_LastAction$last <- "none"
        }
    })
    
    # Changes text based on selection filed
    observe ({
        selected <- input$sky_Defaults
        # if the selection is by the user, update the text input fields
        if(selected != "Custom" && isolate(sky_LastAction$last) != "updateSelect") {
            sky_LastAction$last <- "updateText"
            updateTextInput(session, "sky_H0", value = defaultParams[[selected]]$H0)
            updateTextInput(session, "sky_OmegaM", value = defaultParams[[selected]]$OmegaM)
            updateTextInput(session, "sky_OmegaL", value = defaultParams[[selected]]$OmegaL)
        }
        else {
            sky_LastAction$last <- "none"
        }
    })
    
})