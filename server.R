source("cosdist.R")
library("magicaxis")

lookUpTable <- list(list("label"="z", "unit"="", "val"="z"),
                    list("label"="Comoving Radial Distance LoS", "unit"="(Mpc)", "val"="CoDistLoS"),
                    list("label"="Luminosity Distance", "unit"="(Mpc)", "val"="LumDist"),
                    list("label"="Angular Size Distance", "unit"="(Mpc)", "val"="AngDist"),
                    list("label"="Comoving Radial Distance Tran", "unit"="(Mpc)", "val"="CoDistTran"),
                    list("label"="DistMod", "unit"="(mag)", "val"="DistMod"),
                    list("label"="Angular Size", "unit"="(kpc/arcsec)", "val"="AngArcSec"),
                    list("label"="Comoving Volume", "unit"="(Gpc³)", "val"="CoVolGpc3"),
                    list("label"="Hubble Time", "unit"="(yr)", "val"="HubTime"),
                    list("label"="Universe Age Now", "unit"="(yr)", "val"="UniAgeNow"),
                    list("label"="Universe Age at z", "unit"="(yr)", "val"="UniAgeAtz"),
                    list("label"="Travel Time", "unit"="(yr)", "val"="TravelTime")
)

shinyServer(function(input, output) {
  
  output$calcOut <- renderUI ({
    
    # reactive output
    input$submitCalc
    
    # get variables
    z <- isolate(as.numeric(input$calcz))
    H0 <- isolate(as.numeric(input$calcH0))
    OmegaM <- isolate(as.numeric(input$calcOmegaM))
    if(gsub(" ", "", tolower(isolate(input$calcOmegaL)), fixed = TRUE)=="1-omegam") {
      OmegaL <- 1 - OmegaM
    }
    else {
      OmegaL <- as.numeric(isolate(input$calcOmegaL))
    }
    
    # get results
    r <- cosdist(z, H0, OmegaM, OmegaL, TRUE)
    
    # build output
    l <- paste0("<h4>For z = ", z, ":</h4>")
    for(t in lookUpTable) {
      if(t$label != "z")
        l <- append(l, paste0("<p>The <b>",t$label,"</b> is <span style='color:#08c;'>", r[[t$val]], "</span> ", t$unit, "</p>"))
    }
    
    # the output
    HTML(l)
  })
  
  plotResult <- reactive({
    
    # get variables
    H0 <- as.numeric(input$plotH0)
    OmegaM <- as.numeric(input$plotOmegaM)
    if(gsub(" ", "", tolower(isolate(input$plotOmegaL)), fixed = TRUE)=="1-omegam") {
      OmegaL <- 1 - OmegaM
    }
    else {
      OmegaL <- as.numeric(input$plotOmegaL)
    }
    
    # get graph settings
    start <- as.numeric(input$plotStart)
    end <- as.numeric(input$plotEnd)
    res <- as.numeric(input$plotRes)
    
    # get results
    z <- seq(start, end, (end-start)/res)
    r <- cosdist(z, H0, OmegaM, OmegaL, TRUE)
    
    return (r)
  })
  
  output$saveData_txt <- downloadHandler(
    filename = function() { paste("plot", "txt", sep=".") },
    content = function(file) { write.table(plotResult(), file, sep=" ", row.names=FALSE) }
  )
  
  output$saveData_csv <- downloadHandler(
    filename = function() { paste("plot", "csv", sep=".") },
    content = function(file) { write.table(plotResult(), file, sep=", ", row.names=FALSE) }
  )
  
  output$plotDistOut <- renderPlot({
    
    # get inputs
    input$submitPlot
    r <- isolate(plotResult())
    xAxis <- lookUpTable[[as.numeric(input$plotAxis)]]
    useLog <- input$plotLogY
    
    # check for log
    if(useLog) log <- 'y' else log <- ''
    
    # for scaling
    all <- c(r$LumDist, r$CoDistTran, r$CoDistLoS, r$AngDist)
    ymin <- min(all)
    ymax <- max(all)
    if(useLog && ymin <= 0) # TEMPORARY FIX
      ymin <- 1
    
    # plot
    magplot(x=r[[xAxis$val]], y=r$LumDist, ylim=c(ymin, ymax), main=paste("Distance vs ", xAxis$label), xlab=paste(xAxis$label, xAxis$unit), ylab="Distance (Mpc)", type="l", col='red',log=log)
    lines(x=r[[xAxis$val]], y=r$CoDistTran, type='l', lty=2, col='black')
    lines(x=r[[xAxis$val]], y=r$CoDistLoS, type='l', col='black')
    lines(x=r[[xAxis$val]], y=r$AngDist, type='l', col='blue')
    legend("topleft",bty='n',legend=c('Co Dist (LoS)','Co Dist (tran)','Lum Dist','Ang Dist'),col=c('black','black','red','blue'),lty=c(1,2,1,1))
  })
  
  output$customYValueOut <- renderUI ({
    
    # get inputs
    input$submitPlot
    r <- isolate(plotResult())
    x <- input$customYValue
    xAxis <- lookUpTable[[as.numeric(input$customXAxis)]]
    yAxis <- lookUpTable[[as.numeric(input$customYAxis)]]
    
    # get y value at x
    tempfunc = approxfun(r[[xAxis$val]], r[[yAxis$val]], method="linear")
    y <- tempfunc(x)
    
    # output
    HTML("<p>y is <span style='color:#08c;'>", y, "</span> ", yAxis$unit, "</p>")
  })
  
  output$customPlotOut <- renderPlot({
    
    # get inputs
    input$submitPlot
    r <- isolate(plotResult())
    xAxis <- lookUpTable[[as.numeric(input$customXAxis)]]
    yAxis <- lookUpTable[[as.numeric(input$customYAxis)]]
    useLogX <- input$customLogX
    useLogY <- input$customLogY
    
    tempfunc = approxfun(r[[xAxis$val]], r[[yAxis$val]], method="linear")
    
    # check for log
    log <- ''
    if(useLogX)
      log <- 'x'
    if(useLogY)
      log <- paste0(log, 'y')
    
    # plot
    magplot(x=r[[xAxis$val]], y=r[[yAxis$val]], main=paste0(yAxis$label, " vs ", xAxis$label),
            xlab=paste(xAxis$label, xAxis$unit), ylab=paste(yAxis$label, yAxis$unit),type="l",
            col='black',log=log)
  })
  
})