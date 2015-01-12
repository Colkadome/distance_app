library("celestial")
library("magicaxis")
source("table.R")

shinyServer(function(input, output, clientData, session) {
  
  output$calcOut <- renderUI ({
    
    # reactive output
    input$submitCalc
    
    # HOW TO CHANGE INPUT DATA
    #updateTextInput(session, "custom_calcValue",
    #                value = paste("New text")
    #)
    
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
    
    # custom variables
    useCustom <- FALSE
    if(nchar(isolate(input$custom_calcValue)) > 0) {
      useCustom <- TRUE
      axis <- lookUpTable[[as.numeric(isolate(input$custom_calcAxis))]]
      axisValue <- as.numeric(isolate(input$custom_calcValue))
    }
    
    # get results
    if(useCustom) {
      r <- cosmapval(axisValue, axis$val, H0, OmegaM, OmegaL, res=12, iter=12, age=TRUE)
      updateTextInput(session, "calcz", value = r$z)
    }
    else {
      r <- cosdist(z, H0, OmegaM, OmegaL, age=TRUE)
    }
    
    # build output
    if(useCustom) {
      l <- paste0("<h4>For ", axis$label," = ", r[[axis$val]], " (±", as.numeric(r$error)*100, "%) ", axis$unit, ":</h4>")
    }
    else {
      l <- paste0("<h4>For z = ", z, " :</h4>")
    }
    for(t in lookUpTable) {
      l <- append(l, paste0("<p>The <b>",t$label,"</b> is <span style='color:#08c;'>", r[[t$val]], "</span> ", t$unit, "</p>"))
    }
    
    # the output
    HTML(l)
  })
  
  output$custom_calcUnit <- renderUI({
    
    # append unit to text
    var <- lookUpTable[[as.numeric(input$custom_calcAxis)]]
    str <- paste("Value", var$unit)
    
    # if the input box has something, make text green
    if(nchar(input$custom_calcValue) > 0) {
      HTML("<span style='color:green;'>", str, "</span>")
    }
    else {
      HTML("<span>", str, "</span>")
    }
  })
  
  plotResult <- reactive({
    
    # get variables
    H0 <- as.numeric(input$plotH0)
    OmegaM <- as.numeric(input$plotOmegaM)
    if(gsub(" ", "", tolower(isolate(input$plotOmegaL)), fixed = TRUE)=="1-omegam") {
      OmegaL <- 1 - OmegaM
    }
    else {
      OmegaL <- as.numeric(isolate(input$plotOmegaL))
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
  
  observe ({
    
    # reactive input
    if(input$sky_setArea > 0) {
      
      # get variables
      long1 <- as.numeric(isolate(input$sky_long1))
      long2 <- as.numeric(isolate(input$sky_long2))
      lat1 <- as.numeric(isolate(input$sky_lat1))
      lat2 <- as.numeric(isolate(input$sky_lat2))
      
      # stop weird bug
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
      
      # update area inputs with result
      updateTextInput(session, "sky_area", value = as.numeric(a[1]))
      updateSelectInput(session, "sky_areaUnit", selected = "deg2")
    }
  })
  
  output$sky_out <- renderUI ({
    
    # make reactive
    input$sky_submit
    
    # get variables
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
    s <- cosvol(area, minz, maxz, H0, OmegaM, OmegaL, unit)
    
    # generate output
    HTML("<p>The <b>Comoving Volume</b> is <span style='color:#08c;'>", s, "</span> (Gpc³)</p>")
  })
  
})