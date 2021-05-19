selectionFilterGraphics <- function(input, output, session, rv){
  observeEvent(input$parseVariable,{
    output$numfilter1Plot <- renderPlot({
      req(input$parseVariable)
      if(input$parseVariable < 1 ) return(NULL)
      #browser()
      if(input$numfilter1 != "None" && input$numfilter1 != "Please upload file or do parse"){
        miniHist(dataset = rv$ACSData_parsed, OEMFieldName = input$box_xvar1_ACS,  
                 OEM = input$licenseeSelect, filterVar = input$numfilter1, yRange = input$numfilterVal1)
      } else NULL
    })
    
    output$numfilter2Plot <- renderPlot({
      req(input$parseVariable)
      if(input$parseVariable < 1 ) return(NULL)
      #browser()
      if(input$numfilter2 != "None" && input$numfilter2 != "Please upload file or do parse"){
        miniHist(dataset = rv$ACSData_parsed, OEMFieldName = input$box_xvar1_ACS,  
                 OEM = input$licenseeSelect, filterVar = input$numfilter2, yRange = input$numfilterVal2)
      } else NULL
    })
    
    output$numfilter3Plot <- renderPlot({
      req(input$parseVariable)
      if(input$parseVariable < 1 ) return(NULL)
      #browser()
      if(input$numfilter3 != "None" && input$numfilter3 != "Please upload file or do parse"){
        miniHist(dataset = rv$ACSData_parsed, OEMFieldName = input$box_xvar1_ACS,  
                 OEM = input$licenseeSelect, filterVar = input$numfilter3, yRange = input$numfilterVal3)
      } else NULL
    })
    
    output$numfilter4Plot <- renderPlot({
      req(input$parseVariable)
      if(input$parseVariable < 1 ) return(NULL)
      #browser()
      if(input$numfilter4 != "None" && input$numfilter4 != "Please upload file or do parse"){
        miniHist(dataset = rv$ACSData_parsed, OEMFieldName = input$box_xvar1_ACS,  
                 OEM = input$licenseeSelect, filterVar = input$numfilter4, yRange = input$numfilterVal4)
      } else NULL
    })

    output$chrfilter1Plot <- renderPlot({
      req(input$parseVariable)
      if(input$parseVariable < 1 ) return(NULL)
      #browser()
      if(input$chrfilter1 != "None" && input$chrfilter1 != "Please upload file or do parse"){
        miniBarChart(dataset = rv$ACSData_parsed, OEMFieldName = input$box_xvar1_ACS,  
                     OEM = input$licenseeSelect, filterVar = input$chrfilter1, chrfilterGrp = input$chrfilterGrp1)
      } else NULL
    })
    
    output$chrfilter2Plot <- renderPlot({
      req(input$parseVariable)
      if(input$parseVariable < 1 ) return(NULL)
      #browser()
      if(input$chrfilter2 != "None" && input$chrfilter2 != "Please upload file or do parse"){
        miniBarChart(dataset = rv$ACSData_parsed, OEMFieldName = input$box_xvar1_ACS,  
                     OEM = input$licenseeSelect, filterVar = input$chrfilter2, chrfilterGrp = input$chrfilterGrp2)
      } else NULL
    })
        
    output$chrfilter3Plot <- renderPlot({
      req(input$parseVariable)
      if(input$parseVariable < 1 ) return(NULL)
      #browser()
      if(input$chrfilter3 != "None" && input$chrfilter3 != "Please upload file or do parse"){
        miniBarChart(dataset = rv$ACSData_parsed, OEMFieldName = input$box_xvar1_ACS,  
                     OEM = input$licenseeSelect, filterVar = input$chrfilter3, chrfilterGrp = input$chrfilterGrp3)
      } else NULL
    })
    
    output$chrfilter4Plot <- renderPlot({
      req(input$parseVariable)
      if(input$parseVariable < 1 ) return(NULL)
      #browser()
      if(input$chrfilter4 != "None" && input$chrfilter4 != "Please upload file or do parse"){
        miniBarChart(dataset = rv$ACSData_parsed, OEMFieldName = input$box_xvar1_ACS,  
                     OEM = input$licenseeSelect, filterVar = input$chrfilter4, chrfilterGrp = input$chrfilterGrp4)
      } else NULL
    })
  })
}

miniHist <- function(dataset = rv$ACSData_parsed, OEMFieldName = input$box_xvar1_ACS,  OEM = input$licenseeSelect, filterVar = input$numfilter1, yRange = input$numfilterVal1){
  expr_licensee <- lazyeval::interp(quote(x == y), x = as.name(OEMFieldName), y = OEM)
  numfilter_expr1 <- lazyeval::interp(quote(x1 >= y1), x1 = as.name(filterVar), y1 = as.double(yRange[1]))
  numfilter_expr2 <- lazyeval::interp(quote(x1 <= y1), x1 = as.name(filterVar), y1 = as.double(yRange[2]))
  if(OEM == "All licensees"){
    dataset <- dataset %>% select_(as.name(filterVar))
  } else {
    dataset <- dataset %>% filter_(expr_licensee) %>% select_(as.name(filterVar))
  }
  dataset <- dataset %>% filter_(numfilter_expr1) %>% filter_(numfilter_expr2) %>% unlist() %>% unname()
  #browser()
  
  if (length(unique(dataset)) != 0){
    par(oma=c(0,0,0,0))  # all sides have 3 lines of space  
    par(mar=c(0,2,0,0) + 0.1)  
    par(bg=NA)
    hist(dataset, xlab = NULL, ylab = NULL, xlim= c(yRange[1], yRange[2]), 
         breaks = 10, ann=FALSE, xaxt='n')#, yaxt = 'n')
    axis(side = 1, at = c(yRange[1], yRange[2]))
  } else NULL
}

miniBarChart <- function(dataset = rv$ACSData_parsed, OEMFieldName = input$box_xvar1_ACS,  
                         OEM = input$licenseeSelect, filterVar = input$chrfilter1, chrfilterGrp = input$chrfilterGrp1){
  expr_licensee <- lazyeval::interp(quote(x == y), x = as.name(OEMFieldName), y = OEM)
  chrfilterGrp <- chrfilterGrp %>% unname() %>% unlist()

  if(OEM == "All licensees"){
    NULL
  } else {
    dataset <- dataset %>% filter_(expr_licensee)
  }
  # browser()
  if(filterVar != "None" && filterVar != "Please upload file or do parse") {
    chrfilter_expr <- lazyeval::interp(quote(x1 %in% y1), x1 = as.name(filterVar), y1 = as.character(chrfilterGrp))
    if(filterVar!= "None") {
      # if(chrfilter[i] == "Blank/NA"){
      
      # } else {
      chrfilter <- filterVar
      chrfilterGrp <-chrfilterGrp
      noNAEntry = chrfilterGrp[chrfilterGrp!=""]
      if(length(noNAEntry) > 0) {
        chrfilter_expr <- lazyeval::interp(quote(x1 %in% y1), x1 = as.name(chrfilter), y1 = as.character(chrfilterGrp[chrfilterGrp!=""]))
        if("" %in% chrfilterGrp){
          chrfilter_expr <- lazyeval::interp(quote(x1 %in% y1), x1 = as.name(chrfilter), y1 = c(as.character(chrfilterGrp[chrfilterGrp!=""]),NA))
          # chrfilter_expr_NA <- lazyeval::interp(quote(x1 %>% is.na()), x1 = as.name(chrfilter))
          dataset <- dataset %>% filter_(chrfilter_expr)
        } else {
          dataset <- dataset %>% filter_(chrfilter_expr)
        }
      } else if("" %in% chrfilterGrp){
        chrfilter_expr_NA <- lazyeval::interp(quote(x1 %>% is.na()), x1 = as.name(chrfilter))
        dataset <- dataset %>% filter_(chrfilter_expr_NA)
      } else {
        chrfilter_expr <- lazyeval::interp(quote(x1 %in% y1), x1 = as.name(chrfilter), y1 = as.character(chrfilterGrp[chrfilterGrp!=""]))
        dataset <- dataset %>% filter_(chrfilter_expr)
      }
    } else NULL
    dataset <- dataset %>% select_(as.name(filterVar)) %>% unlist() %>% unname()
  } else NULL
  
  if (length(unique(dataset)) != 0){
    ymax <- dataset %>% table(useNA = "ifany") %>% unname() %>% max(na.rm = TRUE)
    # names(dataset)[is.na(names(dataset))] <- "NA"
    # browser()
    # dataset <- as.character(dataset)
    dataset <- as.factor(dataset)
    levels(dataset) <- c(levels(dataset), "NA")
    dataset[is.na(dataset)] <- "NA"
    
    # browser()
    par(oma=c(0,0,0,0))  # all sides have 3 lines of space  
    par(mar=c(2,2,0,0) + 0.1, cex.lab = 0.8, cex.axis=0.7)
    par(bg=NA)
    plot(dataset, xlab = NULL, ylab = NULL,
         ann=FALSE)#, xaxt='n', yaxt = 'n')
    axis(side = 1, at = c(0, ymax))
  } else NULL
}