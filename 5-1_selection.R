selectionControlUI_part1 <- function(id){
  ns <- NS(id)
  fluidRow(
    useShinyjs(),
    tags$h4(paste0("Selection Control")),
    fileInput(ns("selectionExclusion"), 
              "Upload CSV for selection exclusion", #"List of BMGs to be excluded (NO HEADER, SINGLE COLUMN)",
              multiple = TRUE, buttonLabel = "Browse CSV", width = "100%",
              accept = c('text/csv',
                         'text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain',
                         '.csv',
                         '.tsv')
    ),
    p("Must Upload Single Column CSV with no Header")
  )
}

selectionControlUI_part2 <- function(id){
  ns <- NS(id)
  fluidRow(
    #checkboxInput(inputId = "noExclusion", label = "Ignore Selection Exclusions", value = FALSE),
    selectInput(ns("exclusionFieldSelect"), "Which Field for Exclusion Lookup?", 
                choices = c(paste("Please.upload "," file")), 
                selected = paste("Please upload "," file")),
    # helpText("MAKE SURE the uploaded list includes",
    #          "Previously Tested, ", "Manually Chosen, ", "or Predetermined BMG IDs."),
    #actionButton("resetBMGExclusion", "Delete BMG Exclusion Uploaded"),
    #checkboxInput(inputId = ns("noExclusion"), label = "Ignore Selection Exclusions", value = FALSE),
    
    #checkboxInput(inputId = "HSVTCSelection", label = "HSVTC Selection", value = FALSE),
    
    selectInput(ns("licenseeSelect"), "Select Licensee/OEM for Selection", 
                choices = c(paste("Please upload "," file")), 
                selected = paste("Please upload "," file")),
    
    actionButton(ns("parseVariable"), "1. Initialize Variables"),
    
    
    # Execute Button to update table
    # actionButton("selectionReady", "Commit Selection Options"),
    HTML("<br/>", "<br/>"),
    p(strong(span("Apply once filters are set."))),
    #h5("Once filters are set:"),
    actionButton(ns("applyFilter"), "2. Apply Filter"),
    #tags$div(class="control-label", checked = NA, tags$
    #h6("USAC Selection Results"),
    HTML("<br/>"),
    #helpText("The same seed can be used to reproduce a selection."),
    checkboxInput(inputId = ns("onePerBMG"), label = "Select One per BMG", value = TRUE),
    HTML("<br/>"),
    actionButton(ns("runSelection"), "3. Perform Selection!"),
    #numericInput(inputId = ns("noBMG"), label = "How many BMGs to make selection?", value = 17, min = 0, max = NA),
    #checkboxInput(inputId = ns("onePerBMG"), label = "Pick One Model per BMG", value = TRUE),
    HTML("<br/>", "<br/>"),
    #actionButton(ns("runRankedSelection"), "4. Make All Ranked."),
    numericInput(inputId = ns("selectionSeed"), label = "Random Number Seed", value = 42, min = 0, max = NA),
    downloadButton(ns("download_selection"), "5. Download Selections.")
  )
}

selectionControlFilter <- function(id){
  ns <- NS(id)
  mainPanel(
    fluidRow(
      fluidRow(
        column(width = 3,
               wellPanel( selectInput(ns("chrfilter1"), "Pick Non-numerical Filter 1:", 
                                      c("Please upload file or do parse"), "Please upload file or do parse"),
                          
                          checkboxGroupInput(ns("chrfilterGrp1"), label = "Pick Non-Numerical Value(s)", 
                                             choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                             selected = 1),
                          plotOutput(ns("chrfilter1Plot"), height = "80px"))
        ),
        column(width = 3,
               wellPanel(selectInput(ns("chrfilter2"), "Pick Non-numerical Filter 2:", 
                                     c("Please upload file or do parse"), "Please upload file or do parse"),
                         
                         checkboxGroupInput(ns("chrfilterGrp2"), label = "Pick Non-Numerical Value(s)", 
                                            choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                            selected = 1),
                         plotOutput(ns("chrfilter2Plot"), height = "80px"))
        ),
        column(width = 3,
               wellPanel(selectInput(ns("chrfilter3"), "Pick Non-numerical Filter 3:", 
                                     c("Please upload file or do parse"), "Please upload file or do parse"),
                         
                         checkboxGroupInput(ns("chrfilterGrp3"), label = "Pick Non-Numerical Value(s)", 
                                            choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                            selected = 1),
                         plotOutput(ns("chrfilter3Plot"), height = "80px"))
        ),
        column(width = 3,
               wellPanel(selectInput(ns("chrfilter4"), "Pick Non-numerical Filter 4:", 
                                     c("Please upload file or do parse"), "Please upload file or do parse"),
                         
                         checkboxGroupInput(ns("chrfilterGrp4"), label = "Pick Non-Numerical Value(s)", 
                                            choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                            selected = 1),
                         plotOutput(ns("chrfilter4Plot"), height = "80px"))
        )
      ),
      fluidRow(
        column(width = 3,
               #h3("Pick Variable and ranges after Initialize Variables"),
               wellPanel(selectInput(ns("numfilter1"), "Pick Numerical Filter 1:", 
                                     c("Please upload file or do parse"), "Please upload file or do parse"),
                         sliderInput(inputId = ns("numfilterVal1"), label = "Range of Numerical Filter 1", 
                                     max = 100, min = 0.0, step = 0.5, value = c(min, max)),
                         plotOutput(ns("numfilter1Plot"), height = "60px")),
               #h3("Set Preferences After Applying Filter(s)"),
               wellPanel(radioButtons(ns("numfilter1_scheme"), label = "Selection Preference: ",
                                      choices = list("Higher" = 1, "Lower" = 2, "Median" = 3, "Any within Filter" = 4), 
                                      selected = 4),
                         checkboxInput(inputId = ns("numfilter1_prefed"), label = "Make Strong Preference", value = FALSE))
        ),
        column(width = 3,        
               wellPanel(selectInput(ns("numfilter2"), "Pick Numerical Filter 2:", 
                                     c("Please upload file or do parse"), "Please upload file or do parse"),
                         sliderInput(inputId = ns("numfilterVal2"), label = "Range of Numerical Filter 2", 
                                     max = 100, min = 0.0, step = 0.5, value = c(min, max)),
                         plotOutput(ns("numfilter2Plot"), height = "60px")),
               wellPanel(radioButtons(ns("numfilter2_scheme"), label = "Selection Preference: ",
                                      choices = list("Higher" = 1, "Lower" = 2, "Median" = 3, "Any within Filter" = 4), 
                                      selected = 4),
                         checkboxInput(inputId = ns("numfilter2_prefed"), label = "Make Strong Preference", value = FALSE))
        ),
        column(width = 3,
               wellPanel(selectInput(ns("numfilter3"), "Pick Numerical Filter 3:", 
                                     c("Please upload file or do parse"), "Please upload file or do parse"),
                         sliderInput(inputId = ns("numfilterVal3"), label = "Range of Numerical Filter 3", 
                                     max = 100, min = 0.0, step = 0.5, value = c(min, max)),
                         plotOutput(ns("numfilter3Plot"), height = "60px")),
               wellPanel(radioButtons(ns("numfilter3_scheme"), label = "Selection Preference: ",
                                      choices = list("Higher" = 1, "Lower" = 2, "Median" = 3, "Any within Filter" = 4), 
                                      selected = 4),
                         checkboxInput(inputId = ns("numfilter3_prefed"), label = "Make Strong Preference", value = FALSE))
        ),
        column(width = 3,
               wellPanel(selectInput(ns("numfilter4"), "Pick Numerical Filter 4:", 
                                     c("Please upload file or do parse"), "Please upload file or do parse"),
                         sliderInput(inputId = ns("numfilterVal4"), label = "Range of Numerical Filter 4", 
                                     max = 100, min = 0.0, step = 0.5, value = c(min, max)),
                         plotOutput(ns("numfilter4Plot"), height = "60px")),
               wellPanel(radioButtons(ns("numfilter4_scheme"), label = "Selection Preference: ",
                                      choices = list("Higher" = 1, "Lower" = 2, "Median" = 3, "Any within Filter" = 4), 
                                      selected = 4),
                         checkboxInput(inputId = ns("numfilter4_prefed"), label = "Make Strong Preference", value = FALSE))
        )
      )
    ),
    
    fluidRow(dataTableOutput(ns("selectionView")))
    
  )
}

selection <- function(input, output, session, rv){
  observeEvent(rv$ACSData, {
    ACSLicensee_vec <- rv$ACSData_NA %>% dplyr::select(one_of(input$box_xvar1_ACS)) %>% 
      unique() %>% unlist() %>% unname() %>% as.character( )
    updateSelectInput(session, "licenseeSelect",
                      choices = c("All licensees", ACSLicensee_vec))
  })
  
  observeEvent(input$parseVariable, {
    dataset <- rv$ACSData
    header <- names(dataset)
    req(rv$ACSData)
    if(input$licenseeSelect == "All licensees"){
      rv$ACSData_parsed <- rv$ACSData
    } else {
      expr_licensee <- lazyeval::interp(quote(x == y), x = as.name(input$box_xvar1_ACS), y = input$licenseeSelect)
      rv$ACSData_parsed <- rv$ACSData %>% filter_(expr_licensee) 
    }
    
    numtest <- sapply(dataset, is.numeric) %>% unname()
    logtest <- sapply(dataset, is.logical) %>% unname()
    factest <- sapply(dataset, is.factor) %>% unname()
    chrtest <- sapply(dataset, is.character) %>% unname()
    catetest <- logtest | factest | chrtest
    
    header_num <- header[numtest]
    header_log <- header[logtest]
    header_fac <- header[factest]
    header_chr <- header[chrtest]
    header_cate <- header[catetest]
    
    #Take variables with less 10 unique entries  
    catetest_sml <- sapply(dataset[catetest], function(x) length(unique(x)) <= 10) %>% unname() 
    
    header_cate_sml <- c(header_cate[catetest_sml], names(dataset)[grepl('_year', names(dataset), ignore.case = T)] ) %>% unique()
    
    # logtest_sml <- sapply(dataset[logtest], function(x) length(unique(x))<20) %>% unname()
    # factest_sml <- sapply(dataset[factest], function(x) length(unique(x))<20) %>% unname()
    # chr_test_sml <- sapply(dataset[chrtest], function(x) length(unique(x))<20) %>% unname()
    # browser()
    updateSelectInput(session, "numfilter1", choices = c("None", header_num), selected = "None")
    updateSelectInput(session, "numfilter2", choices = c("None", header_num), selected = "None")
    updateSelectInput(session, "numfilter3", choices = c("None", header_num), selected = "None")
    updateSelectInput(session, "numfilter4", choices = c("None", header_num), selected = "None")
    updateSelectInput(session, "chrfilter1", choices = c("None", header_cate_sml), selected = "None")
    updateSelectInput(session, "chrfilter2", choices = c("None", header_cate_sml), selected = "None")
    updateSelectInput(session, "chrfilter3", choices = c("None", header_cate_sml), selected = "None")
    updateSelectInput(session, "chrfilter4", choices = c("None", header_cate_sml), selected = "None")
    #updateCheckboxGroupInput(session, "inCheckboxGroup2", label = paste("Checkboxgroup label", length(x)), choices = x,  selected = x)
  })
  
  observeEvent(input$parseVariable,{
    req(rv$ACSData)
    # expr_licensee <- lazyeval::interp(quote(x == y), x = as.name(input$box_xvar1_ACS), y = input$licenseeSelect)
    # dataset <- rv$ACSData %>% filter_(expr_licensee) 
    dataset <- rv$ACSData_parsed
    
    observeEvent(input$numfilter1, {
      req(input$parseVariable)
      #browser()
      if(input$numfilter1 != "None"){
        max = signif(max(dataset[[input$numfilter1]], na.rm = TRUE), digits = 2) 
        updateSliderInput(session, "numfilterVal1", max = max, min = 0.0, step = max/2000, value = c(min, max))
      } else updateSliderInput(session, "numfilterVal1", max = 100, min = 0.0, step = 0.5, value = c(min, max))
    })
    
    observeEvent(input$numfilter2, {
      req(input$parseVariable)
      
      if(input$numfilter2 != "None"){
        max = signif(max(dataset[[input$numfilter2]], na.rm = TRUE), digits = 2) 
        updateSliderInput(session, "numfilterVal2", max = max, min = 0.0, step = max/2000, value = c(min, max))
      } else updateSliderInput(session, "numfilterVal2", max = 100, min = 0.0, step = 0.5, value = c(min, max))
    })
    
    observeEvent(input$numfilter3, {
      req(input$parseVariable)
      
      if(input$numfilter3 != "None"){
        max = signif(max(dataset[[input$numfilter3]], na.rm = TRUE), digits = 2) 
        updateSliderInput(session, "numfilterVal3", max = max, min = 0.0, step = max/2000, value = c(min, max))
      } else updateSliderInput(session, "numfilterVal3", max = 100, min = 0.0, step = 0.5, value = c(min, max))
    })
    
    observeEvent(input$numfilter4, {
      req(input$parseVariable)
      
      if(input$numfilter4 != "None"){
        max = signif(max(dataset[[input$numfilter4]], na.rm = TRUE), digits = 2) 
        updateSliderInput(session, "numfilterVal4", max = max, min = 0.0, step = max/2000, value = c(min, max))
      } else updateSliderInput(session, "numfilterVal4", max = 100, min = 0.0, step = 0.5, value = c(min, max))
    })
    
    
    
    
    # numfilterOut1 <- reactive({
    #   if(input$parseVariable > 1) input$numfilter1
    #   else NULL
    #   browser()
    # })
    
    # numfilter2 <- reactive({
    #   input$numfilter2
    # })
    # 
    # numfilter3 <- reactive({
    #   input$numfilter3
    # })
    # 
    # numfilter4 <- reactive({
    #   input$numfilter4
    # })
    
    # numFilter1_median <- reactive({
    #   if(input$numfilter1_scheme == 3) {
    #     dataset <- rv$ACSData 
    #     dataset <- dataset %>% select_(input$numfilter1)  
    #     browser()
    #     numfilter_expr1 <- lazyeval::interp(quote(x1 >= y1), x1 = as.name(input$numfilter1), y1 = as.double(input$numfilterVal1[1]))
    #     numfilter_expr2 <- lazyeval::interp(quote(x1 <= y1), x1 = as.name(input$numfilter1), y1 = as.double(input$numfilterVal1[2]))
    #     median_dataset <- dataset[1] %>% filter_(numfilter_expr1) %>% filter_(numfilter_expr2) %>% unlist() %>% median(na.rm = TRUE)
    #   } else return(NULL)
    #   #req(input$parseVariable)
    #   #if(input$parseVariable < 2) return(NULL)
    #   #if(input$numfilter1 == "Please upload file or do parse") return(NULL)
    # })
    
    observeEvent(input$chrfilter1, {
      req(input$parseVariable)
      # browser()
      if(input$chrfilter1 != "None"){
        updatedlist <- dataset[[input$chrfilter1]] %>% unique() %>% as.character() %>% sort(na.last=TRUE)
        updateCheckboxGroupInput(session, "chrfilterGrp1", choices = c(updatedlist), selected = NULL)
      } else NULL
    })
    
    observeEvent(input$chrfilter2, {
      req(input$parseVariable)
      
      if(input$chrfilter2 != "None"){
        updatedlist <- dataset[[input$chrfilter2]] %>% unique() %>% as.character() %>% sort(na.last=TRUE)
        updateCheckboxGroupInput(session, "chrfilterGrp2", choices = c(updatedlist), selected = NULL)
      } else NULL
    })
    
    observeEvent(input$chrfilter3, {
      req(input$parseVariable)
      
      if(input$chrfilter3 != "None"){
        updatedlist <- dataset[[input$chrfilter3]] %>% unique() %>% as.character() %>% sort(na.last=TRUE)
        updateCheckboxGroupInput(session, "chrfilterGrp3", choices = c(updatedlist), selected = NULL)
      } else NULL
    })
    
    observeEvent(input$chrfilter4, {
      req(input$parseVariable)
      
      if(input$chrfilter4 != "None"){
        updatedlist <- dataset[[input$chrfilter4]] %>% unique() %>% as.character() %>% sort(na.last=TRUE)
        updateCheckboxGroupInput(session, "chrfilterGrp4", choices = c(updatedlist), selected = NULL)
      } else NULL
    })
  })
  
  observeEvent(input$applyFilter, {
    req(input$parseVariable)
    if(input$parseVariable < 1) return(NULL)
    dataset <- rv$ACSData 
    # browser()
    # expr_licensee <- lazyeval::interp(quote(x == y), x = as.name(input$box_xvar1_ACS), y = input$licenseeSelect)
    # dataset <- dataset %>% filter_(expr_licensee) 
    if(input$licenseeSelect != "All licensees"){
      expr_licensee <- lazyeval::interp(quote(x == y), x = as.name(input$box_xvar1_ACS), y = input$licenseeSelect)
      dataset <- dataset %>% filter_(expr_licensee)
    } else NULL
    
    numfilter <- c(input$numfilter1, input$numfilter2, input$numfilter3, input$numfilter4)
    numfilterVal <- list(input$numfilterVal1, input$numfilterVal2, input$numfilterVal3, input$numfilterVal4)
    for(i in seq_along(numfilter)){
      numfilterVal_vec <- numfilterVal[[i]]
      if(numfilter[i] != "None") {
        numfilter_expr1 <- lazyeval::interp(quote(x1 >= y1), x1 = as.name(numfilter[i]), y1 = as.double(numfilterVal_vec[1]))
        numfilter_expr2 <- lazyeval::interp(quote(x1 <= y1), x1 = as.name(numfilter[i]), y1 = as.double(numfilterVal_vec[2]))
        dataset <- dataset %>% filter_(numfilter_expr1) %>% filter_(numfilter_expr2)
      } else NULL
      
    }
    chrfilter <- c(input$chrfilter1, input$chrfilter2, input$chrfilter3, input$chrfilter4)
    chrfilterGrp <- list(input$chrfilterGrp1, input$chrfilterGrp2, input$chrfilterGrp3, input$chrfilterGrp4)
    # browser()
    for(i in seq_along(chrfilter)){
      if(chrfilter[i] != "None") {
        # if(chrfilter[i] == "Blank/NA"){
          
        # } else {
        noNAEntry = chrfilterGrp[[i]][chrfilterGrp[[i]]!=""]
        if(length(noNAEntry) > 0) {
          chrfilter_expr <- lazyeval::interp(quote(x1 %in% y1), x1 = as.name(chrfilter[i]), y1 = as.character(chrfilterGrp[[i]][chrfilterGrp[[i]]!=""]))
          if("" %in% chrfilterGrp[[i]]){
            chrfilter_expr <- lazyeval::interp(quote(x1 %in% y1), x1 = as.name(chrfilter[i]), y1 = c(as.character(chrfilterGrp[[i]][chrfilterGrp[[i]]!=""]),NA))
            # chrfilter_expr_NA <- lazyeval::interp(quote(x1 %>% is.na()), x1 = as.name(chrfilter[i]))
            dataset <- dataset %>% filter_(chrfilter_expr)
          } else {
            dataset <- dataset %>% filter_(chrfilter_expr)
          }
        } else if("" %in% chrfilterGrp[[i]]){
          chrfilter_expr_NA <- lazyeval::interp(quote(x1 %>% is.na()), x1 = as.name(chrfilter[i]))
          dataset <- dataset %>% filter_(chrfilter_expr_NA)
        } else {
          chrfilter_expr <- lazyeval::interp(quote(x1 %in% y1), x1 = as.name(chrfilter[i]), y1 = as.character(chrfilterGrp[[i]][chrfilterGrp[[i]]!=""]))
          dataset <- dataset %>% filter_(chrfilter_expr)
        }
      } else NULL
      
    }
    rv$filtered <- dataset
    #browser()
  })
  return(rv)
}

applyScheme <- function(dataset = dataset, param = numfilter1, scheme = input$numfilter1_scheme, BMGId = input$BMGId, onePerBMG = input$onePerBMG){
  if(onePerBMG == TRUE) {
    dataset <- dataset %>% dplyr::group_by_(as.name(BMGId))
    if(onePerBMG == 1) {
      dataset %>% dplyr::summarize(max = max(as.name(param)))
    } else NULL
    
  } else NULL
}

#callModule(sliderUpdate, "Dir", numfilter = input$numfilter1, dataset = dataset, numfilterVal = input$numfilterVal1)
sliderUpdate <- function(input, output, session, numfilter, dataset, numfilterVal){
  dataset_vec <- dataset[[numfilter]]
  if(is.numeric(dataset_vec)) {
    max <- signif(max(dataset_vec, na.rm = TRUE), digits = 3) + 0.001
    reactive(updateSliderInput(session, numfilterVal, max = max, min = 0.0, step = max/2000, value = c(0,max/3)))
  }
  else updateSliderInput(session, numfilterVal, max = 100, min = 0.0, step = 0.5)
}
