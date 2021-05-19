#Module 1: Upload data and parse them
#Note: Does not support native MS SQLServer yet as of 180417
#argument: type: ACS should trigger ACS only feature

ACSUploadParseUI <- function(id, multiple = FALSE, type = "ACS"){
  ns <- NS(id)
  tagList(
    fileInput(ns("ACSFile"), paste("Please Upload ", type, " Data file"),  multiple = multiple),
    #fileInput(ns("DirDump"), "Please Upload Directory file", multiple = multiple),
    shiny::checkboxInput(ns("impute"), "Fill blank or missing values with 'Blank'", TRUE),
    selectInput(ns("box_xvar1_ACS"), paste("Pick data field representing licensee in ", type, " File:"), 
                ifelse(type == "ACS", c("Company"), c("Company")),
                ifelse(type == "ACS", "Company", "Company")),
    
    # conditionalPanel(
    #   condition = "ACS == true", 
    #   selectInput(ns("testResult_ACS"), paste("Pick data field representing Total TestResult in ", type, " File:"),
    #                              c("TestResult"), "TestResult")
    # ),
    if (type == "ACS"){
      selectInput(ns("BMGId_ACS"), paste("Pick data field representing BMGID in", type, " File:"),
                  c("bmgid"), "bmgid")
    } else NULL, 
    if (type == "ACS"){
    selectInput(ns("testResult_ACS"), paste("Pick data field representing Total TestResult in ", type, " File:"),
                c("testresult"), "testresult")
    } else NULL,
    if (type == "ACS"){
    selectInput(ns("year_ACS"), paste("Pick data field representing Test Year in ", type, " File:"),
                  c("programyear"), "programyear")
    } else NULL, #HTML("<br/>", "<br/>", "<br/>", "<br/>", "<br/>"),
   
    if (type == "Directory"){
    selectInput(ns("BMGId"), paste("Pick data field representing BMGID in", type, " File:"),
                c("Basic.Model.Group.Id"), "Basic.Model.Group.Id")
    } else NULL, 
    if (type == "Directory"){
      selectInput(ns("AHRIRefNo"), paste("Pick data field representing AHRI Reference No. in", type, " File:"),
                  c("AHRI.Certified.Reference.Number"), "AHRI Certified Reference Number")
    } else NULL,
    if (type == "Directory"){
      # selectInput(ns("bmgScreen_ACS"), paste("How many recent years of BMGs to screen out?"),
      #             c(0:6), 0)
      sliderInput(ns("bmgScreen_ACS"), "How many recent years of BMGs to screen out? Note: Future Years==No screening",
                  min = 2014, max = as.integer(format(Sys.Date(),"%Y"))+1,
                  value = c(as.integer(format(Sys.Date(),"%Y")) + 1 ,
                            as.integer(format(Sys.Date(),"%Y"))+ 1),
                  sep="")
      
    } else NULL, #HTML("<br/>", "<br/>", "<br/>", "<br/>", "<br/>"),
    if (type == "Directory"){
      # selectInput(ns("bmgScreen_ACS"), paste("How many recent years of BMGs to screen out?"),
      #             c(0:6), 0)
      # shiny::("Make sure to UPLOAD & PARSE ACS Table for BMG SCREENING!!!")
      span(textOutput(ns("ACSParseFirst")), style="color:red")
    } else NULL, #HTML("<br/>", "<br/>", "<br/>", "<br/>", "<br/>"),
   
    fluidRow(
      column(width = 4,
             actionButton(ns("action_parse"), "Parse the data!")
      ),
      column(width = 4,
             actionButton(ns("reset"), "Reset")
      ),
      column(width = 4)
    ),
    
    HTML("<br/>"),
    
    h4(paste("Uploaded ", type, " file looks like this:")),
    verbatimTextOutput(ns("tbl_ACS")),
    if (type == "Directory"){
      # browser()
      verbatimTextOutput(ns("bmgScreeningResult"))
# <<<<<<< HEAD
    } else NULL
# =======
    # } else NULL,
# >>>>>>> 26969a92391b7e60b897f12f8174bef76e0f3832
    
    # h4("Second file looks like this:"),
    # verbatimTextOutput(ns("tbl_Dir"))
  )
}


ACSUploadParse <- function(input, output, session, rv, type){
  output$ACSParseFirst <- renderText({"Make sure to UPLOAD & PARSE ACS Table FIRST for BMG SCREENING!!!"})
  rv <- reactiveValues(
    dfloaded = NULL,
    reset = FALSE)
  
  anyPastYears = reactive({
    c(input$bmgScreen_ACS[1]:input$bmgScreen_ACS[2]) %in% c(2013:as.integer(format(Sys.Date(), "%Y"))) %>% sum()
  })
  
  observeEvent(input$action_parse, {
    #req(input$DirDump)
    req(input$ACSFile)
    if(type == "ACS"){
      rv$ACSData <- drop_na_(rv$ACSData_NA, input$testResult_ACS)
      # global var to be accessed from directory data side
      ACSParsed <<- rv$ACSData
      participantFieldName <<- input$box_xvar1_ACS
      bmgFieldName <<- input$BMGId_ACS
      prgmYearFieldName <<- input$year_ACS
      
      
    } else if(type=="Directory") {
    # check if the BMG Screen is valid
      
      rv$ACSData <- rv$ACSData_NA
      rv$originalSize <- dim(rv$ACSData_NA)[1]
      rv$originalBMGSize <-
        rv$ACSData[, c(input$box_xvar1_ACS, input$BMGId)] %>% unique() %>% nrow()
      # browser()
      if (anyPastYears() > 0 &&
          exists("ACSParsed") &&
          exists("participantFieldName") && exists("bmgFieldName")) {
        # browser()
        # convert mfg name to lower case
        ACSParsed[[participantFieldName]] <-
          ACSParsed[[participantFieldName]] %>% tolower()
        
        index = ACSParsed[[prgmYearFieldName]] %in% c(input$bmgScreen_ACS[1]:input$bmgScreen_ACS[2])
        smallPartDir = rv$ACSData[c(input$BMGId, input$box_xvar1_ACS, input$AHRIRefNo)]
        smallPartDir[[input$box_xvar1_ACS]] <-
          smallPartDir[[input$box_xvar1_ACS]] %>% tolower()
        
        refNoToKeep = smallPartDir %>% dplyr::anti_join(ACSParsed[index,] %>%
                                                          mutate()
                                                        
                                                        ,
                                                        by =
                                                          setNames(
                                                            c(bmgFieldName, participantFieldName),
                                                            c(input$BMGId, input$box_xvar1_ACS)
                                                          )) %>% pull(input$AHRIRefNo)
        rv$ACSData <-
          rv$ACSData_NA[rv$ACSData_NA[[input$AHRIRefNo]] %in% refNoToKeep, ]
        # rv$ACSData <- rv$ACSData_NA
        rv$reducedSize <- dim(rv$ACSData)[1]
        rv$reducedBMGSize <-
          rv$ACSData[, c(input$box_xvar1_ACS, input$BMGId)] %>% unique() %>% nrow()
        
      }
      
      output$bmgScreeningResult <- renderText({
        # testIfScreeningDone <- rv$reducedBMGSize
        # browser()
        bmgReductionMSG <- tryCatch({
          if(!is.null(rv$reducedBMGSize)){
          return(paste0("BMG Screening Done. Reduced BMGs: ", rv$originalBMGSize , " BMG(s)", " → ", rv$reducedBMGSize, " BMG(s)"))
            
          }
          else {
            return("BMG Screen not occurred")
          }
          # }
        }, warning = function(war) {
        }, error = function(err) {
          return("BMG Screen not occurred")
        }, finally = {
        } 
        # if(exists("testIfScreeningDone") && testIfScreeningDone >0){
        )
      })
    }
  })
  
  
  
  
  observeEvent(input$ACSFile, {
    req(input$ACSFile)
# <<<<<<< HEAD
# <<<<<<< HEAD
    # browser():
    # rawDB_df <- read_csv(input$ACSFile$datapath, na = c("", "NA", "na"))
# =======
    # browser()
    # rawDB_df <- read_csv(input$ACSFile$datapath, na = c("", "NA", "na", "N/A", "null", "NULL"), guess_max = 3000) 
# >>>>>>> 26969a92391b7e60b897f12f8174bef76e0f3832
# =======
    # browser()
    rawDB_df <- read_csv(input$ACSFile$datapath, na = c("", "NA", "na", "N/A", "null", "NULL"), guess_max = 3000) 
    logtest <- sapply(rawDB_df, is.logical) %>% unname() 
    factest <- sapply(rawDB_df, is.factor) %>% unname()
    chrtest <- sapply(rawDB_df, is.character) %>% unname()
    catetest <- logtest | factest | chrtest
    # browser()
    if(input$impute){
      # rawDB_df <-
      chrNames <- names(rawDB_df[chrtest])[!(names(rawDB_df[chrtest]) %>% stringr::str_detect(regex("date", ignore_case = TRUE)))]
      for (i in seq_along(chrNames)) {
        missingMask <- rawDB_df[[chrNames[i]]] %>% is.na() 
        # print(rawDB_df[missingMask, chrNames[i]])
        # print(missingMask %>% head()) 
        rawDB_df[missingMask, chrNames[i]] <- "Blank"
        # print(rawDB_df[chrNames[i]])
      }
    }
# >>>>>>> 26969a92391b7e60b897f12f8174bef76e0f3832
    names(rawDB_df) <- make.names(names(rawDB_df), unique = TRUE)
    # browser()
    if(type=="ACS"){
      # browser()
      tryCatch({
        rm(ACSParsed)
        rm(ACSParsed,  pos = ".GlobalEnv")
      }, warning = function(war) {

      }, error = function(err) {

      }, finally = {

   
      }) # END tryCatch
      # ACS_global <<- rawDB_df

    }
    
    # if(type=="Directory"){
    #   # print("hello!")
    #   
    # }
    # create 'Year' fields
    dateCols = names(rawDB_df)[grepl('date', names(rawDB_df), ignore.case = T)]
    # for (colname in dateCols){
    #   print(colname)
    #   rawDB_df[paste0(colname, '_year')] = rawDB_df[[colname]] %>% lubridate::as_date() %>% year() %>% as.character()
    # }
    
    rawDB_df <- char.to.factors(rawDB_df)
    rv$ACSData_NA <- rawDB_df
    rm(rawDB_df)
    #rv$ACSData_NA <- read.csv(inputestt$ACSFile$datapath, header = TRUE, row.names = NULL, as.is = FALSE, na.strings = c("", "NA", "na"))
    if(names(rv$ACSData_NA)[1] == "row.names"){
      names(rv$ACSData_NA) <- names(rv$ACSData_NA)[-1]
      rv$ACSData_NA <- rv$ACSData_NA[-ncol(rv$ACSData_NA)]
      rv$ACSData_NA[[1]] <- rv$ACSData_NA[[1]] %>% as.numeric()
    } else NULL
    #browser()
    headers_dataset_ACS <- names(rv$ACSData_NA)
    if (is.null(headers_dataset_ACS))
      headers_dataset_ACS <- character(0)
    #cat(file=stderr(), "ACS Name Check ", headers_dataset_ACS, "\n")
    isolate({
      #input$DirDump$datapath
      input$ACSFile$datapath
      input$box_xvar1_ACS
      input$box_yvar1_ACS
      input$scat_xvar1_ACS
      input$scat_yvar1_ACS
      input$scat_xvar2_ACS
      input$scat_yvar2_ACS
      input$licensee1_ACS
      input$SVlicensee1_ACS
      input$SV_xvar1_ACS
      input$year_ACS
      #input$box_xvar1_Dir
      #input$box_yvar1_Dir
    })
    # browser()
  })

  observeEvent(rv$ACSData_NA, {
    req(input$ACSFile)
    headers_dataset_ACS <- names(rv$ACSData_NA)
    ACSLicensee_vec <- rv$ACSData_NA[[input$box_xvar1_ACS]] %>% 
      unique() %>% unlist() %>% unname() %>% as.character( ) %>% sort()
    
    if (is.null(headers_dataset_ACS))
      headers_dataset_ACS <- character(0)
    #browser()
    
    isolate({
      #input$DirDump$datapath
      input$ACSFile$datapath
      input$box_xvar1_ACS
      input$box_yvar1_ACS
      input$scat_xvar1_ACS
      input$scat_yvar1_ACS
      input$scat_xvar2_ACS
      input$scat_yvar2_ACS
      input$licensee1_ACS
      input$SVlicensee1_ACS
      input$SV_xvar1_ACS
      input$year_ACS
      #input$box_xvar1_Dir
      #input$box_yvar1_Dir
    })
    
    #browser()
    dataset <- rv$ACSData_NA
    # browser()
    numtest <- sapply(dataset, is.numeric) %>% unname()
    logtest <- sapply(dataset, is.logical) %>% unname() 
    factest <- sapply(dataset, is.factor) %>% unname()
    chrtest <- sapply(dataset, is.character) %>% unname()
    catetest <- logtest | factest | chrtest
    
    header_num <- headers_dataset_ACS[numtest]
    header_log <- headers_dataset_ACS[logtest]
    header_fac <- headers_dataset_ACS[factest]
    header_chr <- headers_dataset_ACS[chrtest]
    header_cate <- headers_dataset_ACS[catetest]
    
    #Take variables with less 10 unique entries  
    catetest_sml <- sapply(dataset[catetest], function(x) length(unique(x)) <= 10) %>% unname() 
    header_cate_sml <- header_cate[catetest_sml] 
    
    # Can also set the label and select items
    updateSelectInput(session, "box_xvar1_ACS",
                      choices = header_cate, 
                      selected = ifelse(type == "ACS", "manufacturernamerev", "Company"))
    updateSelectInput(session, "testResult_ACS",
                      choices = c(header_cate, header_num), selected = "testresult"
    )
    updateSelectInput(session, "box_yvar1_ACS",
                      choices = header_num
    )
    updateSelectInput(session, "grouped_ACS",
                      choices = c("0.None", header_cate_sml)
    )
    
    updateSelectInput(session, "scat_group1_ACS",
                      choices = c("0.None", header_num, header_cate_sml)
    )
    updateSelectInput(session, "scat_group2_ACS",
                      choices = c("0.None", header_num, header_cate_sml)
    )
    # Update Other Items
    updateSelectInput(session, "scat_xvar1_ACS",
                      choices = headers_dataset_ACS
    )
    updateSelectInput(session, "scat_yvar1_ACS",
                      choices = headers_dataset_ACS
    )
    updateSelectInput(session, "scat_xvar2_ACS",
                      choices = headers_dataset_ACS
    )
    updateSelectInput(session, "scat_yvar2_ACS",
                      choices = headers_dataset_ACS
    )
    updateSelectInput(session, "licensee1_ACS",
                      choices = c("None", ACSLicensee_vec)
    )
    updateSelectInput(session, "SVlicensee1_ACS",
                      choices = c("None", ACSLicensee_vec)
    )
    updateSelectInput(session, "SV_xvar1_ACS",
                      choices = headers_dataset_ACS)
    updateSelectInput(session, "year_ACS",
                      choices = headers_dataset_ACS, selected = "programyear") 
    updateSelectInput(session, "BMGId",
                      choices = headers_dataset_ACS, selected = "Basic.Model.Group.Id")
    #This is for ACS BMG Id (09/17/2020)
    updateSelectInput(session, "BMGId_ACS",
                      choices = headers_dataset_ACS, selected = "bmgid")
    updateSelectInput(session, "AHRIRefNo",
                      choices = headers_dataset_ACS, selected = "AHRI.Certified.Reference.Number") 
    updateSelectInput(session, "exclusionFieldSelect",
                      choices = headers_dataset_ACS, selected = "Basic.Model.Group.Id") 
  })
  
  observeEvent(input$reset, {
    #rv$DirData <- NULL
    rv$ACSData <- NULL
    #reset('DirDump')
    reset('ACSFile')
  })
  
  observeEvent(input$action_parse,{
    output$tbl_ACS <- renderPrint({
      req(rv$ACSData)
      
      #cat(file=stderr(), "Namespace Check: ", type, "\n")
      if(type == "ACS"){
        dataset <- rv$ACSData %>% select_(input$box_xvar1_ACS, input$box_yvar1_ACS, input$testResult_ACS, input$BMGId_ACS,
                                          input$year_ACS)
      } else if(type == "Directory"){
        
        dataset <- rv$ACSData %>% select_(input$box_xvar1_ACS, input$BMGId, input$AHRIRefNo)
      }
      dataset %>% sample_n(20)
    })
    ACSLicensee_vec <- rv$ACSData_NA[[input$box_xvar1_ACS]] %>% 
      unique() %>% unlist() %>% unname() %>% as.character() %>% sort()
    updateSelectInput(session, "SVlicensee1_ACS",
                      choices = c("None", ACSLicensee_vec)
    )
    updateSelectInput(session, "licensee1_ACS",
                      choices = c("None", ACSLicensee_vec)
    )
  })
  
  # observeEvent(input$action_parse,{
  #   output$tbl_Dir <- renderPrint({
  #     req(rv$DirData)
  #     dataset <- rv$DirData 
  #     head(dataset[,1:10])
  #   })
  # })

  # output$inputs <- renderTable({
  #   as.data.frame(reactiveValuesToList(input))
  # })
  #return(input)
  return(rv)
  #browser()
}

UploadParse_pass <- function(input, output, session, rv){
  return(input)
}
# setupUI <- function(id){
#   ns <- NS(id)
#   tagList(tableOutput(ns("inputs")))
# }

# setup <- function(input, output, session){
#   # output$inputs <- renderTable({
#   #   as.data.frame(reactiveValuesToList(input))
#   # })
#   # output$binprint <- renderText({
#   #   req(input$print)
#   #   paste0("Number of bins: ", input$bins)
#   # })
#   return(input)
# }
char.to.factors <- function(df){
  # This function takes a tbl_df and returns same with any character column converted to a factor
  
  require(dplyr)
  #browser()
  chr_test <- df %>% lapply(is.character) %>% unlist() %>% unname()
  df[chr_test] <- df[chr_test] %>% lapply(as.factor) %>% as.tibble()
  #char.cols = names(df)[sapply(df, function(x) {class(x) == "character" })]
  #tmp = mutate_each_(df, funs(as.factor), char.cols)
  return(df)
}
c2 <- function(...) {
  vals <- c(...)
  
  if (is.null(names(vals))) {
    missing_names <- rep(TRUE, length(vals))
  } else {
    missing_names <- names(vals) == ""
  }
  if (any(missing_names)) {
    names <- vapply(substitute(list(...))[-1], deparse, character(1))
    names(vals)[missing_names] <- names[missing_names]
  }
  
  vals
}
