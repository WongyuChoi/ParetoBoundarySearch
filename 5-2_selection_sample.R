consoleUpdateUI <- function(id){
  hr()
  ns <- NS(id)
  # wellPanel(
  #   verbatimTextOutput(ns("filterInfo"))
  # )
  # 
  wellPanel(useShinyjs(),
            htmlOutput(ns("filterInfo")),
            #textOutput(ns("text1")),
            #textOutput(ns("text2")),
            plotOutput(ns("prefPlot"))
  )
}

consoleUpate <- function(input, output, session, rv){
  # output$filterInfo <- renderText({
  #   
  #   paste("Refined Data from ", originalnRow, " to ", dataset_filtered, " models")
  # })
  # output$text1 <- renderText({paste("You have selected", input$var)})
  # output$text2 <- renderText({paste("You have chosen a range that goes from",
  #                                   input$range[1], "to", input$range[2])})
  output$filterInfo <- renderUI({
    
    originalnRow <- nrow(rv$ACSData)
    filterednRow <- nrow(rv$filtered)
    selectednRow <- nrow(rv$paretoFiltered)
    
    # browser()
    if (!is.null(rv$filtered)){
      numBMG_filtered <- rv$filtered %>% select_(as.name(input$BMGId), as.name(input$box_xvar1_ACS)) %>% unique() %>% nrow()  
    } else numBMG_filtered <- NULL
    
    if (!is.null(rv$paretoFiltered)){
      numBMG_selected <- rv$paretoFiltered %>% select_(as.name(input$BMGId), as.name(input$box_xvar1_ACS)) %>% unique() %>% nrow()
    } else numBMG_selected <- NULL
    
    str1 <- paste("Your Refined Data has", filterednRow, "models or model combinations",
                  "among", numBMG_filtered, "Basic Model Groups.")
    str2 <- paste("Your Uploaded Directory File has ", originalnRow, "models or model combinations")
    str3 <- paste("Your selection has", selectednRow, "models or model combinations",
                  "among", numBMG_selected, "Basic Model Groups.")
    HTML(paste(str2, str1, str3, sep = '<br/>'))
  })
}

runParetoSelection <- function(input, output, session, rv, rv_main){
  observe({
    req(input$selectionExclusion)
    req(!rv_main$reset)
    
    rv$dfloaded <- read.csv(input$selectionExclusion$datapath, header = FALSE) %>% unname() %>% as.vector() %>% unlist()
    
    # rv$dfloaded <- read.csv(input$selectionExclusion$datapath, sep = "", quote ="", header = FALSE)
    # rv$dfloaded <- eval(parse(text=rv$dfloaded[1])) 
    # rv$dfloaded <- as.numeric(rv$dfloaded)
    #browser() #checkpoint for exclusion filter
  })
  
  observeEvent(input$selectionExclusion, {
    rv_main$reset <- FALSE
  }, priority = 1000)
  
  observeEvent(rv_main$reset, {
    #browser()
    if(rv_main$reset){
      rv$dfloaded <- NULL
      reset("Dir-selectionExclusion")
    } else NULL
    #rv$reset <- reseted()
  }, priority = 1000)
  
  # observeEvent(input$reset, {
  #   browser()
  #   rv$dfloaded <- NULL
  #   reset("selectionExclusion")
  #   
  # })
  
  observeEvent(input$runSelection, {
    req(input$parseVariable)
    if (input$runSelection == 0) return()
    # browser()
    #browser() #checkpoint to find exclusion filter
    if(!rv_main$reset) { #if(!is.null(input$selectionExclusion$datapath)) {
      #browser()
      tryCatch({
        expr1 <- lazyeval::interp(quote(!x %in% y), x = as.name(input$exclusionFieldSelect), y = rv$dfloaded)
        dataset <- rv$filtered %>% filter_(expr1) %>% group_by_(input$BMGId, input$box_xvar1_ACS)
      }, warning = function(w){
        dataset <- rv$filtered %>% group_by_(input$BMGId, input$box_xvar1_ACS)
      }, error = function(e){
        dataset <- rv$filtered %>% group_by_(input$BMGId, input$box_xvar1_ACS)
      }, finally = {
        dataset <- rv$filtered %>% group_by_(input$BMGId, input$box_xvar1_ACS)
      }
      )
    } else dataset <- rv$filtered %>% group_by_(input$BMGId, input$box_xvar1_ACS)
    
    
    inputNumfilter_vec <- c(input$numfilter1, input$numfilter2, 
                            input$numfilter3, input$numfilter4)
    inputNumfilterSchme_vec <- c(input$numfilter1_scheme, input$numfilter2_scheme, 
                                 input$numfilter3_scheme, input$numfilter4_scheme)
    numfilter_prefed_vec <- c(input$numfilter1_prefed, input$numfilter2_prefed, 
                              input$numfilter3_prefed, input$numfilter4_prefed)
    
    pref_expr <- generate_combined_rPref_expression(dataset = dataset, inputNumfilter_vec = inputNumfilter_vec, 
                                                    inputNumfilterSchme_vec = inputNumfilterSchme_vec, numfilter_prefed_vec = numfilter_prefed_vec)
    # browser()
    if(unique(inputNumfilterSchme_vec) == 4){
      if(input$onePerBMG){
      dataset <- impose_seed(dataset %>% sample_n(1), seed = input$selectionSeed)
        
      } 
        
      
    } else if(input$onePerBMG) { 
      dataset <- psel(dataset, pref_expr, top = 1)
      dataset <- dataset %>% dplyr::ungroup()
      numrow_b4_rank <- nrow(dataset)
      if(numrow_b4_rank > 0 ){
        dataset <- psel(dataset, pref_expr, top = numrow_b4_rank) # Running again for ranking Grouped Pareto
      }
    } else dataset <- psel(dataset, pref_expr)

    rv$paretoFiltered <- dataset

    isolate({
      input$BMGId
    })
    
  })
  
  # observeEvent(input$runRankedSelection, {
  #   req(input$parseVariable)
  #   req(input$runSelection)
  #   if (input$runSelection == 0 || input$parseVariable == 0) return()
  #   
  #   dataset <- rv$paretoFiltered
  #   
  #   
  # })

  output$prefPlot <- renderPlot({
    req(rv$paretoFiltered)
    dataset <- rv$filtered %>% group_by_(input$BMGId)
    
    inputNumfilter_vec <- c(input$numfilter1, input$numfilter2, 
                            input$numfilter3, input$numfilter4)
    inputNumfilterSchme_vec <- c(input$numfilter1_scheme, input$numfilter2_scheme, 
                                 input$numfilter3_scheme, input$numfilter4_scheme)
    numfilter_prefed_vec <- c(input$numfilter1_prefed, input$numfilter2_prefed, 
                              input$numfilter3_prefed, input$numfilter4_prefed)
    
    
    pref_expr <- generate_combined_rPref_expression(dataset = dataset, inputNumfilter_vec = inputNumfilter_vec, 
                                                    inputNumfilterSchme_vec = inputNumfilterSchme_vec,
                                                    numfilter_prefed_vec = numfilter_prefed_vec)
    num_empty_expr <- stringr::str_count(pref_expr %>% as.character(), pattern = "empty") >= 2
    dataset <- psel(dataset, pref_expr, top = 1) %>% dplyr::ungroup()

    if (nrow(dataset) > 70){ #|| num_empty_expr){
      plot(-1:1, -1:1, type = "n", xlab = "", ylab = "")
      text(0,0, "Plot Not Available, ", cex = 1.1)
      text(0, -0.2, "The selection is too big or non-preference based.", cex = 1.1)
      text(0, -0.4, "Please make filters tighter or use generated table.", cex = 1.1)
    } else {
      inputNumfilter_vec <- c(input$numfilter1, input$numfilter2, 
                              input$numfilter3, input$numfilter4)
      inputNumfilterSchme_vec <- c(input$numfilter1_scheme, input$numfilter2_scheme, 
                                   input$numfilter3_scheme, input$numfilter4_scheme)
      numfilter_prefed_vec <- c(input$numfilter1_prefed, input$numfilter2_prefed, 
                                input$numfilter3_prefed, input$numfilter4_prefed)
      
      pref_expr <- generate_combined_rPref_expression(dataset = dataset, inputNumfilter_vec = inputNumfilter_vec, 
                                                      inputNumfilterSchme_vec = inputNumfilterSchme_vec,
                                                      numfilter_prefed_vec = numfilter_prefed_vec
                                                      )
      
      labels <- dataset %>% dplyr::ungroup() %>% select_(as.name(input$AHRIRefNo)) %>% unname() %>% unlist()

      if(nrow(dataset) > 0){
        plot_btg(dataset, pref_expr, labels = labels, use_dot = FALSE)  
      } else NULL
    }
  })
  return(rv)
}

downloadPareto <- function(input, output, session, rv, filename = paste0("selection", Sys.Date(),".csv")){
  output$download_selection <- downloadHandler(
    filename = function() {
      filename
    },
    content = function(file) {
      #browser()
      dataset <- rv$paretoFiltered
      write.csv(dataset, file, na = "")
    }
  )
}

generate_rPref_expression <- function(pref = 1, input = input$numfilter1, medianfilter = output$medianFilter1){
  if(input == "None") {
    return(rPref::empty())
  } else {
    named_input <- as.name(input)
    if(pref == 1){
      prefExpr <- named_input %>% rPref::high_()
    } else if(pref == 2){
      prefExpr <- named_input %>% rPref::low_()
    } else if(pref == 3){
      expr_pref <- interp( quote(around(x1, x2)), x1 = named_input, x2 =  medianfilter)
      prefExpr <- expr_pref %>% eval()
    } else if(pref == 4){
      prefExpr <- empty()
    }
    return (prefExpr)
  }
}

generate_combined_rPref_expression <- function(dataset = dataset , 
                                               inputNumfilter_vec = inputNumfilter_vec, 
                                               inputNumfilterSchme_vec = inputNumfilterSchme_vec, 
                                               numfilter_prefed_vec = numfilter_prefed_vec) {
  numFilterMedian1 <- dataset[[inputNumfilter_vec[1]]] %>% median(na.rm = TRUE)
  numFilterMedian2 <- dataset[[inputNumfilter_vec[2]]] %>% median(na.rm = TRUE)
  numFilterMedian3 <- dataset[[inputNumfilter_vec[3]]] %>% median(na.rm = TRUE)
  numFilterMedian4 <- dataset[[inputNumfilter_vec[4]]] %>% median(na.rm = TRUE)
  
  #dataset[1] %>% filter_(numfilter_expr1) %>% filter_(numfilter_expr2) %>% unlist() %>% median(na.rm = TRUE)
  
  
  pref1 <- generate_rPref_expression(pref = inputNumfilterSchme_vec[1],
                                     input = inputNumfilter_vec[1],
                                     medianfilter = numFilterMedian1)
  pref2 <- generate_rPref_expression(pref = inputNumfilterSchme_vec[2],
                                     input = inputNumfilter_vec[2],
                                     medianfilter = numFilterMedian2)
  pref3 <- generate_rPref_expression(pref = inputNumfilterSchme_vec[3],
                                     input = inputNumfilter_vec[3],
                                     medianfilter = numFilterMedian3)
  pref4 <- generate_rPref_expression(pref = inputNumfilterSchme_vec[4],
                                     input = inputNumfilter_vec[4],
                                     medianfilter = numFilterMedian4)
  pref_list <- c(pref1, pref2, pref3, pref4)

  if(sum(numfilter_prefed_vec) == 0){ 
    pref_expr <- pref1 * pref2 * pref3 * pref4
  } else if(sum(numfilter_prefed_vec) == 1){
    pref_expr1 <- pref_list[numfilter_prefed_vec][[1]]
    pref_expr2 <- pref_list[!numfilter_prefed_vec][[1]] * pref_list[!numfilter_prefed_vec][[2]] * pref_list[!numfilter_prefed_vec][[3]]
    pref_expr <- pref_expr1 & (pref_expr2)
  } else if(sum(numfilter_prefed_vec) == 2){
    pref_expr1 <- pref_list[numfilter_prefed_vec][[1]] * pref_list[numfilter_prefed_vec][[2]]
    pref_expr2 <- pref_list[!numfilter_prefed_vec][[1]] * pref_list[!numfilter_prefed_vec][[2]]
    pref_expr <- pref_expr1 & (pref_expr2)
  } else if(sum(numfilter_prefed_vec) == 3){
    pref_expr1 <- pref_list[numfilter_prefed_vec][[1]] * pref_list[numfilter_prefed_vec][[2]] * pref_list[numfilter_prefed_vec][[3]]
    pref_expr1 <- pref_list[!numfilter_prefed_vec][[1]]
    pref_expr <- (pref_expr1) & (pref_expr2)
  } else if(sum(numfilter_prefed_vec) == 4){
    pref_expr <- pref1 * pref2 * pref3 * pref4
  } else pref_expr <- NULL

  #pref_expr <- pref1 * pref2 * pref3 * pref4 # this needs to be improved for prioritized Pareto. 
  return(pref_expr)
}


#impose seed 
impose_seed <- function(...., seed = myseed) {
  ## set the seed
  if (!missing(seed)) 
    set.seed(seed) 
  ## do other stuff
  ....
}

# Quick exclusion instead of dplyr::dplyr::filter()
'%!in%' <- function(x,y)!('%in%'(x,y))

# Select BMGs and Pick Each
ODID_exclusive <- function(...., model_exclusive = c("ID", "OD", "Both", "None")[3], criteria = c("SSE", "Stat", "Rnd")[3]){
  
  if(criteria == "Stat") criteria <- "SSE" # Even from SEER/EER outliers, max SSE can be chosen...
  
  if(criteria == "SSE"){
    if(model_exclusive == "ID"){
      .... %>% group_by(CoilModelNumber) %>%
        top_n(n=1, criteria) %>% 
        sample_n(1) %>% impose_seed() %>%
        ungroup() %>% group_by(BMGID) %>% 
        top_n(n=1, criteria) %>% 
        sample_n(1) %>% impose_seed() %>% arrange(desc(SSE)) %>% return()
    }
}} 

# Select models based on unique (or exclusive OD, ID model numbers and selection criteria 
ODID_exclusive <- function(...., model_exclusive = c("ID", "OD", "Both", "None")[3], criteria = c("SSE", "Stat", "Rnd")[3]){
  
  if(criteria == "Stat") criteria <- "SSE" # Even from SEER/EER outliers, max SSE can be chosen...
  
  if(criteria == "SSE"){
    if(model_exclusive == "ID"){
      .... %>% group_by(CoilModelNumber) %>%
        top_n(n=1, criteria) %>% 
        sample_n(1) %>% impose_seed() %>%
        ungroup() %>% group_by(BMGID) %>% 
        top_n(n=1, criteria) %>% 
        sample_n(1) %>% impose_seed() %>% arrange(desc(SSE)) %>% return()
    }
    else if(model_exclusive == "OD"){
      .... %>% group_by(ModelNumber) %>%
        top_n(n=1, criteria) %>% 
        sample_n(1) %>% impose_seed() %>%
        ungroup() %>% group_by(BMGID) %>% 
        top_n(n=1, criteria) %>% 
        sample_n(1) %>% impose_seed() %>% arrange(desc(SSE)) %>% return()
    }
    else if (model_exclusive == "Both"){
      .... %>% group_by(CoilModelNumber) %>%
        top_n(n=1, criteria) %>% 
        sample_n(1) %>% impose_seed() %>%
        ungroup() %>% group_by(BMGID) %>% 
        top_n(n=1, criteria) %>% 
        sample_n(1) %>% impose_seed() %>% arrange(desc(SSE)) %>%
        group_by(ModelNumber) %>%
        top_n(n=1, criteria) %>% 
        sample_n(1) %>% impose_seed() %>%
        ungroup() %>% group_by(BMGID) %>% 
        top_n(n=1, criteria) %>% 
        sample_n(1) %>% impose_seed() %>% arrange(desc(SSE)) %>% return()
    }
    else if (model_exclusive == "None"){
      .... %>% group_by(BMGID) %>% 
        top_n(n=1, criteria) %>% 
        sample_n(1) %>% impose_seed() %>% arrange(desc(SSE)) %>% return()
    }
    else return(NULL)
  }
  else if(criteria == "Rnd"){
    if(model_exclusive == "ID"){
      .... %>% group_by(CoilModelNumber) %>%
        sample_n(1) %>% impose_seed() %>%
        ungroup() %>% group_by(BMGID) %>% 
        sample_n(1) %>% impose_seed() %>% arrange(desc(SSE)) %>% return()
    }
    else if(model_exclusive == "OD"){
      .... %>% group_by(ModelNumber) %>%
        sample_n(1) %>% impose_seed() %>%
        ungroup() %>% group_by(BMGID) %>% 
        sample_n(1) %>% impose_seed() %>% arrange(desc(SSE)) %>% return()
    }
    else if (model_exclusive == "Both"){
      .... %>% group_by(CoilModelNumber) %>%
        sample_n(1) %>% impose_seed() %>%
        ungroup() %>% group_by(BMGID) %>% 
        sample_n(1) %>% impose_seed() %>% arrange(desc(SSE)) %>%
        group_by(ModelNumber) %>%
        sample_n(1) %>% impose_seed() %>%
        ungroup() %>% group_by(BMGID) %>% 
        sample_n(1) %>% impose_seed() %>% arrange(desc(SSE)) %>% return()
    }
    else if (model_exclusive == "None"){
      .... %>% group_by(BMGID) %>% 
        sample_n(1) %>% impose_seed() %>% arrange(desc(SSE)) %>% return()
    }
    else return(NULL)
  }
}