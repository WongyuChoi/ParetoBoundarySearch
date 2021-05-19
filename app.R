# main module: this will run all modules and scripts
# make sure some Java related scripts using shinyjs use different name space system such as "reset" function
# library should be in the following order for Docker containerization

library(shiny)
library(shinyjs)
library(pool)
library(tidyverse)
library(DBI)
library(dbplyr)
library(DT)
library(scales)
library(stringr)
library(lubridate)
library(Rmisc)
library(data.table)
library(lazyeval)
library(rlang)
library(rPref) 
library(igraph)
library(fields)
library(grid)
library(reshape2)
library(viridis)
library(MASS)
source("1_uploadParseModule.R")
source("5-1_selection.R")
source("5-2_selection_sample.R")
source("5-3_selectionFIlterGraphics.R")


ui <- fluidPage(
  useShinyjs(),

  navbarPage("Pareto Search Tool-A Part of Data Audit Tool", fluid = TRUE,
             tabPanel("File Processing",
                      
                      fluidRow(
                        column(width = 6,
                               h4("Data Upload: CSV Format"),
                               ACSUploadParseUI("Dir", type = "Directory")
                               )
                      )
            ),
            tabPanel("Pareto Selection", 
                     sidebarLayout(
                       sidebarPanel(width = 2,
                                    selectionControlUI_part1("Dir"),
                                    fluidRow(actionButton("reset", "Reset Uploaded")),
                                    selectionControlUI_part2("Dir")
                                    ),
                       selectionControlFilter("Dir")
                     ),
                     consoleUpdateUI("Dir")
            )
  )
)

server <- function(input, output, session) {
  session$allowReconnect(TRUE)
  
  rv_main <- reactiveValues(
    dfloaded = NULL,
    reset = FALSE
  )
  
  options(shiny.maxRequestSize=3000*1024^2) # File size limited to 3GB
  
  output_ACS_rv <- callModule(ACSUploadParse, "ACS", type = "ACS")
  input_ACS_var <- callModule(UploadParse_pass, "ACS", output_ACS_rv)
  
  output_Dir_rv <- callModule(ACSUploadParse, "Dir", type = "Directory")
  input_Dir_var <- callModule(UploadParse_pass, "Dir", output_Dir_rv)
  
  callModule(ACSBoxChart, "ACS", output_ACS_rv)
  
  callModule(Scat, "ACS", output_ACS_rv)
  
  callModule(SVAnalysis, "ACS", output_ACS_rv, type = "ACS")
  
  callModule(SVAnalysis, "Dir", output_Dir_rv, type = "Directory")
  
  callModule(ACSBoxChart, "Dir", output_Dir_rv)
  
  callModule(Scat, "Dir", output_Dir_rv)
 
  callModule(selection, "Dir", output_Dir_rv)
  
  callModule(consoleUpate, "Dir", output_Dir_rv)
  
  output_Pareto_rv <- callModule(runParetoSelection, "Dir", output_Dir_rv, rv_main)
  
  callModule(selectionFilterGraphics, "Dir", output_Dir_rv)
  
  callModule(downloadPareto, "Dir", output_Pareto_rv)
  
  observeEvent(input$reset, {
    shinyjs::reset("Dir-selectionExclusion")
    rv_main$dfloaded <- NULL
    rv_main$reset <- TRUE
    #browser() #checkpoint reset the exclusion upload
  }, priority = 1000)
  
  # dfexclude <- reactive({rv$dfloaded})
  # reseted <- reactive({rv$reset})
}
  
  
shinyApp(ui, server)