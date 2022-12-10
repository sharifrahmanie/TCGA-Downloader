require(tidyverse)
require(DT)
require(TCGAbiolinks)
require(shiny)
require(shinydashboard)
require(shinyjs)
require(shinyDarkmode)
require(shinyWidgets)
require(shinyBS)
options(shiny.maxRequestSize=500*1024^2) 
options(warn=-1)
graphics.off()

shinyServer(function(input, output, session){
  
  session$sendCustomMessage(type = "manipulateMenuItem1",
                            message = list(action = "hide",toggle = "offcanvas", role = "button"))
  ###################### Dark mode ####################
  darkmode_toggle(inputid = 'togglemode')
  
  # Check all available projects
  available_projects <- reactive({
    if(input$check_tcgaprojects){
      projects <- as.vector(TCGAbiolinks:::getGDCprojects()$project_id)
      tcga <- grep("^TCGA-.+", projects)
      projects <- projects[tcga]
    }
  })
  
  
  observeEvent(input$check_tcgaprojects, {
    if(!is.null(available_projects())){
      req(available_projects())
      choices <- available_projects()
      updateSelectInput(
        session,
        "tcga_project_name",
        choices = choices
      )
    }
  })
  tcga_datacategories <- reactive({
    if(!is.null(available_projects()) && !is.null(input$tcga_project_name)){
      if(input$tcga_genomeref == "hg19"){
        legacy <- TRUE
      } else {
        legacy <- FALSE
      }
      data <- TCGAbiolinks:::getProjectSummary(input$tcga_project_name, legacy = legacy)$data_categories
      data
   }
  })
  output$tcga_datacategories_table <- renderDT({
    if(input$find_tcgadatacategory){
      table <- isolate(tcga_datacategories())
      table %>%
        datatable(extensions = "Buttons", 
                  options = list(autoWidth = FALSE,scrollX = TRUE, pageLength=6,
                                 buttons =c("excel", "csv"),
                                 dom = "rtp"),
                  rownames=FALSE)
      
    }
  })
  
  # Data category choices
  observeEvent(input$find_tcgadatacategory, {
    if(input$find_tcgadatacategory){
      req(tcga_datacategories())
      choices <- tcga_datacategories()[3][,1]
      updateSelectInput(
        session,
        "select_tcga_datacategorices",
        choices = choices
      )
    }
  })
  
  
  # Data type choices
  observeEvent(input$find_tcgadatacategory, {
    if(input$find_tcgadatacategory){
     if(input$tcga_genomeref == "hg19"){
              data.type <- c("Copy number segmentation",
                             "Raw intensities",
                             "Aligned reads",
                             "Copy number estimate",
                             "Simple nucleotide variation",
                             "Gene expression quantification",
                             "Coverage WIG",
                             "miRNA gene quantification",
                             "Genotypes",
                             "miRNA isoform quantification",
                             "Normalized copy numbers",
                             "Isoform expression quantification",
                             "Normalized intensities",
                             "Tissue slide image",
                             "Exon quantification",
                             "Exon junction quantification",
                             "Methylation beta value",
                             "Unaligned reads",
                             "Diagnostic image",
                             "CGH array QC",
                             "Biospecimen Supplement",
                             "Pathology report",
                             "Clinical Supplement",
                             "Intensities",
                             "Protein expression quantification",
                             "Microsatellite instability",
                             "Structural variation",
                             "Auxiliary test",
                             "Copy number QC metrics",
                             "Intensities Log2Ratio",
                             "Methylation array QC metrics",
                             "Clinical data",
                             "Copy number variation",
                             "ABI sequence trace",
                             "Biospecimen data",
                             "Simple somatic mutation",
                             "Bisulfite sequence alignment",
                             "Methylation percentage",
                             "Sequencing tag",
                             "Sequencing tag counts",
                             "LOH")
     } else {
       data.type <- c(
         "Aggregated Somatic Mutation",
         "Gene Expression Quantification",
         "Raw CGI Variant",
         "Methylation Beta Value",
         "Splice Junction Quantification",
         "Annotated Somatic Mutation",
         "Raw Simple Somatic Mutation",
         "Masked Somatic Mutation",
         "Copy Number Segment",
         "Allele-specific Copy Number Segment",
         "Masked Copy Number Segment",
         "Isoform Expression Quantification",
         "miRNA Expression Quantification",
         "Gene Level Copy Number",
         "Biospecimen Supplement",
         "Gene Level Copy Number Scores",
         "Protein Expression Quantification",
         "Clinical Supplement",
         "Masked Somatic Mutation",
         "Slide Image")
     }
      updateSelectInput(
        session,
        "select_tcga_data_type",
        choices = data.type
      )
    }
  })
  tcga_info_table <- reactive({
    if(input$find_tcga_data_parameters){
      if(input$tcga_genomeref == "hg19"){
        legacy <- TRUE
      } else {
        legacy <- FALSE
      }
    wf <- TCGAbiolinks::GDCquery(project = input$tcga_project_name, legacy = legacy,
                                      data.category = input$select_tcga_datacategorices,
                                      data.type = input$select_tcga_data_type)
    
    if(!is.null(wf)){
    dc_name <- gsub(" ", "_", input$select_tcga_datacategorices)
    dt_name <- gsub(" ", "_", input$select_tcga_data_type)
    f_name <- paste0("PRE_",input$tcga_project_name, "_", dc_name, "_", dt_name, ".RData")
    save(wf, file = f_name)
    }
    wf
    }  
  })
  
  # If no data has found
  output$hide_panel_error_in_no_data <- reactive({
    if(input$find_tcga_data_parameters){
      result <- isolate(tcga_info_table())
     hideselect <- function(){
      if(NCOL(result) == 1){
        return(TRUE)
      }
      else if(NCOL(result) > 1){
        return(FALSE)
      }
    }
    hideselect()
    }
  })

  outputOptions(output, 'hide_panel_error_in_no_data', suspendWhenHidden=FALSE)

  
  # Making reactive for info loaded 
  tcga_info_loaded <- reactive({
    if(!is.null(input$uploaded_rdata) && input$load_tcga_info){
      req(input$uploaded_rdata)
      load(input$uploaded_rdata$datapath)
      wf <- data.frame(wf)
      wf
    }
  })

  
  output$loaded_data_table <- renderDT({
    if(input$load_tcga_info){
      req(tcga_info_loaded())
      table <- isolate(tcga_info_loaded()$results[[1]])
      dftype <- sapply(table, class)
      forfactor <- grep("character|logical", dftype)
      for(i in forfactor){
        table[, i] <- factor(table[, i])
      }
        datatable(table,
                  fillContainer = F,
                  extensions = "AutoFill",
                  style = "auto",
                  options = list(autoWidth = TRUE, scrollX = TRUE, pageLength=10,
                                 columnDefs = list(list(width = '300px', targets = '_all')),
                                 buttons =c("excel", "csv"),
                                 dom = "lBfrtip"),
                                 filter = list(
                                   position = 'top', clear = FALSE
                                 ),
                  rownames = FALSE)
    }
  })

  # Filtering rows
  filtered_row <- reactive({
      input[["loaded_data_table_rows_all"]]
    })
  
  
  # Download TCGA data
  downloading_tcga_no <- reactive({
    if(input$tcga_download_action){
      req(tcga_info_loaded())
    data <- isolate(tcga_info_loaded())
    data_df <- data
    data_df$results[[1]] <- data$results[[1]][filtered_row(),]
    GDCdownload(data_df, method = "api", files.per.chunk = 10)
    
    tcga_data <- GDCprepare(data_df)
    name_svae <- gsub("^PRE_", "", input$uploaded_rdata$name)
    save(tcga_data, file = name_svae)
    }
  })
  
  #########################################################################
  withConsoleRedirect <- function(containerId, expr) {
    # Change type="output" to type="message" to catch stderr
    # (messages, warnings, and errors) instead of stdout.
    txt <- capture.output(results <- expr, type = "message")
    if (length(txt) > 0) {
      insertUI(paste0("#", containerId), where = "beforeEnd",
               ui = paste0(txt, "\n", collapse = "")
      )
    }
    results
  }
  
  
  observeEvent(input$tcga_download_action, {
    withConsoleRedirect("console", {
      downloading_tcga_no()
    })
  })
  
  
  observeEvent(input$tcga_download_action, {
    withCallingHandlers(
      downloading_tcga_no(),
      # can use "warning" instead/on top of "message" to catch warnings too 
      message = function(m) {
        shinyjs::html("console", m$message, TRUE)
      }
    )
  })
  

})