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

dashheader <- dashboardHeader(title = "",titleWidth = 200,
                              tags$li(class = "dropdown",fluidRow(
                                
                                tags$div(style = "margin-top: 15px;margin-right: -160px;font-size: 15px",
                                         prettySwitch("togglemode", "Night mode", value = FALSE, fill = TRUE, status = "primary")))))

dashSidebar <- dashboardSidebar(width = 200,collapsed = F,
                                tags$head(tags$link(rel="shortcut icon", href="tcga_logo.gif", height ="100%", width = "100%")),
                                          
                                shinyjs::useShinyjs(),
                                use_darkmode(),
                                sidebarMenu(id = "sidebar",
                                            menuItem(text = "Prepare TCGA",tabName = "prepare_tcga",icon = icon("file")),
                                            menuItem(text = "Download TCGA",tabName = "download_tcga",icon = icon("download")),
                                            uiOutput('style_tag'))
)

dashBody <- dashboardBody(
  useShinyjs(),
  ####################################################
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "TCGA_style.css")),
  #########################################################
  tabItems(
    ################################################### ann #######################################################
    tabItem(
      tabName = "prepare_tcga",
      fluidRow(
        column(3,
               #box(title = "Searching TCGA", status = "primary", solidHeader = T, collapsible = F, width = "100%",
               sidebarPanel(width = "100%",
               actionButton(inputId = "check_tcgaprojects", label = "Check for available projects", width = "100%"),
               column(12, br()),
               selectInput(inputId = "tcga_project_name", label = "Select a project", choices ="", multiple = F),
               selectInput(inputId = "tcga_genomeref", label = "Reference genome", choices =c("hg19", "hg38"), multiple = F),
               actionButton(inputId = "find_tcgadatacategory", label = "Display data categories", width = "100%"),
               column(12, br()),
               selectInput(inputId = "select_tcga_datacategorices", label = "Select a data category", choices ="", multiple = F),
               selectInput(inputId = "select_tcga_data_type", label = "Select a data type", choices ="", multiple = F),
               actionButton(inputId = "find_tcga_data_parameters", label = "Search for data", width = "100%"))),
        
        
        conditionalPanel(condition = "input.find_tcgadatacategory",      
        column(4,      
        box(title = "Data categorices", status = "primary", solidHeader = T, collapsible = F, width = "100%", height = 500, 
        DTOutput("tcga_datacategories_table")))
        ),
        conditionalPanel(condition = c("input.find_tcga_data_parameters && output.hide_panel_error_in_no_data === true"),
                         infoBox(title = "No data",subtitle = "Sorry! There is no data. Please choose another data type.",color = "maroon",icon = icon("bug"),fill = TRUE)),
        
        conditionalPanel(condition = c("input.find_tcga_data_parameters && output.hide_panel_error_in_no_data === false"),
                         infoBox(title = "Successful search",subtitle = "Go to download section.",color = "olive",icon = icon("check"),fill = TRUE))




)
),
tabItem(tabName = "download_tcga",
        fluidRow(
          column(12,
          box(title = "Upload TCGA table", status = "primary", solidHeader = T, collapsible = F, width = "100%",
              column(3),
              column(3, fileInput("uploaded_rdata",label = "Upload RData file",multiple = FALSE,accept = ".RData",width = "100%")),
              column(3, div(id="load_tcga_info_pos", actionButton(inputId = "load_tcga_info", label = "Load the data", width = "100%"))),
          column(3))),
          conditionalPanel(condition = "input.load_tcga_info",
          column(12,
                 box(title = "Table of contents", status = "success", solidHeader = T, collapsible = F, width = "100%",
                     DTOutput("loaded_data_table"))),
          column(5),
          column(2, div(id = "tcga_download_action_style", actionButton(inputId = "tcga_download_action", label = "Download data", width = "100%")),
                 ),
          column(5)
          ),
          column(12, br()),
          
          conditionalPanel(condition = "input.tcga_download_action",
               column(12,
                box(title = "Download status", status = "primary", solidHeader = T, collapsible = F, width = "100%",
                    pre(id = "console")
                )))
          
          
          )
        )
)
)


dashboardPage(
  header = dashheader,
  sidebar = dashSidebar,
  body = dashBody,
  title = "TCGA-Downloader",
  skin = "purple"
  
)