library(shiny)
library(ggplot2)
library(cowplot)
library(dplyr)
library(kableExtra)
library(LRErdd)
library(shinydashboard)
library(rhandsontable)
library(DT)
library(LRErdd)
library(plotly)
library(repmis)
library(raster)
library(openxlsx)
library(gtools)

# source("AdjTP_plotly.R")
# dashboardPage(skin = "black")
## Step 1. UI - basic part of shiny app
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "LRErdd: Regression Discontinuity", titleWidth = 450),
                    dashboardSidebar(width = 350,
                                     tags$head(tags$style(HTML(' .skin-blue .sidebar-menu .treeview-menu> li.active > a,
                                                               .skin-blue .sidebar-menu .treeview-menu> li:hover > a {
                                                               color: #8aa4af;
                                                               }
                                                               
                                                               .skin-blue .main-header .logo {
                                                               background-color: #3c8dbc;
                                                               }
                                                               
                                                               .skin-blue .main-header .logo:hover {
                                                               background-color: #3c8dbc;
                                                               }
                                                               
                                                               .skin-blue .sidebar-menu > li {
                                                               white-space: normal;
                                                               }
                                                               
                                                               .modebar {
                                                               display: none !important;
                                                               }
                                                               
                                                               
                                                               '))), 
                                     tags$style(".fa-table {color:#33cc33}"),     
                                     tags$style(".table2 {color:#0099ff}"),     
                                     
                                     sidebarMenu(width = 350,
                                                 menuSubItem( p(""),
                                                              icon = NULL),
                                                 menuItem("1A-Load Excel file", icon = icon("table"),
                                                          menuSubItem(fileInput("excelfile", 
                                                                                "Upload Excel file", 
                                                                                accept=c('.xls', 
                                                                                         '.xlsx')),
                                                                      icon = NULL),
                                                          # update dropdown for outcome + treatment selection when file is uploaded
                                                          menuSubItem(numericInput('excel_sheetnumber', 'Select the sheet number', 1),icon = NULL),
                                                          menuSubItem(numericInput('excel_startrow', 'Select the start row number', 1),icon = NULL),
                                                          menuSubItem(textInput('excel_stringNA', 'Write the missing strings', "NA"),icon = NULL)
                                                          
                                                          # read.xlsx(file_to_read_excel$datapath, sheet = input$excel_sheetnumber, startRow = input$excel_startrow, na.strings = input$excel_stringNA)
                                                          
                                                          
                                                 ),
                                                 # tags$style(".fa-table {color:#ff3399}"),             
                                                 menuItem("1B-Load R file", icon = icon("table",class="table2"),
                                                          menuSubItem(fileInput("rdatafile", 
                                                                                "Upload R file", 
                                                                                accept=c('.rdata', '.RData',
                                                                                         '.Rdata')),
                                                                      icon = NULL)
                                                          
                                                 ),
                                                 
                                                 menuItem("2-Define settings for summary statistics", icon = icon("cogs"),
                                                          menuSubItem(uiOutput("col_sel_forcing7"),icon = NULL),
                                                          menuSubItem(numericInput('cutvalue2', 'Select threshold value', 15000),icon = NULL),
                                                          menuSubItem(radioButtons('onezero2', 'Select your case:',
                                                                                   c("Z = 1 if S >= s0 and Z = 0 if S < s0" = 'under0',
                                                                                     "Z = 1 if S <= s0 and Z = 0 if S > s0" = 'under1'),"under0" ),icon = NULL),
                                                          menuSubItem(uiOutput("col_sel_covar2"),icon = NULL),
                                                          menuSubItem(actionButton("go_bwsel2", "Run"),icon = NULL)
                                                          
                                                 ), 
                                                 
                                                 menuItem("3-Bandwidth selection", icon = icon("microchip"),  
                                                          
                                                          menuSubItem(radioButtons('typerange', 'Select bandwidth control?',
                                                                                   c("Percentage" = 'perce',
                                                                                     "Balanced units" = 'balunit',
                                                                                     "Unbalanced units" = 'unbalunit'),"perce" ),icon = NULL),
                                                          menuSubItem(numericInput('stepunit', 'Select the minimum unit', 1, width = "60%"),icon = NULL),
                                                          menuSubItem(uiOutput("ui_slide_range"),icon = NULL),
                                                          menuSubItem(numericInput('nsim', 'Number of iterations', 50),icon = NULL),
                                                          menuSubItem(uiOutput("col_sel_covar"),icon = NULL),
                                                          menuSubItem(actionButton("go_bwsel", "Run part A"),icon = NULL),
                                                          menuSubItem(uiOutput("col_sel_covar7"),icon = NULL),
                                                          menuSubItem(radioButtons('intchar', 'Binary or continuous?',
                                                                                   c("Binary" = 'binary',
                                                                                     "Continuous" = 'conti'),"binary" ),icon = NULL),
                                                          menuSubItem(actionButton("go2", "Run part B"),icon = NULL)
                                                          #  )
                                                 ),
                                                 
                                                 menuItem("4-Summary bandwidth selection", icon = icon("object-group"),  
                                                          menuSubItem(textInput("bandwidths9", "Include the bandwidths", "500,1000,1500"),icon = NULL),
                                                          menuSubItem(numericInput('niter9',"Number of iterations",1000),icon = NULL),
                                                          # menuSubItem(textInput("cin9", "Select the alpha level", 0.05),icon = NULL),
                                                          menuSubItem(uiOutput("col_sel_covar_covar9"),icon = NULL),
                                                          menuSubItem(actionButton("go9", "Run Analysis"),icon = NULL)
                                                 ),
                                                 
                                                 menuItem("5-Inference on causal effects", icon = icon("flag"), 
                                                          menuSubItem( p("Please select the method and then the settings"), icon = NULL),
                                                          radioButtons('sel_method', 'Select the method:',
                                                                       c('Sharp RDD: FEP approach'='FEPRDis',
                                                                         'Sharp RDD: Neyman approach' = 'sharpNeyman',
                                                                         'Fuzzy RDD: FEP approach' = 'fuzzyFEP',
                                                                         'Fuzzy RDD: Neyman approach' = 'fuzzyNeyman'),'FEPRDis'),
                                                          textInput("bandwidths", "Include the bandwidths", "500,1000,1500"),
                                                          radioButtons('typemod', 'Select type of outcome:',
                                                                       c('Binary'='binary',
                                                                         'Continuous'='cont'), "binary"),
                                                          uiOutput("col_sel_covar_Y"),
                                                          numericInput('niter',"Number of iterations",1000)
                                                 ),
                                                 
                                                 menuItem("5A-Sharp FEP", icon = icon("flag"),  
                                                          # radioButtons('sel_method', 'Select the method:',
                                                          #              c('Sharp RDD: FEP approach'='FEPRDis'),'FEPRDis'),
                                                          p("---"),
                                                          actionButton("go_sharp_fep", "Run Analysis")
                                                 ),
                                                 
                                                 menuItem("5B-Sharp Neyman", icon = icon("flag"),  
                                                          # radioButtons('sel_method', 'Select the method:',
                                                          #              c('Sharp RDD: Neyman approach' = 'sharpNeyman'),'sharpNeyman'),
                                                          textInput("cin", "Select the alpha level", 0.05),
                                                          actionButton("go_sharp_neyman", "Run Analysis")
                                                 ),
                                                 menuItem("5B-Fuzzy FEP", icon = icon("flag"),  
                                                          # radioButtons('sel_method', 'Select the method:',
                                                          #              c('Fuzzy RDD: FEP approach' = 'fuzzyFEP'),'fuzzyNeyman'),
                                                          textInput('M2',"Number of iterations for fuzzy FEP",5),
                                                          radioButtons('one2side', 'Select type of sided:',
                                                                       c('One sided'='onesided',
                                                                         'Two sided'='twosided'), "onesided"),
                                                          uiOutput("col_sel_covar_W"),
                                                          helpText("Note: Fuzzy FEP take very long time. Be patient"),
                                                          actionButton("go_fuzzy_fep", "Run Analysis")
                                                 ),
                                                 menuItem("5B-Fuzzy Neyman", icon = icon("flag"), 
                                                          # radioButtons('sel_method', 'Select the method:',
                                                          #              c('Fuzzy RDD: Neyman approach' = 'fuzzyNeyman'),'fuzzyNeyman'),
                                                          uiOutput("col_sel_covar_W_neyman"),
                                                          actionButton("go_fuzzy_neyman", "Run Analysis")
                                                 )
                                                 
                                                 # menuItem("Export data", icon = icon("history"),  
                                                 #          
                                                 #          textInput("namerdata", "Write the file name", value = filename),
                                                 #          downloadButton("download ", "Download the table")
                                                 #          
                                                 # )
                                     ) # sidebarMenu
                                     ), # dashboardSidebar
                    
                    
                    dashboardBody(    
                      tabBox(width = "500px",
                             # div(style = 'overflow-x: scroll', DT::dataTableOutput('exptable')),
                             # tabPanel("Plot",
                             #          fluidRow(
                             #                   box(width = 12, height =700, plotlyOutput("ETP", height = "650px", width = "1000px")%>% 
                             #                         withSpinner(color="#0dc5c1")%>% 
                             #                         layout(height = "1000px"))
                             #                   )
                             #          ),
                             
                             tabPanel("Instructions", 
                                      fluidRow(br(),
                                               # basic
                                               box(width = 12, h2("What you can do with LRErdd"),
                                                   h4("Regression Discontinuity as Local Randomized Experiments"),
                                                   p("We present the R LRErdd package with a case study. The package includes a set of functions for the design and analysis of Regression Discontinuity Designs as local randomized experiments within the potential outcome approach as formalized in Li et al (2015). 
                                                     A sub-set of functions implements the design phase of the study where focus is on the selection of suitable subpopulations for which we can draw valid causal inference. 
                                                     These functions provide summary statistics of pre-and post-treatment variables by treatment status, 
                                                     and select suitable subpopulations around the threshold where pre-treatment variables are well balanced between treatment using randomization-based tests with adjustment for multiplicities. Functions for a visual inspection of the results are also provided. 
                                                     Finally the LRErdd package includes a set of functions for drawing inference on causal effects for the selected subpopulations using randomization-based modes of inference. Specifically the Fisher Exact $p-$value and Neyman approaches are implemented for the analysis of both sharp and fuzzy RD designs. We illustrate our approach in a study concerning the effects of University grants on student dropout."), 
                                                   
                                                   p("Li F, Mattei A, Mealli F (2015). Bayesian inference for regression discontinuity designs withapplication to the evaluation of Italian university grants. The Annals of Applied Statistics,9(4), 1906-1931.")),
                                               # data
                                               box(width = 12, h2("Step by step"),
                                                   h4("In the next sections you can see the steps that you should follow"),
                                                   img(src='Squeme.jpg', align = "left", width = 1400, height = 420)
                                               ),
                                               
                                               
                                               box(width = 12, h2("1-Load data"),
                                                   p("You can import the data from excel (xlsx) or R format (RData). Then you can see your data in the second tab called '1-Your Data'. If you don't have appropiate data you can use the dataset is included by default. This ")),
                                               box(width = 12, h2("2-Define the parameters and Summary statistics"),
                                                   p("Forcing variable (S), cut-value (s0)...")),
                                               box(width = 12, h2("3-Bandwidth selection"),
                                                   p("...")),
                                               box(width = 12, h2("4-Summary Bandwidth selection"),
                                                   p("...")),
                                               box(width = 12, h2("5-Inference on causal effects"),
                                                   p("..."))
                                               
                                               ) # fluidRow
                                      ),   # tabPanel
                             
                             tabPanel("1-Your Data", 
                                      fluidRow(
                                        box(title = "View Data", 
                                            width = NULL,
                                            status = "primary", 
                                            solidHeader = TRUE,
                                            collapsible = TRUE,
                                            div(style = 'overflow-x: scroll', DT::dataTableOutput('dto'))
                                            # h2("Experiment Data")
                                            # DTOutput("exptable"))
                                        )
                                        # fluidRow(
                                        #   box(width = 12, h2("Imputed Data"), DTOutput("imptable"))
                                        # )
                                      )
                             ),
                             
                             
                             tabPanel("2-Summary statistics", 
                                      fluidRow(
                                        box(width = NULL,
                                            status = "primary", 
                                            solidHeader = TRUE,
                                            collapsible = TRUE,
                                            # div(style = 'overflow-x: scroll', DT::DTOutput('editable'))
                                            h4("Summary statistics"),
                                            tableOutput("descriptive1")
                                        )
                                        # fluidRow(
                                        #   box(width = 12, h2("Imputed Data"), DTOutput("imptable"))
                                        # )
                                      )
                             ),
                             
                             
                             tabPanel("3-Bandwidth selection", 
                                      fluidRow(
                                        box(width = NULL,
                                            status = "primary", 
                                            solidHeader = TRUE,
                                            collapsible = TRUE,
                                            # div(style = 'overflow-x: scroll', rHandsontableOutput('table'))
                                            
                                            h4("Part A - Histogram"),
                                            plotOutput("histogram"),
                                            h4("Part A - Summary"),
                                            tableOutput("summarydim"),
                                            tableOutput("summary")
                                        )),
                                      fluidRow(
                                        box(width = 12, h2("Imputed Data"), 
                                            h4("Part B - Distribution plot"),
                                            plotOutput("plotprop"))
                                      )
                             ),
                             
                             tabPanel("4-Summary Bandwidth selection", 
                                      fluidRow(
                                        box(width = NULL,
                                            status = "primary", 
                                            solidHeader = TRUE,
                                            collapsible = TRUE,
                                            # div(style = 'overflow-x: scroll', rHandsontableOutput('table'))
                                            h4("Summary"),
                                            dataTableOutput("tablemethod9")
                                            
                                        ))
                             ),
                             
                             tabPanel("5-Inference on causal effects", 
                                      fluidRow(
                                        box(width = NULL,
                                            status = "primary", 
                                            solidHeader = TRUE,
                                            collapsible = TRUE,
                                            # div(style = 'overflow-x: scroll', rHandsontableOutput('table'))
                                            
                                            h4("Summary"),
                                            dataTableOutput("tablemethod"),
                                            h4("Plots"),
                                            plotOutput("plotres")
                                            
                                            
                                        ))
                             )
                             
                             
                             
                                      )   # tabBox
                             )
                                     )
