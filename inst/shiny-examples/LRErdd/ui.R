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

options(shiny.reactlog=TRUE)

# source("AdjTP_plotly.R")
# dashboardPage(skin = "black")
## Step 1. UI - basic part of shiny app
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "LRErdd v0.10_seed27_20181107", titleWidth = 450),
                    dashboardSidebar(width = 350,
                                     tags$head(tags$script(HTML("
                                                                Shiny.addCustomMessageHandler('manipulateMenuItem', function(message){
                                                                var aNodeList = document.getElementsByTagName('a');
                                                                
                                                                for (var i = 0; i < aNodeList.length; i++) {
                                                                if(aNodeList[i].getAttribute('data-value') == message.tabName) {
                                                                if(message.action == 'hide'){
                                                                aNodeList[i].setAttribute('style', 'display: none;');
                                                                } else {
                                                                aNodeList[i].setAttribute('style', 'display: block;');
                                                                };
                                                                };
                                                                }
                                                                });
                                                                "))),
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
                                     tags$head(
                                       tags$script(
                                         HTML(
                                           "
                                           $(document).ready(function(){
                                           // Bind classes to menu items, easiet to fill in manually
                                           var ids = ['bostinfer','5a','5b','5c','5d','tab_loadoriginal','tab_loadexcel','tab_loadrdata'];
                                           for(i=0; i<ids.length; i++){
                                           $('a[data-value='+ids[i]+']').addClass('my_subitem_class');
                                           }
                                           
                                           // Register click handeler
                                           $('.my_subitem_class').on('click',function(){
                                           // Unactive menuSubItems
                                           $('.my_subitem_class').parent().removeClass('active');
                                           })
                                           })
                                           "
                                         )
                                       )
                                       ),
                                     tags$style(".fa-table {color:#33cc33}"),     
                                     tags$style(".table2 {color:#0099ff}"),     
                                     
                                     
                                     
                                     sidebarMenu(width = 350,p("Please read the information before using the app"),
                                                 menuSubItem(selectInput(inputId = 'select_main', label = 'Menu',
                                                                         choices = c("Intro", '1-Source: default dataset','1-Source: excel','1-Source: RData', 
                                                                                     '2-Define setting for RDD analysis', '2-Summary statistics', 
                                                                                     '3-Bandwidth selection', '4-Summary bandwidth selection', '5-Inference on causal effects'))),
                                                 
                                                 conditionalPanel(
                                                   condition = "input.select_main == 'Intro'",
                                                   sidebarMenu(
                                                     menuSubItem(actionButton("go_intro", "Show Information"),icon = NULL)
                                                     
                                                   ) # sidebarmenu
                                                 ), # conditional
                                                 
                                                 conditionalPanel(
                                                   condition = "input.select_main == '1-Source: default dataset'",
                                                   sidebarMenu(
                                                     menuSubItem(actionButton("go_default_dataset", "Data"),icon = NULL,tabName = "tab_loadoriginal", )
                                                     
                                                   ) # sidebarmenu
                                                 ), # conditional
                                                 
                                                 conditionalPanel(
                                                   condition = "input.select_main == '1-Source: excel'",
                                                   sidebarMenu(id = "allcitiestogether",
                                                               menuItem("1A-Load Excel file", icon = icon("table"), tabName = "tab_loadexcel", 
                                                                        menuSubItem(fileInput("excelfile", 
                                                                                              "Upload Excel file", 
                                                                                              accept=c('.xls', 
                                                                                                       '.xlsx')),
                                                                                    icon = NULL),
                                                                        # update dropdown for outcome + treatment selection when file is uploaded
                                                                        menuSubItem(numericInput('excel_sheetnumber', 'Select the sheet number', 1),icon = NULL),
                                                                        menuSubItem(numericInput('excel_startrow', 'Select the start row number', 1),icon = NULL),
                                                                        menuSubItem(textInput('excel_stringNA', 'Write the missing strings', "NA"),icon = NULL),
                                                                        menuSubItem(actionButton("go_excel", "Run"),icon = NULL)
                                                                        
                                                               )
                                                   ) # sidebarmenu
                                                 ), # conditional
                                                 
                                                 conditionalPanel(
                                                   condition = "input.select_main == '1-Source: RData'",
                                                   sidebarMenu(id = "selRData",
                                                               # tags$style(".fa-table {color:#ff3399}"),             
                                                               menuItem("1B-Load R file", icon = icon("table",class="table2"),tabName = "tab_loadrdata", 
                                                                        menuSubItem(fileInput("rdatafile", 
                                                                                              "Upload R file", 
                                                                                              accept=c('.rdata', '.RData',
                                                                                                       '.Rdata')), icon = NULL),
                                                                        menuSubItem(actionButton("go_rdata", "Run"),icon = NULL)
                                                                        
                                                               )
                                                   ) # sidebarmenu
                                                 ), # conditional
                                                 
                                                 conditionalPanel(
                                                   condition = "input.select_main == '2-Define setting for RDD analysis'",
                                                   sidebarMenu(id = "selRData",
                                                               # tags$style(".fa-table {color:#ff3399}"),             
                                                               menuItem("2-Define setting for RDD analysis", icon = icon("cogs"),
                                                                        menuSubItem(uiOutput("col_sel_forcing7"),icon = NULL),
                                                                        menuSubItem(numericInput('cutvalue2', 'Select threshold value', 15000),icon = NULL),
                                                                        menuSubItem(radioButtons('onezero2', 'Select your case:',
                                                                                                 c("Z = 1 if S >= s0 and Z = 0 if S < s0" = 'under0',
                                                                                                   "Z = 1 if S <= s0 and Z = 0 if S > s0" = 'under1'),"under0" ),icon = NULL)
                                                                        
                                                               )
                                                   ) # sidebarmenu
                                                 ), # conditional
                                                 
                                                 conditionalPanel(
                                                   condition = "input.select_main == '2-Summary statistics'",
                                                   sidebarMenu(id = "selRData",
                                                               # tags$style(".fa-table {color:#ff3399}"),             
                                                               menuItem("2-Summary statistics", icon = icon("cogs"),
                                                                        menuSubItem(uiOutput("col_sel_covar2"),icon = NULL),
                                                                        menuSubItem(actionButton("go_bwsel2", "Run"),icon = NULL)
                                                                        
                                                               )
                                                   ) # sidebarmenu
                                                 ), # conditional
                                                 
                                                 conditionalPanel(
                                                   condition = "input.select_main == '3-Bandwidth selection'",
                                                   sidebarMenu(id = "selRData",
                                                               # tags$style(".fa-table {color:#ff3399}"),             
                                                               menuItem("3-Bandwidth selection", icon = icon("microchip"),  
                                                                        
                                                                        menuSubItem(radioButtons('typerange', 'Select bandwidth control?',
                                                                                                 c("Percentage" = 'perce',
                                                                                                   "Symmetric sides" = 'balunit',
                                                                                                   "Asymmetrical sides" = 'unbalunit'),"perce" ),icon = NULL),
                                                                        menuSubItem(numericInput('stepunit', 'Select the scale', 1, width = "60%"),icon = NULL),
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
                                                               )
                                                   ) # sidebarmenu
                                                 ), # conditional
                                                 
                                                 conditionalPanel(
                                                   condition = "input.select_main == '4-Summary bandwidth selection'",
                                                   sidebarMenu(id = "selRData",
                                                               # tags$style(".fa-table {color:#ff3399}"),             
                                                               menuItem("4-Summary bandwidth selection", icon = icon("object-group"),  
                                                                        menuSubItem(textInput("bandwidths9", "Include the bandwidths", "500,1000,1500"),icon = NULL),
                                                                        menuSubItem(numericInput('niter9',"Number of iterations",1000),icon = NULL),
                                                                        # menuSubItem(textInput("cin9", "Select the alpha level", 0.05),icon = NULL),
                                                                        menuSubItem(uiOutput("col_sel_covar_covar9"),icon = NULL),
                                                                        menuSubItem(actionButton("go9", "Run Analysis"),icon = NULL)
                                                               )
                                                   ) # sidebarmenu
                                                 ), # conditional
                                                 
                                                 
                                                 conditionalPanel(
                                                   condition = "input.select_main == '5-Inference on causal effects'",
                                                   sidebarMenu(id = "selRData",
                                                               
                                                               # tags$style(".fa-table {color:#ff3399}"),             
                                                               menuItem("5-Inference on causal effects", icon = icon("flag"), tabName = "bostinfer",
                                                                        menuSubItem( p("Please select the method and then the settings"), icon = NULL),
                                                                        # radioButtons('sel_method', 'Select the method:',
                                                                        #              c('Sharp RDD: FEP approach'='FEPRDis',
                                                                        #                'Sharp RDD: Neyman approach' = 'sharpNeyman',
                                                                        #                'Fuzzy RDD: FEP approach' = 'fuzzyFEP',
                                                                        #                'Fuzzy RDD: Neyman approach' = 'fuzzyNeyman'),'FEPRDis'),
                                                                        textInput("bandwidths", "Include the bandwidths", "500,1000,1500"),
                                                                        radioButtons('typemod', 'Select type of outcome:',
                                                                                     c('Binary'='binary',
                                                                                       'Continuous'='cont'), "binary"),
                                                                        uiOutput("col_sel_covar_Y"),
                                                                        numericInput('niter',"Number of iterations",1000),
                                                                        
                                                                        menuSubItem(selectInput(inputId = 'select_effect', label = 'Method',
                                                                                                choices = c("Select method", "Sharp FEP", 'Sharp Neyman','Fuzzy FEP', 'Fuzzy Neyman')),tabName = "selmethod")
                                                                        # menuSubItem(actionButton("go_sharp_fep", "Run Analysis"),icon = NULL)
                                                               ),
                                                               
                                                               
                                                               
                                                               conditionalPanel(
                                                                 condition = "input.select_effect == 'Sharp FEP'",
                                                                 sidebarMenu(id = "selRData",
                                                                             menuItem("5A-Sharp FEP", tabName = "5a", icon = icon("flag"),
                                                                                      # radioButtons('sel_method', 'Select the method:',
                                                                                      #              c('Sharp RDD: FEP approach'='FEPRDis'),'FEPRDis'),
                                                                                      p("---"),
                                                                                      actionButton("go_sharp_fep", "Run Analysis")
                                                                                      # actionButton("go_bostak", "Run Analysis")
                                                                             )
                                                                 ) # sidebarmenu
                                                               ), # conditional
                                                               
                                                               conditionalPanel(
                                                                 condition = "input.select_effect == 'Sharp Neyman'",
                                                                 sidebarMenu(id = "selRData",
                                                                             menuItem("5B-Sharp Neyman", tabName = "5b", icon = icon("flag"),  
                                                                                      # radioButtons('sel_method', 'Select the method:',
                                                                                      #              c('Sharp RDD: Neyman approach' = 'sharpNeyman'),'sharpNeyman'),
                                                                                      textInput("cin", "Select the alpha level", 0.05),
                                                                                      actionButton("go_sharp_neyman", "Run Analysis")
                                                                                      # actionButton("go_bostak", "Run Analysis")
                                                                             )
                                                                 ) # sidebarmenu
                                                               ), # conditional
                                                               
                                                               conditionalPanel(
                                                                 condition = "input.select_effect == 'Fuzzy FEP'",
                                                                 sidebarMenu(id = "selRData",
                                                                             menuItem("5C-Fuzzy FEP", tabName = "5c", icon = icon("flag"),  
                                                                                      # radioButtons('sel_method', 'Select the method:',
                                                                                      #              c('Fuzzy RDD: FEP approach' = 'fuzzyFEP'),'fuzzyNeyman'),
                                                                                      numericInput('M2',"Number of iterations for fuzzy FEP",5),
                                                                                      radioButtons('one2side', 'Select type of compliance:',
                                                                                                   c('One-sided'='onesided',
                                                                                                     'Two-sided'='twosided'), "onesided"),
                                                                                      uiOutput("col_sel_covar_W"),
                                                                                      helpText("Note: Fuzzy FEP take very long time. Be patient"),
                                                                                      actionButton("go_fuzzy_fep", "Run Analysis")
                                                                                      # actionButton("go_bostak", "Run Analysis")
                                                                             )
                                                                 ) # sidebarmenu
                                                               ), # conditional
                                                               
                                                               conditionalPanel(
                                                                 condition = "input.select_effect == 'Fuzzy Neyman'",
                                                                 sidebarMenu(id = "selRData",
                                                                             menuItem("5D-Fuzzy Neyman", tabName = "5d", icon = icon("flag"), 
                                                                                      # radioButtons('sel_method', 'Select the method:',
                                                                                      #              c('Fuzzy RDD: Neyman approach' = 'fuzzyNeyman'),'fuzzyNeyman'),
                                                                                      uiOutput("col_sel_covar_W_neyman"),
                                                                                      actionButton("go_fuzzy_neyman", "Run Analysis")
                                                                                      # actionButton("go_bostak", "Run Analysis")
                                                                             )
                                                                 ) # sidebarmenu
                                                               ) # conditional
                                                               
                                                               
                                                   ) # sidebarmenu
                                                 ) # conditional
                                                 
                                     ) # sidebarMenu
                                       ), # dashboardSidebar
                    
                    
                    dashboardBody(    
                      tabBox(width = "500px", id = "tabs",
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
                                                   p("LRErdd package (Regression Discontinuity Designs as Local Randomized Experiments) includes a set of functions for the design and analysis of Regression Discontinuity 
                                                     Designs as local randomized experiments within the potential outcome approach as formalized in Li, Mattei and Mealli (2015). 
                                                     A subset of functions implements the design phase of the study where the focus is on the selection of suitable subpopulations for which we can draw valid causal inference. 
                                                     These functions provide summary statistics of pre-and post-treatment variables by treatment status and select suitable subpopulations around the threshold where pre-treatment 
                                                     variables are well balanced between treatment groups using randomization-based tests with adjustment for multiplicities. Functions for a visual inspection of the results are also provided. 
                                                     Finally, the LRErdd package includes a set of functions for drawing inference on causal effects for the selected subpopulations using randomization-based modes of inference. 
                                                     Specifically, the Fisher Exact $p-$value and Neyman approaches are implemented for the analysis of both sharp and fuzzy RD designs. 
                                                     We illustrate our approach in a study concerning the effects of University grants on the dropout."), 
                                                   
                                                   p("Li F, Mattei A, Mealli F (2015). Bayesian inference for regression discontinuity designs withapplication to the evaluation of Italian university grants. The Annals of Applied Statistics,9(4), 1906-1931.")),
                                               # data
                                               box(width = 12, h2("Step by step"),
                                                   h4("In the next sections you can see the steps that you should follow"),
                                                   img(src='Squeme.jpg', align = "left", width = 1400, height = 420)
                                               ),
                                               
                                               
                                               box(width = 12, h2("1-Load data"),
                                                   p("There are three options to use datasets. The first option is to use the by default dataset included in the package. 
                                                     This dataset contains information about a study concerning the effects of University grants on the dropout. 
                                                     The other two options are, uploading dataset from Microsoft Excel (xlxs) format or R (Rdata) format. 
                                                     If the user wants to analyze the by default dataset, he/she needs to click on the 'Data' button. 
                                                     In the case of Excel files, it is necessary to select firstly in the combo box the corresponding option (Source: Excel) and then the file, the position of the sheet, 
                                                     the start row number and the missing characters. Then the user should click on the button to see the table in the result window. In the case of RData file, 
                                                     it is necessary to select firstly in the combo box the corresponding option (Source: RData) and then select the file.")),
                                               box(width = 12, h2("2-Define the settings"),
                                                   p("In this step, the user should select, in the combo box 'Summary statistics' and in the panel, the forcing variable (S), the threshold ($s_0$),  
                                                     and the variables for which the user wants to derive summary statistics for the sample classified by treatment assignment status. 
                                                     There is another option to select the value of the treatment status depending on if the treatment group is below or above the threshold value. 
                                                     A table with the means of the selected variables and general properties will be showed. ")),
                                               box(width = 12, h2("3-Bandwidth selection"),
                                                   p("In this step, the user will select the bandwidths defining suitable subpopulations around the threshold. 
                                                     With the chosen values in the selector bar, the dataset is updated dynamically.
                                                     Then the user can assess the quality of the selection by looking at the results shown in two tables. 
                                                     The first one shows the number of selected records, the lowest and highest values, the forcing variable for the selected bandwidth and the difference between them. 
                                                     The second table shows the mean difference of each variable between treatment groups and the corresponding p-value derived using randomization-based tests adjusted for multiplicities. 
                                                     In the bottom part, the user can evaluate the distribution of each variable separately. ")),
                                               box(width = 12, h2("4-Summary Bandwidth selection"),
                                                   p("In the Summary Bandwidth selection randomization-based tests is implemented with adjustment for multiplicities for assessing balance in the background variables between treatment groups. 
                                                     The user should select different buffers, number of interactions, confidence interval and the variables to be evaluated. ")),
                                               box(width = 12, h2("5-Inference on causal effects"),
                                                   p("In the tab called \textit{Inference on causal effect}, the user can carry out different analysis based on the selected bandwidths. 
                                                     First of all, the user should choose in the \textit{5-Inference on causal effect} menu, the method. 
                                                     In this menu, there are common settings for all the methods, and in the next submenus, there are specific settings for each method. 
                                                     In all the cases the user should select the method, the bandwidths, if the outcome is binary or continuous, choose the outcome and the number of iterations.
                                                     "))
                                               
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
                                        box(width = NULL, h1("Plot"),
                                            status = "primary", 
                                            solidHeader = TRUE,
                                            collapsible = TRUE,
                                            # div(style = 'overflow-x: scroll', rHandsontableOutput('table'))
                                            
                                            h4("Part A - Histogram"),
                                            plotOutput("histogram")
                                        )),
                                      fluidRow(
                                        box(width = NULL, h1("Summary and p-values"),
                                            status = "primary", 
                                            solidHeader = TRUE,
                                            collapsible = TRUE,
                                            # div(style = 'overflow-x: scroll', rHandsontableOutput('table'))
                                            # h4("Part A - Summary"),
                                            tableOutput("summarydim"),
                                            tableOutput("summary")
                                        )),
                                      fluidRow(
                                        box(width = 12,  h1("Barplot and Density plot"),
                                            # h4("Part B - Distribution plot"),
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
                                            
                                            # h4("Summary"),
                                            dataTableOutput("tablemethod")
                                        ),
                                        box(width = 6,
                                            status = "primary", 
                                            solidHeader = TRUE,
                                            collapsible = TRUE,
                                            # h4("Plots"),
                                            plotOutput("plotres"))
                                      )
                             )   # tabBox
                                               )
                                               )
                                                   )
