
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Regression discontinuity"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "Import data"',
        # radioButtons('Mode', 'Select kind of subset:',
        #              c("All Data -> Readable formats:Json, csv, xml, xls(x)"='AllData',
        #                "Sample of dataset (only with csv)"='SmallData'
        #              )),
        # 
        fileInput('file_Db', 'Choose file to upload',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )
        ),
        # p('In case of CSV please make a selection'),
        # 
        # textInput('Samplesize',"Sample size  (if you want to load all data write ALL)",5),
        # 
        # tags$hr(),
        # radioButtons('sampleFL', 'Select kind of subset:',
        #              c(First='First',
        #                Sample='Sample'
        #                # Last='Last'
        #              )),
        
        radioButtons('sep2', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        checkboxInput('header2', 'Header', TRUE),
        radioButtons('quote2', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        tags$hr()
        # selectInput('in3', 'Options', zutabe(), selectize=TRUE,multiple = TRUE),
        # verbatimTextOutput('out3')
        # uiOutput("choose_columns")
      ),
      
      # conditionalPanel(
      #   'input.dataset === "Summary Statistics"',
      #   # uiOutput("col_sel_outcome2"),
      #   uiOutput("col_sel_covar3")
      # ),
      
      
      conditionalPanel(
        'input.dataset === "Summary of variables"',
        uiOutput("col_sel_forcing7"),
        numericInput('cutvalue2', 'Select cut value', 10),
        radioButtons('onezero2', 'Select your case:',
                     c("Z = 1 if S >= s0 and Z = 0 if S < s0" = 'under0',
                       "Z = 1 if S <= s0 and Z = 0 if S > s0" = 'under1'),"under0" ),
        
        uiOutput("col_sel_covar2")
      ),
      
      conditionalPanel(
        'input.dataset === "Bandwidth selection"',
          h2("Steps"),
          p("1) Select % of the range of bandwidth"),
          p("2) Select cut value and number of iterations"),
          p("2) Select forcing variable"),
          p("3) Select covariates"),
          p("4) Run Part A"),
          p("5) To check distribution of each variable select a variable and Run Part B"),
          br(),
          
         
          radioButtons('typerange', 'How do you want to control the range?',
                     c("Percentage" = 'perce',
                       "Balanced units" = 'balunit',
                       "Unbalanced units" = 'unbalunit'),"perce" ),
          numericInput('stepunit', 'Select the minimum unit', 1, width = "25%"),
          uiOutput("ui_slide_range"),
          # sliderInput("bins",
          #           paste0("Select the range %"),
          #           min = 0,
          #           max = 100,
          #           step = 1,
          #           value = 50),
          # numericInput('cutvalue', 'Select cut value', 15000),
          numericInput('nsim', 'Number of iterations', 50),
          # uiOutput("col_sel_forcing7"),
          uiOutput("col_sel_covar"),
          actionButton("go_bwsel", "Run part A"),downloadButton('downloadPlot', label = "Download Histogram"),downloadButton("downloadData", "Download Filtered Data"),
        
        br(),
        br(),
          uiOutput("col_sel_covar7"),
          radioButtons('intchar', 'Binary or continuous?',
                     c("Binary" = 'binary',
                       "Continuous" = 'conti'),"binary" ),
          actionButton("go2", "Run part B")
      ),
      
      conditionalPanel(
        'input.dataset === "Summary bandwidth selection"',
        h2("Steps"),
        p("1) Select bandwidths separated by comma"),
        p("2) Select number of iterations"),
        p("2) In the case of fuzzy FEP select number of iterations"),
        p("3) Write the confident interval and select type of outcome"),
        p("4) Select method, forcing variable, outcome, treatment status and covariates"),
        p("5) To check distribution of each variable select a variable and Run Part B"),
        br(),
        textInput("bandwidths9", "Include the bandwidths", "500,1000,1500"),
        numericInput('niter9',"Number of iterations",1000),
        textInput("cin9", "Write the Confidence Interval", 95),
        # radioButtons('typemod9', 'Select type of outcome:',
        #              c('Binary'='binary',
        #                'Continuous'='cont'), "binary"),
        # radioButtons('onezero9', 'Select your case:',
        #              c("Z = 1 if S >= s0 and Z = 0 if S < s0" = 'under0',
        #                "Z = 1 if S <= s0 and Z = 0 if S > s0" = 'under1'),"under0" ),
        # uiOutput("col_sel_covar_S9"),
        # uiOutput("col_sel_covar_Y9"),
        # 
        # uiOutput("col_sel_covar_W9"),
        uiOutput("col_sel_covar_covar9"),
        actionButton("go9", "Run Analysis")
      ),
      
      conditionalPanel(
        'input.dataset === "Bandwidth analyses"',
        h2("Steps"),
        p("1) Select bandwidths separated by comma"),
        p("2) Select number of iterations"),
        p("2) In the case of fuzzy FEP select number of iterations"),
        p("3) Write the confident interval and select type of outcome"),
        p("4) Select method, forcing variable, outcome, treatment status and covariates"),
        p("5) To check distribution of each variable select a variable and Run Part B"),
        br(),
        textInput("bandwidths", "Include the bandwidths", "500,1000,1500"),
        numericInput('niter',"Number of iterations",1000),
        textInput('M2',"Number of iterations for fuzzy FEP",5),
        textInput("cin", "Write the Confidence Interval", 95),
        radioButtons('typemod', 'Select type of outcome:',
                     c('Binary'='binary',
                       'Continuous'='cont'), "binary"),
        uiOutput("col_sel_covar_Y"),
        radioButtons('sel_method', 'Select the method:',
                     # 
                     c('Sharp RDD: FEP approach'='FEPRDis',
                       'Sharp RDD: Neyman approach' = 'sharpNeyman',
                       'Fuzzy RDD: FEP approach' = 'fuzzyFEP',
                       'Fuzzy RDD: Neyman approach' = 'fuzzyNeyman'),'FEPRDis'),
        
        radioButtons('one2side', 'Select type of sided:',
                     c('One sided'='onesided',
                       'Two sided'='twosided'), "onesided"),
        
        # actionButton("goplot2", "Run Plot"),
        # uiOutput("col_sel_covar_Z"),
        # radioButtons('onezero', 'Select your case:',
        #              c("Z = 1 if S >= s0 and Z = 0 if S < s0" = 'under0',
        #                "Z = 1 if S <= s0 and Z = 0 if S > s0" = 'under1'),"under0" ),
        # uiOutput("col_sel_covar_S"),
        
        
        uiOutput("col_sel_covar_W"),
        uiOutput("col_sel_covar_covar"),
        helpText("Note: Fuzzy FEP take very long time. Be patient"),
        actionButton("go", "Run Analysis")
      )
      

      # conditionalPanel(
      #   'input.dataset === "Regression Discontinuity"',
      #   sliderInput("bins",
      #               "Select the range:",
      #               min = 1,
      #               max = 100,
      #               value = 100),
      #   radioButtons('Method', 'Select the method:',
      #                c("Sharp Fisher"='SharpFisher',
      #                  "Sharp Neyman"='SharpNeyman',
      #                  "Sharp Bayesian"='SharpBAyes',
      #                  "Fuzzy Fisher"='FuzzyFisher',
      #                  "Fuzzy Neyman"='FuzzyNeyman',
      #                  "Fuzzy Bayesian"='FuzzyBayesian'
      #                ))
      #   # numericInput('cutvalue', 'Select cut value', 15000),
      #   # uiOutput("col_sel_outcome"),
      #   # uiOutput("col_sel_contreat"),
      #   # uiOutput("col_sel_covar")
      # ),
      
      # conditionalPanel(
      #   'input.dataset === "Plot"',
      #   textInput("bins", "Write the best bandwidth", 500),
      #   uiOutput("col_sel_plot"),
      #   radioButtons('type_plot', 'Select kind of plot:',
      #                c("Only points"='OnlyPoint',
      #                  "Points with lines"='PointLines',
      #                  "Straight intervals"= "LinesStr",
      #                  "Smooth intervals" = "LinesSmooth"
      #                  # Last='Last'
      #                ))
      # )
     
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('Import data', 
                 dataTableOutput("MySampleTable") 
        ),
        
        # tabPanel('Summary Statistics', 
        #          h4("Summary"),
        #          tableOutput("descriptive2")
        #          
        # ),
        
        tabPanel('Summary of variables', 
                 h4("Summary of variables"),
                 tableOutput("descriptive1")

        ),
        
        tabPanel('Bandwidth selection', 
                          h4("Part A - Histogram"),
                          plotOutput("histogram"),
                          h4("Part A - Summary"),
                          tableOutput("summarydim"),
                          tableOutput("summary"),
                 
                 h4("Part B - Distribution plot"),
                 plotOutput("plotprop")
        ),
        
        tabPanel('Summary bandwidth selection', 
                 h4("Summary"),
                 dataTableOutput("tablemethod9")
                 # plotOutput("plotres")
        ),
        
        tabPanel('Bandwidth analyses', 
                 h4("Summary"),
                 dataTableOutput("tablemethod"),
                 h4("Plots"),
                 plotOutput("plotres")
        )
        
        # tabPanel('Regression Discontinuity', 
        #          # verbatimTextOutput('out3'),
        #          dataTableOutput("data_class"),
        #          # plotOutput("distPlot"),
        #          plotOutput("histogram"),
        #          h4("Summary"),
        #          tableOutput("summary")
        #          # h4("froga"),
        #          # verbatimTextOutput("contreat")
        # ),
        
        # tabPanel('Plot', 
        #          # plotOutput("histogram"),
        #          plotOutput("Plot_lines")
        #          
        # )
      
    )
    )
  )
))
