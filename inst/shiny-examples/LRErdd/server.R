
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(cowplot)
library(dplyr)
library(kableExtra)
library(LRErdd)

# source("RegDis_function.r")
# source("Functions.r")
# source("03_LR_OneSidedFRD_FEP_Functions.R")

shinyServer(function(input, output) {

  # output$distPlot <- renderPlot({
  # 
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # 
  # })
  

  

  
  inFile_ds <- reactive({
    
    inFile2 <- input$file_Db

    print(inFile2$datapath)

  if (is.null(inFile2))
      {
        print("Nulo")
        fitx <- load ("2018_fevdat.RData")
        # fitx <- load ("Grants.RData")
        
        data.frame(get(fitx))
        # return(NULL)
      } else
      {
        form<-which.format(inFile2)
        form <- tolower(form[1])
        if(form=="csv")
        {
          read.csv(inFile2$datapath, header = input$header2,sep = input$sep2, quote = input$quote2)
        } else if(form=="json")
        {

          json_file <- inFile2$datapath
          json_data <- jsonlite::fromJSON(json_file)
          print(json_data)

        } else if(form=="xml")
        {
          books <- inFile2$datapath
          ldply(xmlToList(books), function(x) { data.frame(x[!names(x)=="author"]) } )
        } else if(form=="xls")
        {

          read.xls (inFile2$datapath, sheet = 1, header = TRUE)
        } else if(form=="xlsx")
        {
          read.xls (inFile2$datapath, sheet = 1, header = TRUE)
        } else if(form=="rdata")
        {
          fitx <- load (inFile2$datapath)
          data.frame(get(fitx))

        }



      }

  })
  
  output$MySampleTable = renderDataTable({ 
    d<-inFile_ds()
    d2 <- LRErdd::all_binom2num(d)
    as.data.frame(d)
    
  }) 
  

  ############################### summary of variables
  
  
  output$descriptive1 <- function(){
    
    
    data3 <- inFile_ds()
    
    forcingvar2 <- data3[,input$var_forcing7]
    
    s0<-input$cutvalue2
    
    whichunder2 <- ifelse(input$onezero2=="under1",1,0)
    
    if(whichunder2==1)
    {
      Z <- ifelse(forcingvar2 <= s0, 1, 0)
      # zz <- 1
    } else  if(whichunder2==0)
    {
      Z <- ifelse(forcingvar2 <= s0, 0, 1)
      # zz <- 0
    }
    
    # data.frame(Z)
    # bek <- as.numeric(ifelse(forcingvar2 >= s0,0,1))
    # Z <- bek  #Eligibility Status: Z=1{S<=s0}
    S <- forcingvar2  #Forcing variable
    X <- data3[,which(names(data3)%in%input$var_covar2)]
    
    print(head(X))
    
    whichupper <- ifelse(whichunder2 == 1, 0, 1)
    
    MEAN<-
      cbind(apply(X,2, mean),
            apply(X[Z==0,],2, mean),apply(X[Z==1,],2, mean))
    
    MEAN <- data.frame(names(X),MEAN)
    rownames(MEAN) <- NULL
    # MEAN$Variable <- as.character(input$var_covar2)
    colnames(MEAN)<- c("Name","All", "Z=0", "Z=1")
    
    print(c("N",as.numeric(nrow(X)),as.numeric(nrow(X[Z==0,])),as.numeric(nrow(X[Z==1,]))))
    # rownames(MEAN) <- names(X)
    MEAN[,2:4] <- round(MEAN[,2:4],2)
    MEAN$Name <- as.character(MEAN$Name)
    mmrow <- nrow(MEAN)+1
    MEAN[mmrow,1] <- "N"
    MEAN[mmrow,2] <- round(as.numeric(nrow(X)),0)
    MEAN[mmrow,3] <- round(as.numeric(nrow(X[Z==0,])),0)
    MEAN[mmrow,4] <- round(as.numeric(nrow(X[Z==1,])),0)
    
    MEAN %>%
      # mutate(MEAN = rownames(.)) %>%
      # # select(MEAN, everything()) %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F) 
    
    
    # %>%
    #   add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6))
    
  }
  
  output$col_sel_outcome <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_outcome", "Choose assignment status indicator", 
                choices  = cn,
                selected = cn,
                size=4,
                multiple=FALSE, selectize=FALSE)
  })
  
  output$ui_slide_range <- renderUI({
    
    data3 <- inFile_ds()
    # print(names(data3))
    # print(input$var_forcing7)
    # print(data3[,input$var_forcing7])
    forcingvar2 <- as.numeric(as.character(data3[,input$var_forcing7]))
    # print(forcingvar2)
    # print(head(forcingvar2))
    fmin <- min(forcingvar2,na.rm = TRUE)
    print((fmin))
    fmax <- max(forcingvar2,na.rm = TRUE)
    print((fmax))
    fmean <- mean(forcingvar2,na.rm = TRUE)
    print((fmean))
    s0<-as.numeric(input$cutvalue2)
    print((s0))

    fmins0 <- s0 - fmin
    print((fmins0))
    fmaxs0 <- fmax - s0
    print((fmaxs0))

    

    if(input$typerange == "perce")
    {
      perunit <- "perce"
      rangevalue = 100
      epsilon = 100
    } else {
      perunit <- "unit"

      epsilon <- ifelse(fmaxs0 >= fmins0, fmaxs0, fmins0)
      if(input$typerange == "balunit")
      {
        rangevalue = 0
      } else if(input$typerange == "unbalunit")
      {
        rangevalue = c(0,0)
      }
    }

    sliderInput("bins",
                paste0("Select the range ", perunit),
                min = 0,
                max = epsilon,
                step = input$stepunit,
                value = rangevalue)
    
    # sliderInput("bins",
    #             paste0("Select the range %"),
    #             min = 0,
    #             max = 100,
    #             step = 1,
    #             value = 50)
  })
  
  
  # output$col_sel_outcome2 <- renderUI({
  #   cn <- colnames(inFile_ds())
  #   selectInput("var_forcing7", "Choose forcing variable", 
  #               choices  = cn,
  #               selected = 1,
  #               multiple=FALSE, selectize=TRUE)
  # })
  
  output$col_sel_forcing7 <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_forcing7", "Choose forcing variable", 
                choices  = cn,
                selected = 1,
                multiple=FALSE, selectize=TRUE)
  })
  
  # output$col_sel_outcome_azkena <- renderUI({
  #   cn <- colnames(inFile_ds())
  #   selectInput("var_outcome_azkena", "Choose  the variable to analyze", 
  #               choices  = cn,
  #               selected = cn,
  #               size=4,
  #               multiple=FALSE, selectize=FALSE)
  # })
  
  # Choose Control Treatment variable
  output$col_sel_contreat <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_contreat", "", 
                choices  = cn,
                selected = cn,
                size=4,
                multiple=FALSE, selectize=FALSE)
  })
  
  output$col_sel_covar <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covar", "Choose covariates (continuous or bivariate)", 
                choices  = cn,
                selected = cn[4],
                size=10,
                multiple=TRUE, selectize=FALSE)
  })
  
  output$col_sel_covar7 <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covar7", "Choose a covariate to check the distribution", 
                choices  = cn,
                selected = cn[4],
                size=10,
                multiple=FALSE, selectize=FALSE)
  })
  
  output$col_sel_covar2 <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covar2", "Choose variables (select 2 or more variables)", 
                choices  = cn,
                selected = cn,
                size=10,
                multiple=TRUE, selectize=FALSE)
  })
  
  # output$col_sel_covar_Z <- renderUI({
  #   cn <- colnames(inFile_ds())
  #   selectInput("var_covarZ", "Choose  assignment status indicator (binary)", 
  #               choices  = cn,
  #               selected = 1,
  #               size=5,
  #               multiple=TRUE, selectize=FALSE)
  # })
  # 
  # output$col_sel_covar_S <- renderUI({
  #   cn <- colnames(inFile_ds())
  #   selectInput("var_covarS", "Choose  forcing variable", 
  #               choices  = cn,
  #               selected = cn[1],
  #               multiple=FALSE, selectize=TRUE)
  # })
  
  output$col_sel_covar_W <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covarW", "Choose  treatment status (W) variable", 
                choices  = cn,
                selected = cn[2],
                multiple=FALSE, selectize=TRUE)
  })
  
  
  output$col_sel_covar_Y <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covarY", "Choose outcome variable", 
                choices  = cn,
                selected = cn[3],
                multiple=FALSE, selectize=TRUE)
  })
  
  output$col_sel_covar_covar<- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covarCovar", "Choose the covariates", 
                choices  = cn,
                selected = cn[1:2],
                size=10,
                multiple=TRUE, selectize=FALSE)
  })
  
  # output$col_sel_covar_S9 <- renderUI({
  #   cn <- colnames(inFile_ds())
  #   selectInput("var_covarS9", "Choose  forcing variable", 
  #               choices  = cn,
  #               selected = cn[1],
  #               multiple=FALSE, selectize=TRUE)
  # })
  
  output$col_sel_covar_W9 <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covarW9", "Choose  treatment status (W) variable", 
                choices  = cn,
                selected = cn[2],
                multiple=FALSE, selectize=TRUE)
  })
  
  
  output$col_sel_covar_Y9 <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covarY9", "Choose outcome variable", 
                choices  = cn,
                selected = cn[3],
                multiple=FALSE, selectize=TRUE)
  })
  
  output$col_sel_covar_covar9<- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covarCovar9", "Choose the covariates", 
                choices  = cn,
                selected = cn[1:2],
                size=10,
                multiple=TRUE, selectize=FALSE)
  })
  
  output$col_sel_covar3 <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covar3", "Choose covariates", 
                choices  = cn,
                selected = cn,
                size=10,
                multiple=TRUE, selectize=FALSE)
  })
  
  output$col_sel_plot <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_plot", "Choose a covariate", 
                choices  = cn,
                selected = cn,
                size=10,
                multiple=FALSE, selectize=FALSE)
  })
  
  
  

  listvalues <- reactive({
    data2 <- inFile_ds()
    
    # print(head(data2))

    forcvar <- data2[,input$var_forcing7]
    cut <- input$cutvalue2
    
    r.min <- min(forcvar)
    r.max <- max(forcvar)
    r2a <- cut - r.min
    r2b <- r.max -cut 
    
    if(input$typerange == "perce")
    {
      
      if(r2a >= r2b) 
      {
        rang <- r2a 
      } else {
        rang <- r2b
      }
      
      ver1 <- cut - rang * input$bins/100
      ver2 <- cut + rang * input$bins/100
    } else     {
      if(input$typerange == "balunit")
      {
        ver1 <- cut - input$bins
        ver2 <- cut  + input$bins
      } else if(input$typerange == "unbalunit")
      {
        ver1 <- cut - input$bins[1]
        ver2 <- cut  + input$bins[2]
      }

    }
    
   
    # 
    # print(head(data2))
    # print(ver1)
    # print(ver2)
    
    lista <- list(data2, ver1, ver2)
  })
  
  v2 <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go_bwsel, {
    v2$doPlot <- input$go_bwsel
  })
  
  plotInputhist = function() {
    listb <- listvalues()
    data2 <- data.frame(listb[[1]])
    ver1 <- listb[[2]]
    ver2 <- listb[[3]]
    
    # print(input$var_forcing7)
    # # print(head(data2))
    print(ver1)
    print(ver2)
    
    forcingvalues <- as.vector(data2[,input$var_forcing7])
    print(head(forcingvalues))
    # names(forcingvalues) <- "forcingvalues"
    
    # print(summary(forcingvalues))
    # print(head(data2))
    
    # print(summary(forcingvalues))
    # ggplot(data2, aes(x=forcingvalues))+
    #   geom_density(color="darkblue", fill="lightblue",alpha=0.5)+
    #   geom_vline(xintercept = c(ver1,ver2),colour = "red") +
    #   geom_vline(xintercept = c(input$cutvalue2),colour = "orange") + labs(x = input$var_forcing7)
    
    ggplot(data2, aes(x=forcingvalues)) + 
      geom_histogram(aes(y=..density..), colour="black", fill="deepskyblue3")+
      geom_density(alpha=.6, fill="grey") +
      geom_vline(xintercept = c(ver1,ver2),colour = "red",linetype="dashed",size = 1) +
      geom_vline(xintercept = c(input$cutvalue2),colour = "black",linetype="dashed", size = 1) + 
      labs(x = input$var_forcing7)
  }
  
  output$histogram <- renderPlot({
    
    if (v2$doPlot == FALSE) return()
    
    isolate({
      plotInputhist()
    
    })
  })
  
  filtereddataset <- reactive({
    listb <- listvalues()
    data2 <- data.frame(listb[[1]])
    ver1 <- listb[[2]]
    ver2 <- listb[[3]]
    
    forcingvar <- as.vector(data2[,input$var_forcing7])
    data3 <- data2[forcingvar >= ver1 & forcingvar <= ver2, ]
    data3
  }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dataset", ".csv", sep = "")
    },
    content = function(file) {
      # write.csv(filtereddataset(), file, row.names = FALSE)
      save(filtereddataset(), file)
    }
  )
  
  
  output$downloadPlot <- downloadHandler(
    # filename = function() { paste("Histogram.jpg", sep='') },
    # # filename = "froga.jpg",
    # content = function(filename) {
    #   ggsave(filename, plot = plotInputhist(), device = "jpg")
    # }
    filename = function() { paste("Froga", '.jpg', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "jpg")
    }
  )
  
  output$contreat <- reactive({ 
    
    listb <- listvalues()
    data2 <- listb[[1]]
    ver1 <- listb[[2]]
    ver2 <- listb[[3]]
    forcingvalues <- data2[,input$var_forcing7]
    summary(forcingvalues)
    class(forcingvalues)
    # outcome <- data2[,input$var_outcome]
    d2 <- ifelse(forcingvalues <= input$cutvalue2,0,1)
    d2
    
    # summary(d2)
    # print(head(d2))
    # # as.data.frame(d2)
    # # outcome
    # class(d2)
    
  }) 
  
  v3 <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go, {
    v3$doPlot <- input$go
  })

  taula <- reactive({
    
    if (v3$doPlot == FALSE) return()
    
    isolate({
    
    data2 <- data.frame(inFile_ds())
    bw3 <-  as.numeric(as.character(unlist(strsplit(input$bandwidths, ","))))
    whichunder2 <- ifelse(input$onezero2=="under1",1,0)
    
    print(bw3)
    print(class(data2))
    print(input$onezero2)
    print(input$cutvalue2)
    print(whichunder2)
    
    print(head(data2))
    print(input$var_forcing7)
    print(input$var_covarY)
    print(input$niter)
    
    withProgress(message = 'Making analyses', value = 0, {

      if(input$sel_method == 'FEPRDis')
      {
        taula <- LRErdd::fep_values (dataset = data2, forcing_var_name =input$var_forcing7, Y_name =input$var_covarY, niter=input$niter,
                             bandwidth = bw3, cut_value = input$cutvalue2, whichunder = whichunder2)
      }
      
      if(input$sel_method == 'sharpNeyman')
      {
        taula <- LRErdd::sharp_neyman_bw (dataset = data2,forcing_var_name =input$var_forcing7,Y_name =input$var_covarY,niter=input$niter,
                                  bandwidth = bw3, cut_value = input$cutvalue2, whichunder = whichunder2, cin = as.numeric(as.character(input$cin)))
      }
      
      if(input$sel_method == 'fuzzyNeyman')
      {
        taula <- LRErdd::fuzzy_neyman_bw (dataset = data2,forcing_var_name =input$var_forcing7,Y_name =input$var_covarY,niter=input$niter,
                                  W = input$var_covarW, bandwidth = bw3, cut_value = input$cutvalue2, 
                                  whichunder = whichunder2, cin =as.numeric(as.character(input$cin)))
      }
      
      if(input$sel_method == 'fuzzyFEP')
      {
        
        incProgress(1/2, detail = "Be patient")
        
        if(input$typemod=="binary")
        {
          if(input$one2side=="onesided")
          {
            
            s0 <- input$cutvalue2
            if (whichunder2 == 1) {
              assigVar2 <- ifelse(data2[, input$var_forcing7] <= s0, 1, 0)
            } else if (whichunder2 == 0) {
              assigVar2 <- ifelse(data2[, input$var_forcing7] <= s0, 0, 1)
            }
            
            dat2b <- data2[,input$var_covarW]
            data2[,input$var_covarW] <- ifelse(assigVar2 == 0, 0, data2[,input$var_covarW])
              
              
            taula <- LRErdd::fuzzy_fep_bw (dataset = data2,forcing_var_name =input$var_forcing7,Y_name =input$var_covarY,niter=input$niter,
                                           W = input$var_covarW,typemod = "binary",typesided = "onesided",
                                           bandwidth = bw3, cut_value = input$cutvalue2, M2 = input$M2, whichunder = whichunder2)
            
          } else if(input$one2side=="twosided")
          {
            taula <- LRErdd::fuzzy_fep_bw (dataset = data2,forcing_var_name =input$var_forcing7,Y_name =input$var_covarY,niter=input$niter,
                                           W = input$var_covarW,typemod = "binary",typesided = "twosided",
                                           bandwidth = bw3, cut_value = input$cutvalue2, M2 = input$M2, whichunder = whichunder2)
          }

          
        } else if(input$typemod=="cont")
        {
          
          if(input$one2side=="onesided")
          {
            
            s0 <- input$cutvalue2
            if (whichunder2 == 1) {
              assigVar2 <- ifelse(data2[, input$var_forcing7] <= s0, 1, 0)
            } else if (whichunder2 == 0) {
              assigVar2 <- ifelse(data2[, input$var_forcing7] <= s0, 0, 1)
            }
            
            dat2b <- data2[,input$var_covarW]
            data2[,input$var_covarW] <- ifelse(assigVar2 == 0, 0, data2[,input$var_covarW])
            
            taula <- LRErdd::fuzzy_fep_bw (dataset = data2,forcing_var_name =input$var_forcing7,Y_name =input$var_covarY,niter=input$niter,
                                           W = input$var_covarW,typemod = "numeric",typesided = "onesided",
                                           bandwidth = bw3, cut_value = input$cutvalue2, M2 = input$M2, whichunder = whichunder2)
            
          } else if(input$one2side=="twosided")
          {
            taula <- LRErdd::fuzzy_fep_bw (dataset = data2,forcing_var_name =input$var_forcing7,Y_name =input$var_covarY,niter=input$niter,
                                           W = input$var_covarW,typemod = "numeric",typesided = "twosided",
                                           bandwidth = bw3, cut_value = input$cutvalue2, M2 = input$M2, whichunder = whichunder2)
          }
        }
        taula <- taula[[1]]
      }
    })
    })
    print(taula)
    
  })
  
  v9 <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go9, {
    v9$doPlot <- input$go9
  })
  
  taula9 <- reactive({
    
    if (v9$doPlot == FALSE) return()
    
    isolate({
      
      data2 <- inFile_ds()
      bw3 <-  as.numeric(as.character(unlist(strsplit(input$bandwidths9, ","))))
      whichunder2 <- ifelse(input$onezero2=="under1",1,0)
      
      withProgress(message = 'Making analyses', value = 0, {
        
          taula <- LRErdd::sharp_FEP_adj_bw (dataset = data2, forcing_var_name =input$var_forcing7, covariates = input$var_covarCovar9,
                                     niter=input$niter9, bandwidth = bw3, cut_value = input$cutvalue2, whichunder = whichunder2)
      })
    })
    print(taula)
    
  })
  
  
  # methodVals <- eventReactive(input$go, {
  #   
  #   taula()
  # })
  
  vplot <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go, {
    vplot$doPlot <- input$go
  })


  
  output$plotres <- renderPlot({
    
    if (vplot$doPlot == FALSE) return()
    
    isolate({
    
    tt <- taula()
    
    print(head(tt))
    
    if(input$sel_method%in%c("sharpNeyman","fuzzyNeyman"))
    {
      names(tt)[c(3,5,6)] <- c("ACE","L","U")
      
      if(input$sel_method%in%c("sharpNeyman"))
      {
          ggplot(tt, aes(x = Bandwith, y = ACE)) +
            geom_point(size = 4) +
            geom_errorbar(aes(ymax = U, ymin = L))+ geom_hline(yintercept = 0,col= "red")
        
      } else if(input$sel_method%in%c("fuzzyNeyman"))
      {
        ggplot(tt, aes(x = Bandwith, y = ACE)) +
          geom_point(size = 4) +
          geom_errorbar(aes(ymax = U, ymin = L)) + facet_grid(.~Estimand)+ geom_hline(yintercept = 0,col= "red")
      }
      
    } else  if(input$sel_method%in%c("FEPRDis","fuzzyFEP"))
    {
      
      if(input$sel_method%in%c("FEPRDis"))
      {
        names(tt)[c(1,3,5)] <- c("Bandwith","Dif","Pvalue")
        kol <- ifelse(tt[,"Pvalue"]<0.05,"blue","red")
        ggplot(tt, aes(x = Bandwith, y = Dif)) +
          geom_point(size = 4,col=kol) +
          geom_hline(yintercept = 0,col= "red")
        
      } else if(input$sel_method%in%c("fuzzyNeyman"))
      {
        
        data2 <- inFile_ds()
        bw3 <-  as.numeric(as.character(unlist(strsplit(input$bandwidths, ","))))
        whichunder2 <- ifelse(input$onezero2=="under1",1,0)
        
        
        if(input$sel_method == 'fuzzyFEP')
        {
          
          if(input$typemod=="binary")
          {

            taula <- LRErdd::fuzzy_fep_bw (dataset = data2,forcing_var_name =input$var_forcing7,Y_name =input$var_covarY,niter=input$niter,
                                   W = input$var_covarW,typemod = "binary",
                                   bandwidth = bw3, cut_value = input$cutvalue2, M2 = input$M2, whichunder = whichunder2)
            
            taula
            # hist(taula[[2]])
            
          } else if(input$typemod=="cont")
          {
            
            taula <- LRErdd::fuzzy_fep_bw (dataset = data2,forcing_var_name =input$var_forcing7,Y_name =input$var_covarY,niter=input$niter,
                                   W = input$var_covarW,typemod = "numeric",
                                   bandwidth = bw3, cut_value = input$cutvalue2, M2 = input$M2, whichunder = whichunder2)
            # hist(taula[[2]])
            
          }
        }
      }
    }

    })
    
  })
  
  output$tablemethod <- renderDataTable({
      taula()
  })
  
  output$tablemethod9 <- renderDataTable({
      taula9()
  })
  
  output$summary <- renderTable({
    
    if (v2$doPlot == FALSE) return()
    
    isolate({
    listb <- listvalues()
    data2 <- data.frame(listb[[1]])
    ver1 <- listb[[2]]
    ver2 <- listb[[3]]
    
    forcingvar <- as.vector(data2[,input$var_forcing7])
    
    # filtering the table with new data
    
    data3 <- data2[forcingvar >= ver1 & forcingvar <= ver2, ]
    forcingvar2 <- data3[,input$var_forcing7]
    bek <- as.numeric(ifelse(forcingvar2 <= input$cutvalue2,0,1))
    # save.image("20171212_mit.RData")
    
    fmin <- min(forcingvar2,na.rm = TRUE)
    print((fmin))
    fmax <- max(forcingvar2,na.rm = TRUE)
    print((fmax))
    
    print(head(bek))
    
    lencov <- length(input$var_covar)
    print(lencov)
    # bek <- contreat()

    if(lencov == 1)
    {
      df <- data.frame(matrix(888,1,9))
      names(df) <- c("Length","lim1", "lim2", "Covariate","Difference","Adj pvalue","CovariateIni","DifferenceIni","Adj pvalueIni")

      get <- LRErdd::rd_pvalue (dataset = data3, covariate = input$var_covar, CT = bek,Nsim = input$nsim, cutvar = input$cutvalue2,
                        low_lim = ver1, high_lim = ver2)
      
      get2 <- LRErdd::rd_pvalue (dataset = data2, covariate = input$var_covar, CT = bek,Nsim = input$nsim, cutvar = input$cutvalue2,
                                low_lim = fmin, high_lim = fmax)

      print(get)
      
      df[1,1]<-get[[2]]
      df[1,2]<-ver1
      df[1,3]<-ver2
      df[1,4]<-get[[3]]
      df[1,5]<-get[[4]]
      df[1,6]<-get[[1]]
      
      df[1,7]<-get2[[3]]
      df[1,8]<-get2[[4]]
      df[1,9]<-get2[[1]]

    } else if(lencov > 1)
    {
      df <- data.frame(matrix(888,lencov,9))
      names(df) <- c("Length","lim1", "lim2", "Covariate","Difference","Adj pvalue","CovariateIni","DifferenceIni","Adj pvalueIni")

      covv <- input$var_covar

      for(g in 1:lencov)
      {
        get <- LRErdd::rd_pvalue (dataset = data3, covariate = covv[g], CT = bek,Nsim = input$nsim, cutvar = input$cutvalue2,
                          low_lim = ver1, high_lim = ver2)
        
        get2 <- LRErdd::rd_pvalue (dataset = data2, covariate = covv[g], CT = bek,Nsim = input$nsim, cutvar = input$cutvalue2,
                                   low_lim = fmin, high_lim = fmax)

        df[g,1]<-get[[2]]
        df[g,2]<-ver1
        df[g,3]<-ver2
        df[g,4]<-get[[3]]
        df[g,5]<-get[[4]]
        df[g,6]<-get[[1]]
        
        df[g,7]<-get2[[3]]
        df[g,8]<-get2[[4]]
        df[g,9]<-get2[[1]]
      }
    }

    df[,4:9]

    })

  })
  
  output$summarydim <- renderTable({
    
    if (v2$doPlot == FALSE) return()
    
    isolate({
    listb <- listvalues()
    data2 <- listb[[1]]
    ver1 <- listb[[2]]
    ver2 <- listb[[3]]
    
    forcingvar <- data2[,input$var_forcing7]
    
    # filtering the table with new data
    
    data3 <- data2[forcingvar >= ver1 & forcingvar <= ver2, ]
    forcingvar2 <- data3[,input$var_forcing7]
    
    df <- data.frame(nrow(data3),ver1,ver2,ver2-ver1)
    names(df) <- c("Number of records","Down limit","Up limit", "Difference")
    df
    })
    
  })
  
 

  output$Plot_lines <- renderPlot({
    
    data2 <- inFile_ds()
    
    h <- as.numeric(as.character(input$bins))

    ver1 <- input$cutvalue2 - h
    ver2 <- input$cutvalue2 + h
    
    forcingvar <- data2[,input$var_forcing7]
    data3 <- data2[forcingvar >= ver1 & forcingvar <= ver2, ]

    # forcingvare <- data2[,input$var_forcingvar] 
    # data3 <- data2[forcingvare >= ver1 & forcingvare <ver2,]
    plotvar <- as.numeric(as.character(data3[,input$var_plot]))
    forcingvar2 <- as.numeric(as.character(data3[,input$var_forcing7] ))
    data3[,"group"] <- ifelse(forcingvar2 <= input$cutvalue2, "Under", "Below")
    

    if(input$type_plot == "OnlyPoint")
    {
      ggplot(data3, aes(x=forcingvar2, y=plotvar, color=group, shape=group)) +
        geom_point() + labs(x = "S") + labs(y = "Outcome") 
      
    } else if(input$type_plot == "PointLines")
    {
      ggplot(data3, aes(x=forcingvar2, y=plotvar, color=group, shape=group)) +
        geom_point() + labs(x = "S") + labs(y = "Outcome") +  
        geom_smooth(method=lm, aes(fill=group),se =FALSE)
      
    } else if(input$type_plot == "LinesStr")
    {
      ggplot(data3, aes(x=forcingvar2, y=plotvar, color=group, shape=group)) +
        geom_point()  + labs(x = "S") + labs(y = "Outcome") +  
        geom_smooth(method=lm, aes(fill=group))
      
    } else if(input$type_plot == "LinesSmooth")
    {
      ggplot(data3, aes(x=forcingvar2, y=plotvar, color=group, shape=group)) +
        geom_point() + labs(x = "S") + labs(y = "Outcome") +
        geom_smooth(aes(fill=group))
    }

  })
  
  v <- reactiveValues(doPlot = FALSE)
  
  observeEvent(input$go2, {
    v$doPlot <- input$go2
  })
  
  observeEvent(input$"Bandwidth selection", {
    v$doPlot <- FALSE
  })  
  
  output$plotprop <- renderPlot({
    if (v$doPlot == FALSE) return()
    
    isolate({
          dataori <- inFile_ds()
          
          listb <- listvalues()
          data2 <- data.frame(listb[[1]])
          ver1 <- listb[[2]]
          ver2 <- listb[[3]]
          
          forcori <- dataori[,input$var_forcing7]
      
          # # filtering the table with new data
          forcingvar <- data2[,input$var_forcing7]
          data3 <- data2[forcingvar >= ver1 & forcingvar <= ver2, ]
          forc3 <- data3[,input$var_forcing7]
          
          print(paste0("data3dim - ",dim(data3)))
      
          Zori <- as.factor(ifelse(forcori <= input$cutvalue2, 1, 0))
          Z3 <- as.factor(ifelse(forc3 <= input$cutvalue2, 1, 0))
      
          covaname <- input$var_covar7
          
          print("dims")
          print(length(Zori))
          print(length(Z3))
      
          covaori <- data.frame(dataori[,covaname])
          cova3 <- data.frame(data3[,covaname])
          
          print("then")
          print(dim(covaori))
          print(dim(cova3))
          
          # print(head(cova3))
      
          # if(class(covaori)=="factor")
          # {
          #   covaori <- as.character(covaori)
          # }
          
          print("binary")
      
          if(input$intchar=="binary")
          {
            # factor or character
            
            print(length(Zori))
            print(dim(covaori))
            print(class(Zori))
            print(class(covaori))
            print(head(Zori))
            print(head(covaori))
            
            covaori <- as.vector(covaori[,1])
            
            print(table(Zori,covaori))
            
            d <- as.data.frame(prop.table(table(data.frame(Zori,covaori)),1))
            d$Dataset <- "Complete"
            names(d) <- c("Var1", "Var2",   "Freq",   "Dataset")
            
            print(head(Z3))
            print(head(cova3))
            
            cova3 <- as.vector(cova3[,1])
            
            print(table(Z3,cova3))
            
            d2 <- as.data.frame(prop.table(table(data.frame(Z3,cova3)),1))
            d2$Dataset <- "Balanced"
            names(d2) <- c("Var1", "Var2",   "Freq",   "Dataset")
            
            print(d)
            print(d2)
            
            d3 <- rbind(d,d2)
            d3 <- d3[order(d3$Var1),]
            
            d3$Dataset <- factor(d3$Dataset,levels = c("Complete","Balanced"))
            
            d3$Freq <- round(d3$Freq,2)
            
            print(d3)
            
            # for(h in seq(1,8,by=2)) d3$sum[c(h,h+1)] <- sum(d3$Freq[c(h,h+1)])
            # 
            # d3$Freq2 <- d3$Freq/d3$sum
            # names(d3)[c(2,6)] <- c("ControlTreatment","Proportion")
            
            
            gg <- ggplot(data=d3, aes(x=Var1, y=Freq, fill=Var2)) +
              geom_bar(stat="identity") +
              geom_text(aes(label=Freq), vjust=1.6, color="white",size=3.5)+
              scale_fill_brewer(palette="Paired") + theme(legend.position="none") + facet_grid(.~Dataset)
            

          } else if(input$intchar=="conti")
          {
            # numeric or integer or double
            datori2 <- data.frame(covaori,Zori)
            names(datori2) <- c("Independent", "Dependent")
            datori3 <- data.frame(cova3,Z3)
            names(datori3) <- c("Independent", "Dependent")
            
            print(head(datori3))
      
            a <- ggplot(datori2, aes(x=Independent, fill=Dependent)) +
              geom_density(alpha=0.4)
      
      
            b <- ggplot(datori3, aes(x=Independent, fill=Dependent)) +
              geom_density(alpha=0.4)
      
            gg <- plot_grid(a, b, labels=c("A", "B"), ncol = 2, nrow = 1)
          }
      
          print(gg)
      })
  })
  
  
})
