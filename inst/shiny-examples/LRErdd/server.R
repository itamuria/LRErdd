
## Step 2. Server
## input (widgets specified in "ui" also), output (results sent to "ui")
## session argument --> listening to what is happening on page
## basic listening functions: observe(), reactive(), render()
## Observe looks for updates in inputs
## ObserveEvent() specifies events that trigger reactions
## Render() displays outputs
## Reactive() needs user interaction, stored as objects

set.seed(27)

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 9*1024^2)
  
  # observeEvent(input$select_effect,{
  #   if(input$select_effect == "Sharp FEP"){
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "5a"))
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "5b"))
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "5c"))
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "5d"))
  #   } else if(input$select_effect == "Sharp Neyman"){
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "5a"))
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "5b"))
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "5c"))
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "5d"))
  #   } else if(input$select_effect == "Fuzzy FEP"){
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "5a"))
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "5b"))
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "5c"))
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "5d"))
  #   } else if(input$select_effect == "Fuzzy Neyman"){
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "5a"))
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "5b"))
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "5c"))
  #     session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "5d"))
  #   }
  #   
  # })
  

# 1-Load data -------------------------------------------------------------

  
  inFile_ds <- reactive({
    
    fitx <- source_data("https://github.com/itamuria/LRErdd_dataset/blob/master/Grants.RData?raw=true")
    fitx <- data.frame(get(fitx))
    
    file_to_read_excel <- NULL
    file_to_read_rdata <- NULL
    
    file_to_read_excel <- input$excelfile
    print(file_to_read_excel)
    if (!is.null(file_to_read_excel)) {
      print(extension(file_to_read_excel$name))
      if(extension(file_to_read_excel$name)%in%c(".xls",".xlsx"))
      {
        
        fitx <- NULL
        fitx <- read.xlsx(file_to_read_excel$datapath, sheet = input$excel_sheetnumber, 
                          startRow = input$excel_startrow, na.strings = input$excel_stringNA)
      }
    }
    
    file_to_read_rdata <- input$rdatafile
    print(file_to_read_rdata)
    if (!is.null(file_to_read_rdata)) {
      print(extension(file_to_read_rdata$name))
      if(extension(file_to_read_rdata$name)%in%c(".rdata",".Rdata",".RData"))
      {
        print(file_to_read_rdata$datapath)
        data <- load (file_to_read_rdata$datapath)
        fitx <- NULL
        fitx <- get(data)
      }
    }
    
    
    
    fitx <- all_binom2num(fitx)
    fitx
  })
  
  

# View data -------------------------------------------------------------

  
  output$dto <- renderDataTable(inFile_ds(), 
                                extensions = 'Buttons', 
                                options = list(dom = 'Bfrtip',buttons = c('copy', 'print'),pageLength = 10)
  )
  
  
# 2-Define settings for summary statistics --------------------------------

  
  
  # forcing variable
  output$col_sel_forcing7 <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_forcing7", "Choose forcing variable", 
                choices  = cn,
                selected = 1,
                multiple=FALSE, selectize=TRUE)
  })
  
  # covariates
  output$col_sel_covar2 <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covar2", "Choose variables (select 2 or more variables)", 
                choices  = cn,
                selected = 1,
                multiple=TRUE, selectize=TRUE)
  })
  
  
  
  # values <- reactiveValues()
  # 
  # output$table <- renderRHandsontable({
  # 
  #   DF <- values[["DF"]]
  #   
  #   print(DF[1:5,1:5])
  #   
  #   signsourc <- c("=","<","<=",">",">=","sel","desel")
  #   
  #   if (!is.null(DF))
  #     rhandsontable(DF, stretchH = "all",useTypes=TRUE) %>%
  #     hot_col(col = "Sign", type = "autocomplete", source = signsourc,strict = FALSE) %>%
  #     hot_col(col = "Value", type = "autocomplete", source = "",strict = FALSE)
  # })
  
  # ## Save
  # observeEvent(input$save, {
  #   finalDF <- isolate(values[["DF"]])
  #   outdir <- "D:\\Projects\\Causality\\Shiny001a_makingready"
  #   print(outdir)
  #   outfilename <- "aldatu"
  #   print(file.path(outdir, sprintf("%s.rdata", outfilename)))
  #   save(finalDF, file=file.path(outdir, sprintf("%s.RData", outfilename)))
  # }
  # )
  

#  descriptive -----------------------------------------------------------

  
  
  v7 <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go_bwsel2, {
    v7$doPlot <- input$go_bwsel2
  })
  
  output$descriptive1 <- renderTable({
    
    if (v7$doPlot == FALSE) return()
    
    isolate({
      descriptive1b()
      
    })
  })
  
  
  
  descriptive1b <- function(){
    
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
    
    data.frame(MEAN)
    
  }  
  

# 3-Bandwidth selection ---------------------------------------------------

  
  
  output$ui_slide_range <- renderUI({
    
    data3 <- inFile_ds()
    forcingvar2 <- as.numeric(as.character(data3[,input$var_forcing7]))
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
      perunit <- "%"
      rangevalue = 100
      epsilon = 100
    } else {
      perunit <- "S unit"
      
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
                paste0("Select the scale in ", perunit),
                min = 0,
                max = epsilon,
                step = input$stepunit,
                value = rangevalue)
    
  })
  
  
  
  output$col_sel_covar <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covar", "Choose covariates", 
                choices  = cn,
                selected = 1,
                multiple=TRUE, selectize=TRUE)
  })
  
  output$col_sel_covar7 <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covar7", "Choose a covariate to check the distribution", 
                choices  = cn,
                selected = 1,
                multiple=FALSE, selectize=TRUE)
  })

# listvalues --------------------------------------------------------------

  
  
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
        ver1 <- cut - input$bins/2
        ver2 <- cut  + input$bins/2
      } else if(input$typerange == "unbalunit")
      {
        ver1 <- cut - input$bins[1]/2
        ver2 <- cut  + input$bins[2]/2
      }
      
    }
    
    lista <- list(ver1, ver2)
  })
  
  v2 <- reactiveValues(doPlot = FALSE)
  
  observeEvent(input$go_bwsel, {
    v2$doPlot <- input$go_bwsel
  })
  
  
  plotInputhist = function() {
    listb <- listvalues()
    data2 <- inFile_ds()
    ver1 <- listb[[1]]
    ver2 <- listb[[2]]
    
    print(ver1)
    print(ver2)
    
    forcingvalues <- as.vector(data2[,input$var_forcing7])
    print(head(forcingvalues))
    
    data2 <- data.frame(forcingvalues, data2)
    names(data2)[1] <- "forcingvalues"
    
    # browser()
    
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
  
  output$summarydim <- renderTable({
    
    if (v2$doPlot == FALSE) return()
    
    isolate({
      listb <- listvalues()
      data2 <- inFile_ds()
      ver1 <- listb[[1]]
      ver2 <- listb[[2]]
      
      forcingvar <- data2[,input$var_forcing7]
      
      # filtering the table with new data
      
      data3 <- data2[forcingvar >= ver1 & forcingvar <= ver2, ]
      forcingvar2 <- data3[,input$var_forcing7]
      
      df <- data.frame(nrow(data3),ver1,ver2,ver2-ver1)
      names(df) <- c("Number of records","Down limit","Up limit", "Difference")
      df
    })
    
  })
  
  output$summary <- renderTable({
    
    if (v2$doPlot == FALSE) return()
    
    isolate({
      listb <- listvalues()
      data2 <- inFile_ds()
      ver1 <- listb[[1]]
      ver2 <- listb[[2]]
      
      print(paste0("ver1: ", ver1))
      print(paste0("ver2: ", ver2))
      print(paste0("data2: ", dim(data2)))
      
      forcingvar <- as.vector(data2[,input$var_forcing7])
      
      # filtering the table with new data
      
      data3 <- data2[forcingvar >= ver1 & forcingvar <= ver2, ]
      print(paste0("data3: ", dim(data3)))
      forcingvar2 <- data3[,input$var_forcing7]
      bek <- as.numeric(ifelse(forcingvar2 <= input$cutvalue2,0,1))
      # save.image("20171212_mit.RData")
      
      fmin <- min(forcingvar2,na.rm = TRUE)
      print(paste0("fmin: ", fmin))
      fmax <- max(forcingvar2,na.rm = TRUE)
      print(paste0("fmax: ", fmax))
      
      maxmax <- ifelse(fmin > fmax, fmin, fmax)
      bbw <- (ver2 - ver1)/2

      print("bek")
      print(head(bek))
      
      whichunder <- ifelse(input$onezero2=="under1",1,0)
      
      lencov <- length(input$var_covar)
      print(paste0("fmax: ", fmax))
      
      set.seed(27)
      
      if(lencov == 1)
      {
        df <- data.frame(matrix(888,1,5))
        names(df) <- c("Covariate","Initial p-value","Initial Adjusted p-value","Bandwidth Specific p-value","Bandwidth Specific Adjusted p-value")
        

        if(dim(data2)==dim(data3))
        {
          get <- LRErdd::rand_pajd (dataset = data3, forcing_var_name = input$var_forcing7, covariate = input$var_covar, niter = input$nsim,
                                    cut_value = input$cutvalue2, whichunder = whichunder, bandwidth = bbw)

          get2 <- get
        } else {
          get <- LRErdd::rand_pajd (dataset = data3, forcing_var_name = input$var_forcing7, covariate = input$var_covar, niter = input$nsim,
                                    cut_value = input$cutvalue2, whichunder = whichunder, bandwidth = bbw)

          get2 <- LRErdd::rand_pajd (dataset = data2, forcing_var_name = input$var_forcing7, covariate = input$var_covar, niter = input$nsim, 
                                     cut_value = input$cutvalue2, whichunder = whichunder, bandwidth = maxmax)
        }
        
        
        print(get)
        
        df[1,1]<-input$var_covar
        df[1,2]<-get2[,2]
        df[1,3]<-get2[,3]
        df[1,4]<-get[,2]
        df[1,5]<-get[,3]

        
      } else if(lencov > 1)
      {
        df <- data.frame(matrix(888,lencov,5))
        names(df) <- c("Covariate","Initial Pvalue","Initial Adj pvalue","Bandwidth specific Pvalue","Bandwidth specific Adj pvalue")
        
        covv <- input$var_covar
        
        print(dim(data3))
        print(input$var_forcing7)
        print(input$var_covar)
        print(input$cutvalue2)
        print(whichunder)
        print(bbw)
        print(covv)
        
        
        
        # for(g in 1:lencov)
        # {
        #  print(g) 
        if(dim(data2)==dim(data3))
        {
          get <- LRErdd::rand_pajd (dataset = data3, forcing_var_name = input$var_forcing7, covariate = covv, niter = input$nsim, 
                                    cut_value = input$cutvalue2, whichunder = whichunder, bandwidth = bbw)
          print(get)
          get2 <- get
          print(get2)
        } else {
          get <- LRErdd::rand_pajd (dataset = data3, forcing_var_name = input$var_forcing7, covariate = covv, niter = input$nsim, 
                                    cut_value = input$cutvalue2, whichunder = whichunder, bandwidth = bbw)
          print(get)
          get2 <- LRErdd::rand_pajd (dataset = data2, forcing_var_name = input$var_forcing7, covariate = covv, niter = input$nsim, 
                                     cut_value = input$cutvalue2, whichunder = whichunder, bandwidth = maxmax)
          print(get2)
        }
  
          
          # df[g,1]<-input$var_covar[g]
          # df[g,2]<-get2[,2]
          # df[g,3]<-get2[,3]
          # df[g,4]<-get[,2]
          # df[g,5]<-get[,3]
          
          df <- data.frame(get2,get)
          df <- df[,-4]
          names(df) <- c("Variable","Initial Pvalue","Initial Adj pvalue","Bandwidth specific Pvalue","Bandwidth specific Adj pvalue")
        # }
      }
      
      df
      
    })
    
  })
  
  
  
  v <- reactiveValues(doPlot = FALSE)
  
  observeEvent(input$go2, {
    v$doPlot <- input$go2
  })
  
  output$plotprop <- renderPlot({
    if (v$doPlot == FALSE) return()
    
    isolate({
      dataori <- inFile_ds()
      
      listb <- listvalues()
      data2 <- inFile_ds()
      ver1 <- listb[[1]]
      ver2 <- listb[[2]]
      
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
  
  

# 4-Summary bandwidth selection -------------------------------------------

  
  
  output$col_sel_covar_covar9<- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covarCovar9", "Choose the covariates", 
                choices  = cn,
                selected = 1,
                multiple=TRUE, selectize=TRUE)
  })
  
  v9 <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go9, {
    v9$doPlot <- input$go9
  })
  
  taula9 <- reactive({
    
    if (v9$doPlot == FALSE) return()
    # browser()
    isolate({
      
      data2 <- inFile_ds()
      bw3 <-  as.numeric(as.character(unlist(strsplit(input$bandwidths9, ","))))
      whichunder2 <- ifelse(input$onezero2=="under1",1,0)
      
      withProgress(message = 'Making analyses', value = 0, {
        
        taula <- LRErdd::rand_pajd_bw (dataset = data2, forcing_var_name =input$var_forcing7, covariates = input$var_covarCovar9,
                                           niter=input$niter9, bandwidth = bw3, cut_value = input$cutvalue2, whichunder = whichunder2)
      })
    })
    print(taula)
    
  })
  
  output$tablemethod9 <- renderDataTable({
    taula9()
  })
  

# 5-Inference on causal effects -------------------------------------------

  output$col_sel_covar_Y <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covarY", "Choose outcome variable", 
                choices  = cn,
                selected = cn[3],
                multiple=FALSE, selectize=TRUE)
  })
  
  
  
  output$col_sel_covar_W <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covarW", "Choose  treatment status (W) variable", 
                choices  = cn,
                selected = cn[2],
                multiple=FALSE, selectize=TRUE)
  })
  
  output$col_sel_covar_W_neyman <- renderUI({
    cn <- colnames(inFile_ds())
    selectInput("var_covarW_neyman", "Choose  treatment status (W) variable", 
                choices  = cn,
                selected = cn[2],
                multiple=FALSE, selectize=TRUE)
  })
  
  
  output$tablemethod <- renderDataTable({
    taula_post()
  })
  
  
  v_bostak<- reactiveValues(doPlot = FALSE)

  observeEvent(input$go_bostak, {
    v_bostak$doPlot <- input$go_bostak
  })

  # method button
  v_sharp_fep <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go_sharp_fep, {
    v_sharp_fep$doPlot <- input$go_sharp_fep
  })
  
  v_sharp_neyman <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go_sharp_neyman, {
    v_sharp_neyman$doPlot <- input$go_sharp_neyman
  })
  
  v_fuzzy_fep <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go_fuzzy_fep, {
    v_fuzzy_fep$doPlot <- input$go_fuzzy_fep
  })
  
  v_fuzzy_neyman <- reactiveValues(doPlot = FALSE)
  observeEvent(input$go_fuzzy_neyman, {
    v_fuzzy_neyman$doPlot <- input$go_fuzzy_neyman
  })
  

#  Taula ------------------------------------------------------------------

  
  
  taula <- reactive({
    
    if (v_sharp_fep$doPlot == TRUE & v_sharp_neyman$doPlot == TRUE & v_fuzzy_fep$doPlot == TRUE & v_fuzzy_neyman$doPlot == TRUE ) return()
    # if (v_bostak$doPlot == FALSE) return()
    isolate({
      
      data2 <- data.frame(inFile_ds())
      bw3 <-  as.numeric(as.character(unlist(strsplit(input$bandwidths, ","))))
      whichunder2 <- ifelse(input$onezero2=="under1",1,0)
      
      cin2 <- as.numeric(as.character(input$cin))
      cin3 <- 100-(cin2*100)
      # cin3 <- as.numeric(as.character(input$cin))
      
      withProgress(message = 'Making general analyses', value = 0.5, {
        
        if(input$select_effect == 'Sharp FEP')
        {
          taula <- LRErdd::sharp_fep_bw (dataset = data2, forcing_var_name =input$var_forcing7, Y_name =input$var_covarY, niter=input$niter,
                                       bandwidth = bw3, cut_value = input$cutvalue2, whichunder = whichunder2)
        }
        
        if(input$select_effect == 'Sharp Neyman')
        {
          
          taula <- LRErdd::sharp_neyman_bw (dataset = data2,forcing_var_name =input$var_forcing7,Y_name =input$var_covarY,niter=input$niter,
                                            bandwidth = bw3, cut_value = input$cutvalue2, whichunder = whichunder2, cin = cin3)
        }
        
        if(input$select_effect == 'Fuzzy Neyman')
        {
          taula <- LRErdd::fuzzy_neyman_bw (dataset = data2,forcing_var_name =input$var_forcing7,Y_name =input$var_covarY,niter=input$niter,
                                            W = input$var_covarW_neyman, bandwidth = bw3, cut_value = input$cutvalue2, 
                                            whichunder = whichunder2, cin = cin3)
        }
        
        if(input$select_effect == 'Fuzzy FEP')
        {
          

# hasi --------------------------------------------------------------------

          
          if(input$one2side=="onesided")
          {
            
            s0 <- input$cutvalue2
            if (whichunder2 == 1) {
              assigVar2 <- ifelse(data2[, input$var_forcing7] <= s0, 1, 0)
            } else if (whichunder2 == 0) {
              assigVar2 <- ifelse(data2[, input$var_forcing7] <= s0, 0, 1)
            }
            
            data2[,input$var_covarW] <- ifelse(assigVar2 == 0, 0, data2[,input$var_covarW])
          }
          
          Y_name <- input$var_covarY
          W_name <- input$var_covarW
          M2 = input$M2
          
          data2$W <- data2[, W_name]
          data2$Y <- data2[, Y_name]
          bandwidth <- bw3
          len_bw <- length(bandwidth)
          cut_value <- input$cutvalue2
          s0 <- cut_value
          
          whichunder <- whichunder2
          
          if (whichunder == 1) {
            data2$assigVar <- ifelse(data2[, input$var_forcing7] <= 
                                         cut_value, 1, 0)
          }
          else if (whichunder == 0) {
            data2$assigVar <- ifelse(data2[, input$var_forcing7] <= 
                                         cut_value, 0, 1)
          }
          data2$Z <- data2$assigVar
          Sh <- data2[, input$var_forcing7]
          pbalioak <- c()
          nak <- c()
          df2 <- data.frame(matrix(888, 1, 4))
          names(df2) <- c("Bandwidth", "Statistic: IV estimate of CACE", 
                          "Statistic: MLE of CACE", "Statistic: Posterior median of CACE")
          dfvec <- data.frame(matrix(888, 1, 3))
          names(dfvec) <- c("CACE.IV", "CACE.MLE", "CACE.PM")
          dfp <- 0

          
          withProgress(message = 'Looping bandwidths', value = 0, {
            # Number of times we'll go through the loop
            n <- len_bw
          
          
          for (b in 1:len_bw) {
            
            incProgress(1/n, detail = paste("Loop", b))
            
            h <- bandwidth[b]/2
            dat_bw <- data2[Sh >= s0 - h & Sh <= s0 + h, ]
            N <- dim(dat_bw)[1]
            print(N)
            zg <- dat_bw[, "assigVar"]
            yg <- dat_bw[, Y_name]
            wg <- dat_bw[, W_name]
            if (input$typemod == "binary") {
              if (input$one2side == "onesided") {
                fu <- fuzzy_fep1sided(dat_bw, Y = yg, W = wg, 
                                      Z = zg, Y_name = Y_name, M2 = M2)
                ft <- c(bandwidth[b], fu[[1]])
              }
              else if (input$one2side == "twosided") {
                fu <- fuzzy_fep2sided(dat_bw, Y = yg, W = wg, 
                                      Z = zg, Y_name = Y_name, M2 = M2)
                ft <- c(bandwidth[b], fu[[1]])
              }
            }
            else if (input$typemod == "numeric") {
              if (input$one2side == "onesided") {
                fu <- fuzzy_fep_numeric1sided(dat_bw, Y = yg, 
                                              W = wg, Z = zg, Y_name = Y_name, M2 = M2)
                ft <- c(bandwidth[b], fu[[1]])
              }
              else if (input$one2side == "twosided") {
                fu <- fuzzy_fep_numeric2sided(dat_bw, Y = yg, 
                                              W = wg, Z = zg, Y_name = Y_name, M2 = M2)
                ft <- c(bandwidth[b], fu[[1]])
              }
            }
            df2 <- rbind(df2, ft)
            dfvec <- rbind(dfvec, fu[[2]])
            dfp <- rbind(dfp, fu[[3]])
          }
          df2 <- df2[-1, ]
          dfvec <- dfvec[-1, ]
          dfp <- dfp[-1, ]
          taula <- list(df2, dfvec, dfp)

           })
          }
        })
      })
    print(taula)
    })

  
  taula_post <- reactive({
    
    if (v_sharp_fep$doPlot == TRUE & v_sharp_neyman$doPlot == TRUE & v_fuzzy_fep$doPlot == TRUE & v_fuzzy_neyman$doPlot == TRUE ) return()
    # if (v_bostak$doPlot == FALSE) return()
    isolate({
      
      tau3 <- taula()
      # cin3 <- as.numeric(as.character(input$cin))
      
      # withProgress(message = 'Making analyses', value = 0, {
        
        if(input$select_effect == 'Sharp FEP')
        {
          
          tau3 <- tau3$df
          
        }
        
        if(input$select_effect == 'Fuzzy FEP')
        {
          # browser()
          tau3 <- tau3[[1]]
        }
      # })
    })
    print(tau3)
    
  })

# Plot --------------------------------------------------------------------

  
  output$plotres <- renderPlot({
    
    if (v_sharp_fep$doPlot == TRUE & v_sharp_neyman$doPlot == TRUE & v_fuzzy_fep$doPlot == TRUE & v_fuzzy_neyman$doPlot == TRUE ) return()
    # if (v_bostak$doPlot == FALSE) return()
    isolate({
      
      tt <- taula()
      # browser()
      print(head(tt))
      # 
      
      if(input$select_effect%in%c("Sharp Neyman","Fuzzy Neyman"))
      {
        names(tt)[c(3,5,6)] <- c("ACE","L","U")
        # browser()
        tt <- tt[order(tt$Bandwidth),]
        tt$Bandwidth <- as.factor(tt$Bandwidth)
        if(input$select_effect%in%c("Sharp Neyman"))
        {
         
          ggplot(tt, aes(x = Bandwidth, y = ACE)) +
            geom_point(size = 4) +
            geom_errorbar(data = tt, aes(x = Bandwidth, ymax = U, ymin = L), width = 0, size = 1) + geom_hline(yintercept = 0,col= "red")
          
        } else if(input$select_effect%in%c("Fuzzy Neyman"))
        {
          ggplot(tt, aes(x = Bandwidth, y = ACE)) +
            geom_point(size = 4) +
            geom_errorbar(data = tt, aes(x = Bandwidth, ymax = U, ymin = L),width = 0, size = 1) + facet_grid(.~Estimand, scales = "free") + geom_hline(yintercept = 0,col= "red")
        }
        
      } else  if(input$select_effect%in%c("Sharp FEP","Fuzzy FEP"))
      {
        # browser()
        if(input$select_effect%in%c("Sharp FEP"))
        {
          l3 <- length(tt$hist_data)
          l4 <- length(tt$hist_data[[1]])
          ve <- 0
          ge <- "k"
          obs <- 0
          for(h in 1:l3)
          {
            ve <- c(ve,tt$hist_data[[h]])
            ge <- c(ge,rep(tt$df[h,1],l4))
            obs <- c(obs,rep(tt$df[h,4],l4))
          }
          ve <- ve[-1]
          ge <- ge[-1]
          obs <- obs[-1]
          
          # browser()
          
          dfg <- data.frame(ge,ve,obs)
          dfg$ge <- as.numeric(as.character(dfg$ge))
          dfg <- dfg[order(dfg$ge),]
          # dfg$ge <- paste0("Bandwidth",dfg$ge)
          # browser()
          print(levels(dfg$ge))
          
          ggplot(dfg, aes(x=ve))+
            geom_histogram(color="black", fill="white") + xlab("Fisher test") + 
            facet_grid(. ~ ge) + geom_vline(aes(xintercept=obs, color="red"),linetype="dashed") + theme(legend.position="none")
          
        } else if(input$select_effect%in%c("Fuzzy FEP"))
        {
          
          bnd <- as.numeric(as.character(tt[[1]][,1]))
          if(length(unique(bnd))==1)
          {
            
            l3 <- length(tt[[2]][,1])*3 # number of sim
            l4 <- length(tt[[1]][,1]) #number of bandwidth
            ve <- 0
            ge <- "k"
            obs <- 0
            izen <- "K"
            # izen2 <- "k"
            
            ve <- c(ve,tt[[2]][,1],tt[[2]][,2],tt[[2]][,3]) # simulated data
            
            # for(h in 1:l4)
            # {
            # for(t in 1:l3)
            # {
            h <- 1
            ge <- c(ge,rep(tt[[1]][h,1],l3*3))  # bandwidth
            izen <- c(izen,rep("CACE.IV",l3),rep("CACE.MLE",l3),rep("CACE.PM",l3)) # test
            # izen2 <- c(izen2,rep("CACE.IV",l3),rep("CACE.MLE",l3),rep("CACE.PM",l3))
            obs <- c(obs,rep(tt[[3]][1],l3), rep(tt[[3]][2],l3), rep(tt[[3]][3],l3))
            #   # }
            #   
            # }
            ve <- ve[-1]
            ge <- ge[-1]
            obs <- obs[-1]
            izen <- izen[-1]
            
            
            
            dfg <- data.frame(ge,ve,obs,izen)
            
            dfg$ge <- bnd
            dfg <- dfg[order(dfg$ge),]
            # dfg$ge <- factor(dfg$ge)
            # dfg$ge2 <- paste0("Bandwidth",as.character(dfg$ge))
            
            ggplot(dfg, aes(x=ve))+
              geom_histogram(color="black", fill="white") + xlab("Simulated values") + 
              facet_grid(izen ~ .) + geom_vline(aes(xintercept=obs, color="red"),linetype="dashed") + theme(legend.position="none")
            
          } else if(length(unique(bnd)) > 1)
          {
            
            l3 <- length(tt[[2]][,1])*3 # number of sim
            l4 <- length(tt[[1]][,1]) #number of bandwidth
            ve <- 0
            ge <- "k"
            obs <- 0
            izen <- "K"
            # izen2 <- "k"
            
            ve <- c(ve,tt[[2]][,1],tt[[2]][,2],tt[[2]][,3]) # simulated data
            
            for(h in 1:l4)
            {
              # for(t in 1:l3)
              # {
              
              ge <- c(ge,rep(tt[[1]][h,1],l3*3))  # bandwidth
              izen <- c(izen,rep("CACE.IV",l3),rep("CACE.MLE",l3),rep("CACE.PM",l3)) # test
              # izen2 <- c(izen2,rep("CACE.IV",l3),rep("CACE.MLE",l3),rep("CACE.PM",l3))
              obs <- c(obs,rep(tt[[3]][h,1],l3), rep(tt[[3]][h,2],l3), rep(tt[[3]][h,3],l3))
              # }
              
            }
            ve <- ve[-1]
            ge <- ge[-1]
            obs <- obs[-1]
            izen <- izen[-1]
            
            
            
            dfg <- data.frame(ge,ve,obs,izen)
            bnd <- as.numeric(as.character(dfg$ge))
            dfg$ge <- bnd
            dfg <- dfg[order(dfg$ge),]
            # dfg$ge <- factor(dfg$ge)
            # dfg$ge2 <- paste0("Bandwidth",as.character(dfg$ge))
            
            ggplot(dfg, aes(x=ve))+
              geom_histogram(color="black", fill="white") + xlab("Simulated values") + 
              facet_grid(izen ~ ge) + geom_vline(aes(xintercept=obs, color="red"),linetype="dashed") + theme(legend.position="none")
          }
          
          
        }
      }
      
    })
    
  })
  

# 6-Hiding ------------------------------------------------------------------

  observeEvent(input$go_intro, {
    showTab(inputId = "tabs", target = "Instructions")
    hideTab(inputId = "tabs", target = "1-Your Data")
    hideTab(inputId = "tabs", target = "2-Summary statistics")
    hideTab(inputId = "tabs", target = "3-Bandwidth selection")
    hideTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    hideTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })
  
  observeEvent(input$go_default_dataset, {
    hideTab(inputId = "tabs", target = "Instructions")
    showTab(inputId = "tabs", target = "1-Your Data")
    hideTab(inputId = "tabs", target = "2-Summary statistics")
    hideTab(inputId = "tabs", target = "3-Bandwidth selection")
    hideTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    hideTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })
  
  observeEvent(input$go_excel, {
    hideTab(inputId = "tabs", target = "Instructions")
    showTab(inputId = "tabs", target = "1-Your Data")
    hideTab(inputId = "tabs", target = "2-Summary statistics")
    hideTab(inputId = "tabs", target = "3-Bandwidth selection")
    hideTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    hideTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })
  
  observeEvent(input$go_rdata, {
    hideTab(inputId = "tabs", target = "Instructions")
    showTab(inputId = "tabs", target = "1-Your Data")
    hideTab(inputId = "tabs", target = "2-Summary statistics")
    hideTab(inputId = "tabs", target = "3-Bandwidth selection")
    hideTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    hideTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })
  
  observeEvent(input$go_bwsel2, {
    hideTab(inputId = "tabs", target = "Instructions")
    hideTab(inputId = "tabs", target = "1-Your Data")
    showTab(inputId = "tabs", target = "2-Summary statistics")
    hideTab(inputId = "tabs", target = "3-Bandwidth selection")
    hideTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    hideTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })
  
  observeEvent(input$go_bwsel, {
    hideTab(inputId = "tabs", target = "Instructions")
    hideTab(inputId = "tabs", target = "1-Your Data")
    hideTab(inputId = "tabs", target = "2-Summary statistics")
    showTab(inputId = "tabs", target = "3-Bandwidth selection")
    hideTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    hideTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })
  
  observeEvent(input$go2, {
    hideTab(inputId = "tabs", target = "Instructions")
    hideTab(inputId = "tabs", target = "1-Your Data")
    hideTab(inputId = "tabs", target = "2-Summary statistics")
    showTab(inputId = "tabs", target = "3-Bandwidth selection")
    hideTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    hideTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })
  
  observeEvent(input$go9, {
    hideTab(inputId = "tabs", target = "Instructions")
    hideTab(inputId = "tabs", target = "1-Your Data")
    hideTab(inputId = "tabs", target = "2-Summary statistics")
    hideTab(inputId = "tabs", target = "3-Bandwidth selection")
    showTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    hideTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })
  
  observeEvent(input$go5, {
    hideTab(inputId = "tabs", target = "Instructions")
    hideTab(inputId = "tabs", target = "1-Your Data")
    hideTab(inputId = "tabs", target = "2-Summary statistics")
    hideTab(inputId = "tabs", target = "3-Bandwidth selection")
    hideTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    showTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })
  
  observeEvent(input$go_sharp_fep, {
    hideTab(inputId = "tabs", target = "Instructions")
    hideTab(inputId = "tabs", target = "1-Your Data")
    hideTab(inputId = "tabs", target = "2-Summary statistics")
    hideTab(inputId = "tabs", target = "3-Bandwidth selection")
    hideTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    showTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })
  
  observeEvent(input$go_sharp_neyman, {
    hideTab(inputId = "tabs", target = "Instructions")
    hideTab(inputId = "tabs", target = "1-Your Data")
    hideTab(inputId = "tabs", target = "2-Summary statistics")
    hideTab(inputId = "tabs", target = "3-Bandwidth selection")
    hideTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    showTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })
  
  observeEvent(input$go_fuzzy_fep, {
    hideTab(inputId = "tabs", target = "Instructions")
    hideTab(inputId = "tabs", target = "1-Your Data")
    hideTab(inputId = "tabs", target = "2-Summary statistics")
    hideTab(inputId = "tabs", target = "3-Bandwidth selection")
    hideTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    showTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })
  
  
  observeEvent(input$go_fuzzy_neyman, {
    hideTab(inputId = "tabs", target = "Instructions")
    hideTab(inputId = "tabs", target = "1-Your Data")
    hideTab(inputId = "tabs", target = "2-Summary statistics")
    hideTab(inputId = "tabs", target = "3-Bandwidth selection")
    hideTab(inputId = "tabs", target = "4-Summary Bandwidth selection")
    showTab(inputId = "tabs", target = "5-Inference on causal effects")
    
  })

  
}