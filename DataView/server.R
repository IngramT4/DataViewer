server <- function(input, output) {
    #setting global variables
    options(shiny.maxRequestSize=30*1024^2)
    reactvars <- reactiveValues(editdata=mtcars,viewdata=mtcars,resetdata=mtcars,resetclass=mtcars,holdclass=mtcars,coladded=0,filecheck=0,listindex=0,fileinput=list(),datanames=c(),filtervar=names(mtcars))
    
    #HOME TAB
    
    #upload files
    observeEvent(input$fileinputcsv,{
        for (i in 1:length(input$fileinputcsv[[1]])) {
            reactvars$listindex <- reactvars$listindex + 1
            reactvars$fileinput[[reactvars$listindex]] <- read.csv(input$fileinputcsv[[i,"datapath"]])
            reactvars$datanames[reactvars$listindex] <- input$fileinputcsv[[1]][i]
        }
        reactvars$filtervar <- names(reactvars$fileinput[[1]])
        reactvars$editdata <- reactvars$fileinput[[1]]
        reactvars$viewdata <- reactvars$fileinput[[1]]
        reactvars$resetdata <- reactvars$fileinput[[1]]
        reactvars$resetclass <- reactvars$fileinput[[1]]
        reactvars$holdclass <- reactvars$fileinput[[1]]
        
    })
    
    observeEvent(input$fileinputxl,{
        for (i in 1:length(input$fileinputxl[[1]])) {
            reactvars$listindex <- reactvars$listindex + 1
            reactvars$fileinput[[reactvars$listindex]] <- read_excel(input$fileinputxl[[i,"datapath"]])
            reactvars$datanames[reactvars$listindex] <- input$fileinputxl[[1]][i]
        }
        reactvars$filtervar <- reactvars$datanames[1]
        reactvars$editdata <- reactvars$fileinput[[1]]
        reactvars$viewdata <- reactvars$fileinput[[1]]
        reactvars$resetdata <- reactvars$fileinput[[1]]
        reactvars$resetclass <- reactvars$fileinput[[1]]
        reactvars$holdclass <- reactvars$fileinput[[1]]
    })
    
    #change class
    output$chngclass <- renderUI({
        selectInput("chngclass","Select variable(s)",try(names(reactvars$fileinput[[as.integer(substr(input$classdataset,1,1))]]),silent = TRUE),multiple = TRUE)
        
    })
    
    output$choosedataset <- renderUI({
        position <- 0
        select <- c()
        if(!is.null(reactvars$datanames)){
            for (i in 1:length(reactvars$datanames)) {
                position <- position + 1
                select <- append(select,paste(as.character(position),":",reactvars$datanames[i]))
            }
            selectInput("classdataset","Choose file",select)
        }
        else{
            selectInput("classdataset","Choose file","Upload data")
        }
    })
    
    observe({
        if(input$classtype=="Date"){
            show("dateinput")
            show("dateselect")
        }
        else{
            hide("dateinput")
            hide("dateselect")
        }
    })
    
    observeEvent(input$butconvert, {
        changeclass <- reactvars$fileinput[[as.integer(substr(input$classdataset,1,1))]]
        for(i in input$chngclass){
            if(input$classtype == "Date"){
                if(input$dateinput!=""){
                    a <- try(as.Date(changeclass[,i],format=as.character(input$dateinput)),silent = TRUE)
                }
                else{
                    a <- try(as.Date(changeclass[,i],tryFormats=c("%d/%m/%Y","%d/%m/%y","%d-%m-%Y","%d-%m-%y","%Y/%m/%d","%y/%m/%d","%Y-%m-%d","%Y-%m-%d"),silent = TRUE))
                }
            }
            if(input$classtype == "Character"){
                a <- try(as.character(changeclass[,i]),silent = TRUE)
            }
            if(input$classtype == "Numeric"){
                a <- try(as.numeric(changeclass[,i]),silent = TRUE)
            }
            if(input$classtype == "Factor"){
                a <- try(factor(changeclass[,i]),silent = TRUE)
            }
            
            if(class(a) == "try-error"){
                b <- "One or more of these variables cannot be converted into the selected class"
                break()
            }
            else{
                if(all(is.na(a))){
                    b <- "Something went wrong! One of these variables is now all NA. It is suggested to reset your classes and try again."
                }
                else{
                    changeclass[,i] <- a
                    b <- "You have succesfully converted the variables into a the selected class"}
            }
        }
        reactvars$fileinput[[as.integer(substr(input$classdataset,1,1))]] <- changeclass
        showNotification(b,duration = 8)
    })
    
    
    output$checkclass <- renderText({
        c <- c()
        v <- try(lapply(reactvars$fileinput[[as.integer(substr(input$classdataset,1,1))]],class),silent = TRUE)
        for (i in input$chngclass) {
            c <- append(c, v[i])
            
        }
        if(length(c)==0){
            msg <- "Select variables to see their class"
        }
        else{
            if(length(c)==1){
                msg <- paste(input$chngclass[1],":",c[1])
            }
            else{
                msg <- paste(input$chngclass[1],":",c[1])
                c <- c[-1]
                inputhold <- input$chngclass[-1]
                for(i in 1:length(c)){
                    msg <- paste(msg,",",inputhold[i],":",c[i])}
            }}
        msg 
    })
    
    
    #MERGE DATA
    output$choosefilemerge1 <- renderUI({
        position <- 0
        select <- c()
        if(!is.null(reactvars$datanames)){
            for (i in 1:length(reactvars$datanames)) {
                position <- position + 1
                select <- append(select,paste(as.character(position),":",reactvars$datanames[i]))
            }
            selectInput("filechosemerge1","Choose file",select)
        }
        else{
            selectInput("filechosemerge1","Choose file","Upload data")
        }
    })
    
    output$choosefilemerge2 <- renderUI({
        position <- 0
        select <- c()
        if(!is.null(reactvars$datanames)){
            for (i in 1:length(reactvars$datanames)) {
                position <- position + 1
                select <- append(select,paste(as.character(position),":",reactvars$datanames[i]))
            }
            selectInput("filechosemerge2","Choose file",select)
        }
        else{
            selectInput("filechosemerge2","Choose file","Upload data")
        }
    })
    
    output$choosemergevar1 <- renderUI({
        if(is.null(reactvars$datanames)){
            selectInput("mergevar1","Dataset 1 column","")
        }
        else{
            try(selectInput("mergevar1","Dataset 1 column",names(reactvars$fileinput[[as.integer(substr(input$filechosemerge1,1,1))]]),multiple = TRUE),silent=TRUE)
        }
    })
    
    output$choosemergevar2 <- renderUI({
        if(is.null(reactvars$datanames)){
            selectInput("mergevar2","Dataset 2 variables","")
        }
        else{
            try(selectInput("mergevar2","Dataset 2 variables",names(reactvars$fileinput[[as.integer(substr(input$filechosemerge2,1,1))]]),multiple = TRUE),silent=TRUE)
        }
    })
    
    observeEvent(input$butmerge,{
        merge1 <- reactvars$fileinput[[as.integer(substr(input$filechosemerge1,1,1))]]
        merge2 <- reactvars$fileinput[[as.integer(substr(input$filechosemerge2,1,1))]]
        reactvars$listindex <- reactvars$listindex+1
        if(input$join=="Inner join"){    
            reactvars$fileinput[[reactvars$listindex]] <- merge(merge1,merge2,by.x=input$mergevar1,by.y=input$mergevar2)
        }
        if(input$join=="Full join"){
            reactvars$fileinput[[reactvars$listindex]] <- merge(merge1,merge2,by.x=input$mergevar1,by.y=input$mergevar2,all.x=TRUE,all.y=TRUE)
        }
        if(input$join=="Left join"){
            reactvars$fileinput[[reactvars$listindex]] <- merge(merge1,merge2,by.x=input$mergevar1,by.y=input$mergevar2,all.x=TRUE)
        }
        else{
            reactvars$fileinput[[reactvars$listindex]] <- merge(merge1,merge2,by.x=input$mergevar1,by.y=input$mergevar2,all.y=TRUE)
        }
        reactvars$datanames <- append(reactvars$datanames,input$namemerge)
    })
    
    #VIEW TAB
    
    #choose dataset
    output$choosefileuse <- renderUI({
        position <- 0
        select <- c()
        if(!is.null(reactvars$datanames)){
            for (i in 1:length(reactvars$datanames)) {
                position <- position + 1
                select <- append(select,paste(as.character(position),":",reactvars$datanames[i]))
            }
            selectInput("filechoseuse","Choose file",select)
        }
        else{
            selectInput("filechoseuse","Choose file","Upload data")
        }
    })
    
    
    
    observeEvent(input$butchoose,{
        reactvars$filtervar <- names(reactvars$fileinput[[as.integer(substr(input$filechoseuse,1,1))]])
        reactvars$editdata <- reactvars$fileinput[[as.integer(substr(input$filechoseuse,1,1))]]
        reactvars$viewdata <- reactvars$fileinput[[as.integer(substr(input$filechoseuse,1,1))]]
        reactvars$resetdata <- reactvars$fileinput[[as.integer(substr(input$filechoseuse,1,1))]]
        reactvars$resetclass <- reactvars$fileinput[[as.integer(substr(input$filechoseuse,1,1))]]
        reactvars$holdclass <- reactvars$fileinput[[as.integer(substr(input$filechoseuse,1,1))]]
    }) 
    
    #filtering table
    output$view <- renderUI({
        selectInput("viewdata","Select Variables for subset",
                    choices = reactvars$filtervar,
                    multiple = TRUE)
    })
    
    variblesViewing <- eventReactive({
        input$butchoose
        input$fileinputcsv
        input$fileinputxl
    },{
        names(reactvars$editdata)
    })
    
    output$filter <- renderUI({
        selectInput("varfilter","Select variable for filtering", reactvars$filtervar)
    })
    
    observeEvent(input$varfilter,{
        if(is.numeric(reactvars$editdata[,input$varfilter])|class(reactvars$editdata[,input$varfilter])=="Date"){
            show("choiceint")
        }
        else{
            hide("choiceint")
        }
    })
    
    observeEvent(input$chooseaction,{
        if(input$chooseaction=="Add/remove column"){
            hide("view")
            hide("constraint")
            hide("choiceint")
            hide("butview")
            hide("butreset")
            show("createcol")
            show("namecolumn")
            show("butcalculate")
            show("removecol")
            show("butremovecol")
        }
        else{
            show("view")
            show("constraint")
            show("butview")
            show("butreset")
            hide("createcol")
            hide("namecolumn")
            hide("butcalculate")
            hide("removecol")
            hide("butremovecol")
            if(is.numeric(reactvars$editdata[,reactvars$filtervar])|class(reactvars$editdata[,reactvars$filtervar])=="Date"){
                show("choiceint")
            }
            else{
                hide("choiceint")
            }
            
        }
    })
    
    observeEvent(input$butview,{
        
        if(input$constraint!=""){
            if(length(input$viewdata) != 0){
                if(is.numeric(reactvars$editdata[,input$varfilter])){
                    reactvars$editdata <- reactvars$editdata[!is.na(reactvars$editdata[,input$varfilter]),]
                    if(input$choiceint=="Greater"){
                        reactvars$editdata <- reactvars$editdata[reactvars$editdata[,input$varfilter]>as.numeric(input$constraint),]
                        reactvars$viewdata <- reactvars$editdata[,input$viewdata]
                    }
                    if(input$choiceint=="Equal"){
                        reactvars$editdata <- reactvars$editdata[reactvars$editdata[,input$varfilter]==as.numeric(input$constraint),]
                        reactvars$viewdata <- reactvars$editdata[,input$viewdata]
                    }
                    if(input$choiceint=="Less"){
                        reactvars$editdata <- reactvars$editdata[reactvars$editdata[,input$varfilter]<as.numeric(input$constraint),]
                        reactvars$viewdata <- reactvars$editdata[,input$viewdata]
                    }
                }
                if(is.character(reactvars$editdata[,input$varfilter])){
                    reactvars$editdata <- reactvars$editdata[!is.na(reactvars$editdata[,input$varfilter]),]
                    reactvars$editdata <- reactvars$editdata[stri_detect_fixed(reactvars$editdata[,input$varfilter],input$constraint),]
                    reactvars$viewdata <- reactvars$editdata[,input$viewdata]
                }
                if(class(reactvars$editdata[,input$varfilter])=="Date"){
                    reactvars$editdata <- reactvars$editdata[!is.na(reactvars$editdata[,input$varfilter]),]
                    if(input$choiceint=="Less"){
                        reactvars$editdata <- reactvars$editdata[reactvars$editdata[,input$varfilter]<as.Date(input$constraint),]
                        reactvars$viewdata <- reactvars$editdata[,input$viewdata]
                    }
                    if(input$choiceint=="Equal"){
                        reactvars$editdata <- reactvars$editdata[reactvars$editdata[,input$varfilter]==as.Date(input$constraint),]
                        reactvars$viewdata <- reactvars$editdata[,input$viewdata]
                    }
                    else{
                        reactvars$editdata <- reactvars$editdata[reactvars$editdata[,input$varfilter]>as.Date(input$constraint),]
                        reactvars$viewdata <- reactvars$editdata[,input$viewdata]
                    }
                }
            }
            else{
                if(is.numeric(reactvars$editdata[,input$varfilter])){
                    reactvars$editdata <- reactvars$editdata[!is.na(reactvars$editdata[,input$varfilter]),]
                    if(input$choiceint=="Greater"){
                        reactvars$editdata <- reactvars$editdata[reactvars$editdata[,input$varfilter]>as.numeric(input$constraint),]
                        reactvars$viewdata <- reactvars$editdata
                    }
                    if(input$choiceint=="Equal"){
                        reactvars$editdata <- reactvars$editdata[reactvars$editdata[,input$varfilter]==as.numeric(input$constraint),]
                        reactvars$viewdata <- reactvars$editdata
                    }
                    if(input$choiceint=="Less"){
                        reactvars$editdata <- reactvars$editdata[reactvars$editdata[,input$varfilter]<as.numeric(input$constraint),]
                        reactvars$viewdata <- reactvars$editdata
                    }
                }
                if(is.character(reactvars$editdata[,input$varfilter])){
                    reactvars$editdata <- reactvars$editdata[!is.na(reactvars$editdata[,input$varfilter]),]
                    reactvars$editdata <- reactvars$editdata[stri_detect_fixed(reactvars$editdata[,input$varfilter],input$constraint),]
                    reactvars$viewdata <- reactvars$editdata
                }
                if(class(reactvars$editdata[,input$varfilter])=="Date"){
                    reactvars$editdata <- reactvars$editdata[!is.na(reactvars$editdata[,input$varfilter]),]
                    if(input$choiceint=="Less"){
                        reactvars$editdata <- reactvars$editdata[reactvars$editdata[,input$varfilter]<as.Date(input$constraint),]
                        reactvars$viewdata <- reactvars$editdata
                    }
                    if(input$choiceint=="Equal"){
                        reactvars$editdata <- reactvars$editdata[reactvars$editdata[,input$varfilter]==as.Date(input$constraint),]
                        reactvars$viewdata <- reactvars$editdata
                    }
                    else{
                        reactvars$editdata <- reactvars$editdata[reactvars$editdata[,input$varfilter]>as.Date(input$constraint),]
                        reactvars$viewdata <- reactvars$editdata
                    }
                }
            }
        }
        
        else{
            
            if(length(input$viewdata) != 0){
                reactvars$viewdata <- reactvars$editdata[,input$viewdata]
            }
            else{
                reactvars$viewdata <- reactvars$editdata
            }
        }
    })
    
    observeEvent(input$butreset,{
        reactvars$editdata <- reactvars$holdclass
        reactvars$viewdata <- reactvars$editdata
    })
    
    
    #create columns
    output$createcol <- renderUI({
        intnames <- c()
        for (i in names(reactvars$resetdata)) {
            if(is.numeric(reactvars$resetdata[,i])){
                intnames <- append(intnames,i)
            }
        }
        if(length(intnames)>=2){
            textInput("exprsion","Use existing columns to make new one",placeholder = paste("(",intnames[1],"+",intnames[2],")","/","2"))
        }
        else{
            textInput("exprsion","Use existing columns to make new one",placeholder = "(col1+col1)/2")
        }
    })
    
    output$removecol <- renderUI({
        selectInput("removecol","Remove this column",reactvars$filtervar,multiple = TRUE)
    })
    
    observeEvent(input$butremovecol,{
        drop <- input$removecol
        reactvars$editdata <- reactvars$editdata[,!names(reactvars$editdata)%in%drop]
        reactvars$holdclass <- reactvars$holdclass[,!names(reactvars$holdclass)%in%drop]
        reactvars$resetclass <- reactvars$resetclass[,!names(reactvars$resetclass)%in%drop]
        reactvars$viewdata <- reactvars$editdata
    })
    
    observeEvent(input$butcalculate,{
        newcol_nofilter <- try(as.data.frame(with(data = reactvars$holdclass,eval(parse(text = input$exprsion)))))
        newcol_filter <- try(as.data.frame(with(data = reactvars$editdata,eval(parse(text = input$exprsion)))))
        if(class(newcol_nofilter)=="try-error"){
            showNotification("An error occured and the column wasn't added",duration = 5)
        }
        else{
            reactvars$coladded <- reactvars$coladded+1
            if(input$namecolumn==""){
                names(newcol_nofilter) <- paste("newvar",as.character(reactvars$coladded))
                names(newcol_filter) <- paste("newvar",as.character(reactvars$coladded))
                
            }
            else{
                names(newcol_nofilter) <- input$namecolumn
                names(newcol_filter) <- input$namecolumn
            }
            reactvars$holdclass <- cbind(reactvars$holdclass,newcol_nofilter)
            reactvars$resetclass <- cbind(reactvars$resetclass,newcol_nofilter)
            reactvars$editdata<-cbind(reactvars$editdata,newcol_filter)
            if(length(input$viewdata)!=0){
                reactvars$viewdata <- reactvars$editdata[,input$viewdata]
            }
            else{
                reactvars$viewdata <- reactvars$editdata
            }
        }
        
    })
    
    observeEvent(input$butcompreset,{
        reactvars$editdata <- reactvars$resetdata
        reactvars$holdclass <- reactvars$resetdata
        reactvars$resetclass <- reactvars$resetdata
        reactvars$viewdata <- reactvars$editdata
    })
    
    #output table
    output$data <- renderDataTable({
        reactvars$viewdata
    },options = list(scrollX=TRUE))
    
    
    
    #SUMMARY TAB
    
    #UI
    output$sumryvarsori <- renderUI({
        selectInput("sumvarsori","Select a variable for original data",reactvars$filtervar)
    })
    
    output$sumryvarsfil <- renderUI({
        selectInput("sumvarsfil","Select a variable for filtered",reactvars$filtervar)
    })
    
    #outputs
    output$hist_original <- renderPlot({
        hist(reactvars$resetclass[,input$sumvarsori], xlab = as.character(input$sumvarsori),main = as.character(input$sumvarsori))
    })
    
    output$describe_original <- renderPrint({
        describe(reactvars$resetclass[,input$sumvarsori])
    })
    
    output$summary_original <- renderPrint({
        summary(reactvars$resetclass[,input$sumvarsori])
    })
    
    output$table_original <- renderPrint({
        table(reactvars$resetclass[,input$sumvarsori])
    })
    
    output$hist_filter <- renderPlot({
        hist(reactvars$editdata[,input$sumvarsfil], xlab = as.character(input$sumvarsfil),main = as.character(input$sumvarsfil))
    })
    
    output$describe_filter <- renderPrint({
        describe(reactvars$editdata[,input$sumvarsfil])
    })
    
    output$summary_filter <- renderPrint({
        summary(reactvars$editdata[,input$sumvarsfil])
    })
    
    output$table_filter <- renderPrint({
        table(reactvars$editdata[,input$sumvarsfil])
    })
    
    # Group by page
    
    output$sumdata <- renderUI({
        position <- 0
        select <- c()
        if(!is.null(reactvars$datanames)){
            for (i in 1:length(reactvars$datanames)) {
                position <- position + 1
                select <- append(select,paste(as.character(position),":",reactvars$datanames[i]))
            }
            selectInput("sumdatause","Choose file",select)
        }
        else{
            selectInput("sumdatause","Choose file","Upload data")
        }
    })
    
    output$group_by <- renderUI({
        selectInput("group_by_var","Choose variable(s) to group by",names(reactvars$fileinput[[as.integer(substr(input$sumdatause,1,1))]]),multiple = TRUE)
    })
    
    output$sumvars <- renderUI({
        selectInput("summaryvars","Choose variable to be summarised",names(reactvars$fileinput[[as.integer(substr(input$sumdatause,1,1))]]))
    })
    
    generate_group_by_table <- eventReactive(input$butgroup,{
        if(input$sumdatause!="Upload data"){
            table1 <- reactvars$fileinput[[as.integer(substr(input$sumdatause,1,1))]]
            table1 <- group_by_(table1,input$group_by_var)
            table1 <- summarize(table1, n(),"mean"=mean(eval(as.symbol(input$summaryvars)),na.rm=TRUE),"SD"=sd(eval(as.symbol(input$summaryvars)),na.rm=TRUE),"max"=max(eval(as.symbol(input$summaryvars)),na.rm=TRUE))
            table1
        }
    })
    
    output$group_by_table <- renderDataTable({
        generate_group_by_table()
    },options = list(scrollX=TRUE))
}