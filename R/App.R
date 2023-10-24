


library(shiny)
library(sortable)
library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(bslib)
library(shinydashboard)
library(fontawesome)
library(shinyFiles)
library(shinyjs)
library(fmsb)
library(RColorBrewer)
library(reshape2)
library(plot.matrix)
library(htmlwidgets)



############### USER INTERFACE CODE ###############

ui <- dashboardPage(
  
  dashboardHeader(titleWidth=920, title = span(" ", span("Ordered Ranking Tool for EKE", style = "color: WHITE; font-size: 28px")),
                  tags$li(class = "dropdown", tags$a(HTML(paste("Click horizontal lines to see explanations ", textOutput("See explanations "))),style = "color: silver; font-size: 14px;font-family: Arial;"))
  ),
  
  dashboardSidebar(collapsed = TRUE, width = 550,
                   h4("Explanations of Items",style="color:white"),
                   h4(tableOutput("table"),style = "line-height: 3px ;font-size:13px;")
  ),
  
  
  dashboardBody(
    
    fluidRow(column(width = 10, offset = 1,height = 2,
                    h6(textInput("filename",  em("Input your name here"), value = ("")),style="background-color:#ff8282;color:white; border-color:#ff8282;padding:2px; font-size:120%; font-family: Arial;"),
                    
                    tabsetPanel(
                      tabPanel(
                        
                        h4(uiOutput("title_in", style="background-color:white;color:blue;font-weight: bold;  font-size:110%; font-family: Arial")),
                        br(),
                        fluidRow(column( 10, 
                                         uiOutput("sortable", style =  " line-height: 5px;font-weight: bold;font-size:16px;font-family: Arial")
                        ),
                        column(2, offset = 1,
                               br(),
                               br(),
                               
                               
                        )
                        )),
                      
                      tabPanel(
                        actionButton("finalise", "Click here to finalise selection", style="background-color:red;color:white; border-color:red;padding:4px; font-size:110%; font-family: Arial"),
                        fluidRow(
                          column(width = 4, 
                                 br(),
                                 br(),
                                 tags$head(tags$style(HTML(".myclass3 pre {color: blue;font-size:150%; background-color: lightgrey ;border-colour:lightgrey;  font-weight: bolder;font-size: 12px;font-family: Arial; width: 200px; }"))),
                                 
                                 DT::dataTableOutput("my_datatable"),
                                 br(),
                                 br()
                          ),
                          column(width = 4, offset=2,
                                 br(),
                                 br(),
                                 actionButton("saveBtn", "Submit final ranking", style="background-color:red;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                                 br(),
                                 br(),
                                 div(class = "myclass3",verbatimTextOutput("submitted", placeholder = FALSE)),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 h6(textAreaInput("free_text", "Please enter your reasoning:", width = "500px", rows = 8), style= "font-size:80%; font-family: Arial;"),  
                                 br()
                          )
                        )),
                      
                      tabPanel(""),
                      tabPanel(""),
                      tabPanel(""),
                      tabPanel(""),
                      tabPanel(""),
                      
                      
                      tabPanel(
                        h6("Facilitator", style="background-color:white;font-size:75%; font-family: Arial"),
                        
                        tabsetPanel(
                          
                          tabPanel(
                            h6("Collect Data"),
                            fluidRow(column(3,
                                            numericInput("password", "Password:", "enter", width = "100px"),
                                            column(4,  
                                                   tags$style("#name_openend {font-size:20px;color:red;display:block;font-style: italic;width:400px}"),
                                                   uiOutput("name_openend"))
                            )),
                            br(),
                            actionButton("saveBtn2", "PRESS HERE TO COLLECT DATA", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                            br(),
                            br(),
                            tags$head(tags$style(HTML(".myclass pre {color: blue;font-size:150%; background-color: pink;font-weight: bolder;font-size: 12px;font-family: Arial; width: 180px; }"))),
                            div(class = "myclass",verbatimTextOutput("done", placeholder = FALSE)),
                            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                             tags$div("Loading...",id="loadmessage"))
                            
                          ),
                          
                          
                          tabPanel(
                            h6("Participants Submitted"),
                            fluidRow(column(4, 
                                            br(),
                                            tableOutput('table2'))),
                          ),  
                          
                          tabPanel(
                            h6("Results"),
                            fluidRow(column(10, offset=1,
                                            br(),
                                            h4("Boxplot of Combined Results" , style="color:blue; font-weight:bold%; font-family: Arial"),
                                            h6("Variability between expert in ranking scores" , style="color:blue; font-family: Arial"),
                                            br(),
                                            h5(verbatimTextOutput("info", placeholder = FALSE)) ,
                                            tags$head(tags$style("#info{border-color:#6BADCE;;background-color:#6BADCE;color: white; font-size: 14px;font-weight: bold;font-family: Arial;}")),
                                            plotOutput("plot1", width = "100%"),
                                            br(),
                                            h4("Table of Combined Results" , style="color:blue; font-weight:bold%; font-family: Arial"),
                                            br(),
                                            tableOutput('table1'),
                                            br(),
                                            br(),
                            ))),
                          
                          tabPanel(
                            h6("Spider plots of expert variation"),
                            fluidRow(column(10, offset=1,
                                            br(),
                                            h4("Expert Spider Plots" , style="color:blue; font-weight:bold%; font-family: Arial"),
                                            h6("Individual comparison to mean" , style="color:blue; font-family: Arial"),
                                            br(),
                                            uiOutput("filter_name"),
                                            br(),
                                            plotOutput("plot_radar2", height = "600px"),
                                            br(),
                                            br(),
                                            h6("All expert responses" , style="color:blue; font-family: Arial"),
                                            br(),
                                            plotOutput("plot_radar", height = "600px"),
                                            
                                            br(),
                                            br(),
                                            
                            ))),
                          
                          tabPanel(
                            h6("Further details of expert variation"),
                            fluidRow(column(10, offset=1,
                                            br(),
                                            h4("Expert Deviation Heat Map" , style="color:blue; font-weight:bold%; font-family: Arial"),
                                            h6("Relative deviation of experts from the mean ranking (in standard deviations of difference)" , style="color:blue; font-family: Arial"),
                                            br(),
                                            plotOutput("expert_heat", height = "600px"),
                                            br(),
                                            h4("Expert Bar Charts" , style="color:blue; font-weight:bold%; font-family: Arial"),
                                            h6("Comparison of individual ranking scores" , style="color:blue; font-family: Arial"),
                                            br(),
                                            plotOutput("plot_bar", height = "600px"),
                                            br(),
                                            br()
                                            
                            ))),
                          
                          tabPanel(
                            h6("Download Results Files"),
                            br(),
                            br(),
                            downloadButton("saveBtn_raw", "PRESS HERE TO SAVE RAW DATA", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                            br(), 
                            br(),
                            downloadButton("saveBtn_reasons", "PRESS HERE TO SAVE EXPERT REASONS TEXT", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                            br(), 
                            br(), 
                            h4("Raw Data Table"),
                            br(),
                            tableOutput('raw_data'),
                            br(),
                            h4("Expert Reasons Text"),
                            br(),
                            tableOutput('reasons'),
                            br(),
                            br(),
                            br()
                            
                          ),
                          
                          tabPanel(""),
                          tabPanel(""),
                          tabPanel(""),
                          tabPanel(""),
                          tabPanel(""),
                          tabPanel(""),
                          tabPanel(""),
                          tabPanel(""),
                          tabPanel(""),
                          tabPanel(""),
                          tabPanel(""),
                          
                          tabPanel(
                            h6("Set up Input Files"),
                            
                            tabsetPanel(
                              
                              tabPanel(
                                h6("Upload file"),
                                br(),
                                br(),
                                h4("The data input (CSV) file requires two columns WITHOUT HEADINGS:"),
                                h4("Column 1 should contain the elements to be ranked and column 2 their explanations"),
                                br(),
                                br(),
                                fileInput("file1", "Choose CSV File"),
                                br(),
                                br(),
                                tableOutput('contents'),
                                br(),
                                br(),
                                actionButton("saveBtn_input", "PRESS HERE TO SEND INPUT FORM, THEN REFRESH BELOW - REQUIRES PASSWORD", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                                
                                shinyjs::useShinyjs(),
                                shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                                br(),
                                actionButton("refresh", "Refresh"),
                                br(),
                                br()
                                
                              ),
                              
                            ))))))))) 

#
############## SERVER CODE TO ACTION APP ###############


server <- function(input, output) {
  
  ## TO GET NEW INPUT FILE WITH NAMES TO RANK  
  mydata <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tbl <- read.csv(inFile$datapath, header=FALSE)
    return(tbl)
  })
  
  ## TO SHOW TABLE OF NEW INPUT NAMES FILE ##
  output$contents <- renderTable({
    mydata()
    
  })
  
  
  
  
  ### WRITE NEW FILE WITH NAMES TO SERVER
  observeEvent(input$saveBtn_input, {
    if(input$password==1234){
      write_csv(mydata(), paste0("input_ranking_terms_2", ".csv"))
      
      input_new <- as.data.frame(read_csv(paste0("input_ranking_terms_2", ".csv")))
      
      write_csv(input_new, "input_ranking_terms.csv")
    }
  }) 
  
  ## LOAD NAME INPUT FILE INTO RANKING TOOL
  
  input_data_names <- as.data.frame(read_csv("input_ranking_terms.csv"))
  input_data_names_2 <- sample(as.vector(noquote((input_data_names$V1))))
  labels <- unique(input_data_names_2)
  names_explained <- input_data_names
  colnames(names_explained) <- c("Item", "Explanation")
  
  output$table <- renderTable({
    names_explained 
  })
  
  ### LOAD TITLE INTO TAB ##
  
  output$title_in <- renderText({
    input_data_names$V3[1]
    
  })
  
  ## SET OUT THE LIST OF ITEMS TO BE RANKED
  
  output$sortable <- renderUI({
    rank_list(
      text = h5("Drag items into the desired order with most important at the top, then press red button to finalise selection and note any items you consider equal", style="color:blue; font-size:100%; font-family: Arial; text-align: center"), 
      labels = labels,
      input_id = "rank_list_basic"
    )
  })
  
  ## SET THE OUTPUT RANKED LIST 
  output$results_basic <- renderDataTable({
    (input$rank_list_basic )
  })
  
  
  ## SAVE PARTICIPANTS SELECTIONS
  # FIRST LINK TO THE SPECIFIC LIST OF SELECTIONS AVAILABLE FOR THIS EKE
  s <- strsplit(input_data_names$V3[1], " ")
  title_string <- sapply(s, function(x){
    toupper(paste(substring(x, 1, 1), collapse = ""))
  })
  
  my_string<-sort(c( as.vector(title_string) , as.vector(substr(labels, start = 1, stop = 1)), as.vector(substr(labels, start = min(nchar(labels)) , stop = min(nchar(labels)) ))))
  updated_string <- paste(my_string,collapse='')
  updated_string2 <-gsub(" ", "", updated_string)
  
  
  
  ### TO SET UP A DATATABLE FOR RANKINGS ##
  
  
  shinyInput <- function(FUN, ids, ...) {
    inputs <- NULL
    inputs <- sapply(ids, function(x) {
      inputs[x] <- as.character(FUN(inputId = x, label = NULL, ...))
    })
    inputs
  }
  
  
  observeEvent(input$finalise, {
    
    write.csv(input$rank_list_basic, paste0(input$filename, "_RNK2_" ,updated_string2, format(Sys.Date(), format="%b_%d_%y"), ".csv"))
    
    selected <- as.data.frame(read.csv(paste0(input$filename, "_RNK2_" ,updated_string2, format(Sys.Date(), format="%b_%d_%y"), ".csv")))
    colnames(selected) <- c("Your Ranking", "Items")
    selected_s <- selected[,c(2)]
    selected_s_reactive <- reactive(selected_s)
    
    # b) create dataframe with the checkboxes
    df_in <- reactive({
      
      df_in = data.frame(
        Items = selected_s,
        Ties = shinyInput(checkboxInput, selected_s),
        stringsAsFactors = FALSE
      )
      row.names(df_in) <- selected$`Your Ranking`
      df_in
    })
    
    # c) create the datatable
    
    output$my_datatable <- DT::renderDataTable(
      
      df_in(),
      escape = FALSE,
      selection = "none",
      rownames = TRUE,
      colnames=c("Items", "Check box if your item has EQUAL WEIGHTING to the item in the ROW ABOVE"),
      editable=FALSE,
      options = list(
        dom = 't', columnDefs = list(list(className = 'dt-center', targets = 0:2, width = '50px', targets = 2)),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      )
    )
    
  })
  
  ### 2. save rows when user hits submit -- either to new or existing csv ###
  observeEvent(input$saveBtn, {
    
    selected_p <- as.data.frame(read.csv(paste0(input$filename, "_RNK2_" ,updated_string2, format(Sys.Date(), format="%b_%d_%y"), ".csv")))
    colnames(selected_p) <- c("Your Ranking", "Items")
    selected_s_p <- selected_p[,c(2)]
    
    responses <- data.frame(user = input$filename,
                            activity = selected_s_p,
                            enjoy = sapply(selected_s_p, function(i) input[[i]], USE.NAMES = FALSE))
    
    write.table(responses, paste0(input$filename, "_RNK_t1" ,updated_string2, format(Sys.Date(), format="%b_%d_%y"), ".csv"), 
                col.names = FALSE, 
                row.names = FALSE,
                append = FALSE, 
                sep = ",")
    
    
    output$submitted <- renderText({
      ".... file submitted thank you"
    })
    
    write.csv(input$free_text, paste0(input$filename, "_reason_" ,updated_string2, format(Sys.Date(), format="%b_%d_%y"), ".csv"))
    
    
    ## sort ranking on ties ##
    
    ties_dat_in <- as.data.frame(read.csv(paste0(input$filename, "_RNK_t1" , updated_string2,format(Sys.Date(),  format="%b_%d_%y"), ".csv")))
    
    #ties_dat_in <- as.data.frame(read.csv("Expert2_RNK_t1eeiILMMoPRRRstttTJul_30_23.csv"))
    row_1 <- colnames(ties_dat_in)
    ties_dat_in <- rbind(row_1, ties_dat_in)
    colnames(ties_dat_in) <- c("name", "item", "tie")
    ties_dat_in$item <- gsub(".", " ", ties_dat_in$item, fixed=TRUE)
    ties_dat_in$tie <- gsub(".", " ", ties_dat_in$tie, fixed=TRUE)
    ties_dat_in$tie <- ifelse(ties_dat_in$tie == TRUE,1,0)
    ties_dat_in$rank <- as.numeric(rownames(ties_dat_in))
    
    ties_dat_in$new <- 0
    for(i in 2:nrow(ties_dat_in)){
      
      ties_dat_in$new[1] <- 1
      ties_dat_in$new[i] <- ifelse(ties_dat_in$tie[i]==0, ties_dat_in$new[i-1]+1, ties_dat_in$new[i-1])
      
    }
    
    ties_dat_in_final <- as.data.frame(ties_dat_in[,c(5,2)])
    ties_dat_in_final$ranking <-  rank(ties_dat_in_final$new,  ties.method = "average")
    ties_dat_in_final2 <- ties_dat_in_final[,2:3]
    
    write.csv(ties_dat_in_final2, paste0(input$filename, "_RNK_tt_" , updated_string2,format(Sys.Date(),  format="%b_%d_%y"), ".csv"))
    
    
  })
  
  
  
  ### Facilitator TABS CODE ##
  
  # SET USE OF A PASSWORD
  
  p_w <- reactive(input$password) 
  
  output$name_openend <- renderUI({
    req(input$password)
    if(input$password==1234){
      condition <- tags$div(
        tags$i(class = "fa fa-check")) # SHOWS TICK WHEN PASSWORD CORRECT
    } else{
      condition <- "PASSWORD INCORRECT"
    }
  })
  
  ## RETRIEVE FILES SUBMITTED BY PARTICIPANTS FROM SERVER AND ASSESS ##
  
  observeEvent(input$saveBtn2, {
    list_in <- as.data.frame(list.files(pattern="_RNK_tt_"))
    colnames(list_in)[1] <- "nme"
    files <- dplyr::filter(list_in, grepl(updated_string2,nme))
    files$name_included <- substr(files$nme,1,nchar(files$nme)-(21+ nchar(updated_string2)))
    
    files_all <- as.data.frame(files) ## FOR THE LATER SERVER VIEWING OF FILES
    colnames(files_all) <- c("FILE CODE_DATE", "Participant")
    
    db_all <- data.frame()
    for( i in 1:nrow(files)){
      test <- read_csv(files$nme[i])
      test$name <- files$nme[i]
      test$name <- gsub(paste0("RNK_",updated_string2),'',test$name)
      test$rank <- seq(1, nrow(test),1)
      db_all <- rbind(db_all, test)
    }
    
    
    db_all <- db_all[,c(3,2,4)]
    colnames(db_all) <- c("rank", "name", "Participant")
    db_all_out <<- db_all
    
    #print(db_all,n=30)
    
    ## THE FOLLOWING ALL CONDITIONAL ON PASSWORD BEING ENTERED    
    
    if(p_w() == 1234){
      
      output$info <- renderText({
        paste0(nrow(files)," input files included in graph" )
      })
      
      
      output$done <- renderText({
        paste0(nrow(files)," participants data received" )
      })
      
      
      
      ## BOXPLOT OF DISTRIBUTIONS AND SUMMARY TABLE OF SELECTIONS IN RESULTS TAB
      
      output$plot1 <- renderPlot({
        ggplot(db_all, aes(y=reorder(name, -rank, FUN=median), x=rank))+
          geom_boxplot()+
          xlab("Ranking Scores (1 represents the highest rank)")+
          ylab("")+
          ggtitle("Expert's Ranking of Items")+
          theme(axis.text=element_text(size=14),plot.title = element_text( size=18),plot.margin = margin(0.5,1,0.3,1, "cm"))+
          scale_x_continuous(breaks=c(seq(0,length(labels),1)))
        
      })
      
      rank_gp <- reactive({data.frame(db_all %>%
                                        group_by(name)%>%
                                        summarise(median_rank = median(rank), mean_rank = mean(rank))%>%
                                        arrange(median_rank))
      })
      
      
      output$table1 <- renderTable({
        rank_gp()
      })
      
      ## TABLE OF NAMES PARTICIPANTS SUBMITTED
      
      names_done <- reactive({
        df_name <- as.data.frame(t(files$name_included))
        names(df_name) <- NULL
        df_name
      })
      
      output$table2 <- renderTable({
        names_done()
      })
      
      output$filter_name <- renderUI({
        
        selectInput("filter_name", "Filter by expert", c( files$name_included ))
        
      })
      
      db_rad <- data.frame()
      names_rad <- unique(db_all$Participant)
      
      if(length(names_rad)==1){
        
        dat_in_1 <- db_all
        dat_in <- t(dat_in_1[, 1:2])
        colnames(dat_in) <- dat_in[2,]
        new_order = sort(colnames(dat_in))
        dat_in_final <- as.data.frame(t(dat_in[1, new_order]))
        dat_in_final[1, ] <- as.numeric(dat_in_final[1,])
        db_rad <- as.data.frame(rbind(db_rad,dat_in_final))
        db_rad[1,] <- as.numeric(db_rad[1,])
        
      }else{
        
        for(i in 1:length(names_rad)){
          dat_in_1 <- db_all %>% filter(Participant==names_rad[i])
          dat_in <- t(dat_in_1[, 1:2])
          colnames(dat_in) <- dat_in[2,]
          new_order = sort(colnames(dat_in))
          dat_in_final <- as.data.frame(t(dat_in[1, new_order]))
          dat_in_final[1, ] <- as.numeric(dat_in_final[1,])
          db_rad <- rbind(db_rad,dat_in_final)
          
        }
        db_rad <- as.data.frame(sapply(db_rad, as.numeric))
      }
      
      
      
      
      max_rad <- as.data.frame(rep(length(labels), length(labels)))
      min_rad <- as.data.frame(rep(1,length(labels)))
      df_mm <- as.data.frame(t(cbind(min_rad,max_rad)))
      colnames(df_mm) <- new_order
      
      db_rad_plot <- rbind(df_mm,db_rad )
      db_rad_plot<- as.data.frame(sapply(db_rad_plot, as.numeric))
      
      qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
      colours_in <-  brewer.pal.info[brewer.pal.info$category == 'qual',]
      col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
      col_vector <- col_vector[-4]
      
      create_beautiful_radarchart <- function(data, color = col_vector[1:length(names_rad)], 
                                              vlabels = colnames(data), vlcex = 0.7,
                                              caxislabels = NULL, title = NULL, ...){
        radarchart(
          data, axistype = 1,
          # Customize the polygon
          pcol = color, pfcol = NULL, plwd = 2, plty = 1,
          # Customize the grid
          cglcol = "grey", cglty = 1, cglwd = 0.8,
          # Customize the axis
          axislabcol = "grey", 
          # Variable labels
          vlcex = 1, vlabels = vlabels,
          caxislabels = caxislabels, title = title, ...
        )
      }
      
      output$plot_radar <- renderPlot({
        
        op <- par(mar = c(.1, .1, .1, 1))
        create_beautiful_radarchart(db_rad_plot, caxislabels = c(length(labels),"","","",1))
        par(op)
        legend(x=1.5, y=1, legend = c(files$name_included), bty = "n", pch=20 , col=col_vector[1:length(names_rad)] , text.col = "black", cex=1.1, pt.cex=2.5)
        
      })
      
      react_name <- reactive({
        input$filter_name
      })
      
      ### PREP FOR PLOT ##
      
      mycol <- rgb(0, 0, 255, max = 255, alpha = 5, names = "blue50")
      mycol1 <- rgb(0, 0, 255, max = 255, alpha = 0, names = "blue50")
      
      create_mean_radarchart <- function(data, color = col_vector[1:length(names_rad)], 
                                         vlabels = colnames(data), vlcex = 0.7,
                                         caxislabels = NULL, title = NULL, ...){
        radarchart(
          data, axistype = 1,
          # Customize the polygon
          pcol = color, pfcol = c(mycol1, mycol), 
          plwd = 2, plty = 1,
          # Customize the grid
          cglcol = "grey", cglty = 1, cglwd = 0.8,
          # Customize the axis
          axislabcol = "grey", 
          # Variable labels
          vlcex = 1, vlabels = vlabels,
          caxislabels = caxislabels, title = title, ...
        )
      }
      
      dat_rad_exp <- db_rad_plot
      dat_rad_exp_only <- dat_rad_exp[3:nrow(dat_rad_exp),]
      col_means <- t(colMeans(dat_rad_exp_only))
      dat_rad_exp2 <- rbind(dat_rad_exp, col_means)
      
      
      t_dat_rad_exp3 <- as.data.frame(t(dat_rad_exp2[,1:(ncol(dat_rad_exp2))]))
      colnames(t_dat_rad_exp3)[ncol(t_dat_rad_exp3)] <- "mean"
      sort_t_dat_rad_exp2 <-t_dat_rad_exp3 %>% arrange(desc(mean))
      sort_t_dat_rad_exp2 <- rbind(sort_t_dat_rad_exp2[nrow(sort_t_dat_rad_exp2),],sort_t_dat_rad_exp2[1:(nrow(sort_t_dat_rad_exp2)-1),])
      dat_rad_exp4 <- as.data.frame(t(sort_t_dat_rad_exp2))
      dat_rad_exp4$name <- c("Min", "Max", files$name_included, "Mean")
      
      
      observeEvent(input$filter_name, {
        
        
        output$plot_radar2 <- renderPlot({
          
          
          dat_rad_exp_plot2 <- dat_rad_exp4[(dat_rad_exp4$name == react_name() | dat_rad_exp4$name == "Max" | dat_rad_exp4$name == "Min" | dat_rad_exp4$name == "Mean"),]
          
          op <- par(mar = c(.1, .1, .1, 1))
          create_mean_radarchart(dat_rad_exp_plot2[,-ncol(dat_rad_exp_plot2)], caxislabels = c(length(labels),"","","",1))
          par(op)
          legend(x=1.5, y=1, legend = c(react_name(), "Mean"), bty = "n", pch=20 , col=col_vector[1:length(names_rad)] , text.col = "black", cex=1.1, pt.cex=2.5)
          
          
        })
        
      })
      
      
      dat_bar_plot <- dat_rad_exp4[3:nrow(dat_rad_exp4),]
      melt_dat_bar_plot <- melt(dat_bar_plot)
      melt_dat_bar_plot$new_val <- 10- melt_dat_bar_plot$value
      vec<- c(seq(length(labels), 1,-1))
      vec1 <- dput(as.character(vec))
      
      output$plot_bar <- renderPlot({
        
        
        ggplot(melt_dat_bar_plot, aes(x=reorder(variable, new_val), y=new_val)) +
          geom_bar(stat="identity", fill="pink", width=0.6) +
          coord_flip() +
          xlab("")+
          ylab("Ranking")+
          theme_minimal()+
          theme(axis.text=element_text(size=12),strip.text = element_text(
            size = 14))+
          scale_y_continuous(breaks=seq(1, length(labels), 1),labels=vec1)+
          facet_wrap(~factor(name,levels=c('Mean',dat_bar_plot$name[1:(length(dat_bar_plot$name)-1)])))
      })
      
      ### TO CALCULATE EXPERT DIFFERENCES FROM MEAN ###
      
      if(length(names_rad)==1){
        output$expert_heat <- NULL
        
      } else{
        
        dat_diffs_calc <- as.data.frame(dat_rad_exp4[3:nrow(dat_rad_exp4),1:(ncol(dat_rad_exp4)-1)])
        dat_diffs <- as.data.frame(t(dat_diffs_calc))
        colnames(dat_diffs) <- c(dat_bar_plot$name)
        str(dat_diffs)
        
        df_diffs_1 <- data.frame()
        for(i in 1:(nrow(dat_diffs))){
          
          scaled_row <-t(apply(dat_diffs[i, 1:(ncol(dat_diffs)-1)], 1, scale))
          
          scaled_row[is.nan(scaled_row)] <- 0
          
          df_diffs_1 <- rbind(df_diffs_1, scaled_row)
          
        }
        
        colnames(df_diffs_1) <- colnames(dat_diffs[1:(ncol(dat_diffs)-1)])
        rownames(df_diffs_1) <- rownames(dat_diffs)
        df_diffs_2 <- as.matrix(df_diffs_1)
        
        output$expert_heat <- renderPlot({
          
          
          class(df_diffs_2)
          par(mar=c( 2, 14,14,4)) # adapt margins
          # omit all borders
          plot(df_diffs_2,  breaks= c( -3, -1.5, -1, 0, 1, 1.5, 3), axis.col=list(side=3, las=2, cex.axis=0.9), 
               axis.row = list(side=2, las=1, cex.axis=0.9), xlab="", ylab="", main="",
               text.cell=list(cex=0.9),fmt.cell='%.2f', digits=2, col= c("#FF9999","#FFCC99", "white", "white","#FFCC99", "#FF9999" ))
          
          
        }) 
        
      }
      ####### RETRIEVE REASONING TEXT FORM EXPERTS ###
      
     
      observeEvent(input$saveBtn2, {
        list_in_r <- as.data.frame(list.files(pattern="_reason_"))
        colnames(list_in_r)[1] <- "nme"
        files_r <- dplyr::filter(list_in_r, grepl(updated_string2,nme))
        files_r$name_included <- substr(files_r$nme,1,nchar(files_r$nme)-(18+ nchar(updated_string2)))
        
       db_all_r <- data.frame()
        for( i in 1:nrow(files_r)){
          test <- read_csv(files_r$nme[i])
          test$name <- files_r$name_included[i]
          test$name <- gsub(paste0("RNK_",updated_string2),'',test$name)
          db_all_r <- rbind(db_all_r, test)
        }
        
        
        colnames(db_all_r)[2:3] <- c("Expert Reasoning Text", "Name")
        db_text_out <<- as.data.frame(db_all_r[,2:3])
        
        
        
      })
      
      
      
    } ## END CONDITIONAL PASSWORD SECTION
  }) 
  
  ## TABLE OF RAW DATA
  output$raw_data<- renderTable({
    as.data.frame(db_all_out)
  })
  
  ### TABLE OF REASONING ##
  
  output$reasons<- renderTable({
    db_text_out
  })
  
  ## SAVING OF RAW DATA FROM ALL PARTICIPANTS #
  
  output$saveBtn_raw <- downloadHandler(
    filename = function() {
      paste("Raw_ranking_data", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(db_all_out, file)
    }
  )
  
  observeEvent(input$refresh, {
    shinyjs::js$refresh_page()
  })
  
  
  ########## TO CAPTURE TEXT INPUT FROM REASONING ###
  
  output$reason_text <- renderDataTable({
    (input$free_text)
  })
  
  
  
  ## DOWNLOAD REASONING TEXT ##
  
  
  output$saveBtn_reasons <- downloadHandler(
    filename = function() {
      paste("Reasoning_data", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(db_text_out, file)
    }
  )
  
  
}


shinyApp(ui, server)



