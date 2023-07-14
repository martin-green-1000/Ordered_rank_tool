



library(shiny)
library(sortable)
library(readr)
library(readxl)
library(tidyverse)
library(bslib)
library(shinydashboard)
library(fontawesome)
library(shinyFiles)
library(shinyjs)
library(fmsb)
library(RColorBrewer)
library(reshape2)



############### USER INTERFACE CODE ###############

ui <- dashboardPage(
  
  dashboardHeader(titleWidth=920, title = span(" ", span("Ordered Ranking Tool for EKE", style = "color: WHITE; font-size: 28px")),
                  tags$li(class = "dropdown", tags$a(HTML(paste("Click horizontal lines to see explanations ", textOutput("See explanations "))),style = "color: silver; font-size: 14px;font-family: Arial;"))
  ),
  
  dashboardSidebar(collapsed = TRUE, width = 550,
                   h4("Explanations of Items",style="color:white"),
                   id=h4(tableOutput("table"),style ="line-height: 3px;font-size:13px;")
  ),
  
  
  dashboardBody(
    
    fluidRow(column(width = 10, offset = 1,height = 2,
                    h6(textInput("filename",  em("Input your name here"), value = ("")),style="background-color:#ff8282;color:white; border-color:#ff8282;padding:2px; font-size:120%; font-family: Arial;"),
                    
                    tabsetPanel(
                      tabPanel(
                        h4("Items to Rank", style="background-color:white;color:blue;font-weight: bold;  font-size:100%; font-family: Arial"),
                        br(),
                        uiOutput("sortable", style =  " line-height: 5px;font-weight: bold;font-size:16px;font-family: Arial")
                      ),
                      
                      tabPanel(
                        actionButton("saveBtn", "Submit", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                        br(),
                        br(),
                        tags$head(tags$style(HTML(".myclass3 pre {color: blue;font-size:150%; background-color: lightgrey ;border-colour:lightgrey;  font-weight: bolder;font-size: 12px;font-family: Arial; width: 200px; }"))),
                        div(class = "myclass3",verbatimTextOutput("submitted", placeholder = FALSE)),
                        
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
                      tabPanel(""),
                      
                      tabPanel(
                        h6("Moderator", style="background-color:white;font-size:75%; font-family: Arial"),
                        
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
                                            h5(verbatimTextOutput("info", placeholder = FALSE)) ,
                                            tags$head(tags$style("#info{border-color:#6BADCE;;background-color:#6BADCE;color: white; font-size: 14px;font-weight: bold;font-family: Arial;}")),
                                            plotOutput("plot1", width = "100%"),
                                            br(),
                                            tableOutput('table1'),
                            ))),
                          
                          tabPanel(
                            h6("Plots of expert variation"),
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
                            br(), 
                            h4("Raw Data Table"),
                            br(),
                            tableOutput('raw_data')
                            
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
                                actionButton("saveBtn_input", "PRESS HERE TO SEND INPUT FORM - REQUIRES PASSWORD UNDER COLLECTION TAB", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                                br(),
                                br(),
                                useShinyjs(),
                                actionButton("refresh", "Then refresh here", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                                br(),
                                
                              ),
                              tabPanel(
                                h6("Server file viewer"),
                                br(),
                                br(),
                                h4("Table of all App files on the server"),
                                br(),
                                br(),
                                tableOutput('files_all'),
                                br(),
                                numericInput("nmb_del","Enter row number of file to delete, then press delete button below.... App will refresh in a few seconds", "" ),
                                br(),
                                actionButton("del_one", "Delete row of table", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                                br(),
                                br()
                                
                              )
                              
                            ))))))))) 


############## SERVER CODE TO ACTION APP ###############


server <- function(input, output,session) {
  
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
  
  input_ranking_terms <- as.data.frame(read_csv("input_ranking_terms.csv"))
  
  ### WRITE NEW FILE WITH NEW NAMES TO SERVER
  
  observeEvent(input$saveBtn_input, {
    if(input$password==1234){
      write_csv(mydata(), paste0("input_ranking_terms_2", ".csv"))
      
      input_new <- as.data.frame(read_csv(paste0("input_ranking_terms_2", ".csv")))
      
      write_csv(input_new, "input_ranking_terms.csv")
    }
  }) 
  
  observeEvent(input$refresh, {
    refresh()
  })
  
  ## LOAD NAME INPUT FILE INTO RANKING TOOL
  
  input_data_names <- as.data.frame(read_csv("input_ranking_terms.csv"))
  input_data_names_2 <- sample(as.vector(noquote((input_data_names$V1))))
  labels <- unique(input_data_names_2)
  names_explained <- input_data_names
  
  ## SET OUT THE LIST OF ITEMS TO BE RANKED
  
  output$sortable <- renderUI({
    rank_list(
      text = h5("Drag items into the desired order with most important at the top, press 'Submit' button when finalised", style="color:blue; font-size:100%; font-family: Arial; text-align: center"), 
      labels = labels,
      input_id = "rank_list_basic"
    )
  })
  
  ## SET THE OUTPUT RANKED LIST 
  
  output$results_basic <- renderDataTable({
    (input$rank_list_basic )
  })
  
  ##
  
  output$table <- renderTable({
    names_explained 
  })
  
  ## SAVE PARTICIPANTS SELECTIONS
  # FIRST LINK TO THE SPECIFIC LIST OF SELECTIONS AVAILABLE FOR THIS EKE
  
  my_string<-sort(c(as.vector(substr(labels, start = 1, stop = 1)), as.vector(substr(labels, start = min(nchar(labels)) , stop = min(nchar(labels)) ))))
  updated_string <- paste(my_string,collapse='')
  updated_string2 <-gsub(" ", "", updated_string)
  
  
  observeEvent(input$saveBtn, {
    
    write_csv(as.data.frame(input$rank_list_basic), paste0(input$filename, "_RNK_" ,updated_string2, format(Sys.Date(), format="%b_%d_%y"), ".csv"))
    
    
    
    dat_to_save <- as.data.frame(read_csv(paste0(input$filename, "_RNK_" ,updated_string2, format(Sys.Date(), format="%b_%d_%y"), ".csv")))
    dat_to_save$rank <- seq(1,nrow(dat_to_save),1)
    colnames(dat_to_save)[1] <- input$filename
    write_csv(dat_to_save, paste0(input$filename, "_expert_answers_",format(Sys.Date(), format="%b_%d_%y"), ".csv"))
    
    output$submitted <- renderText({
      ".... file submitted thank you"
    })
    
  })
  
  ### MODERATOR TABS CODE ##
  
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
    
    list_in <- as.data.frame(list.files(pattern="*.csv"))
    colnames(list_in)[1] <- "nme"
    files <- dplyr::filter(list_in, grepl(updated_string2,nme))
    files$name_included <- substr(files$nme,1,nchar(files$nme)-(18+ nchar(updated_string2)))
    
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
    
    colnames(db_all) <- c( "name", "Participant","rank")
    db_all_out <<- db_all
    
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
      }, digits = 1)
      
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
      
      for(i in 1:length(names_rad)){
        dat_in_1 <- db_all %>% filter(Participant==names_rad[i])
        dat_in <- t(dat_in_1[, c(3)])
        colnames(dat_in) <- dat_in_1$name
        new_order = sort(colnames(dat_in))
        dat_in_final <- as.data.frame(t(dat_in[1, new_order]))
        dat_in_final[1, ] <- as.numeric(dat_in_final[1,])
        db_rad <- rbind(db_rad,dat_in_final)
        
      }
      db_rad <- as.data.frame(sapply(db_rad, as.numeric))
      
      max_rad <- as.data.frame(rep(length(labels), length(labels)))
      min_rad <- as.data.frame(rep(1,length(labels)))
      df_mm <- as.data.frame(t(cbind(min_rad,max_rad)))
      colnames(df_mm) <- new_order
      
      db_rad_plot <- rbind(df_mm,db_rad )
      
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
      
      observeEvent(input$filter_name, {
        
        
        output$plot_radar2 <- renderPlot({
          
          
          
          dat_rad_exp <- db_rad_plot
          dat_rad_exp_only <- dat_rad_exp[3:nrow(dat_rad_exp),]
          col_means <- t(colMeans(dat_rad_exp_only))
          dat_rad_exp2 <- rbind(dat_rad_exp, col_means)
          dat_rad_exp2$name <- c("Min", "Max", files$name_included, "Mean")
          
          dat_rad_exp_plot2 <- dat_rad_exp2[(dat_rad_exp2$name == react_name() | dat_rad_exp2$name == "Max" | dat_rad_exp2$name == "Min" | dat_rad_exp2$name == "Mean"),]
          
          op <- par(mar = c(.1, .1, .1, 1))
          create_beautiful_radarchart(dat_rad_exp_plot2[,-ncol(dat_rad_exp_plot2)], caxislabels = c(length(labels),"","","",1))
          par(op)
          legend(x=1.5, y=1, legend = c(react_name(), "Mean"), bty = "n", pch=20 , col=col_vector[1:length(names_rad)] , text.col = "black", cex=1.1, pt.cex=2.5)
          
          
        })
        
      })
      
      output$plot_bar <- renderPlot({
        
        dat_bar_plot <- db_rad_plot[3:nrow(db_rad_plot),]
        dat_bar_plot$name <- files$name_included
        melt_dat_bar_plot <- melt(dat_bar_plot)
        ggplot(melt_dat_bar_plot, aes(x=variable, y=value)) +
          geom_bar(stat="identity", fill="pink", width=0.6) +
          coord_flip() +
          xlab("")+
          theme_minimal()+
          theme(axis.text=element_text(size=12),strip.text = element_text(
            size = 14))+
          facet_wrap(~name)
      })
      
      
      
      ## FILE VIEWER FOR MODERATOR
      
      files_all$Row <- seq(1,nrow(files_all),1)
      files_all <- files_all[,c(3,2,1)]
      observeEvent(input$del_one, {
        
        file.remove(paste0(files_all$`FILE CODE_DATE`[input$nmb_del]))
        
        session$reload()
        return()
      })
      
      output$files_all<- renderTable({
        
        files_all
      }, digits = 0)
      
      
    } ## END CONDITIONAL PASSWORD SECTION
  }) 
  
  ## TABLE OF RAW DATA
  output$raw_data<- renderTable({
    db_all_out
  }, digits = 0)
  
  ## SAVING OF RAW DATA FROM ALL PARTICIPANTS #
  
  output$saveBtn_raw <- downloadHandler(
    filename = function() {
      paste("Raw_ranking_data", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(db_all_out, file)
    }
  )
}


shinyApp(ui, server)



