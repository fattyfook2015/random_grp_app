## app.R ##
library(shinydashboard)
library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "Random Allocation Dashboard"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Importing Data", tabName = "Importing_d", icon = icon("dashboard")),
      menuItem("Uploaded Data View", icon = icon("th"), tabName = "up_data"),
      menuItem("Sample Size Generator", icon = icon("th"), tabName = "Random_s"),
      menuItem("Random Group allocator", icon = icon("th"), tabName = "random_grp")
    )
  ),
  
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Importing_d",
              h2("Importing Data Page"),
              # br(),
              h4("Step 1: Please download the sample file and replace the contents with your name list, do not touch replace the column name."),
              h4("Step 2: Please upload your file on the 2nd box."),
              h4("Step 3: From the 2 value boxes on the left to see if the uploaded rows and if there is any duplication."),
              h4("Step 4: Proceed to 2nd tab to view your uploaded data."),
              h4("Step 5: Proceed to 3rd tab for random size needed to generate."),
              
              # downloadButton('downloadData', 'Download'),
              # DT::dataTableOutput('table1'),
              
              # let user download sample file
              fluidRow(
                # first column  lyaout
                box(
                  title = "Sample Data download",
                    downloadButton('downloadsample', 'Download_sample')
                  ),
                
                box(
                  title = "Upload Your file",
                  fileInput("file1", "choose your csv",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                            )
                  )
                ),

          
              fluidRow(
                # sample data view
                box(
                  title = "Sample Data View",
                    DT::dataTableOutput('table1')
                  ),
                
                # to show no. of rows loaded from user. 
                valueBoxOutput("rowbox"),
                
                # to show if there is any duplicatied rows 
                valueBoxOutput("dup_box")

                )
              
      ),
      
      # 2nd tab content
      tabItem(tabName = "up_data",
              h2("Your uploaded Data View"),
              h4("This is the page to view your uploaded data"),
              DT::dataTableOutput('table3')
              ),
      
      # 3rd tab content
      tabItem(tabName = "Random_s",
              h2("Random Sample Size Generator for your Contact List"),
              h4("This Page allow you to select the sample size required (random)."),
              h4("Step 1: Choose the required sample size required and press the submit button."),
              h4("Step 2: It will generate for you the required sample size and display the results."),
              h4("Step 3: You can download the generated file."),
              
              # allow user to set the number of sample size required
              fluidRow(
                box(
                  title = "Choose your sample size required",
                  uiOutput("size_control"),
                  submitButton("Submit")
                ),
                
                # to show number of size choosen
                valueBoxOutput("req_size_box")
              ),
              
              # to show e generated data
              fluidRow(
                box(
                  title = "your generated data",
                  DT::dataTableOutput('req_table')
                ),
                # to download the generated data
                box(
                  title = "Download Generated Data",
                  downloadButton('download_g', 'Download_Data')
                )
              )
      ), 
      
      # 4th tab content
      tabItem(tabName = "random_grp",
              h2("Random Group Allocator"),
              h4("This Page allow you to random group your contact list"),
              h4("Step 1: Select the number of groups to randomise your contact list and press submit button"),
              h4("Step 2: It will generate for you the random group allocation and display the results."),
              h4("Step 3: You can download the generated file."),
              
              # allow user to set the number of group required
              fluidRow(
                box(
                  title = "Choose your sample size required",
                  uiOutput("grp_control"),
                  submitButton("Submit")
                ),
                # to show number of groups selected
                valueBoxOutput("req_grp_box"),
                
                # to show if this is whole number
                valueBoxOutput("whole_box"),
                
                # to download the generated data
                box(
                  title = "Download Generated Grouping",
                  downloadButton('download_grp', 'Download_Groups')
                )
              ),
              
              # to show e generated data
              fluidRow(
                box(width = 12, 
                  title = "You generated Grouping",
                  DT::dataTableOutput('grp_table')
                )
              )
              

      )
      
    )
    
  )
)

      

server <- function(input, output) {
  library(dplyr)
  
  #loading the sample data for testing purpose
  s_data <- read.csv("data/sample.csv", header = TRUE)
  
  
  #printing sample data view
  output$table1 <- DT::renderDataTable(
    DT::datatable(s_data, options = list(pageLength = 10))
  )
  
  # importing user's upload data
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)){
      return(NULL)      
    }
    read.csv(infile$datapath)
  })
  
  # if there uploaded data, print the no.of.row, or else say "no data uploaded"
  up_row <- reactive({
    df <- filedata()
    if (is.null(df)) return(0)
    uprow1 = dim(df)[1] # this will indicate the number of rows
  })

  # if there duplicated data, print the no.of.row, or else say "no data uploaded"
  dup_name <- reactive({
    df <- filedata()
    if (is.null(df)) return("no data uploaded")
    if (dim(df)[1] > length(unique(df[,1]))) {
      return(dim(df)[1] - length(unique(df[,1])))
    } else {
      return("No")
    } # this will indicate the number of rows
  })
  
  # to get a list of number from 1 to no. of row uploaded
  sample_seq <- reactive({
    df <- filedata()
    if (is.null(df)) return(0)
    n_seq <- seq(1: dim(df)[1])
  })
  
  # render no. of rows 
  output$rowbox <- renderValueBox({
    valueBox(
      paste0(up_row()), "Names uploaded", icon = icon("bars"),
      color = "purple"
    )
  })
  
  # render no. of dup rows 
  output$dup_box <- renderValueBox({
    valueBox(
      paste0(dup_name()), "duplicated names", icon = icon("check"),
      color = "red"
    )
  })
  
  # dynamic slider for sample size
  output$size_control <- renderUI({
    maxvalue1<-ifelse(up_row() == 0, 1, up_row())
    sliderInput("req_size", "required samples size", min =1, max = maxvalue1, value =1, step =1)
  })
  
  # render no. sample size
  output$req_size_box <- renderValueBox({
    valueBox(
      paste0(input$req_size), "sample size required",
      color = "green"
    )
  })
  
  #generate random number and the required dataframe.
  r_size_df <- function(dataframe1, requiredsize){
    final_df <-if(is.null(dataframe1) | is.null(requiredsize)) {
      join_d <- data.frame()
      return(join_d)
    } else {
      dataframe1["id"] = seq(1:dim(dataframe1)[1])
      g_id <- sample(1: dim(dataframe1)[1], requiredsize, replace = FALSE)
      n_g_df <- as.data.frame(g_id)
      join_d <- left_join(n_g_df, dataframe1, by = c("g_id" = "id"))  
      return(dplyr::arrange(join_d, g_id))
      }
  }
  
  # dynamic slider for number of group
  output$grp_control <- renderUI({
    maxvalue1<-ifelse(up_row() == 0, 1, up_row())
    sliderInput("req_group", "number of groups", min =1, max = maxvalue1, value =1, step =1)
  })
  
  # render no. of groups
  output$req_grp_box <- renderValueBox({
    valueBox(
      paste0(input$req_group), "number of groups",
      color = "aqua"
    )
  })
  
  # function to check remainder after choosin the group
  check_remainder <- function(dataframe2, requiredgrp){
    final_r <- if(is.null(dataframe2) | is.null(requiredgrp)){
      return("No data")
    } else if (dim(dataframe2)[1] %% requiredgrp > 0){
      return(dim(dataframe2)[1] %% requiredgrp)
    } else {
      return(0)
    }
  }
  
  # function to create the required groupings
  grouping_fn <- function(dataframe3, requiredgrp){
    got_remainder <- check_remainder(dataframe3, requiredgrp)
    
    final_df_g <- if(got_remainder == "No data"){
      df_g_f <- data.frame()
      return (df_g_f)
    } else if (got_remainder >0){
      v_g <- c(as.vector(dataframe3[,1]), rep("", got_remainder))
      df_g_f <- as.data.frame(matrix(sample(v_g), ncol = requiredgrp))
      return(df_g_f)
    } else {
      v_g2 <- as.vector(dataframe3[,1])
      df_g_f2 <- as.data.frame(matrix(sample(v_g2), ncol = requiredgrp))
      return(df_g_f2)
    }
  }
  
  # render if there if is a whole number after divided by e no. of groups
  output$whole_box <- renderValueBox({
    grp_value <- check_remainder(filedata(), input$req_group)
    grp_msg <- ifelse(grp_value == "No data", "No Data loaded", ifelse(grp_value == 0, "Perfect", "Unbalance"))
    valueBox(
      paste0(grp_msg), "Grouping", 
      color = "black"
    )
  })
  
  # render user's uploded data
  output$table3 <- DT::renderDataTable(
    DT::datatable(filedata(), options = list(pageLength = 10))
  )
  
  # render generated sample size data
  output$req_table <- DT::renderDataTable(
    DT::datatable(r_size_df(filedata(), input$req_size), options = list(pageLength = 10))
  )  
  
  # render generated grouping data
  output$grp_table <- DT::renderDataTable(
    DT::datatable(grouping_fn(filedata(), input$req_group), options = list(pageLength = 10))
  )  
  
  # user download sample file
  output$downloadsample <- downloadHandler(
    filename = function() { paste("sample", '.csv', sep='') },
    content = function(file) {
      write.csv(s_data, file, row.names = FALSE)
    } 
  )
  
  # user download generated randome size file
  output$download_g <- downloadHandler(
    filename = function() { paste("generated_data", '.csv', sep='') },
    content = function(file) {
      write.csv(r_size_df(filedata(), input$req_size), file, row.names = FALSE)
    } 
  )
  
  # user download generated grouping file
  output$download_grp <- downloadHandler(
    filename = function() { paste("generated_grouping", '.csv', sep='') },
    content = function(file) {
      write.csv(grouping_fn(filedata(), input$req_group), file, row.names = FALSE)
    } 
  )
}

shinyApp(ui, server)


