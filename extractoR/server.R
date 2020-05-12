##################################
# Alexander Kutschera 2020/04/21 #
# alexander.kutschera@gmail.com  #
#     §§  CC-BY-SA 4.0  §§       #
Version = 0.1          
##################################

source("fun_extracto.R") # load extractor functions

library(dplyr)
library(lubridate)
library(reshape)
library(ggsci)
library(gridExtra)
library(grid)
library(ggplot2)
library(ggpubr)
library(stringr)
library(shiny)
         
# Settings 
## Analysis and data preparation
thresh <<- 0.1  # set and change the thresh
remove_Cy5 <- TRUE # remove the Cy5 channel
use_samplename <- TRUE # use sample names to get info about the treatment

#### Naming convention! ####
# separate infos with underscores: ID_concentration_treatment1_treatment2
# e. g. DH12345_100µl_old-protocol_95°C-10min
# for now the script extracts the ID by using everything until the first underscore.
# If you want to record concentrations in the format of 10^-3 write 10e-3 rather than 10-3!
# Keep this in mind when naming your samples


shinyServer(function(input, output) {
   
  calculate_dataset <- reactive({
    file <- input$data_file
    #file <- "test.dat"
    if (is.null(file)) return(NULL)
    data <- read_dat_neo(file$datapath) # read the data file
    results <- extract_info(data$no_temp, thresh, file$name, use_samplename, remove_Cy5)
    
    output <- list("data" = data,
                   "results" = results)
    
    return(output)
  })
  
  plot <- reactive({
    plot_data <- calculate_dataset()
    if (is.null(plot_data)) return(NULL)
    
    graph <- create.plot(plot_data$data, plot_data$results, input$data_file$name, thresh = thresh, remove_Cy5 = TRUE)
    return(graph)
  })
  
  table <- reactive({
    plot_data <- calculate_dataset()
    if (is.null(plot_data)) return(NULL)
    table <- create.table(plot_data$results)
    ggtable <- ggtexttable(table, rows = NULL, 
                           theme = ttheme("mCyan"))
    
    return(ggtable)
  })
  
  output$results_table <- DT::renderDataTable({
    
    DT::datatable(calculate_dataset()$results, options = list(orderClasses = TRUE))
  })
  
  output$plot_output <- renderPlot({
    graph <- plot()
    if (is.null(graph)) return(NULL)
    show(graph)
  })
#  output$table_output <- renderPlot({
#    graph <- table()
#    if (is.null(graph)) return(NULL)
#    show(graph)
#  })
  
  output$ui.downloaddata <- renderUI({
    #    if (is.null(calculate())) 
    #      return(NULL)
    downloadButton('downloadData', 'Download Data', class = "dbutton")
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {paste0(gsub(unlist(strsplit(input$data_file$name, "[.]")[1])[-1], "", input$data_file$name), "csv")},
    content = function(file) {
      data <- calculate_dataset()
      fname <- paste0(gsub(unlist(strsplit(input$data_file$name, "[.]")[1])[-1], "", input$data_file$name), "csv")
      write.csv2(data$results, fname)
      file.rename(fname,file)
    }
  )
  
  output$ui.downloadplot <- renderUI({
    #    if (is.null(calculate()))
    #      return(NULL)
    downloadButton('downloadPlot', 'Download Plot', class = "dbutton")
  })
  output$downloadPlot <- downloadHandler(
    filename = function() {paste0(gsub(unlist(strsplit(input$data_file$name, "[.]")[1])[-1], "", input$data_file$name), "pdf")},
    content = function(file) {
      plot <- plot()
      ggtable <- table()
      
      pdf(file, width = 21.0, height = 29.7, paper = "a4")
      
      fig <- ggarrange(plot, ggtable, ncol = 1, nrow = 2)
      fig <- annotate_figure(fig, right = fig,bottom = paste0("Analysed with the ExtractoR App Version ", Version, " (function v", fun_version, "), Alexander Kutschera"))
      print(fig)
      
      dev.off()
      
    }
  )
})


