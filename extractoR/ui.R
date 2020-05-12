##################################
# Alexander Kutschera 2020/04/21 #
# alexander.kutschera@gmail.com  #
#     §§  CC-BY-SA 4.0  §§       #
Version = 0.1          
##################################


library(shiny)
library(shinydashboard)

dashboardPage(skin = "yellow",
              dashboardHeader(
                title = paste0("ExtractoR App v",Version)
              ),
              
              dashboardSidebar(
                fileInput("data_file", label = h4("Data file input"), accept = c(".dat")),
                #checkboxGroupInput("plot_options", label = h4("Plot Options"), 
                #                   choices = list("Show ANOVA results" = 1, "Show n count" = 2, "Use standard colors" = 3, "Sort by elicitor" = 4),
                #                   selected = c(1,2,4)),
                #
                #radioButtons("letters", label = h4("Letter Threshold"),
                #             choices = list("p = 0.05" = 1, "p = 0.01" = 2, "p = 0.001" = 3, "p = 0.0001" = 4),  
                #             selected = 1),
                #checkboxGroupInput("download_options", label = h4("Download Option"),
                #                   choices = list("Add plots to data file" = 1), 
                #                   selected = 1),
                uiOutput("ui.downloaddata"),
                br(),
                uiOutput("ui.downloadplot")
              ),
              
              dashboardBody(
                tags$head(tags$style(HTML('
                                          #      .skin-green .main-sidebar {
                                          #        background-color: #666666;
                                          #      }
                                          #      .skin-green .sidebar-menu>li.active>a, .skin-green .sidebar-menu>li:hover>a {
                                          #        background-color: #444444;
                                          #      }
                                          .content-wrapper,
                                          .right-side {
                                          background-color: #ffffff;
                                          }
                                          .dbutton{
                                          background-color:white;
                                          height:35px;
                                          width:170px;
                                          margin-left: 12px
                                          }
                                          .skin-green .sidebar a { color: white; }
                                          
                                          
                                          '))),
                
                tabItem(tabName = "analysis",
                        fluidRow(
                          box(title = "Plot",
                              #solidHeader = TRUE,
                              status = "warning",
                              #background = "black",
                              width = 12,
                              collapsible = FALSE,
                              plotOutput("plot_output"))
                        ),
                        #fluidRow(
                        #  box(title = "Data",
                        #      #solidHeader = TRUE,
                        #      status = "warning",
                        #      #background = "black",
                        #      width = 12,
                        #      collapsible = FALSE,
                        #      plotOutput("table_output"))
                        #),
                        fluidRow(
                          box(title = "Data", 
                              solidHeader = TRUE, 
                              collapsible = FALSE,
                              width = 12,
                              DT::dataTableOutput("results_table")
                          )
                        )
                )
                )
                )