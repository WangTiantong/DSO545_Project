library(leaflet)
library(shinythemes)
library(shiny)
library(shinyBS)
library(plotly)

shinyUI(
  
  
  fluidPage(theme = shinytheme("flatly"), 
            
    # Application Title
    navbarPage("LA City Homelessness Project",   
               
       #---------------------------------------------------------------------------------------------------------------------------------#  
               
       # 1st tab - Homelessness Overview
       tabPanel("Homelessness Overview", tabName = "overview",
                  
                  tabsetPanel(type = "tabs", selected="Homeless Estimate by Street Count",
                      tags$head(
                        tags$style(type='text/css',
                                   ".nav-tabs {font-size: 14px} ")),
                      
                      tabPanel("Homeless Estimate by Street Count",
                               column(4,
                                      # selectInput(inputId = "yearStreetCount",
                                      #             label = "Select Year",
                                      #             choices = list("2015", "2016", "2017"),
                                      #             selected = "2017"), 
                                      plotlyOutput("plotStreetCount", width = 350, height = 255),
                                      p(),
                                      plotlyOutput("plot2", width = 350, height = 255)
                               ),
                               
                               column(8,
                                      h5("Homelessness Estimates by Street Count"),
                                      leafletOutput("living_map",width = 775, height = 480)
                               )
                               
                      ),
                      
                      tabPanel("311 Reports",
                               column(4,
                                      plotlyOutput("plot4", width = 350, height = 255),
                                      p(),
                                      plotlyOutput("plot3", width = 350, height = 255)
                                      
                               ),
                               
                               column(8,
                                      h5("311 Homeless Encampments Requests"),
                                      leafletOutput("map1",width = 775, height = 480)
                               )
                      ),
                      
                      tabPanel("Homeless Crime Victims",
                               column(4,
                                      plotlyOutput("crime_plot", width = 350, height = 255),
                                      p(),
                                      plotlyOutput("crimePremisesPlot", width = 350, height = 255)
                               ),
                               
                               column(8,
                                      h5("Crime, Homeless Victim Aug 2016 - Aug 2017"),
                                      leafletOutput("crime_map",width = 775, height = 480)
                               )
                      ),
                      
                      tabPanel("Customized Metric for Homelessness Density",
                               column(4,
                                      p(),
                                      h5("Select factors and weights"),
                                      p(),
                                      p(),
                                      numericInput("pit_wgt", "Coefficient for PIT Count (# of homeless people)", value = 1, min = 0, max = 1, step = NA, width = NULL),
                                      numericInput("t311_wgt", "Coefficient for # of 311 reports", value = 1, min = 0, max = 1, step = NA, width = NULL),
                                      numericInput("crime_wgt", "Coefficient for # of homeless crime victims", value = 1, min = 0, max = 1, step = NA, width = NULL),
                                      actionButton("gen_metric", label = "Visualize Density Map",style='padding:5px; height:80%')
                               ),
                               
                               column(8,
                                      h5("Estimated Homelessness Density"),
                                      leafletOutput("estimate_map",width = 775, height = 480)
                               )
                      )
                               
                  )
                
       ), # \1st tab
       
        #---------------------------------------------------------------------------------------------------------------------------------#  
        
        # 2nd tab - Homeless Shelters
        tabPanel("Shelters & Housing", tabName = "shelters",
                 
            tabsetPanel(type = "tabs", selected="Overview",
                        tags$head(
                          tags$style(type='text/css', ".nav-tabs {font-size: 14px} ")
                          # tags$style(type="text/css", 
                          #            "label{ display: table-cell; text-align: center; vertical-align: middle; font-weight: normal; margin-bottom: 1px} 
                          #            .form-group { display: table-row;}")
                          ),
                        
                        tabPanel("Overview",
                                 column(12, 
                                        column(6,
                                               plotOutput("overview1"),
                                               p(),
                                               plotOutput("overview2")
                                        ),
                                        
                                        column(6,
                                               plotOutput("overview3"),
                                               p(),
                                               plotOutput("overview4")
                                        )
                                        
                                 )
                                 
                        ), # \Overview subtab
                        
                        tabPanel("Shelters",
                                   column(4,
                                          div(style="display: inline-block;vertical-align:top;",
                                              actionButton(inputId = "btn_shelter", label = "", icon = icon("arrows-alt"),
                                                           style='padding:3px; font-size:80%;')),
                                          
                                          div(style="display: inline-block;vertical-align:top; padding:0px; font-size:80%;",
                                              selectInput("yORs_shelter", "",
                                                          choices = c("Year-round", "Winter Shelters"), selected = "Year-round",
                                                          multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)),
                                          
                                          plotOutput("shelter1", width = 350, height = 260),
                                          p(),
                                          plotOutput("shelter2", width = 350, height = 260),
                                          bsModal("shelter_modal", "Shelters", "btn_shelter", size = "large", plotOutput("shelter1m"), plotOutput("shelter2m"))
                                          
                                   ),

                                   column(8,
                                          h5("Unsheltered Homeless and Shelter Utilization"),
                                          leafletOutput("shelter_map",width = 775, height = 480)
                                   )

                        ), # \Shelters subtab

                        tabPanel("Permanent Housing",
                                 column(4,
                                        actionButton(inputId = "btn_housing", label = "", icon = icon("arrows-alt"), 
                                                     style='padding:3px; font-size:80%; background-color:"#ffffff"; color:"#000000" '
                                                     ),
                                        plotOutput("housing1", width = 350, height = 260),
                                        p(),
                                        plotOutput("housing2", width = 350, height = 260),
                                        bsModal("housing_modal", "Permanent Housing", "btn_housing", size = "large", plotOutput("housing1m"), plotOutput("housing2m"))
                                       
                                 ),

                                 column(8,
                                        #textOutput("housing_map_title"),
                                        h5("Homeless People and Housing Utilization (Year-round)"),
                                        leafletOutput("housing_map",width = 775, height = 480)
                                 )
                        ) # \Shelters subtab
            )# \tabsetPanel

       ) # \2nd tab
                 
               
    ) # \navbarPage
    
  ) # \fluidPage
  
  
  
) # \shinyUI