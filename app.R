
### Make time series plot of image data and display images
### Kaitlyn Strickfaden
### 3/7/2021


library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinythemes)
library(plotly)
library(lubridate)
library(htmltools)
library(htmlwidgets)


#   Launch the camTS app
#   Launches the app for viewing time series data and associated images.
# 
# 
#   @import dplyr
#   @import htmltools
#   @import htmlwidgets
#   @import lubridate
#   @import plotly
#   @import shiny
#   @import shinydashboard
#   @import shinythemes
#   @import stringr
#   @import tidyverse
#   @return A Shiny window
#   @export
# 
# 
# TS_launch_app <- function() {



  ui <- fluidPage(theme = shinytheme("slate"),
                  
                  # App title ----
                  titlePanel("Plot Time Series with Associated Images"),
                  
                  # Sidebar layout with input and output definitions ----
                  sidebarLayout(
                    
                    # Panel for inputs ----
                    sidebarPanel(
                    
                      width = 5,
                
                      
                      # Input: Choose a directory
                      
                      textOutput("fdir"),
                      
                      shinyDirButton(id = "fdir",
                                     label = "Choose a local directory:",
                                     title = "Navigate to the parent folder:"
                                     ),

                      
                      
                      
                      # Input: Selection of site
                      
                      # You will have to add folders containing images and
                      # CSV's of data to the "www" folder in the app folder.
                      # The display name will be the folder name.
                      
                      selectInput(inputId = "site",
                                  label = "Site",
                                  choices = list.files("www"),
                                  selected = "TW",
                                  width = "50%"
                      ),
                      
                      
                      # Input: Selection of variables for secondary y axis
                      
                      # if you add more choices here, you will also have to add them
                      # to the "data" reactive argument in the server.
                      
                      radioButtons(inputId = "secondy",
                                  label = "Secondary Y Axis",
                                  choices = c("None", "Turbidity (NTU)", "Temperature (°C)"),
                                  selected = "None",
                                  width = "50%"
                      ),
                      
                      
                      
                      checkboxInput(inputId = "dayonly",
                                    label = "Daytime images only?",
                                    value = FALSE
                      ),
                      
                      
                      ## Sidebar Panel Outputs: ---------------
                      
                      textOutput("text1"), # Date and time
                      textOutput("text2"), # Y value at click
                      textOutput("text3"), # Image file path
                      
                      imageOutput("image"), # Image
  
                    ), # end sidebar panel
                    
                    
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                      
                      width = 7,
                      
                      ## Main Panel Output: ----
                      
                      plotlyOutput("clickplot"), # Time series plot
                      
                      plotlyOutput("densplot", height = "150px")
                      
                    ) # end mainPanel
                    
                  ) # end sidebarLayout
                  
  ) # end fluidPage
  
  
  
  
  
  
  server <- function(input, output, session) {
    
    ################ Choose the directory ###################
    
    shinyDirChoose(input, "fdir",
                   roots = c(home = getwd()),
                   session = session,
                   updateFreq = 0,
                   defaultRoot = getwd(),
                   #defaultPath = "www",
                   #filetypes = c("", "jpg", "jpeg", "png", "tif", "tiff", "gif", "csv"),
                   allowDirCreate = FALSE
                   )

    # fdir <- reactive(input$fdir)
    # 
    # observeEvent(ignoreNULL = TRUE,
    #              eventExpr = {
    #                input$fdir
    #              },
    #              handlerExpr = {
    #                if (!"path" %in% names(fdir())) return()
    #                home <- normalizePath(file.path(input$fdir))
    #              })
    # 
    
    output$fdir <- renderText({
      "The button is here, but it doesn't work yet... :( "
    })
    
    
    
    ############### Render clickplot #####################
    
    output$clickplot <- renderPlotly({
      
      # if (is.null(input$fdir)) {
      #   return(NULL)
      # }
      
      # Find files corresponding to selected site
      myImages <- str_c("www", input$site, 
                        list.files(str_c("www", input$site, sep = "/")), 
                        sep = "/" )
      
      # Find csv file in wd
      d <- myImages[str_ends(myImages, ".csv")]
      
      # Find images in wd
      #myImages <- myImages[!str_ends(myImages, ".csv")]
      
      # Read in and clean up the data
      dat <- read_csv(d, col_types = list( "TriggerMode" = col_character()))
      dat <- dat %>%
        mutate(Stage = case_when(Stage > 0 ~ Stage, TRUE ~ 0),
               Datetime = ymd_hms(Datetime),
               image_url = SourceFile) %>%
        arrange(Datetime)
      
      if (input$dayonly == TRUE) {
        
        dat <- dat %>%
          mutate(image_url = case_when(
            SceneCaptureType == "Night" ~ str_c(image_url, "(nighttime image)", sep = " "),
            TRUE ~ image_url)
          )
        
      }
      
      hdat <- highlight_key(dat, ~Datetime)
      # lets you highlight the click event
      
     
      
      # Allow secondary y axis variables to be a user input
      
      # if you add more choices here, you will also have to give them
      # radioButtons in the UI.
      
      data <- reactive({
        switch(input$secondy,
               "None" = NA,
               "Turbidity (NTU)" = dat$WT,
               "Temperature (°C)" = dat$TW
        )
        })
      
      
      
      # Make the plot
      plot_ly(
        hdat,
        x         = ~ Datetime,
        y         = ~ Stage,
        type      = 'scattergl',
        mode      = 'lines+markers',
        marker = list(
          color = 'rgb(0, 0, 0)'
          ),
        line = list(
          color = 'rbg(0,0,0)'
        ),
        name = "Stream Stage (m)",
        hoverinfo = 'y',
        source = "hoverplotsource",
        customdata = ~ image_url
      ) %>%
        
        # Second y axis
        add_trace(x = dat$Datetime, y = ~data(), yaxis = "y2",
                  name = input$secondy, mode = 'lines+markers', 
                  marker = list(
                    color = 'rgb(0,0,255)',
                    size = 6,
                    symbol = "triangle-up"
                  ),
                  line = list(
                    color = 'rbg(0,0,255)',
                    width = 1
                  )
                  ) %>%
        
        layout(
          title = str_glue("Time series for {input$site}"),
          xaxis = list(
            title = "Datetime"
          ),
          yaxis = list(
            title = "Stream Stage (m)"
          ),
          yaxis2 = list(
            tickfont = list(color = "blue"),
            overlaying = "y",
            side = "right",
            title = str_glue("{input$secondy}")
          )
          ) %>%
        
        event_register('plotly_click') %>% # lets you click points
        
        # highlights the clicked point
        highlight("plotly_click", off = "plotly_doubleclick",
                  color = toRGB("red"), opacityDim = 1)
  
      
    }) # End of clickplot renderPlotly
    
    
    
    
    ############# Get click information ############################
    
    # Store click information
    click_event <- reactive({
      event_data(event = "plotly_click", source = "hoverplotsource")
    })
    
    
    # Once a click happens...
    observeEvent(click_event(), {
      
      
      # Write the datetime, response variable value, and image file path in sidebar
      output$text1 <- renderText(
        str_glue("Date and Time: ", as.character(click_event()$x), sep = " ")
      )
      
      output$text2 <- renderText(
        str_glue("Y value at click: ", as.character(click_event()$y), sep = " ")
      )
      
      output$text3 <- renderText(
        str_glue("Image Path: ", as.character(click_event()$customdata), sep = " ")
      )
      
      # Draw the corresponding image in the sidebar
      output$image <- renderImage({
        filename <- normalizePath(file.path(
          click_event()$customdata))
        list(src = filename,
             width  = session$clientData$output_image_width,
             height = session$clientData$output_image_height)}, 
        deleteFile = FALSE)
      
      
      
    }) # end of observeEvent
    
    
    
    
    ############ Render plot of image occurrence #################### 
    
    output$densplot <- renderPlotly({
      
      # if (is.null(input$fdir)) {
      #   return(NULL)
      # }
       
      
      # Find files corresponding to selected site
      myImages <- str_c("www", input$site, 
                        list.files(str_c("www", input$site, sep = "/")), 
                        sep = "/" )
      
      # Find csv file in wd
      d <- myImages[str_ends(myImages, ".csv")]
      
      # Find images in wd
      #myImages <- myImages[!str_ends(myImages, ".csv")]
      
      # Read in and clean up the data
      dat <- read_csv(d, col_types = list("TriggerMode" = col_character()))
      dat <- dat %>%
        mutate(Datetime = ymd_hms(Datetime)
        ) %>%
        arrange(Datetime)
      
      #dens <- dat %>%
        #mutate(TriggerMode = as.factor(TriggerMode))
      
      
      plot_ly(
        dat,
        x         = ~ Datetime,
        y         = ~ TriggerMode,
        type      = 'scattergl',
        mode      = 'markers',
        marker = list(
          symbol = "line-ns",
          size = 2
        ),
        name = "Image Density",
        transforms = list(
          list(
            type = 'groupby',
            groups = dat$TriggerMode
          )
        )
      )
      
    }) # End of densplot renderPlotly
    
    
    
  } # end of server
    
    
  
  shinyApp(ui = ui, server = server)

  
#} end TS_launch_app
  
  
  
  
  
