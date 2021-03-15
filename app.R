
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
                      
                      checkboxGroupInput(inputId = "site",
                                  label = "Site",
                                  choices = list.files("www"),
                                  selected = c("TE", "TW"),
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
                      
                      
                      # Input: Decide if only daytime images should be rendered in side panel
                      
                      checkboxInput(inputId = "dayonly",
                                    label = "Daytime images only?",
                                    value = FALSE
                      ),
                      
                      checkboxInput(inputId = "matchonly",
                                    label = "Only show exact images?",
                                    value = FALSE
                      ),
                      
                      
                      ## Sidebar Panel Outputs: ---------------
                      
                      textOutput("text1"), # Date and time
                      textOutput("text2"), # Stream Stage value
                      textOutput("text3"), # Secondary y axis value
                      textOutput("text4"), # Image file path
                      
                      imageOutput("image"), # Image
  
                    ), # end sidebar panel
                    
                    
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                      
                      width = 7,
                      
                      ## Main Panel Output: ----
                      
                      plotlyOutput("clickplot"), # Time series plot
                      
                      plotOutput("densplot", height = "150px")
                      
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
      
      # Make an empty tibble for storing data from selected sites
      dat <- tibble()
      
      for (i in seq_along(input$site)) {
      
        # Find files corresponding to a site
        myImages <- str_c("www", input$site[i], 
                          list.files(str_c("www", input$site[i], sep = "/")), 
                          sep = "/" )
        
        # Find csv file in wd
        d <- myImages[str_ends(myImages, ".csv")]
        
        # Read in and clean up the data
        d1 <- read_csv(d, col_types = list("TriggerMode" = col_character(), 
                                            "Datetime" = col_datetime()))
        d1 <- d1 %>%
          mutate(Stage = case_when(Stage > 0 ~ Stage, TRUE ~ 0),
                 Datetime = ymd_hms(Datetime),
                 image_url = SourceFile) %>%
          arrange(Datetime)
        
        
        # Rename nighttime photos so they do not render
        # Probably needs a neater solution - this throws errors in the R console
        
        # if (input$dayonly == TRUE) {
        #   d1 <- d1 %>%
        #     mutate(TimeofDay = case_when(
        #       SceneCaptureType == "Night" ~ "Night",
        #       TRUE ~ "Day")
        #     )
        # }
        # 
        # if (input$matchonly == TRUE) {
        #   d1 <- d1 %>%
        #     mutate(DTMatch = case_when(
        #       as.numeric(Datetime) == as.numeric(Image_Datetime) ~ "Yes",
        #       TRUE ~ "No")
        #     )
        # }
        
        
        # Bind these to the compiled tibble
        dat <- rbind(dat, d1)
      
        
      }
      
      
      # Lets you highlight the click event
      hdat <- highlight_key(dat, ~Datetime)

      
     
      
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
        color     = ~ UserLabel,
        colors    = "Set1",
        type      = 'scattergl',
        mode      = 'lines+markers',
        #name = str_glue(input$site, "Stream Stage (m)"),
        hoverinfo = 'y',
        source = "hoverplotsource",
        customdata = ~ UserLabel
      ) %>%
        
        # Second y axis
        add_trace(
          x = dat$Datetime, 
          y = ~ data(), 
          color = ~ UserLabel,
          colors = "Set1",
          yaxis = "y2",
          name = input$secondy, 
          mode = 'lines+markers', 
          marker = list(
            symbol = "triangle-up",
            size = 6
          )
                  ) %>%
        
        # Extra plot customization
        layout(
          title = paste0("Time series for ", str_flatten(input$site, collapse = ", ")),
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
          ),
          legend = list(
            font = list(size = 8)
          )
          ) %>%
      
        
        event_register('plotly_click') %>% # lets you click points
        
        # Changes the color of the clicked point
        highlight("plotly_click", off = "plotly_doubleclick",
                  color = toRGB("black"), opacityDim = 1,
                  selected = attrs_selected(showlegend = FALSE))
  
      
    }) # End of clickplot renderPlotly
    
    
    
    
    ############# Get click information ############################
    
    # Store click information
    click_event <- reactive({
      event_data(event = "plotly_click", source = "hoverplotsource")
    })
    
    
    # Once a click happens...
    observeEvent(click_event(), {
      
      
      dat <- tibble()
      
      for (i in seq_along(input$site)) {
        
        # Find files corresponding to a site
        myImages <- str_c("www", input$site[i], 
                          list.files(str_c("www", input$site[i], sep = "/")), 
                          sep = "/" )
        
        # Find csv file in wd
        d <- myImages[str_ends(myImages, ".csv")]
        
        # Read in and clean up the data
        d1 <- read_csv(d, col_types = list("TriggerMode" = col_character(), 
                                           "Datetime" = col_datetime()))
        d1 <- d1 %>%
          mutate(Stage = case_when(Stage > 0 ~ Stage, TRUE ~ 0),
                 Datetime = ymd_hms(Datetime),
                 TimeofDay = "Day",
                 DTMatch = "Yes",
                 image_url = SourceFile) %>%
          arrange(Datetime)
        
        
        # Allow for only showing certain images
        
        if (input$dayonly == TRUE) {
          d1 <- d1 %>%
            mutate(TimeofDay = case_when(
              SceneCaptureType == "Night" ~ "Night",
              TRUE ~ "Day")
            )
        }
        
        if (input$matchonly == TRUE) {
          d1 <- d1 %>%
            mutate(DTMatch = case_when(
              as.numeric(Datetime) == as.numeric(Image_Datetime) ~ "Yes",
              TRUE ~ "No")
            )
        }
        
        
        # Bind site data to the compiled tibble
        dat <- rbind(dat, d1)
        
      }
      
      
      dat <- dat %>%
        filter(Datetime == click_event()$x & UserLabel == click_event()$customdata)
      
      
      # Write the datetime, response variable value, and image file path in sidebar
      output$text1 <- renderText(
        str_glue("Date and Time: ", as.character(dat$Datetime), sep = " ")
      )
      
      output$text2 <- renderText(
        str_glue("Stream Stage: ", as.character(dat$Stage), " m", sep = " ")
      )
      
      #"None", "Turbidity (NTU)", "Temperature (°C)"
      if (input$secondy == "None") {
      output$text3 <- renderText(
        str_glue("No secondary y axis value")
      )
      }
      if (input$secondy == "Turbidity (NTU)") {
        output$text3 <- renderText(
          str_glue("Turbidity: ", as.character(dat$WT), " NTU", sep = " ")
        )
      }
      if (input$secondy == "Temperature (°C)") {
        output$text3 <- renderText(
          str_glue("Stream Temperature: ", as.character(dat$TW), " °C", sep = " ")
        )
      }
      
      output$text4 <- renderText(
        str_glue("Image Path: ", as.character(dat$SourceFile), sep = " ")
      )
      
      
      # Draw the corresponding image in the sidebar
      # Change to renderPlot with rasterGrob to display multiple images
      
      if (input$dayonly == TRUE & dat$TimeofDay == "Night") {
        output$image <- renderImage({list(src = "NA")}, deleteFile = FALSE)
        output$text4 <- renderText(
          str_glue("Image Path: ", 
                   as.character(dat$SourceFile), 
                   " (nighttime image)", sep = " ")
        )
      } else if (input$matchonly == TRUE & dat$DTMatch == "No") {
        output$image <- renderImage({list(src = "NA")}, deleteFile = FALSE)
        output$text4 <- renderText(
          str_glue("Image Path: ", 
                   as.character(dat$SourceFile), 
                   " (not exact image)", sep = " ")
        )
      } else {
        output$image <- renderImage({
          filename <- normalizePath(file.path(
            dat$SourceFile))
          list(src = filename,
               width  = session$clientData$output_image_width,
               height = session$clientData$output_image_height)}, 
          deleteFile = FALSE)
      }
      
      
    #   output$plot <- renderPlot({
    #     filename <- normalizePath(file.path("<path>", paste0(input$countyInput, " ", input$reasonInput, ".png", sep = ""))) # you had one extra space before .png
    #     filename <- filename[file.exists(filename)]
    #     pngs = lapply(filename, readPNG)
    #     asGrobs = lapply(pngs, rasterGrob)
    #     p <- grid.arrange(grobs=asGrobs, nrow = 1)
    #   }, width = 1000)
    # }
      
      
      
    }) # end of observeEvent
    
    
    
    
    ############ Render plot of image occurrence #################### 
    
    output$densplot <- renderPlot({
      
      # if (is.null(input$fdir)) {
      #   return(NULL)
      # }
       
      dat <- tibble()
      
      for (i in seq_along(input$site)) {
      
        # Find files corresponding to a site
        myImages <- str_c("www", input$site[i], 
                          list.files(str_c("www", input$site[i], sep = "/")), 
                          sep = "/" )
        
        # Find csv file in wd
        d <- myImages[str_ends(myImages, ".csv")]
        
        # Find images in wd
        #myImages <- myImages[!str_ends(myImages, ".csv")]
        
        # Read in and clean up the data
  
        d1 <- read_csv(d)
        dat <- rbind(dat, d1)
        
      }
      
        dat <- dat %>%
          select(FileName, UserLabel, Image_Datetime) %>%
          distinct() %>%
          mutate(Datetime = ymd_hms(Image_Datetime),
                 Date = as_date(Datetime),
                 Hour = hour(Datetime)
          ) %>%
          group_by(Date, Hour) %>%
          count()
        

      ggplot(dat) +
        geom_tile(aes(x = Date, y = Hour, fill = n)) +
        ylim(0,24) +
        scale_fill_gradient(name = "# of\nImages") +
        theme_minimal()
      
    }) # End of densplot renderPlotly
    
    
    
  } # end of server
    
    
  
  shinyApp(ui = ui, server = server)

  
#} end TS_launch_app
  
  
  
  
  
