

#' Launch the camTS app
#' Launches the app for viewing time series data and associated images.
#' 
#' @import dplyr
#' @importFrom ggplot2 ggplot aes geom_tile ylim scale_fill_gradient theme_minimal
#' @import htmltools
#' @import htmlwidgets
#' @import lubridate
#' @import magrittr
#' @import plotly
#' @import shiny
#' @import shinydashboard
#' @import shinyFiles
#' @import shinythemes
#' @import stringr
#' @import tidyverse
#' @importFrom utils read.csv
#' 
#' @param main_y a named vector of options for the primary y axis. The name will be the name shown on the radio button, and the value should be the column name in the data. For example, an input like 'c("Stream Stage (m)" = "Stage")' tells the app to have a radio button called "Stream Stage (m)" that will be associated with the "Stage" column in the data. The app will open with the first item in the vector as the primary y-axis variable.
#' @param second_y a named vector of options for the secondary y axis formatted the same way as the main_y parameter. A "none" option will be automatically added. The Shiny app will open with no variable on the secondary y axis.
#' @param tz the timezone of your computer (not of the data!). Please refer to OlsonNames() for a list of valid timezones.
#' @return A Shiny window
#' @export


TS_launch_app <- function(main_y = c("Stream Stage (m)" = "Stage"), 
                          second_y = c("Turbidity (NTU)" = "WT", 
                                       "Temperature (C)" = "TW"), 
                          tz = "America/Los_Angeles") {
  
  ### Make time series plot of image data and display images
  ### Kaitlyn Strickfaden
  ### 5/6/2021
  
  # 
  # library(grid)
  # library(gridExtra)
  # library(htmltools)
  # library(htmlwidgets)
  # library(lubridate)
  # library(plotly)
  # library(shiny)
  # library(shinydashboard)
  # library(shinyFiles)
  # library(shinythemes)
  # library(stringr)
  # library(tidyverse)
  
  
  if (!is.vector(main_y) | is.null(names(main_y)) | any(is.na(names(main_y))) == TRUE) {
    stop("main_y must be a named vector")
  }
  
  if (!is.null(second_y)) {
    if (!is.vector(second_y) | is.null(names(second_y)) | any(is.na(names(second_y))) == TRUE) {
      stop("second_y must be a named vector")
    }
  }
  
  if (tz %in% OlsonNames() == FALSE) {
    stop("You did not input a valid timezone. Use the function OlsonNames() to see a list of valid timezones.")
  }
  
  
  ui <- fluidPage(theme = shinytheme("slate"),
                  
                  # App title ----
                  titlePanel("Plot Time Series with Associated Images"),
                  
                  # Sidebar layout with input and output definitions ----
                  sidebarLayout(
                    
                    # Panel for inputs ----
                    sidebarPanel(
                      
                      width = 5,
                      
                      
                      # Input: Choose a directory
                      
                      
                      shinyDirButton(id = "fdir",
                                     label = "Choose a local directory:",
                                     title = "Navigate to the parent folder:"
                      ),
                      
                      
                      textOutput("fdirt"),
                      
                      
                      # Input: Selection of site
                      # Placeholder for update after selection of parent directory
                      
                      checkboxGroupInput(inputId = "site",
                                         label = "Site",
                                         choices = NULL,
                                         selected = NULL,
                                         width = "50%"
                      ),
                      
                      
                      # Input: Selection of variables for primary y axis
                      
                      radioButtons(inputId = "mainy",
                                   label = "Primary Y Axis",
                                   choices = names(main_y),
                                   selected = names(main_y)[1],
                                   width = "50%"
                      ),
                      
                      
                      # Input: Selection of variables for secondary y axis
                      
                      radioButtons(inputId = "secondy",
                                   label = "Secondary Y Axis",
                                   choices = c("None", names(second_y)),
                                   selected = "None",
                                   width = "50%"
                      ),
                      
                      
                      # Input: Decide if only daytime images should be rendered in side panel
                      
                      checkboxInput(inputId = "dayonly",
                                    label = "Daytime images only?",
                                    value = FALSE
                      ),
                      
                      checkboxInput(inputId = "matchonly",
                                    label = "Exact images only?",
                                    value = FALSE
                      ),
                      
                      
                      ## Sidebar Panel Outputs: ---------------
                      
                      textOutput("text1"), # Date and time
                      textOutput("text2"), # Site
                      textOutput("text3"), # Stream stage
                      textOutput("text4"), # Secondary y axis value
                      textOutput("text5"), # Image file path
                      
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
    
    # Find a reasonable root directory to start with
    # Equivalent of saying " wd <- 'C://Users/{user}' "
    wd <- c(`User Directory` = file.path(Sys.getenv("USERPROFILE"), fsep = "\\"))
    
    # Matching server-side to shinyDirButton in UI
    shinyDirChoose(input, "fdir",
                   roots = wd,
                   session = session,
                   updateFreq = 0,
                   defaultRoot = getwd(),
                   allowDirCreate = FALSE
                   #filetypes = c("", "jpg", "jpeg", "png", "tif", "tiff", "gif", "csv"),
                   # (filetypes is apparently a required input, but shiny
                   # seems to be doing fine without it?)
    )
    
    
    # Observe selection of parent directory
    observe({
      
      # Tell Shiny the right parent directory to use
      w <- parseDirPath(wd, input$fdir)
      inpath <- normalizePath(file.path(w))
      # ShinyDirChoose outputs the file path in a goofy way, so parseDirPath 
      # fixes the formatting and makes it into a more recognizable file path
      
      
      # Update checkboxes for sites in parent directory
      updateCheckboxGroupInput(session, "site",
                               label = "Site",
                               choices = list.files(inpath),
                               selected = list.files(inpath)[1]
      )
      
    })
    
    # Outputs parent directory file path in sidebar
    output$fdirt <- renderText({
      w <- parseDirPath(wd, input$fdir)
      normalizePath(file.path(w))
      
    })
    
    
    
    
    ############### Render clickplot #####################
    
    
    
    output$clickplot <- renderPlotly({
      
      # Require a parent directory
      req(input$fdir)
      
      # Tell Shiny the right parent directory to use
      w <- parseDirPath(wd, input$fdir)
      inpath <- normalizePath(file.path(w))
      
      
      # Make an empty tibble for storing data from selected sites
      dat <- dplyr::tibble()
      
      # For each of the selected sites...
      for (i in seq_along(input$site)) {
        
        # Find all files corresponding to the site
        myImages <- stringr::str_c(inpath, input$site[i], 
                                   list.files(stringr::str_c(inpath, input$site[i], sep = "/")), 
                                   sep = "/" )
        
        # Find the csv file for the site
        d <- myImages[stringr::str_ends(myImages, ".csv")]
        
        # Read in and clean up the data
        d1 <- read.csv(d)
        d1 <- d1 %>%
          dplyr::mutate(Datetime = lubridate::ymd_hms(Datetime, 
                                                      tz = tz),
                        Image_Datetime = lubridate::ymd_hms(Image_Datetime, 
                                                            tz = tz),
                        TimeofDay = "Day",
                        DTMatch = "Yes",
                        SibFolder = input$site[i]
          ) %>%
          dplyr::arrange(Datetime)
        
        
        # Allow for only showing certain images
        
        if (input$dayonly == TRUE) {
          d1 <- d1 %>%
            dplyr::mutate(TimeofDay = dplyr::case_when(
              SceneCaptureType == "Night" ~ "Night",
              TRUE ~ "Day")
            )
        }
        
        if (input$matchonly == TRUE) {
          d1 <- d1 %>%
            dplyr::mutate(DTMatch = dplyr::case_when(
              as.numeric(Datetime) == as.numeric(Image_Datetime) ~ "Yes",
              TRUE ~ "No")
            )
        }
        
        
        # Make TS points size 0 if they have nighttime or non-exact images
        d1 <- d1 %>%
          mutate(Size = case_when(
            TimeofDay == "Night" ~ 0,
            DTMatch == "No" ~ 0,
            TRUE ~ 6
          ))
        
        
        # Bind these to the compiled tibble
        dat <- rbind(dat, d1)
        
      }
      
      # Lets you highlight the click event
      hdat <- highlight_key(dat, ~Datetime)
      
      
      # # Allow primary y axis variables to be a user input
      
      y_prime <- as.character(input$mainy)
      
      mainydata <- reactive({
        switch(EXPR = input$mainy,
               (y_prime = dat[,main_y[y_prime]])
        )
      })
      
      
      # Allow secondary y axis variables to be a user input
      
      y_sec <- as.character(input$secondy)
      
      secondydata <- reactive({
        switch(EXPR = input$secondy,
               (y_sec = dat[,second_y[y_sec]]),
               "None" = NA
        )
      })
      
      
      
      # Make the main clickplot
      plotly::plot_ly(
        hdat,
        x         = ~ Datetime,
        y         = mainydata(),
        color     = ~ UserLabel,
        colors    = "Set1",
        type      = 'scattergl',
        mode      = 'lines+markers',
        line = list(
          width = 0.5),
        marker = list(
          size = ~ as.numeric(Size)),
        hoverinfo = 'y',
        source = "hoverplotsource",
        customdata = ~ UserLabel
      ) %>%
        
        
        event_register('plotly_click') %>% # lets you click points
        
        # Second y axis
        plotly::add_trace(
          x = ~ Datetime,
          y = secondydata(), 
          color = ~ UserLabel,
          colors = "Set1",
          yaxis = "y2",
          mode = 'lines+markers', 
          line = list(
            width = 0.5),
          marker = list(
            symbol = "triangle-up",
            size = ~ as.numeric(Size))
        ) %>%
        
        # Extra plot customization
        plotly::layout(
          title = paste0("Time series for ", stringr::str_flatten(input$site, collapse = ", ")),
          xaxis = list(
            title = "Datetime"
          ),
          yaxis = list(
            title = stringr::str_glue("{input$mainy}")
          ),
          yaxis2 = list(
            tickfont = list(color = "blue"),
            overlaying = "y",
            side = "right",
            title = stringr::str_glue("{input$secondy}")
          ),
          legend = list(
            font = list(size = 8)
          )
        ) %>%
        
        
        # Changes the color of the clicked point
        plotly::highlight("plotly_click", off = "plotly_doubleclick",
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
      
      # Tell Shiny the right parent directory to use
      w <- parseDirPath(wd, input$fdir)
      inpath <- normalizePath(file.path(w))
      
      
      # Make an empty tibble for storing data from selected sites
      dat <- dplyr::tibble()
      
      # For each of the selected sites...
      for (i in seq_along(input$site)) {
        
        # Find all files corresponding to that site
        myImages <- stringr::str_c(inpath, input$site[i], 
                                   list.files(stringr::str_c(inpath, input$site[i], sep = "/")), 
                                   sep = "/" )
        
        # Find the csv file for the site
        d <- myImages[stringr::str_ends(myImages, ".csv")]
        
        # Read in and clean up the data
        d1 <- read.csv(d)
        d1 <- d1 %>%
          dplyr::mutate(Datetime = lubridate::ymd_hms(Datetime, 
                                                      tz = tz),
                        Image_Datetime = lubridate::ymd_hms(Image_Datetime, 
                                                            tz = tz),
                        TimeofDay = "Day",
                        DTMatch = "Yes",
                        SibFolder = input$site[i]
          ) %>%
          dplyr::arrange(Datetime)
        
        
        # Allow for only showing certain images
        
        if (input$dayonly == TRUE) {
          d1 <- d1 %>%
            dplyr::mutate(TimeofDay = dplyr::case_when(
              SceneCaptureType == "Night" ~ "Night",
              TRUE ~ "Day")
            )
        }
        
        if (input$matchonly == TRUE) {
          d1 <- d1 %>%
            dplyr::mutate(DTMatch = dplyr::case_when(
              as.numeric(Datetime) == as.numeric(Image_Datetime) ~ "Yes",
              TRUE ~ "No")
            )
        }
        
        
        # Bind site data to the compiled tibble
        dat <- rbind(dat, d1)
        
      }
      
      
      # Filter out all data except the clicked point
      dat <- dat %>%
        dplyr::filter(Datetime == click_event()$x & 
                        UserLabel == click_event()$customdata)
      
      
      # Allow primary y axis variables to be a user input
      
      y_prime <- as.character(input$mainy)
      
      mainydata <- reactive({
        switch(EXPR = input$mainy,
               (y_prime = dat[dat$Datetime == click_event()$x & 
                                dat$UserLabel == click_event()$customdata,
                              main_y[y_prime]])
        )
      })
      
      
      # Allow secondary y axis variables to be a user input
      
      y_sec <- as.character(input$secondy)
      
      secondydata <- reactive({
        switch(EXPR = input$secondy,
               (y_sec = dat[dat$Datetime == click_event()$x & 
                              dat$UserLabel == click_event()$customdata,
                            second_y[y_sec]]),
               "None" = NA
        )
      })
      
      
      # Write the datetime, site name, primary y-axis value, 
      # secondary y-axis value, and image file name in sidebar
      
      # Datetime
      output$text1 <- renderText(
        stringr::str_glue("Date and Time: ", as.character(dat$Datetime), sep = " ")
      )
      
      # Site name
      output$text2 <- renderText(
        stringr::str_glue("Site: ", as.character(dat$UserLabel), sep = " ")
      )
      
      # Primary y value
      output$text3 <- renderText(
        stringr::str_glue({input$mainy}, ": ", as.character(mainydata()), sep = " ")
      )
      
      # Secondary y value
      if (input$secondy == "None") {
        output$text4 <- renderText(
          stringr::str_glue("No secondary y axis value")
        )
      }
      if (input$secondy != "None") {
        output$text4 <- renderText(
          stringr::str_glue({input$secondy}, ": ", as.character(secondydata()))
        )
      }
      
      # Image file name
      output$text5 <- renderText(
        stringr::str_glue("File Name: ", as.character(dat$FileName), sep = " ")
      )
      
      
      # Draw the corresponding image in the sidebar
      # Change to renderPlot with rasterGrob to display multiple images
      
      # Nighttime image
      if (input$dayonly == TRUE & dat$TimeofDay == "Night") {
        output$image <- renderImage({list(src = "NA")}, deleteFile = FALSE)
        output$text5 <- renderText(
          stringr::str_glue("File Name: ",
                            as.character(dat$FileName),
                            " (nighttime image)", sep = " ")
        )
        # Non-exact image
      } else if (input$matchonly == TRUE & dat$DTMatch == "No") {
        output$image <- renderImage({list(src = "NA")}, deleteFile = FALSE)
        output$text5 <- renderText(
          stringr::str_glue("File Name: ",
                            as.character(dat$FileName),
                            " (not exact image)", sep = " ")
        )
        # Anything else
      } else {
        output$image <- renderImage({
          filename <- normalizePath(file.path(
            stringr::str_c(inpath, dat$SibFolder, dat$FileName, sep = "/")))
          list(src = filename,
               width  = session$clientData$output_image_width,
               height = session$clientData$output_image_height)},
          deleteFile = FALSE)
      }
      
      
      # Some code for plotting multiple images at once that I'm not thrilled with...
      # Lots of white space forces images to be very small
      
      # output$image <- renderPlot({
      #   
      #   filename <- normalizePath(file.path(dat$SourceFile)) 
      #   filename <- filename[file.exists(filename)]
      #   
      #   ims <- lapply(filename, readJPEG)
      #   asGrobs <- lapply(ims, rasterGrob)
      #   grid.arrange(grobs = asGrobs, nrow = 1)},
      #   
      # width  = session$clientData$output_image_width,
      # height = session$clientData$output_image_height
      # )
      
      
    }) # end of observeEvent
    
    
    
    
    ############ Render plot of image occurrence #################### 
    
    output$densplot <- renderPlot({
      
      req(input$fdir)
      
      # Tell Shiny the right parent directory to use
      w <- parseDirPath(wd, input$fdir)
      inpath <- normalizePath(file.path(w))
      
      # Make an empty tibble for storing data from selected sites
      dat <- dplyr::tibble()
      
      for (i in seq_along(input$site)) {
        
        # Find files corresponding to a site
        myImages <- stringr::str_c(inpath, input$site[i], 
                                   list.files(stringr::str_c(inpath, input$site[i], sep = "/")), 
                                   sep = "/" )
        
        # Find csv file in wd
        d <- myImages[stringr::str_ends(myImages, ".csv")]
        
        
        # Read in and append data to tibble
        d1 <- read.csv(d)
        dat <- rbind(dat, d1)
        
      }
      
      # Summarize image data
      dat <- dat %>%
        dplyr::select('FileName', 'UserLabel', 'Image_Datetime') %>%
        dplyr::distinct() %>%
        dplyr::mutate(Datetime = ymd_hms(Image_Datetime),
                      Date = as_date(Datetime),
                      Hour = hour(Datetime)
        ) %>%
        dplyr::group_by(Date, Hour) %>%
        dplyr::count()
      
      # Plot image data as geom_tiles
      ggplot2::ggplot(dat) +
        ggplot2::geom_tile(ggplot2::aes(x = Date, y = Hour, fill = n)) +
        ggplot2::ylim(0,24) +
        ggplot2::scale_fill_gradient(name = "# of\nImages") +
        ggplot2::theme_minimal()
      
    }) # End of densplot renderPlotly
    
    
  } # end of server
  
  
  shinyApp(ui = ui, server = server)
  
  
} # end TS_launch_app





