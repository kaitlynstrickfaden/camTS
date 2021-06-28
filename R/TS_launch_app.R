

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


### Make time series plot of image data and display images
### Kaitlyn Strickfaden
### 6/26/2021
# 
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



TS_launch_app <- function(main_y = c("Stream Stage (m)" = "Stage"), 
                          second_y = c("Turbidity (NTU)" = "WT", 
                                       "Temperature (C)" = "TW"), 
                          tz = "America/Los_Angeles") 
  
{
  
  if (!is.vector(main_y) | is.null(names(main_y)) | any(is.na(names(main_y))) == TRUE) {
    stop("main_y must be a named vector")
  }
  
  if (!is.null(second_y)) {
    if (!is.vector(second_y) | is.null(names(second_y)) | any(is.na(names(second_y))) == TRUE) {
      stop("second_y must be a named vector")
    }
  }
  
  if (tz %in% OlsonNames() == FALSE) {
    stop("You did not input a valid timezone. Use OlsonNames() to see a list of valid timezones.")
  }
  
  
  
  ui <- fluidPage(theme = shinytheme("sandstone"),
                  
                  # App title ----
                  titlePanel("Plot Time Series with Associated Images"),
                  
                  # Top Output
                  fluidRow(
                    
                    
                    # Image Output
                    column(5, style = "height:70vh;background-color: black; color: white;",
                           
                           "Image", br(), br(),
                           
                           imageOutput("image", height = "60vh"), # Image
                           
                           # Text Output
                           fluidRow(  
                             
                             column(6,
                                    textOutput("text1"), # Date and time
                                    textOutput("text2"), # Site
                                    textOutput("text5"),  # Image file path
                             ),
                             
                             column(6,
                                    textOutput("text3"), # Main y value
                                    textOutput("text4")  # Secondary y axis value
                             )
                             
                           ) # end of text output
                           
                    ), # end of image output
                    
                    # Plot Output
                    column(7,
                           
                           "Time Series", br(),
                           
                           plotlyOutput("clickplot", height = "54vh"), # Time series plot
                           plotlyOutput("densplot", height = "15vh")
                           
                    ) # End of plot output
                    
                  ), # End of top output
                  
                  
                  # User Inputs
                  fluidRow(
                    
                    # User Inputs
                    column(5, style = "background-color: LightGray;",
                           
                           fluidRow(
                             
                             # Left Side of Inputs
                             column(5, 
                                    
                                    # Input: Choose a directory
                                    shinyDirButton(id = "fdir",
                                                   label = "Choose a local directory:",
                                                   title = "Navigate to the parent folder:",
                                                   buttonType = "custom",
                                                   style = "background-color: orange; color: black; margin-top: 10px; margin-bottom: 10px;"
                                    ),
                                    
                                    
                                    textOutput("fdirt"),
                                    
                                    # Input: Selection of site
                                    # Placeholder for update after selection of parent directory
                                    selectInput(inputId = "site",
                                                label = "Site",
                                                choices = NULL,
                                                selected = NULL,
                                                multiple = TRUE,
                                                width = "90%"
                                    ),
                                    
                                    # Input: Decide if only daytime images or images with 
                                    # a timestamp that match a data point exactly should 
                                    # be rendered in side panel
                                    checkboxInput(inputId = "dayonly",
                                                  label = "Daytime images only?",
                                                  value = FALSE
                                    ),
                                    
                                    checkboxInput(inputId = "matchonly",
                                                  label = "Exact images only?",
                                                  value = FALSE
                                    )
                                    
                             ),
                             
                             
                             # Right Side of Inputs
                             column(7,
                                    
                                    br(), "Inputs", br(), br(),
                                    
                                    # Input: Selection of variables for primary y axis
                                    selectInput(inputId = "mainy",
                                                label = "Primary Y Axis",
                                                choices = names(main_y),
                                                selected = names(main_y)[1],
                                                multiple = FALSE,
                                                width = "90%"
                                    ),
                                    
                                    
                                    # Input: Selection of variables for secondary y axis
                                    selectInput(inputId = "secondy",
                                                label = "Secondary Y Axis",
                                                choices = c("None", names(second_y)),
                                                selected = "None",
                                                multiple = FALSE,
                                                width = "90%"
                                    )
                                    
                             ))
                           
                    ),
                    
                    # Data Table
                    column(7,
                           
                           "Data Table", br(),
                           
                           DTOutput("dattable")
                    )
                    
                  ) # end of user inputs
                  
                  
  ) # End of fluidpage ui
  
  
  
  
  
  
  
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
                   defaultRoot = wd,
                   allowDirCreate = FALSE
                   #filetypes = c("", "jpg", "jpeg", "png", "tif", "tiff", "gif", "csv"),
                   # (filetypes is apparently a required input, but Shiny
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
      updateSelectInput(session, "site",
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
    
    
    
    ############ Render data table #################### 
    
    output$dattable <- renderDT({
      
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
        
        myFiles <- stringr::str_c(inpath, input$site[i], 
                                  list.files(stringr::str_c(inpath, input$site[i], sep = "/")), 
                                  sep = "/" )
        
        
        # Read in and clean up the data
        
        d <- read.csv(myFiles[stringr::str_ends(myFiles, ".csv")])
        d <- d %>%
          dplyr::mutate(Datetime = lubridate::ymd_hms(.data$Datetime, 
                                                      tz = tz),
                        Image_Datetime = lubridate::ymd_hms(.data$Image_Datetime, 
                                                            tz = tz)
          ) %>%
          dplyr::arrange(.data$Datetime)
        
        # Bind these to the compiled tibble
        dat <- rbind(dat, d)
        
      }
      
      DT::datatable(dat, rownames = F) #, editable = "row")
      
    }) # end of render data table
    
    
    
    
    
    ############### Render clickplot #####################
    
    
    
    output$clickplot <- renderPlotly({
      
      # Require a parent directory
      req(input$fdir)
      
      # Tell Shiny the right parent directory to use
      
      w <- parseDirPath(wd, input$fdir)
      inpath <- normalizePath(file.path(w))
      
      
      # Make an empty tibble for storing data from selected sites
      dat1 <- dplyr::tibble()
      
      # For each of the selected sites...
      for (i in seq_along(input$site)) {
        
        # Find all files corresponding to the site
        
        myFiles <- stringr::str_c(inpath, input$site[i],
                                  list.files(stringr::str_c(inpath, input$site[i], sep = "/")),
                                  sep = "/" )
        
        
        # Read in and clean up the data
        
        d <- read.csv(myFiles[stringr::str_ends(myFiles, ".csv")])
        d <- d %>%
          dplyr::mutate(Datetime = lubridate::ymd_hms(.data$Datetime,
                                                      tz = tz),
                        Image_Datetime = lubridate::ymd_hms(.data$Image_Datetime,
                                                            tz = tz),
                        Folder = input$site[i]
          ) %>%
          dplyr::arrange(.data$Datetime)
        
        # Bind these to the compiled tibble
        dat1 <- rbind(dat1, d)
        
      }
      
      dat2 <- reactiveValues(data = { 
        dat1
      })
      
      
      # Theoretically, this is supposed to allow the data table to be edited.
      # Right now, it just throws mystery errors
      
      observe({
        #get values
        req(input$dattable_cell_edit)
        
        info <- input$dattable_cell_edit
        editrow <- as.numeric(info$row)
        editcol <- as.numeric(info$col)
        editval <- as.numeric(info$value)
        
        #write values to reactive
        dat2$data[editrow,editcol] <<- DT::coerceValue(editval, dat2$data[editrow,editcol])
      })
      
      dat <- isolate(dat2$data)
      
      
      ### Data-cleaning
      
      dat <- dat %>%
        mutate(TimeofDay = "Day",
               DTMatch = "Yes"
        )
      
      
      # Allow for only showing certain images
      
      if (input$dayonly == TRUE) {
        dat <- dat %>%
          dplyr::mutate(TimeofDay = dplyr::case_when(
            SceneCaptureType == "Night" ~ "Night",
            TRUE ~ "Day")
          )
      }
      
      if (input$matchonly == TRUE) {
        dat <- dat %>%
          dplyr::mutate(DTMatch = dplyr::case_when(
            as.numeric(.data$Datetime) == as.numeric(.data$Image_Datetime) ~ "Yes",
            TRUE ~ "No")
          )
      }
      
      # Make TS points size 0 if they have nighttime or non-exact images
      # Change hover info dependent on point size (i.e. image status)
      
      dat <- dat %>%
        mutate(PointSize = dplyr::case_when(
          TimeofDay == "Night" ~ 0,
          DTMatch == "No" ~ 0,
          TRUE ~ 6),
          
          # Text to display on the hover
          # Needs to be its own column because case_when doesn't work within paste
          
          Hover = case_when(PointSize == 0 & input$secondy != "None" ~
                              paste("%{x}<br>",
                                    "%{yaxis.title.text}: %{y}<br>",
                                    "%{text}<br>",
                                    "Non-target Image"),
                            PointSize == 0 & input$secondy == "None" ~
                              paste("%{x}<br>",
                                    "%{yaxis.title.text}: %{y}<br>",
                                    "Non-target Image"),
                            PointSize != 0 & input$secondy != "None" ~
                              paste("%{x}<br>",
                                    "%{yaxis.title.text}: %{y}<br>",
                                    "%{text}<br>",
                                    "Target Image"),
                            TRUE ~
                              paste("%{x}<br>",
                                    "%{yaxis.title.text}: %{y}<br>",
                                    "Target Image"))
        )
      
      
      
      
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
        x             = ~ Datetime,
        y             = mainydata(),
        text          = stringr::str_glue("{y_sec}: {secondydata()}"),
        # Have to store text on secondary y data so it can get picked up on the hover
        color         = ~ UserLabel,
        colors        = "Set1",
        type          = 'scattergl',
        mode          = 'lines+markers',
        line          = list(
          width       = 0.5),
        marker        = list(
          size        = ~ PointSize),
        source        = "hoverplotsource",
        customdata    = ~ UserLabel,
        hovertemplate = ~ Hover
      ) %>%
        
        
        event_register('plotly_click') %>% # lets you click points
        
        # Second y axis
        plotly::add_trace(
          x             = ~ Datetime,
          y             = secondydata(),
          text          = stringr::str_glue("{y_prime}: {mainydata()}"),
          # Have to store text on main y data so it can get picked up on the hover
          color         = ~ UserLabel,
          colors        = "Set1",
          yaxis         = "y2",
          mode          = 'lines+markers', 
          line          = list(
            width       = 0.5),
          marker        = list(
            symbol      = "triangle-up",
            size        = ~ PointSize),
          hovertemplate = ~ Hover
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
          ),
          hovermode = "closest"
          
        ) %>%
        
        
        # Customize highlighting
        
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
        myFiles <- stringr::str_c(inpath, input$site[i], 
                                  list.files(stringr::str_c(inpath, input$site[i], sep = "/")), 
                                  sep = "/" )
        
        
        # Read in and clean up the data
        
        d <- read.csv(myFiles[stringr::str_ends(myFiles, ".csv")])
        d <- d %>%
          dplyr::mutate(Datetime = lubridate::ymd_hms(.data$Datetime, 
                                                      tz = tz),
                        Image_Datetime = lubridate::ymd_hms(.data$Image_Datetime, 
                                                            tz = tz),
                        TimeofDay = "Day",
                        DTMatch = "Yes",
                        Folder = input$site[i]
          ) %>%
          dplyr::arrange(.data$Datetime)
        
        
        # Allow for only showing certain images
        
        if (input$dayonly == TRUE) {
          d <- d %>%
            dplyr::mutate(TimeofDay = dplyr::case_when(
              SceneCaptureType == "Night" ~ "Night",
              TRUE ~ "Day")
            )
        }
        
        if (input$matchonly == TRUE) {
          d <- d %>%
            dplyr::mutate(DTMatch = dplyr::case_when(
              as.numeric(.data$Datetime) == as.numeric(.data$Image_Datetime) ~ "Yes",
              TRUE ~ "No")
            )
        }
        
        
        # Bind site data to the compiled tibble
        dat <- rbind(dat, d)
        
      }
      
      
      # Filter out all data except the clicked point
      dat <- dat %>%
        dplyr::filter(.data$Datetime == click_event()$x & 
                        .data$UserLabel == click_event()$customdata)
      
      
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
      # secondary y-axis value, and image file name under image
      
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
      
      
      # Render the corresponding image in the sidebar
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
                            " (non-exact image)", sep = " ")
        )
        # Anything else
      } else {
        output$image <- renderImage({
          filename <- normalizePath(file.path(
            stringr::str_c(inpath, dat$Folder, dat$FileName, sep = "/")))
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
    
    output$densplot <- renderPlotly({
      
      req(input$fdir)
      
      # Tell Shiny the right parent directory to use
      w <- parseDirPath(wd, input$fdir)
      inpath <- normalizePath(file.path(w))
      
      # Make an empty tibble for storing data from selected sites
      dat <- dplyr::tibble()
      
      for (i in seq_along(input$site)) {
        
        # Find files corresponding to a site
        myFiles <- stringr::str_c(inpath, input$site[i], 
                                  list.files(stringr::str_c(inpath, input$site[i], sep = "/")), 
                                  sep = "/" )
        
        
        # Read in and append data to tibble
        d <- read.csv(myFiles[stringr::str_ends(myFiles, ".csv")])
        dat <- rbind(dat, d)
        
      }
      
      # Summarize image data
      
      dat <- dat %>%
        dplyr::select(FileName, UserLabel, Image_Datetime) %>%
        dplyr::distinct() %>%
        dplyr::mutate(Datetime = ymd_hms(.data$Image_Datetime),
                      Date = as_date(.data$Datetime),
                      Hour = hour(.data$Datetime)
        ) %>%
        dplyr::group_by(.data$Date, .data$Hour) %>%
        dplyr::count()
      
      dat1 <- expand.grid(Date = unique(dat$Date),
                          Hour = c(0:23))
      
      dat2 <- full_join(dat1, dat, by = c("Date", "Hour"))
      
      dat2[is.na(dat2)] <- 0
      
      
      # Plot image data as geom_tiles
      # p <- ggplot2::ggplot(dat) +
      #   ggplot2::geom_tile(ggplot2::aes(x = Date, y = Hour, fill = n)) +
      #   ggplot2::ylim(0,24) +
      #   ggplot2::scale_fill_gradient(name = "# of\nImages") +
      #   ggplot2::theme_minimal()
      
      plotly::plot_ly(
        dat2,
        x             = ~ Date,
        y             = ~ Hour,
        z             = ~ n,
        text          = ~ n,
        colors        = "Greys",
        type          = 'heatmap',
        hovertemplate = paste("%{x}<br>",
                              "Hour of Day: %{y}<br>",
                              "# Images: %{text}<extra></extra>")
      )
      
      
    }) # End of densplot renderPlotly
    
    
    
  } # end of server
  
  
  shinyApp(ui = ui, server = server)
  
  
} # end TS_launch_app



