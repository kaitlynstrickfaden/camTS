
### Make time series plot of image data and display images
### Kaitlyn Strickfaden
### 1/10/2021


library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(lubridate)
library(htmltools)
library(htmlwidgets)
library(DT)
#source("R/TS_check_image.R")



ui <- fluidPage(theme = shinytheme("slate"),
                
                # App title ----
                titlePanel("Time Series"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Panel for inputs ----
                  sidebarPanel(
                  
                    
                    # Input: Selection of site
                    
                    # You will have to add folders containing images and
                    # CSV's of data to the "www" folder in the app folder.
                    # The display name will be the folder name.
                    
                    selectInput(inputId = "Site",
                                label = "Site",
                                choices = list.files("www"),
                                selected = "TW"
                    ),
                    
                    
                    # Input: Selection of variable on y axis
                    
                    # if you add more choices here, you will also have to add them
                    # to the "data" reactive argument in the server.
                    
                    selectInput(inputId = "var",
                                label = "Response",
                                choices = c("Stage"),
                                selected = "Stage"
                    ),
                    
                    
                    ## Sidebar Panel Outputs: ---------------
                    
                    textOutput("text1"), # Date and time
                    textOutput("text2"), # Response variable value
                    textOutput("text3"), # Image file path
                    
                    imageOutput("image"), # Image
                    
                    height = 1200

                  ), # end sidebar panel
                  
                  
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    ## Main Panel Output: ----
                    
                    plotlyOutput("clickplot"), # Time series plot
                    #height = 800,
                    
                    plotlyOutput("table")
                    
                  ) # end mainPanel
                  
                ) # end sidebarLayout
                
) # end fluidPage






server <- function(input, output, session) {
  
  
  output$clickplot <- renderPlotly({
    
    # Find files corresponding to selected site
    myImages <- str_c("www", input$Site, 
                      list.files(str_c("www", input$Site, sep = "/") ), 
                      sep = "/" )
    
    # Find csv file in wd
    d <- myImages[str_ends(myImages, ".csv")]
    
    # Find images in wd
    #myImages <- myImages[!str_ends(myImages, ".csv")]
    
    # Read in and clean up the data
    dat <- read_csv(d)
    dat <- dat %>%
      filter(!Stage < 0) %>%
      mutate(#Time = as.character(Time),
             #Datetime = str_c(Date, Time, sep = " "),
             Datetime = ymd_hms(Datetime),
             image_url = SourceFile) %>%
      arrange(Datetime)
    
    hdat <- highlight_key(dat, ~Datetime)
    # lets you highlight the click event
    
    
    ## This commented out bit is supposed to let you choose any column from the data frame as the dependent variable, but I can't get it to work yet... 
    # outVar <- observe({
    #   names(dat)
    # })
    # 
    # reactive({
    #   updateSelectInput(session,
    #                     inputId = "var",
    #                     choices = outVar(),
    #                     selected = input$var)
    # })
    
    # Allow response variable to be a user input
    
    # if you add more choices here, you will also have to add them
    # to the response variable "selectInput" in the UI.
    
    data <- reactive({
      switch(input$var,
             "Stage" = dat$Stage
             #"Temperature" = dat$Temperature
      )
      })
    
    # Make the plot
    plot_ly(
      hdat,
      x         = ~ Datetime,
      y         = data(),
      type      = 'scatter',
      mode      = 'lines+markers',
      marker = list(
        color = 'rgb(0, 0, 0)'
        ),
      line = list(
        color = 'rbg(0,0,0)'
      ),
      hoverinfo = 'y',
      source = "hoverplotsource",
      customdata = ~ image_url#,
      #width  = session$clientData$output_clickplot_width,
      #height = 800
    ) %>%
      layout(
        title = str_glue("Time series for {input$Site}"),
        xaxis = list(
          title = "Datetime"
        ),
        yaxis = list(
          title = str_glue("{input$var}")
        )
        ) %>%
      
      event_register('plotly_click') %>% # lets you click points
      #event_register('plotly_doubleclick') %>%
      highlight("plotly_click", color = toRGB("red")) # highlights the clicked point


    
  }) # End of renderPlotly
  
  
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
      str_glue("{input$var}: ", as.character(click_event()$y), sep = " ")
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
  
  
  
  # doubleclick_event <- reactive({
  #   event_data(event = "plotly_doubleclick", source = "hoverplotsource")
  # })
  # 
  # 
  # # Once a double click happens...
  # observeEvent(doubleclick_event(), {
  #   
  #   output$table <- DT::renderDataTable({
  #     
  #     myImages <- str_c("www", input$Site, 
  #                       list.files(str_c("www", input$Site, sep = "/") ), 
  #                       sep = "/" )
  #     
  #     # Find csv file in wd
  #     d <- myImages[str_ends(myImages, ".csv")]
  #     
  #     # Find images in wd
  #     myImages <- myImages[!str_ends(myImages, ".csv")]
  #     
  #     # Read in and clean up the data
  #     dat <- read_csv(d)
  #     dat <- dat %>%
  #       mutate(Time = as.character(Time),
  #              Datetime = str_c(Date, Time, sep = " "),
  #              Datetime = dmy_hms(Datetime),
  #              image_url = myImages,
  #              OtherWhat = as.character(OtherWhat))
  #     
  #     dat
  #     
  #   })
  # 
  # })
  
  
}

shinyApp(ui = ui, server = server)

