# camTS: A Shiny app for visualizing image data as a time series

The Shiny app in this package allows the user to generate time series plots with the dependent variable(s) of his/her choice. Then, the user can click on data points within the time series plot to view the image associated with that data point.

To use the app, navigate to a parent folder containing folders of data for each site. Each site folder should contain exactly one CSV file of data and the associated images. Your CSV's will need to contain a date-time data column called "Datetime". See the other helper functions in the `camTS` package for prepping your data to feed into the app. 

To download the package, run the following code:

```
devtools::install_github("kaitlynstrickfaden/camTS")
```

<br>

![screengrab](/CamTS_Screengrab.jpg?raw = TRUE "CamTS Screengrab") 
