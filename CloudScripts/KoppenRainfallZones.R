#################
#Koppen and Rainfall zones
#################

seas1 = read.csv("https://raw.githubusercontent.com/hinestein/Koppenandrainfallzones/main/seasrain.txt", header = FALSE, sep = " ")

nr = 139
nc = 178

xll = 111.875
yll = -44.625

cs = 0.25

x.vec = seq(xll, length.out = nc, by = cs)
y.vec = rev(seq(yll, length.out = nr, by = cs))


seas.mat = matrix(0, ncol = 3, nrow = nr * nc)
k = 1
for(i in 1:nr){
  for(j in 1:nc){
    seas.mat[k,] = c(seas1[i,j], y.vec[i], x.vec[j])
    k = k + 1
  }
}

colnames(seas.mat) = c("Class", "Lat", "Lon")
seas.mat = as.data.frame(seas.mat)

seas.mat= seas.mat[seas.mat$Class != -9999,]

plot(seas.mat$Lon, seas.mat$Lat)



A1 = seas.mat[,3:2]
coordinates(A1) = ~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)
Aus.1 = which(a3$ISO3 == "AUS")

seas.mat = seas.mat[Aus.1,]

ggplot() + 
  geom_point(seas.mat, mapping = aes(x = Lon, y = Lat, colour = Class), size = 0.5)+ 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Square Root Precipitation (mm)^(1/2)")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)




kop = read.csv("https://raw.githubusercontent.com/hinestein/Koppenandrainfallzones/main/kpngrp.txt", header = FALSE, sep = " ")


nr = 1361
nc = 1681

xll = 112
yll = -44

cs = 0.025

x.vec = seq(xll, length.out = nc, by = cs)
y.vec = rev(seq(yll, length.out = nr, by = cs))


kop.mat = matrix(0, ncol = 3, nrow = nr * nc)
k = 1
for(i in 1:nr){
  for(j in 1:nc){
    kop.mat[k,] = c(kop[i,j], y.vec[i], x.vec[j])
    k = k + 1
  }
}

colnames(kop.mat) = c("Class", "Lat", "Lon")
kop.mat = as.data.frame(kop.mat)

kop.mat= kop.mat[kop.mat$Class != -9999,]


A1 = kop.mat[,3:2]
coordinates(A1) = ~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)
Aus.1 = which(a3$ISO3 == "AUS")

kop.mat = kop.mat[Aus.1,]

ggplot() +
  geom_point(kop.mat, mapping = aes(x = Lon, y = Lat, colour = Class), size = 0.5)+ 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Square Root Precipitation (mm)^(1/2)")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)









# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

# Load data
trend_data <- read_csv("data/trend_data.csv")
trend_description <- read_csv("data/trend_description.csv")

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Google Trend Index"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "type", label = strong("Trend index"),
                                choices = unique(trend_data$type),
                                selected = "Travel"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = "2007-01-01", end = "2017-07-31",
                                   min = "2007-01-01", max = "2017-07-31"),
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness.")
                    )
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"),
                    textOutput(outputId = "desc"),
                    tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    trend_data %>%
      filter(
        type == input$type,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
        ))
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_trends()$date, y = selected_trends()$close, type = "l",
         xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$close, f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  # Pull in description of trend
  output$desc <- renderText({
    trend_text <- filter(trend_description, type == input$type) %>% pull(text)
    paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)



# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      sliderInput("integer", "Integer:",
                  min = 0, max = 1000,
                  value = 500),
      
      # Input: Decimal interval with step value ----
      sliderInput("decimal", "Decimal:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1),
      
      # Input: Specification of range within an interval ----
      sliderInput("range", "Range:",
                  min = 1, max = 1000,
                  value = c(200,500)),
      
      # Input: Custom currency format for with basic animation ----
      sliderInput("format", "Custom Format:",
                  min = 0, max = 10000,
                  value = 0, step = 2500,
                  pre = "$", sep = ",",
                  animate = TRUE),
      
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      sliderInput("animation", "Looping Animation:",
                  min = 1, max = 2000,
                  value = 1, step = 10,
                  animate =
                    animationOptions(interval = 300, loop = TRUE))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      tableOutput("values")
      
    )
  )
)

library(shiny)
runExample("05_sliders")
