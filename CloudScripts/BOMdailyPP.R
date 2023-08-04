########################
#BOM daily point process
########################

StatNum = read.csv("BOMDailyNumber.csv", header = FALSE)


BOM.daily = list()
k = 1
min1 = 9
max1 = 0
Lon1 = rep(0, nrow(StatNum))
Lat1 = rep(0, nrow(StatNum))
file0 = "https://raw.githubusercontent.com/hinestein/BOMDaily/main/"
for(i in 1:nrow(StatNum)){
  tryCatch({
    BOM.daily[[1]] = read.csv(paste0(file0, StatNum[i,1]), header = TRUE)[,-1]
    if(BOM.daily[[1]]$Date[1] < min1){
      min1 = BOM.daily[[1]]$Date[1]
    }
    if(BOM.daily[[1]]$Date[nrow(BOM.daily[[1]])] > max1){
      max1 = BOM.daily[[1]]$Date[nrow(BOM.daily[[1]])]
    }
    Lon1[i] = BOM.daily[[1]]$Lon[1]
    Lat1[i] = BOM.daily[[1]]$Lat[1]
    k = k + 1
  }, error = function(e){})
  print(i)
}

s2 = NULL
for(i in 1:length(BOM.daily)){
  s2 = c(s2, BOM.daily[[i]]$Station[1])
}

Loc.df = data.frame(Lon = Lon1, Lat = - Lat1)

ggplot() + 
  geom_point(Loc.df, mapping = aes(x = Lon, y = Lat), size = 0.1) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "sLID-Precipitation Correlation", colour = "Correlation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)




library(RCurl)
getURL("https://unimelbcloud-my.sharepoint.com/personal/benjamin_hines_unimelb_edu_au/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Fbenjamin%5Fhines%5Funimelb%5Fedu%5Fau%2FDocuments%2FPrecipitation%20Data%2FBOMDaily")







setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/BOMDaily")
BOM.daily = list()

k = 1
for(i in 1:nrow(Station.Num)){
  tryCatch({
  if(nchar(Station.Num[i,1]) == 4){
    s1 = paste0("00", Station.Num[i,1])
  }else if(nchar(Station.Num[i,1]) == 5){
    s1 = paste0("0", Station.Num[i,1])
  }else{
    s1 = Station.Num[i,1]
  }
  BOM.daily[[k]] = read.csv(paste0("BOMDaily", s1), header = TRUE)[,2:10]
  k = k + 1
  }, error = function(e){})
  print(i)
}

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")

Day.loc = NULL
for(i in 1:length(BOM.daily)){
  Day.loc = rbind(Day.loc, c(BOM.daily[[i]][1,1], BOM.daily[[i]][1,2], length(complete.cases(BOM.daily[[i]]))))
}

colnames(Day.loc) = c("Lon", "Lat", "n")
Day.loc = as.data.frame(Day.loc)


ggplot() + 
  geom_point(Day.loc, mapping = aes(x = Lon, y = -Lat, colour = n), size = 1) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = " ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") + 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)



setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/BOMDaily")

for(i in 1:length(BOM.daily)){
  BOM.daily[[i]]$Lat = -BOM.daily[[i]]$Lat
}

for(i in 1:length(BOM.daily)){
  s2 = BOM.daily[[i]]$Station[1]
  if(nchar(s2) == 4){
    s1 = paste0("00", s2)
  }else if(nchar(s2) == 5){
    s1 = paste0("0", s2)
  }else{
    s1 = Station.Num[i,1]
  }
  write.csv(BOM.daily[[i]], file = paste0(BOMDaily, s1), row.names = FALSE)
  print(i)
}

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")

PP1 = list()
k = 1
for(i in 1:length(BOM.daily)){
  tryCatch({
    Temp = BOM.daily[[i]]
    Temp = Temp[complete.cases(Temp),]
    Temp.year = unique(Temp$Year)
    PP1[[k]] = matrix(0, ncol = 3, nrow = length(Temp.year))
    for(j in 1:length(Temp.year)){
      Temp1 = Temp[Temp$Year == Temp.year[j],]
      PP1[[k]][j,] = c(Temp.year[j], mean(Temp1$Rain > 0), nrow(Temp1))
    }
    k = k + 1
  }, error = function(e){})
  if(i %% 1000 == 0){
    print(i)
  }
}


PP1[[3]]


ncount = NULL
for(i in 1:length(PP1)){
  ncount = c(ncount, nrow(PP1[[i]]))
}






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










## Only run examples in interactive R sessions
if (interactive()) {
  
  # Example of UI with fluidPage
  ui <- fluidPage(
    
    # Application title
    titlePanel("Hello Shiny!"),
    
    sidebarLayout(
      
      # Sidebar with a slider input
      sidebarPanel(
        sliderInput("obs",
                    "Number of observations:",
                    min = 0,
                    max = 1000,
                    value = 500)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot")
      )
    )
  )
  
  # Server logic
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
  
  
  # UI demonstrating column layouts
  ui <- fluidPage(
    title = "Hello Shiny!",
    fluidRow(
      column(width = 4,
             "4"
      ),
      column(width = 3, offset = 2,
             "3 offset 2"
      )
    )
  )
  
  shinyApp(ui, server = function(input, output) { })
}








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

# Define server logic for slider examples ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Integer",
               "Decimal",
               "Range",
               "Custom Format",
               "Animation"),
      Value = as.character(c(input$integer,
                             input$decimal,
                             paste(input$range, collapse = " "),
                             input$format,
                             input$animation)),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
}







## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    sliderInput("obs", "Number of observations:",
                min = 0, max = 1000, value = 500
    ),
    plotOutput("distPlot")
  )
  
  # Server logic
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
}



## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    sliderInput("obs", "Number of observations:",
                min = 0, max = 1000, value = 500
    ),
    plotOutput("distPlot")
  )
  
  # Server logic
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
}


library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)

ui <- dashboardPage(
  
  dashboardHeader(title = "Example App"),
  dashboardSidebar(),
  dashboardBody(
    
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        title = "Line chart", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("plot1", height = 250)),
      box(
        title = "Inputs", status = "primary", solidHeader = TRUE,  collapsible = TRUE,
        sliderInput("obs", "Number of observations:",
                    min = 1, step = 1, max = nrow(mtcars), value = nrow(mtcars)),
        selectInput("carbs", "Select carb to show",
                    choices = c('All', unique(mtcars$carb))
        )
      ),
    )
  )
)

server <- function(input, output) {
  
  sample_cars <- reactive({
    set.seed(8675309)  # for some consistent sampling
    df <- mtcars[sample(x=1:nrow(mtcars), size = input$obs),]
    if(input$carbs != "All")
      df <- df %>% dplyr::filter(carb == input$carbs)
    return(df)
  })
  
  output$plot1 <- renderPlot({
    ggplot(sample_cars(), aes(mpg, disp)) + geom_point() +
      labs(title=paste('You selected',input$obs, 'cars\n and to show',input$carbs, 'values of carb!'))
  }, bg="transparent")
}

shinyApp(ui, server)


## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    sliderInput("n", "Day of month", 1, 30, 10),
    dateRangeInput("inDateRange", "Input date range")
  )
  
  server <- function(input, output, session) {
    observe({
      date <- as.Date(paste0("2013-04-", input$n))
      
      updateDateRangeInput(session, "inDateRange",
                           label = paste("Date range label", input$n),
                           start = date - 1,
                           end = date + 1,
                           min = date - 5,
                           max = date + 5
      )
    })
  }
  
  shinyApp(ui, server)
}




library(shiny)
library(ggplot2)
library(dplyr)

load("resource_day_sum")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "trajectory", 
                  label = "Date Range:",
                  min = as.POSIXct("2000-04-01"),
                  max = as.POSIXct("2022-03-31"),
                  value = c(as.POSIXct("2000-04-01"), as.POSIXct("2022-03-31")), 
                  timeFormat="%Y-%m-%d", step = 1)),
    mainPanel(plotOutput(outputId = "lines", height = "70vh"))))



server <- function(input, output){
  
  output$lines <- renderPlot({
    A.1 %>%
      filter(Time >= input$trajectory[1] & Time <= input$trajectory[2]) %>% 
      ggplot(aes(x = X, y = Y)) + facet_wrap(~Quantile) +
      geom_path() + theme_bw() + 
      theme(plot.title = element_text(size = 18, face = "bold"),
            legend.title=element_text(size=18), axis.text=element_text(size=18), legend.text=element_text(size=18),
            legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) + labs(x = "mm/month", y = "mm/month^2")
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("myplot", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file=file)
      plot(myplot())
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)


PP1 = data.frame(x = rnorm(length(d1)), y = rnorm(length(d1)), Dates = as.yearmon(2000 + (seq(0, 521) + 3)/12))



substr(Sys.time(),0,10)
curr.year = as.numeric(substr(Sys.time(),0,4))
curr.month = as.numeric(substr(Sys.time(),6,7))
curr.day = as.numeric(substr(Sys.time(), 9,10))

years1 = c(rep(1979:(curr.year - 1), each = 12), if(curr.month != 1){rep(curr.year, curr.month - 1)})

months1 = c(rep(1:12, length(1979:(curr.year - 1))), if(curr.month != 1){1:(curr.month - 1)})
dates1 = cbind(years1, months1)
days1 = NULL
for(i in 1:nrow(dates1)){
  if(dates1[i,2] == 1 | dates1[i,2] == 3 | dates1[i,2] == 5 | dates1[i,2] == 7 | dates1[i,2] == 8 | dates1[i,2] == 10 | dates1[i,2] == 12){
    days1 = c(days1, 31)
  }else if(dates1[i,2] == 2){
    if(dates1[i,1] %% 4 == 0){
      days1 = c(days1, 29)
    }else{
      days1 = c(days1, 28)
    }
  }else if(dates1[i,2] == 4 | dates1[i,2] == 6 | dates1[i,2] == 9 | dates1[i,2] == 11){
    days1 = c(days1, 30)
  }
}

datesNOAA = cbind(dates1, days1)

d1 = paste0(datesNOAA[,1], "-", ifelse(nchar(datesNOAA[,2]) == 1, paste0("0", datesNOAA[,2]), datesNOAA[,2]), "-", 15)
d1 = as.POSIXct(d1)



A.1 = read.csv("https://raw.githubusercontent.com/hinestein/FDAworl/main/PhaseData", header = TRUE)

A.1$Time = as.POSIXct(A.1$Time)

A.1$Quantile = factor(A.1$Quantile, levels = c("p = 0", "p = 0.5", "p = 1"))




## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    dateRangeInput("daterange1", "Date range:",
                   start = "2001-01-01",
                   end   = "2010-12-31"),
    
    # Default start and end is the current date in the client's time zone
    dateRangeInput("daterange2", "Date range:"),
    
    # start and end are always specified in yyyy-mm-dd, even if the display
    # format is different
    dateRangeInput("daterange3", "Date range:",
                   start  = "2001-01-01",
                   end    = "2010-12-31",
                   min    = "2001-01-01",
                   max    = "2012-12-21",
                   format = "mm/dd/yy",
                   separator = " - "),
    
    # Pass in Date objects
    dateRangeInput("daterange4", "Date range:",
                   start = Sys.Date()-10,
                   end = Sys.Date()+10),
    
    # Use different language and different first day of week
    dateRangeInput("daterange5", "Date range:",
                   language = "de",
                   weekstart = 1),
    
    # Start with decade view instead of default month view
    dateRangeInput("daterange6", "Date range:",
                   startview = "decade")
  )
  
  shinyApp(ui, server = function(input, output) { })
}



# Generate random variates
TotsLactul        <- rep(ymd("2016-01-01"),10000)
randomMonths      <- round(runif(n = 10000,min = 0,max = 11),0) 
randomDays        <- round(runif(n = 10000,min = 0,max = 28),0)

# Increments days
month(TotsLactul) <- month(TotsLactul) + randomMonths  
day(TotsLactul)   <- day(TotsLactul)   + randomDays  

# Make it a DT
TotsLactul        <- data.table(x=TotsLactul)


ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("St Thomas' Physiology Data Console"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("DatesMerge",
                  "Dates:",
                  min = as.Date("2016-01-01","%Y-%m-%d"),
                  max = as.Date("2016-12-01","%Y-%m-%d"),
                  value=as.Date("2016-12-01"),
                  timeFormat="%Y-%m-%d")
    ),
    mainPanel(
      plotOutput("distPlotLactul"))
    
  )
))

server <- shinyServer(function(input, output) {
  
  output$distPlotLactul <- renderPlot({
    #Create the data
    DatesMerge<-input$DatesMerge
    
    # draw the histogram with the specified number of bins
    ggplot(TotsLactul[month(x) == month(DatesMerge)],mapping=aes(x=x))+
      geom_histogram(bins=100)+
      labs(title=paste("Num")) +
      xlab("Time") +
      ylab("NumP") +
      theme(axis.text.x=element_text(angle=-90)) +
      theme(legend.position="top")+
      theme(axis.text=element_text(size=6))
    
    
  })
  
  
})







