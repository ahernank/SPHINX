# install.packages(c('shiny', 'shinythemes', 'tidyverse'))


library(shiny)
library(shinythemes)
library(tidyverse)

# Load data
# trend_data <- read_csv("data/trend_data.csv")
# trend_description <- read_csv("data/trend_description.csv")
coreDum<-data.frame(
  ptId=paste0("pt", c(1,2,3,4,5,6,7,8,9,10)), 
  wardSamp=c("C", "C", "A", "A", "C", "A", "C", "A", "C", "B"), 
  # admDat=dmy(c("1/1/2017", "03/1/2017", "02/1/2017", "07/1/2017", "04/1/2017", 
               # "05/1/2017", "09/1/2017", "05/1/2017", "02/1/2017", "07/1/2017")),
  # disDat=dmy(c("17-01-2017", "20-01-2017", "07-01-2017", "25-01-2017", "28-01-2017", "11-01-2017", 
               # "19-01-2017", "17-01-2017", "27-01-2017", "25-01-2017")),
  samp=dmy(c("14/1/2017", "7/1/2017","4/1/2017", "18/1/2017","13/1/2017",
             "10/1/2017", "12/1/2017","9/1/2017", "26/1/2017","19/1/2017")), 
  # genClus=c(1,2,3,3,1,2,1,2,3,3),
  # var1=c("A", "A", NA, "B", "A", "A", "B", "A", "B", "B"),
  # var2=c("B", NA, "B", "B", "B", "B", "B", "B", "B", "A"),
  # var3=c("B", "A", "B", "B", "A", "A", "A", "A", "B", "A"),
  stringsAsFactors = FALSE
)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Hospital outbreak visualization"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select variable type of trend to plot
                    selectInput(inputId = "type",
                                label = strong("Trend index"),
                                choices = c("Advertising & marketing" = "advert",
                                            "Education" = "educat",
                                            "Small business" = "smallbiz",
                                            "Travel" = "travel",
                                            "Unemployment" = "unempl"),
                                selected = "travel"),
                    
                    # Descriptor text
                    HTML("The index is set to 1.0 on January 1, 2004 and is
                         calculated only for US search traffic."),
                    
                    # Line for visual separation
                    hr(),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"),
                                   start = "2007-01-01", end = "2017-07-31",
                                   min = "2007-01-01", max = "2017-07-31"),
                    
                    # Line for visual separation
                    hr(),
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother",
                                  label = strong("Overlay smooth trend line"),
                                  value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f",
                                                 label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness.")
                    )
                    ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    textOutput(outputId = "description"),
                    plotOutput(outputId = "lineplot"),
                    tags$a(href="https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends"))
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  # selected_trends <- reactive({
  #   req(input$date)
  #   validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
  #   validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
  #   trend_data %>%
  #     filter(
  #       type == input$type,
  #       date > input$date[1] & date < input$date[2]
  #     )
  # })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    
    tab1<-table(coreDum$samp)
    barplot(x = names(tab1), y = tab,
         xlab = "Date", ylab = "Trend index",
         col = color, fg = color,
         col.lab = color, col.axis = color)
    # Display only if smoother is checked
    # if(input$smoother){
    #   smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$close, f = input$f)
    #   lines(smooth_curve, col = "#E6553A", lwd = 3)
    # }
  })
  
  # Pull in description of trend
  # output$description <- renderText({
  #   trend_description %>%
  #     filter(type == input$type) %>%
  #     pull(text)
  # })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
