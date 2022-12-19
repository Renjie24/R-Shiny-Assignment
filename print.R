library(shiny)
library(ggplot2)
library(readr)
ui <- fluidPage(
  titlePanel("R Shiny Assignment"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", label = "Choose Claims Data Files", accept = ".csv", buttonLabel = "Browse...", multiple = TRUE),
      tableOutput("files"),
      sliderInput("tail", label = "Tail Factor", 1.1, min = 1.0, max = 2.0)),
    mainPanel(
      tabsetPanel(
        tabPanel("plot", fluidRow(plotOutput("loss_development", click = "plot_click"),
                                  dataTableOutput("table")))),
        verbatimTextOutput("info"))))
loss_year <- c("2017", "2018", "2019")
development_year_1 <- c(524792, 798502, 917636)
development_year_2 <- c(743057, 995659, 1205710)
development_year_3 <- c(745282, 998640, 1209320)
development_year_4 <- c(819810, 1098504, 1330252)
claim <- as.data.frame(cbind(loss_year, development_year_1, development_year_2, development_year_3, development_year_4))
server <- function(input, output){
  output$files <- renderTable({  
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    read.csv(file$datapath, header = input$tail)})
  observe({
    data = input$file1
    if(is.null(data))
      return(NULL)})
  output$loss_development <- renderPlot({
    data <- read_csv("ggplot2/data.csv")
    plot(data$"1", data$"Loss Year", col="red", main = "Cummulative Claims Paid ($)", col.main="black", xlab="Development Years", ylab="Claims Paid")
    lines(data$"2", data$"Loss Year", col="green")
    lines(data$"3", data$"Loss Year", col="blue")
    lines(data$"4", data$"Loss Year", col="purple")
       legend("bottomright", inset = 0.05, c("2017", "2018", "2019"), lty=1, col=c("red", "green", "blue"), title = "Years")
  }, res=96)
  output$table <- renderDataTable(claim)
  output$info <- renderPrint({
    req(input$plot_click)
    x <- round(input$plot_click$x, 2)
    y <- round(input$plot_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")})}
shinyApp(ui, server)
