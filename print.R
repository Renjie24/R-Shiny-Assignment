ui <- fluidPage(
  titlePanel("R Shiny Assignment"),
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", label = "Choose Claims Data Files", NULL, buttonLabel = "Browse...", multiple = TRUE),
      tableOutput("files"),
      numericInput("m", "Tail Factor", 1.1, min = 1.0, max = 2.0)),
    mainPanel(
      tabsetPanel(
        tabPanel("plot", fluidRow(plotOutput("plot", click = "plot_click"),
                                  dataTableOutput("table")))),
      verbatimTextOutput("info"))))
loss_year <- c("2017", "2018", "2019")
development_year_1 <- c(524792, 798502, 917636)
development_year_2 <- c(743057, 995659, 1205710)
development_year_3 <- c(745282, 998640, 1209320)
development_year_4 <- c(819810, 1098504, 1330252)
claim <- as.data.frame(cbind(loss_year, development_year_1, development_year_2, development_year_3, development_year_4))
server <- function(input, output){
  output$plot <- renderPlot({
    x <- c(1, 2, 3, 4)
    y1 <- c(524792, 743057, 745282, 819810)
    y2 <- c(798502, 995659, 998640, 1098504)
    y3 <- c(917636, 1205710, 1209320, 1330252)
    plot(x, y1, xlim=c(1, 4), ylim=c(500000, 1500000), type="l", col="red", lwd=4, main="Cummulative Claims Paid ($)", col.main="black", xlab="Development Years", ylab="Claims Paid")
    lines(x, y2, col="green")
    lines(x, y3, col="blue")
    legend("bottomright", inset = 0.05, c("2017", "2018", "2019"), lty=1, col=c("red", "green", "blue"), title = "Years")
  }, res=96)
  output$table <- renderDataTable(claim)
  output$info <- renderPrint({
    req(input$plot_click)
    x <- round(input$plot_click$x, 2)
    y <- round(input$plot_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")})}
shinyApp(ui, server)

