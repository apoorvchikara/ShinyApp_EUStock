library(shiny)
library(ggplot2)
library(reshape2)  # for melt function

# Load the EuStockMarkets dataset
data("EuStockMarkets")

# Convert to data frame for ggplot2
stock_data <- as.data.frame(EuStockMarkets)
# Generate sequence of dates starting from 1991-01-01 with daily frequency (excluding weekends)
start_date <- as.Date("1991-01-01")
stock_data$Date <- seq.Date(start_date, by = "day", length.out = nrow(stock_data))

# UI
ui <- fluidPage(
  titlePanel("Trend of European Stock Markets (1991-1998)"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "stocks",
        "Choose stock indices to display:",
        choices = colnames(EuStockMarkets),
        selected = colnames(EuStockMarkets)
      ),
      sliderInput(
        "bins",
        "Number of bins:",
        min = 5,
        max = 100,
        value = 30
      )
    ),
    mainPanel(
      tags$p("Contains the daily closing prices of major European stock indices: Germany DAX (Ibis), Switzerland SMI, France CAC, and UK FTSE. The data are sampled in business time, i.e., weekends and holidays are omitted.",sep ="\n"),
      plotOutput("stockPlot")
    )
  )
)

# Server
server <- function(input, output) {
  output$stockPlot <- renderPlot({
    # Filter the selected stocks
    selected_data <- stock_data[, c("Date", input$stocks), drop = FALSE]
    melted_data <- melt(selected_data, id.vars = "Date")
    
    # Plot the data as a bar plot
    ggplot(melted_data, aes(x = value, fill = variable)) +
      geom_histogram(bins = input$bins, position = "dodge") +
      labs(
        title = "Distribution of European Stock Markets",
        x = "Closing Price",
        y = "Frequency",
        fill = "Stock Index"
      ) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
