library(shiny)
library(bslib)


# Define the tax calculation function
calculate_tax <- function(income) {
  if (is.na(income)) {
    return(list(tax = NA, net_pay = NA))# Handle NA values
  }
  per_allow <- 12570
  basic_var <- 50270
  high_var <- 125140
  
  if (income <= per_allow) {
    tax <- 0.0
  } else if (income <= basic_var) {
    tax <- (income - per_allow) * 0.2
  } else if (income <= high_var) {
    tax <- (income - basic_var) * 0.4 + (basic_var - per_allow) * 0.2
  } else {
    tax <- (income - high_var) * 0.45 + (high_var - basic_var) * 0.4 + (basic_var - per_allow) * 0.2
  }
  
  # Calculate net pay
  net_pay <- income - tax
  
  return(list(tax = tax, net_pay = net_pay))
}

ui <- fluidPage(
  titlePanel(title = "Payroll Tax Calculator"),
  theme = bs_theme(
    bg = "white",
    fg = "black",
    primary = "blue",
    base_font = font_google("Nunito")
  ),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Full Name:"),
      textInput("organization", "Organization:"),
      numericInput("income", "Income (£):", value = 0),
      actionButton("calculate", "Calculate Tax"),
      width = 5
    ),
    mainPanel(
      uiOutput("summary"),  # Use textOutput instead of verbatimTextOutput
      div(
        img(src='tax.png', height="450px", width="450px", style = "position: absolute; bottom: 0; right: 0; opacity: 0.65;"),
      ),
      width = 6,
      position = "left"
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output) {
  output$summary <- renderUI({
    if (is.null(input$name) | is.null(input$organization) | is.null(input$income)) {
      return(NULL)
    } else {
      if (input$calculate > 0) {
        results <- calculate_tax(input$income)
        summary <- HTML(paste(
          "Summary for ", input$name, ", working at ", input$organization, ".<br>",
          "Income: £" , round(input$income, 2), ",<br>",
          "Tax: £" , round(results$tax, 2), ",<br>",
          "Net Pay: £" , round(results$net_pay, 2), ".", sep = ""
        ))
        return(summary)
      }
    }
  })
}

# Run the Shiny app

shinyApp(ui = ui, server = server)
