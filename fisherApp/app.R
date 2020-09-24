#Import library
suppressWarnings(library(CVXR, warn.conflicts=FALSE))
suppressWarnings(library(tidyverse, warn.conflicts=FALSE))

# Input Variables
num_goods <- 3
num_buyers <- 3


rho <- 0.5

get_prices <- function(budgets, valuations, utilities, rho) {
  
  if (utilities == "linear") {
    # Linear Prices
    # Variables to solve for
    allocations <- Variable(num_buyers, num_goods)
    
    # Common constraints across all programs for all utilities
    constraint1 <- sum_entries(allocations, axis = 2) <= 1
    constraint2 <- allocations >= 0  
    
    # Program
    objective <- Maximize(sum( budgets * log(sum_entries( valuations*allocations, axis = 1 ) ) )  ) 
    problem <- Problem(objective, constraints = list(constraint1, constraint2))
    result <- solve(problem)
    prices <- result$getDualValue(constraint1)
    
  } else if (utilities == "Leontief") {
    # Leontief Prices
    # Variables to solve for
    price <- Variable(num_goods)
    # allocations <- Variable(num_buyers, num_goods)
    # utils <- Variable(num_buyers)
    
    # Common constraints across all programs for all utilities
    # constraint1 <- sum_entries(allocations, axis = 2) <= 1
    constraint <- price >= 0  
    # constraint3 <- utils <= min_entries(allocations/valuations, axis = 1)
    
    # Program
    objective <- Minimize(sum(price) - t(budgets) %*% log(valuations %*% price))
    problem <- Problem(objective, constraints = list(constraint))
    result <- solve(problem)
    prices <- result$getValue(price)
  } else if (utilities == "CES") {
    # CES Prices
    # Variables to solve for
    allocations <- Variable(num_buyers, num_goods)
    utils <- Variable(num_buyers)
    # Common constraints across all programs for all utilities
    constraint1 <- sum_entries(allocations, axis = 2) <= 1
    constraint2 <- allocations >= 0  
    constraint3 <- utils == sum_entries( valuations*(power(allocations, rho)), axis = 1 )
    
    # Program
    objective <- Maximize(sum( budgets * log( utils ) )  ) 
    problem <- Problem(objective, constraints = list(constraint1, constraint2, constraint3))
    result <- psolve(problem, ignore_dcp = T)
   
    prices <- result$getDualValue(constraint1)
    
  } else {
    # Cobb-Douglas prices
    v <- apply(valuations, MARGIN = 1, function(x) x/sum(x))
    prices <- v %*% budgets
  }
  return(prices)
}

inputsFirstPage <- navlistPanel(
  tabPanel("Values",
           flowLayout(
           column( width = 12, h3("Buyer 1"),
           sliderInput("value1buyer1",
                       "Good 1:",
                       value = 0.5,
                       min = 0,
                       max = 1,
                       step = 0.01),
           sliderInput("value2buyer1",
                       "Good 2:",
                       value = 0.3,
                       min = 0,
                       max = 1,
                       step = 0.01),
           sliderInput("value3buyer1",
                       "Good 3:",
                       value = 0.2,
                       min = 0,
                       max = 1,
                       step = 0.01)),
           column( width = 12, 
                   h3("Buyer 2"),      
           sliderInput("value1buyer2",
                       "Good 1:",
                       value = 0.5,
                       min = 0,
                       max = 1,
                       step = 0.01),
           sliderInput("value2buyer2",
                       "Good 2:",
                       value = 0.3,
                       min = 0,
                       max = 1,
                       step = 0.01),
           sliderInput("value3buyer2",
                       "Good 3:",
                       value = 0.2,
                       min = 0,
                       max = 1,
                       step = 0.01)),
           column( width = 12, 
                   h3("Buyer 3"),      
           sliderInput("value1buyer3",
                       "Value for good 1:",
                       value = 0.5,
                       min = 0,
                       max = 1,
                       step = 0.01),
           sliderInput("value2buyer3",
                       "Value for good 2:",
                       value = 0.3,
                       min = 0,
                       max = 1,
                       step = 0.01),
           sliderInput("value3buyer3",
                       "Value for good 3:",
                       value = 0.2,
                       min = 0,
                       max = 1,
                       step = 0.01)
           ))
  ),
  tabPanel("Budgets",
           sliderInput("budget1",
                       "Buyer 1:",
                       value = 0.5,
                       min = 0,
                       max = 1,
                       step = 0.01),
           sliderInput("budget2",
                       "Buyer 2:",
                       value = 0.3,
                       min = 0,
                       max = 1,
                       step = 0.01),
           sliderInput("budget3",
                       "Buyer 3",
                       value = 0.2,
                       min = 0,
                       max = 1,
                       step = 0.01)
  )
) 

# Second page
secondPage <- sidebarLayout(
  sidebarPanel(
    sliderInput("rho",
                "Value of Rho for CES Utilities",
                min = 0.01,
                max = 1,
                step = 0.01,
                ticks = F,
                value = 0.5),
    navlistPanel(
      tabPanel("Buyer 1",
               numericInput("CESvalue1buyer1",
                            "Value for good 1 of buyer 1:",
                            value = 0.5,
                            min = 0,
                            max = 1,
                            step = 0.01),
               numericInput("CESvalue2buyer1",
                            "Value for good 2 of buyer 1:",
                            value = 0.3,
                            min = 0,
                            max = 1,
                            step = 0.01),
               numericInput("CESvalue3buyer1",
                            "Value for good 3 of buyer 1:",
                            value = 0.2,
                            min = 0,
                            max = 1,
                            step = 0.01),
               numericInput("CESbudget1",
                            "Budget for buyer 1:",
                            value = 0.5,
                            min = 0,
                            max = 1,
                            step = 0.01)
      ),
      tabPanel("Buyer 2",
               numericInput("CESvalue1buyer2",
                            "Value for good 1 of buyer 2:",
                            value = 0.5,
                            min = 0,
                            max = 1,
                            step = 0.01),
               numericInput("CESvalue2buyer2",
                            "Value for good 2 of buyer 2:",
                            value = 0.3,
                            min = 0,
                            max = 1,
                            step = 0.01),
               numericInput("CESvalue3buyer2",
                            "Value for good 3 of buyer 2:",
                            value = 0.2,
                            min = 0,
                            max = 1,
                            step = 0.01),
               numericInput("CESbudget2",
                            "Budget for buyer 2:",
                            value = 0.3,
                            min = 0,
                            max = 1,
                            step = 0.01)
      ),
      tabPanel("Buyer 3",
               numericInput("CESvalue1buyer3",
                            "Value for good 1 of buyer 3:",
                            value = 0.5,
                            min = 0,
                            max = 1,
                            step = 0.01),
               numericInput("CESvalue2buyer3",
                            "Value for good 2 of buyer 3:",
                            value = 0.3,
                            min = 0,
                            max = 1,
                            step = 0.01),
               numericInput("CESvalue3buyer3",
                            "Value for good 3 of buyer 3:",
                            value = 0.2,
                            min = 0,
                            max = 1,
                            step = 0.01),
               numericInput("CESbudget3",
                            "Budget for buyer 3",
                            value = 0.2,
                            min = 0,
                            max = 1,
                            step = 0.01)
      )
    )
  ),
  
  # Show a plot 
  mainPanel(
    plotOutput("cesPlot")
  )
)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Fisher Market Equilibrium Prices for Market with 3 Goods and 3 Buyers"),
   fluidRow(column(width = 5, h3("Equilibrium Prices"), tableOutput("prices")),
            column(width = 5, plotOutput("summaryPlot", height = 250))
   ),
   fluidRow(inputsFirstPage)
   
   # tabsetPanel(
   #   tabPanel("Summary Plots", firstPage),
   #   tabPanel("CES Fisher Markets", secondPage)
   #   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$summaryPlot <- renderPlot({
     # Set inputs
     budgets <- c(input$budget1, input$budget2, input$budget3)
     valuations <- matrix(c(input$value1buyer1, input$value2buyer1, input$value3buyer1, 
                            input$value1buyer2, input$value2buyer2, input$value3buyer2,
                            input$value1buyer3, input$value2buyer3, input$value3buyer3), byrow = T, nrow = num_buyers, ncol = num_goods)

     # calculate prices based on inputs
     cd_prices <- round(get_prices(budgets, valuations, "cobb-douglas"), 3)
     linear_prices <-  round(get_prices(budgets, valuations, "linear"), 3)
     leontief_prices <-  round(get_prices(budgets, valuations, "Leontief"), 3)
    
     prices_output <- cbind(c("1", "2", "3"), cd_prices, linear_prices, leontief_prices)
     rownames(prices_output) <- c("1", "2", "3")
     colnames(prices_output) <- c("Goods", "Cobb-Douglas", "Linear", "Leontief")
     # Set dataframe   
     prices <- rbind(cd_prices, linear_prices, leontief_prices)
     prices_data <- data.frame(cbind(c(rep("Cobb-Douglas", num_goods), rep("Linear", num_goods), rep("Leontief", num_goods)), rep(1:num_goods, 3)), prices)
     colnames(prices_data) <- c("Utilities", "Goods", "Prices")
     
     output$prices <- renderTable(prices_output)
     # Return Plot
     return(ggplot(data = prices_data, aes(fill = Utilities, y = Prices, x = Goods)) + geom_col(position = "dodge") )
 
   })
 
   output$cesPlot <- renderPlot({
     # Set inputs
     budgets <- c(input$CESbudget1, input$CESbudget2, input$CESbudget3)
     valuations <- matrix(c(input$CESvalue1buyer1, input$CESvalue2buyer1, input$CESvalue3buyer1, 
                            input$CESvalue1buyer2, input$CESvalue2buyer2, input$CESvalue3buyer2,
                            input$CESvalue1buyer3, input$CESvalue2buyer3, input$CESvalue3buyer3), byrow = T, nrow = num_buyers, ncol = num_goods)
     # calculate prices based on inputs
     prices <- get_prices(budgets, valuations, "CES", input$rho)
     
     # Set dataframe   
     prices_data <- data.frame(cbind(prices, paste("Good", 1:num_goods)))
     colnames(prices_data) <- c("Prices", "Good")
     
     # Return Plot
     return(ggplot(data = prices_data, aes(fill = Goods, y = Prices, x = Goods)) + geom_col(position = "dodge") )
     
   })
}



# Run the application 
shinyApp(ui = ui, server = server)

