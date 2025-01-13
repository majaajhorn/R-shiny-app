install.packages(c("shiny", "lpSolve", "shinythemes", "plotly"))

library(shiny)
library(lpSolve)
library(shinythemes)
library(plotly)

# UI definition
ui <- fluidPage(
  theme = shinytheme("flatly"),  
  
  tags$head(
    tags$style(HTML("
            .well { background-color: #ffffff; }
            .box-shadow { 
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                padding: 15px;
                border-radius: 5px;
                margin-bottom: 20px;
                background-color: white;
            }
            .category-header {
                color: #2c3e50;
                margin-bottom: 20px;
                padding-bottom: 10px;
                border-bottom: 2px solid #ecf0f1;
            }
            .pie-chart-section {
                padding-top: 40px;
            }
            .shiny-input-container {
                margin-bottom: 15px;
            }
            .action-button {
                margin-top: 20px;
                width: 100%;
            }
            #optimize {
                background-color: #2980b9;
                color: white;
                padding: 10px;
                font-size: 16px;
                border: none;
                border-radius: 5px;
                transition: background-color 0.3s;
            }
            #optimize:hover {
                background-color: #3498db;
            }
        "))
  ),
  
  titlePanel(
    div(class = "category-header",
        h1("Optimizacija mjesečnog budžeta", 
           style = "color: #2c3e50; font-weight: 300;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "box-shadow",
          h4("Osnovne postavke", class = "category-header"),
          numericInput("total_budget", 
                       "Koliko iznose Vaša ukupna mjesečna primanja (plaća i ostali izvori)? (€)",
                       value = 0, min = 0),
          numericInput("additional_income",
                       "Imate li dodatni izvor zarade (npr. iznajmljivanje nekretnine)? Ako ne, unesite 0. (€)",
                       value = 0, min = 0),
          numericInput("savings_target",
                       "Koliki iznos mjesečno želite uštedjeti? (€)",
                       value = 0, min = 0)
      ),
      
      div(class = "box-shadow",
          h4("Stanovanje", class = "category-header"),
          numericInput("housing_min",
                       "Koliko najmanje iznose Vaši mjesečni troškovi za stanovanje (najam i režije)? (€)",
                       value = 0, min = 0),
          numericInput("housing_max",
                       "Koliko najviše iznose Vaši mjesečni troškovi za stanovanje (najam i režije)? (€)",
                       value = 0, min = 0)
      ),
      
      div(class = "box-shadow",
          h4("Hrana", class = "category-header"),
          numericInput("food_min",
                       "Koliko najmanje mjesečno trošite na hranu? (€)",
                       value = 0, min = 0),
          numericInput("food_max",
                       "Koliko najviše mjesečno trošite na hranu? (€)",
                       value = 0, min = 0)
      ),
      
      div(class = "box-shadow",
          h4("Prijevoz", class = "category-header"),
          numericInput("transport_min",
                       "Koliko najmanje mjesečno trošite na prijevoz? (€)",
                       value = 0, min = 0),
          numericInput("transport_max",
                       "Koliko najviše mjesečno trošite na prijevoz? (€)",
                       value = 0, min = 0)
      ),
      
      div(class = "box-shadow",
          h4("Luksuz/zabava", class = "category-header"),
          numericInput("luxury_min",
                       "Koliko najmanje mjesečno trošite na luksuz i zabavu? (€)",
                       value = 0, min = 0),
          numericInput("luxury_max",
                       "Koliko najviše mjesečno trošite na luksuz i zabavu? (€)",
                       value = 0, min = 0)
      ),
      
      actionButton("optimize", "Optimiziraj budžet", 
                   class = "action-button")
    ),
    
    mainPanel(
      width = 8,
      div(class = "box-shadow",
          h3("Optimalna raspodjela troškova:", 
             class = "category-header"),
          verbatimTextOutput("results")
      ),
      
      div(class = "box-shadow",
          h3("Vizualizacija:", 
             class = "category-header"),
          plotlyOutput("budget_plot"),
          div(class = "pie-chart-section",
              h3("Udio u ukupnom budžetu:", 
                 class = "category-header"),
              plotlyOutput("pie_chart")
          )
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  optimizeBudget <- eventReactive(input$optimize, {
    # Postavljanje problema linearnog programiranja
    
    # Koeficijenti funkcije cilja (minimiziramo ukupne troškove)
    obj <- c(1, 1, 1, 1)
    
    # Matrica ograničenja ostaje ista
    const.mat <- matrix(c(
      1, 1, 1, 1,    # Ukupni budžet
      1, 0, 0, 0,    # Min stanovanje
      1, 0, 0, 0,    # Max stanovanje
      0, 1, 0, 0,    # Min hrana
      0, 1, 0, 0,    # Max hrana
      0, 0, 1, 0,    # Min prijevoz
      0, 0, 1, 0,    # Max prijevoz
      0, 0, 0, 1,    # Min luksuz
      0, 0, 0, 1     # Max luksuz
    ), nrow = 9, byrow = TRUE)
    
    # Ažurirana desna strana ograničenja s dodatnim izvorom zarade
    total_income <- input$total_budget + input$additional_income
    
    rhs <- c(
      total_income - input$savings_target,  # Ukupni budžet (primanja + dodatni izvor) minus ušteda
      input$housing_min,    # Min stanovanje
      input$housing_max,    # Max stanovanje
      input$food_min,       # Min hrana
      input$food_max,       # Max hrana
      input$transport_min,  # Min prijevoz
      input$transport_max,  # Max prijevoz
      input$luxury_min,     # Min luksuz
      input$luxury_max      # Max luksuz
    )
    
    # Znakovi ograničenja ostaju isti
    const.dir <- c("<=", ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=")
    
    # Rješavanje problema
    solution <- lp("min", obj, const.mat, const.dir, rhs)
    
    return(list(
      status = solution$status,
      solution = solution$solution
    ))
  })
  
  output$results <- renderPrint({
    result <- optimizeBudget()
    
    if (result$status == 0) {
      total_income <- input$total_budget + input$additional_income
      total_spent <- sum(result$solution)
      savings <- total_income - total_spent
      
      cat("Optimalna raspodjela troškova:\n\n")
      cat(sprintf("💰 Ukupna primanja: %.2f €\n", input$total_budget))
      cat(sprintf("💵 Dodatni izvor zarade: %.2f €\n", input$additional_income))
      cat(sprintf("📊 Ukupno raspoloživo: %.2f €\n\n", total_income))
      cat(sprintf("📍 Stanovanje: %.2f €\n", result$solution[1]))
      cat(sprintf("🍽️ Hrana: %.2f €\n", result$solution[2]))
      cat(sprintf("🚌 Prijevoz: %.2f €\n", result$solution[3]))
      cat(sprintf("🎉 Luksuz/zabava: %.2f €\n", result$solution[4]))
      cat("\n--------------------------------\n")
      cat(sprintf("💰 Ukupni troškovi: %.2f €\n", total_spent))
      cat(sprintf("💵 Mjesečna ušteda: %.2f €\n", savings))
      cat(sprintf("📊 Postotak uštede: %.1f%%\n", (savings/total_income)*100))
    } else {
      cat("❌ Nije moguće pronaći optimalno rješenje s zadanim ograničenjima.\n")
      cat("⚠️ Molimo prilagodite parametre.")
    }
  })
  
  output$budget_plot <- renderPlotly({
    result <- optimizeBudget()
    if (result$status == 0) {
      categories <- c("Stanovanje", "Hrana", "Prijevoz", "Luksuz")
      plot_ly(
        x = categories,
        y = result$solution,
        type = "bar",
        marker = list(
          color = c("#3498db", "#2ecc71", "#e74c3c", "#f1c40f")
        )
      ) %>%
        layout(
          title = "Raspodjela troškova po kategorijama",
          xaxis = list(title = "Kategorija"),
          yaxis = list(title = "Iznos (€)"),
          showlegend = FALSE
        )
    }
  })
  
  output$pie_chart <- renderPlotly({
    result <- optimizeBudget()
    if (result$status == 0) {
      total_spent <- sum(result$solution)
      savings <- input$total_budget - total_spent
      
      values <- c(result$solution, savings)
      labels <- c("Stanovanje", "Hrana", "Prijevoz", "Luksuz", "Ušteda")
      colors <- c("#3498db", "#2ecc71", "#e74c3c", "#f1c40f", "#9b59b6")
      
      plot_ly(
        labels = labels,
        values = values,
        type = "pie",
        marker = list(colors = colors),
        textinfo = "label+percent"
      ) %>%
        layout(
          title = "Udio u ukupnom budžetu",
          showlegend = TRUE
        )
    }
  })
}

# Pokretanje aplikacije
shinyApp(ui = ui, server = server)
