library(shiny)

# Define recipes and default parameters
recipes_defaults <- list(
  "Lilo: Basic 1" = list(
    temp = 92, ratio = 16, coffee = 15, steps = list(
      list(action = "Bloom", amount = 30, time = "0:00 - 0:40"),
      list(action = "1st infusion", amount = 65, time = "0:40 - 0:55"),
      list(action = "2nd infusion", amount = 70, time = "1:05 - 1:20"),
      list(action = "3rd infusion", amount = 75, time = "1:30 - 1:50"),
      list(action = "Finish", amount = NA, time = "~2:45")
    )
  ),
  "LiLo: Basic 2" = list(
    temp = 90, ratio = 230/15, coffee = 15, 
    steps = list(
      list(action = "Bloom", amount = 30, time = "0:00 - 0:40"),
      list(action = "1st infusion", amount = 70, time = "0:40 - 0:50"),
      list(action = "2nd infusion", amount = 60, time = "1:00 - 1:25"),
      list(action = "3rd infusion", amount = 70, time = "1:35 - 2:00"),
      list(action = "Finish", amount = NA, time = "~2:30")
    )
  ),
  "Lilo: Dark Roast" = list(
    temp = 89, ratio = 15, coffee = 15, steps = list(
      list(action = "Bloom", amount = 30, time = "0:00 - 0:40"),
      list(action = "1st infusion", amount = 70, time = "0:40 - 1:00"),
      list(action = "2nd infusion", amount = 60, time = "1:10 - 1:30"),
      list(action = "3rd infusion", amount = 65, time = "1:40 - 2:00"),
      list(action = "Finish", amount = NA, time = "~2:20")
    )
  ),
  "Lilo: Light Roast" = list(
    temp = 92, ratio = 15, coffee = 15, steps = list(
      list(action = "Bloom", amount = 30, time = "0:00 - 0:40"),
      list(action = "1st infusion", amount = 70, time = "0:40 - 1:00"),
      list(action = "2nd infusion", amount = 60, time = "1:10 - 1:30"),
      list(action = "3rd infusion", amount = 65, time = "1:40 - 2:00"),
      list(action = "Finish", amount = NA, time = "~2:20")
    )
  ),
  "Glitch: Dark Roast" = list(
    temp = 86, ratio = 260/15, coffee = 15, 
    steps = list(
      list(action = "Bloom", amount = 70, time = "0:00 - 0:30"),
      list(action = "1st infusion", amount = 140, time = "0:30 - 1:00"),
      list(action = "2nd infusion", amount = 50, time = "1:20 - 1:40"),
      list(action = "Finish", amount = NA, time = "~2:30")
    )
  ),
  "Leaves" = list(
    temp = 90, ratio = 200/12.5, coffee = 12.5, 
    steps = list(
      list(action = "Bloom", amount = 50, time = "0:00 - 0:30"),
      list(action = "1st infusion", amount = 50, time = "0:30 - 0:50"),
      list(action = "2nd infusion", amount = 50, time = "1:00 - 1:20"),
      list(action = "3rd infusion", amount = 50, time = "1:30 - 1:50"),
      list(action = "Finish", amount = NA, time = "~2:30")
    )
  ),
  "Ogawa" = list(
    temp = 91, ratio = 270/14, coffee = 14, 
    steps = list(
      list(action = "Bloom", amount = 40, time = "0:00 - 0:30"),
      list(action = "1st infusion", amount = 60, time = "0:40 - 1:05"),
      list(action = "2nd infusion", amount = 60, time = "1:15 - 1:40"),
      list(action = "3rd infusion", amount = 60, time = "1:50 - 2:15"),
      list(action = "4th infusion", amount = 50, time = "2:25 - 2:40"),
      list(action = "Finish", amount = NA, time = "~3:00")
    )
  ),
  "L'Arbre à Café" = list(
    temp = 94, ratio = 250/15, coffee = 15, 
    steps = list(
      list(action = "Bloom", amount = 45, time = "0:00 - 0:30"),
      list(action = "1st infusion", amount = 155, time = "0:30 - 2:30"),
      list(action = "2nd infusion", amount = 50, time = "2:30 - 3:00"),
      list(action = "Finish", amount = NA, time = "~3:00")
    )
  )
)

# UI
ui <- fluidPage(
  titlePanel("V60 Recipe Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("recipe", "Choose a recipe:", choices = names(recipes_defaults)),
      numericInput("temp", "Water Temperature (°C):", value = 92, min = 80, max = 100, step = 1),
      numericInput("coffee_dose", "Coffee Dose (g):", value = 15, min = 10, max = 30, step = 0.5),
      numericInput("ratio", "Coffee to Water Ratio:", value = 16, min = 10, max = 20, step = 0.1),
      textOutput("total_water")
    ),
    
    mainPanel(
      h3("Recipe Steps"),
      tableOutput("recipe_steps")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Update parameters when recipe changes
  observeEvent(input$recipe, {
    defaults <- recipes_defaults[[input$recipe]]
    
    updateNumericInput(session, "temp", value = defaults$temp)
    updateNumericInput(session, "coffee_dose", value = defaults$coffee)
    updateNumericInput(session, "ratio", value = round(defaults$ratio, 2))
  })
  
  # Dynamic calculation of total water
  total_water <- reactive({
    input$coffee_dose * input$ratio
  })
  
  output$total_water <- renderText({
    paste("Total Water:", round(total_water(), 1), "g")
  })
  
  # Update recipe steps with recalculated quantities
  output$recipe_steps <- renderTable({
    defaults <- recipes_defaults[[input$recipe]]
    
    ref_coffee <- defaults$coffee
    ref_ratio <- defaults$ratio
    ref_water <- ref_coffee * ref_ratio
    
    total_cumulative <- 0
    
    adjusted_steps <- lapply(defaults$steps, function(step) {
      # Don't adjust quantity for "Finish", leave blank
      if (step$action == "Finish") {
        return(list(
          Step = step$action,
          Quantity = "",  # Leave blank
          Time = step$time,
          Total = ""  # Leave blank
        ))
      } else {
        adjusted_amount <- round(step$amount * (input$coffee_dose / ref_coffee) * (input$ratio / ref_ratio), 1)
        total_cumulative <<- total_cumulative + adjusted_amount
        return(list(
          Step = step$action,
          Quantity = paste(adjusted_amount, "g"),
          Time = step$time,
          Total = paste(total_cumulative, "g")
        ))
      }
    })
    
    do.call(rbind, adjusted_steps)
  })
}

# Launch the Shiny app
shinyApp(ui = ui, server = server)