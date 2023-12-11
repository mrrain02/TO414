library(shiny)
library(ggplot2)
library(ggforce)

# Load the shot data
shot_data <- read.csv("2022_shot_data_new.csv")

# Define the UI for the application
ui <- fluidPage(
  titlePanel("NBA Shot Chart Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("player_name", "Select Player:", 
                  choices = c("All", sort(unique(shot_data$PLAYER_NAME))),
                  selected = sort(unique(shot_data$PLAYER_NAME))[3]),
      selectInput("action_type", "Action Type:", 
                  choices = c("All", unique(shot_data$ACTION_TYPE)), 
                  selected = unique(shot_data$ACTION_TYPE)[1]),
      selectInput("period", "Game Period:", 
                  choices = c("All", sort(unique(shot_data$PERIOD), decreasing = FALSE)), 
                  selected = "1"),
      sliderInput("filter_distance", "Filter by Shot Distance:", min = 0, max = 50, value = c(0, 50)),
      sliderInput("pred_distance", "Prediction: Shot Distance", min = 0, max = 50, value = 25),
      sliderInput("loc_x", "Select X Coordinate:", min = -250, max = 250, value = 0),
      sliderInput("loc_y", "Select Y Coordinate:", min = -50, max = 500, value = 0),
      sliderInput("minutes_remaining", "Minutes Remaining", min = 0, max = 12, value = 12),
      sliderInput("seconds_remaining", "Seconds Remaining", min = 0, max = 60, value = 60),
      actionButton("predict", "Predict"),
      textOutput("accuracy_info")
    ),
    mainPanel(
      plotOutput("shot_chart"),
      actionButton("predict", "Predict"),
      textOutput("predictionOutput"),  # Add this line to display the prediction
      textOutput("test_prediction_output")
    )
  )
)

# Define the server logic for the application
server <- function(input, output, session) {
  
  # Initialize action type with 'All'
  updateSelectInput(session, "action_type", choices = c("All", unique(shot_data$ACTION_TYPE)), selected = "All")
  
  # Load the logistic regression model
  model <- readRDS("NbaModel_2.rds")
  print(model)
  
  observeEvent(input$predict, {
    print("Predict button clicked") # Debugging
    
    # Prepare user input for prediction
    user_input <- data.frame(
      SHOT_DISTANCE = input$pred_distance,
      LOC_X = input$loc_x,
      LOC_Y = input$loc_y
    )
    
    print(user_input) # Debugging
    
    # Apply log1p transformation to SHOT_DISTANCE
    user_input$SHOT_DISTANCE <- log1p(user_input$SHOT_DISTANCE)
    
    # Make prediction
    prediction_prob <- predict(model, newdata = user_input, type = "response")
    prediction_prob <- prediction_prob - 0.2
    print(prediction_prob) # Debugging
    
    prediction_result <- ifelse(prediction_prob > 0.6, "Likely to Make", "Likely to Miss")
    
    # Display the prediction result
    output$predictionOutput <- renderText({
      paste("Prediction: ", prediction_result)
      
      # Format probability as a percentage
      prediction_percentage <- sprintf("%.2f%%", prediction_prob * 100)
      
      # Combine prediction result with percentage for output
      prediction_text <- paste("Prediction: ", prediction_result, 
                               " (Probability: ", prediction_percentage, ")")
      
      # Display the prediction result with probability
      output$predictionOutput <- renderText(prediction_text)
    })
  })
  
  
  
  # Update action types based on selected player
  observe({
    player_shots <- if (input$player_name == "All") {
      shot_data
    } else {
      subset(shot_data, PLAYER_NAME == input$player_name)
    }
    action_types <- c("All", unique(player_shots$ACTION_TYPE))
    updateSelectInput(session, "action_type", choices = action_types, selected = "All")
  })
  
  # Reactive expression to filter shot data
  filtered_shots <- reactive({
    data <- shot_data
    if (input$player_name != "All") {
      data <- data[data$PLAYER_NAME == input$player_name, ]
    }
    if (input$action_type != "All") {
      data <- data[data$ACTION_TYPE == input$action_type, ]
    }
    if (input$period != "All") {
      data <- data[data$PERIOD == input$period, ]
    }
    
    # Use 'filter_distance' for filtering the shots
    data <- data[data$SHOT_DISTANCE >= input$filter_distance[1] & data$SHOT_DISTANCE <= input$filter_distance[2], ]
    data
  })
  
  
  
  # Calculate shooting accuracy near the cursor
  observe({
    shots <- filtered_shots()
    cursor_radius <- 20 # Define the radius around the cursor
    near_cursor <- subset(shots, sqrt((LOC_X - input$loc_x)^2 + (LOC_Y - input$loc_y)^2) <= cursor_radius)
    if (nrow(near_cursor) > 0) {
      accuracy <- mean(near_cursor$SHOT_MADE_FLAG) * 100
      output$accuracy_info <- renderText(sprintf("Shooting accuracy within %d units of cursor: %.1f%%", cursor_radius, accuracy))
    } else {
      output$accuracy_info <- renderText("No shots within the specified radius of the cursor.")
    }
  })
  
  # Generate shot chart
  output$shot_chart <- renderPlot({
    shots <- na.omit(filtered_shots()) # Remove rows with NA values
    if (nrow(shots) == 0) return() # Exit if no data to plot
    
    ggplot(shots, aes(x = LOC_X, y = LOC_Y, color = as.factor(SHOT_MADE_FLAG))) +
      geom_point() +
      # Key
      annotate("rect", xmin = -80, xmax = 80, ymin = -50, ymax = 140, color = "blue", alpha = 0.5) +
      # Cursor
      geom_point(aes(x = input$loc_x, y = input$loc_y), color = "black", size = 5) +
      xlim(-250, 250) +
      ylim(-50, 500) +
      coord_fixed(ratio = 1.2) +
      scale_color_manual(values = c("1" = "green", "0" = "red")) +
      labs(color = "Shot Made Flag", title = "NBA Shot Chart") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)