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
                  selected = sort(unique(shot_data$PLAYER_NAME))[3]),  # Default to the first player in the list
      selectInput("action_type", "Action Type:", 
                  choices = c("All", unique(shot_data$ACTION_TYPE)), 
                  selected = unique(shot_data$ACTION_TYPE)[1]),  # Default to the first action type in the list
      selectInput("period", "Game Period:", 
                  choices = c("All", sort(unique(shot_data$PERIOD), decreasing = FALSE)), 
                  selected = "1"),  # Default to the first period (assuming it's labeled as "1")
      sliderInput("distance", "Shot Distance:", min = 0, max = 50, value = c(0, 50)),
      sliderInput("loc_x", "Select X Coordinate:", min = -250, max = 250, value = 0),
      sliderInput("loc_y", "Select Y Coordinate:", min = -50, max = 500, value = 0),
      textOutput("accuracy_info")
    ),
    mainPanel(
      plotOutput("shot_chart")
    )
  )
)

# Define the server logic for the application
server <- function(input, output, session) {
  
  # Initialize action type with 'All'
  updateSelectInput(session, "action_type", choices = c("All", unique(shot_data$ACTION_TYPE)), selected = "All")
  
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
    data <- data[data$SHOT_DISTANCE >= input$distance[1] & data$SHOT_DISTANCE <= input$distance[2], ]
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
