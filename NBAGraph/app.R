library(shiny)
library(ggplot2)

# Load the shot data
shot_data <- read.csv("2022_shot_data_new.csv")

# Define the UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("NBA Shot Chart Explorer"),
  
  # Sidebar with player selection dropdown
  sidebarLayout(
    
    sidebarPanel(
      selectInput("player_name", "Select Player:", choices = unique(shot_data$full_name))
    ),
    
    # Main panel with shot chart
    mainPanel(
      plotOutput("shot_chart")
    )
  )
)

# Define the server logic for the application
server <- function(input, output) {
  
  # Reactive expression to filter shot data based on selected player
  filtered_shots <- reactive({
    player_name <- input$player_name
    if (player_name == "") {
      return(shot_data)
    } else {
      return(shot_data[shot_data$full_name == player_name, ])
    }
  })
  
  # Generate shot chart
  output$shot_chart <- renderPlot({
    filtered_shots <- filtered_shots()
    made_shots <- filtered_shots[filtered_shots$SHOT_MADE_FLAG == 1, ]
    missed_shots <- filtered_shots[filtered_shots$SHOT_MADE_FLAG == 0, ]
    
    total_made <- nrow(made_shots)
    total_missed <- nrow(missed_shots)
    total_shots <- total_made + total_missed
    
    fga_percentage <- ifelse(total_shots > 0, sprintf("%.1f%%", (total_made / total_shots) * 100), "N/A")
    
    court_plot <- ggplot() +
      geom_point(data = made_shots, aes(x = LOC_X, y = LOC_Y), color = "green", size = 5) +
      geom_point(data = missed_shots, aes(x = LOC_X, y = LOC_Y), color = "red", size = 5) +
      coord_fixed(xlim = c(-250, 250), ylim = c(-250, 250)) +
      scale_x_continuous(breaks = seq(-225, 225, 50), labels = seq(-225, 225, 50)) +
      scale_y_continuous(breaks = seq(-225, 225, 50), labels = seq(-225, 225, 50)) +
      theme_bw() +
      theme(panel.grid = element_blank()) +  # Remove gridlines
      labs(title = paste0("Shot Chart for Player: ", filtered_shots()$PLAYER_NAME[1]),
           subtitle = paste("Total Made Shots:", total_made, " | Total Missed Shots:", total_missed,
                            " | FGA Percentage:", fga_percentage),
           caption = "Note: Green dots represent made shots, and red dots represent missed shots.")
    
    print(court_plot)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
