
library(plotly)
library(shiny)
library(grid)
library(ggplot2)
library(jpeg)

# Read the CSV file with player data
all_shots <- read.csv("2022_shot_data_new.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Basketball Shots Explorer"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown for player selection
      selectInput("player", "Select Player:", choices = unique(all_shots$full_name)),
      br(),
      helpText("Select a player from the dropdown.")
    ),
    mainPanel(
      # Output: Basketball court plot
      plotlyOutput("basketballCourt")
    )
  )
)

# Define server
server <- function(input, output) {
  # Reactive function to filter data based on player selection
  selected_player_shots <- reactive({
    all_shots[all_shots$full_name == input$player, ]
  })
  # courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
  # court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
  #                     width=unit(1,"npc"), height=unit(1,"npc"))
  # Output: ggplot plot
  output$ggplotChart <- renderPlot({
    ggplot(selected_player_shots(), aes(x = LOC_X, y = LOC_Y)) +
      geom_point(aes(colour = SHOT_ZONE_BASIC, shape = EVENT_TYPE)) +
      xlim(250, -250) +
      ylim(-50, 420) +
      geom_rug(alpha = 0.2) +
      coord_fixed() +
      ggtitle(paste("Shot Chart\n", unique(selected_player_shots()$full_name), sep = "")) +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(size = 15, lineheight = 0.9, face = "bold"))
  })
}





# Run the application
shinyApp(ui = ui, server = server)
