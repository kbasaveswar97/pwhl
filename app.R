library(shiny)
library(dplyr)
library(DT) 
library(ggplot2)
library(lubridate)
library(viridis)
library(grid)  # Load the grid package
library(png)
library(shinydashboard)



# Sample data (replace with your actual data)
data <- read.csv("hackathon_nwhl.csv")
# Transforming coordinates


# Convert game_date to Date
data$game_date <- as.Date(data$game_date)
# Replace spaces with underscores in column names
colnames(data) <- gsub(" ", "_", colnames(data))
# Ensure syntactically valid column names
colnames(data) <- make.names(colnames(data), unique = TRUE)
# Check the modified column names
sorted_players <- sort(unique(data$Player))


# Assuming the image is in the 'www' directory of your Shiny app
pitch_image_path <- "./www/rink_coords.png"
pitch_image <- readPNG(pitch_image_path)



# Extract year from 'game_date' and create 'season' column
data <- mutate(data, season = year(game_date))

ui <- dashboardPage(
  dashboardHeader(title = "PWHL Offensive Performance Tracking Panel."),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Player Stats", tabName = "player_stats", icon = icon("user")),
      menuItem("Season Stats", tabName = "season_stats", icon = icon("calendar-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard tab content
      tabItem(tabName = "player_stats",
              fluidRow(
                box(title = "Welcome", status = "primary", solidHeader = TRUE, width =12, 
                    "An all-encompassing solution/tool for dissecting offensive strategies within the league. 
              Dive into the offensive data of individual players."),
                box(selectizeInput("player_search", "Player:", choices = NULL, selected = "Mikyla Grant-Mentis")),
                box(selectInput("season_player", "Season:", choices = NULL, selected = "2021")),
                box(DTOutput("player_stats_table"), solidHeader = TRUE,width = 12),
                box(plotOutput("player_pass_plot"), solidHeader = TRUE,width = 12),
                box(plotOutput("player_goalmap_plot"), solidHeader = TRUE,width = 12),
                box(plotOutput("zone_entry_plot"), solidHeader = TRUE,width = 12)
              )
      ),
      # Season Stats tab content
      tabItem(tabName = "season_stats",
              fluidRow(
                box(selectInput("season", "Season", choices = NULL), width = 4),
                box(DTOutput("summary_table"), width = 12)
              )
      )
    )
  )
)



server <- function(input, output, session) {
  
  # Your existing server code with adjustments for dynamic UI updates
  updateSelectizeInput(session, "player_search", choices = sorted_players, selected = "Mikyla Grant-Mentis")
  updateSelectInput(session, "season_player", choices = c("", unique(data$season)), selected = "2021")
  updateSelectInput(session, "season", choices = c("", sort(unique(data$season), decreasing = TRUE)), selected = sort(unique(data$season), decreasing = TRUE)[1])
  
  # Other reactive outputs and render functions as previously defined
  
  
  # Define a reactive expression for season data
  season_summary_data <- reactive({
    req(input$season)  # Ensure a season is selected
    # Filter the data based on the selected season
    selected_season_data <- data %>%
      filter(season == input$season)
    
    if (nrow(selected_season_data) == 0) {
      # Return an empty data frame with the correct column names if there's no data
      return(data.frame(Player=character(), Total_Goals=integer(), Assists=integer(), 
                        Shots_on_Goal=integer(), Faceoff_Wins=integer(), Penalties_Drawn=integer(), 
                        Power_Play_Goals=integer(), Blocks=integer(), Giveaways=integer(), 
                        Takeaways=integer(), Shot_Accuracy=numeric()))
    }
    
    # Calculate some basic statistics
    player_stats <- selected_season_data %>%
      group_by(Player, Team) %>%
      summarize(
        Total_Goals = sum(Event == "Goal", na.rm = TRUE),
        Shots_on_Goal = sum(Event == "Shot" & Detail.2 == "On Net", na.rm = TRUE),
        Faceoff_Wins = sum(Event == "Faceoff Win", na.rm = TRUE),
        Penalties_Drawn = sum(Event == "Penalty Taken", na.rm = TRUE),
        Takeaways = sum(Event == "Takeaway", na.rm = TRUE),
        Incomplete_Plays = sum(Event == "Incomplete Play", na.rm = TRUE),
        Dump_Ins_Outs = sum(Event == "Dump In/Out", na.rm = TRUE),
        Zone_Entries = sum(Event == "Zone Entry", na.rm = TRUE),
        Plays_Made = sum(Event == "Play", na.rm = TRUE),
        Shot_Attempts = Shots_on_Goal + sum(Event == "Shot" & Detail.2 != "On Net", na.rm = TRUE)
        ) %>%
      mutate(
        Shot_Accuracy = ifelse(Shots_on_Goal > 0, Total_Goals / Shots_on_Goal, 0) # Protect against division by zero
      ) %>%
      mutate(Shot_Accuracy = round(Shot_Accuracy, 3)) %>%
      arrange(desc(Total_Goals))
    
    # Replace Inf and NaN with 0 for Shot_Accuracy
    player_stats$Shot_Accuracy <- ifelse(is.infinite(player_stats$Shot_Accuracy) | is.nan(player_stats$Shot_Accuracy), 0, player_stats$Shot_Accuracy)
    
    # Return the data frame
    player_stats
  })
  
  # Render the summary table for the selected season
  output$summary_table <- renderDT({
    datatable(season_summary_data(), options = list(pageLength = 20, autoWidth = TRUE))
  })
  
  
  
#####dynamic UI when player stats button clicked
  output$player_stats_panel <- renderUI({
    if (active_panel() == "player") {
      fluidRow(
        column(3, selectizeInput("player_search", "Player:", 
                                 choices = sorted_players, 
                                 selected = "Mikyla Grant-Mentis")),  # Pre-select player
        column(2, selectInput("season_player", "Season:", choices = c("", unique(data$season)), 
                              selected = "2021")), 
        column(8, DTOutput("player_stats_table")),
        column(12, plotOutput("player_pass_plot")),
        column(12, plotOutput("player_goalmap_plot")),
        column(12, plotOutput("zone_entry_plot"))
      )
    }
  })
  
  
##reactive UI for player stats
  player_specific_season_data <- reactive({
    req(input$player_search, input$season_player)
    # Filter with standardized values
    selected_player_season_data <- data %>%
      filter(Player == input$player_search, season == input$season_player)
    
    if (nrow(selected_player_season_data) == 0) {
      # Return an empty data frame with the correct column names if there's no data
      return(data.frame(Player=character(), Total_Goals=integer(), Assists=integer(), 
                        Shots_on_Goal=integer(), Faceoff_Wins=integer(), Penalties_Drawn=integer(), 
                        Power_Play_Goals=integer(), Blocks=integer(), Giveaways=integer(), 
                        Takeaways=integer(), Shot_Accuracy=numeric()))
    }
    
    # Calculate some basic statistics
    player_stats <- selected_player_season_data %>%
      group_by(Player, Team) %>%
      summarize(
        Total_Goals = sum(Event == "Goal", na.rm = TRUE),
        Shots_on_Goal = sum(Event == "Shot" & Detail.2 == "On Net", na.rm = TRUE),
        Faceoff_Wins = sum(Event == "Faceoff Win", na.rm = TRUE),
        Penalties_Drawn = sum(Event == "Penalty Taken", na.rm = TRUE),
        Takeaways = sum(Event == "Takeaway", na.rm = TRUE),
        Incomplete_Plays = sum(Event == "Incomplete Play", na.rm = TRUE),
        Dump_Ins_Outs = sum(Event == "Dump In/Out", na.rm = TRUE),
        Zone_Entries = sum(Event == "Zone Entry", na.rm = TRUE),
        Plays_Made = sum(Event == "Play", na.rm = TRUE),
        Shot_Attempts = Shots_on_Goal + sum(Event == "Shot" & Detail.2 != "On Net", na.rm = TRUE)
      ) %>%
      mutate(
        Shot_Accuracy = ifelse(Shots_on_Goal > 0, Total_Goals / Shots_on_Goal, 0) # Protect against division by zero
      ) %>%
      mutate(Shot_Accuracy = round(Shot_Accuracy, 3)) %>%
      arrange(desc(Total_Goals))
    
    # Replace Inf and NaN with 0 for Shot_Accuracy
    player_stats$Shot_Accuracy <- ifelse(is.infinite(player_stats$Shot_Accuracy) | is.nan(player_stats$Shot_Accuracy), 0, player_stats$Shot_Accuracy)
    
    # Return the data frame
    player_stats
  })
  
  output$player_stats_table <- renderDT({
    datatable(player_specific_season_data(),
              options = list(
                pageLength = 20,
                autoWidth = TRUE,
                scrollX = TRUE  # Enable horizontal scrolling
              )
    )
  })
  
###original plot!!!!
  
  player_original_plot <- reactive({
    req(input$player_search, input$season_player)
    
    # Filter the data for the selected player and season
    player_data <- data %>%
      filter(Player == input$player_search, season == input$season_player)
    
    # Filter to include only "Play" and "Pass" events
    play_pass_data <- player_data[player_data$Event %in% c("Play", "Pass"), ]
    
    # Check if there is data to plot
    if(nrow(play_pass_data) > 0) {
      ggplot(play_pass_data, aes(x = X.Coordinate, y = Y.Coordinate, xend = X.Coordinate.2, yend = Y.Coordinate.2)) +
        geom_segment(color = "blue", arrow = arrow(length = unit(0.2, "inches")), size = 1) +
        geom_point(color = "red", size = 3) +
        geom_text(aes(label = Detail.1), hjust = 0, vjust = 0) +  # Add pass details as text
        labs(title = "Plays and Passes", x = "X Coordinate", y = "Y Coordinate") +
        theme_minimal()
    } else {
      # Return an empty plot if no data is available
      ggplot() + 
        theme_void() + 
        ggdraw() + 
        draw_label("No 'Play' or 'Pass' events available for the selected player and season.")
    }
  })
  
  # Render the plot
  output$original_plot <- renderPlot({
    req(input$player_search, input$season_player)  # Ensure player name and season are selected
    player_original_plot()  # Pass player_search as an argument to the reactive function
  })
  
####plots with map behind
  # Define the dimensions of the hockey pitch image
  pitch_width <- 220  # Adjust according to the dimensions of your image
  pitch_height <- 85  # Adjust according to the dimensions of your image
  
  
  # Render the player-specific plot
  player_pass_plot <- reactive({
    req(input$player_search, input$season_player)
    
    # Filter the data for the selected player and season
    player_data <- data %>%
      filter(Player == input$player_search, season == input$season_player)
    
    # Filter to include only "Play" and "Pass" events
    play_pass_data <- player_data[player_data$Event %in% c("Play", "Pass"), ]
    
    # Check if there is data to plot
    if (nrow(play_pass_data) > 0) {
      ggplot(play_pass_data, aes(x = X.Coordinate, y = Y.Coordinate, xend = X.Coordinate.2, yend = Y.Coordinate.2)) +
        annotation_raster(pitch_image, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +  # Add the hockey pitch map image
        geom_segment(aes(color = Detail.1), arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "last"), size = 0.5, linetype = "dotted") +
        geom_point(color = "grey", size = 0.1, show.legend = FALSE) +
        labs(title = "Play map", x = "X Coordinate", y = "Y Coordinate") + 
        scale_color_manual(values = c("Direct" = "darkgreen", "Indirect" = "brown"),
                           labels = c("Direct", "Indirect"),
                           name = "Play Type") +
        guides(color = guide_legend(override.aes = list(size = 3, linetype = 0))) +
        theme_minimal() +
        theme(legend.position = "right",
              axis.title = element_blank(),  # Remove axis titles
              axis.text = element_blank(),  # Remove axis text
              axis.ticks = element_blank(),  # Remove axis ticks
              panel.background = element_blank(),  # Remove background
              panel.grid = element_blank())
      
    } else {
      # Return an empty plot if no data is available
      ggplot() + 
        theme_void()  
    }
  })
  
  # Render the player-pass plot
  output$player_pass_plot <- renderPlot({
    req(input$player_search, input$season_player)  # Ensure player name and season are selected
    player_pass_plot()  # Pass player_search as an argument to the reactive function
  })
  

  

####goal map plot
  player_goalmap_plot <- reactive({
    req(input$player_search, input$season_player)
    
    # Filter the data for the selected player and season
    # Filter the data for the selected player and season
    player_goalmap_data <- data %>%
      filter(Player == input$player_search, season == input$season_player) %>%
      filter(Event == "Shot" | Event == "Goal") %>%
      mutate(Is_Goal = if_else(Event == "Goal", TRUE, FALSE))
    # Check if there is data to plot
    if (nrow(player_goalmap_data) > 0) {
      gg <- ggplot(player_goalmap_data, aes(x = X.Coordinate, y = Y.Coordinate, shape = Detail.1, color = Detail.2, fill = Is_Goal)) +
        annotation_raster(pitch_image, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +  # Add the hockey pitch map image
        geom_point(size = 4.5) +
        scale_shape_manual(values = c("Wristshot" = 23, "Slapshot" = 21, "Snapshot" = 25, "Deflection" = 14, "Wrap Around" = 13, "Fan" = 4),
                           limits = c("Wristshot", "Slapshot", "Snapshot", "Deflection", "Wrap Around", "Fan"),
                           name = "Shot Type") +  # Add title for shape legend
        scale_color_manual(values = c("Blocked" = "black", "Missed" = "gold3", "On Net" = "springgreen3"),
                           limits = c("On Net", "Missed", "Blocked"),
                           name = "Result") +
        theme(legend.position = "right",
              axis.title = element_blank(),  # Remove axis titles
              axis.text = element_blank(),  # Remove axis text
              axis.ticks = element_blank(),  # Remove axis ticks
              panel.background = element_blank(),  # Remove background
              panel.grid = element_blank()) +
        labs(title = "Shot Map") 
      
      if(length(unique(player_goalmap_data$Is_Goal)) == 1) {
        gg <- gg + scale_fill_manual(values = c("TRUE" = "springgreen3", "FALSE" = "transparent"),
                                     breaks = c(TRUE),
                                     labels = c("Goal"),
                                     name = "Event",
                                     guide = guide_legend(override.aes = list(shape = c(21, 1))))
      } else {
        gg <- gg + scale_fill_manual(values = c("TRUE" = "springgreen3", "FALSE" = "transparent"),
                                     breaks = c(TRUE, FALSE),
                                     labels = c("Goal", "Shot"),
                                     name = "Event",
                                     guide = guide_legend(override.aes = list(shape = c(21, 1))))
      }
      
      gg
    } else {
      # Return an empty plot if no data is available
      ggplot() + 
        theme_void()
    }
  })
  
  # Render the plot
  output$player_goalmap_plot <- renderPlot({
    req(input$player_search, input$season_player)  # Ensure player name and season are selected
    player_goalmap_plot()  # Pass player_search as an argument to the reactive function
  })

  
  ####zone-entry plot
  zone_entryplot <- reactive({
    req(input$player_search, input$season_player)
    
    # Filter the data for the selected player and season
    zone_entry_data <- data %>%
      filter(Player == input$player_search, season == input$season_player) %>%
      filter(Event == "Zone Entry")
    
    # Check if there is data to plot
    if(nrow(zone_entry_data) > 0) {
      ggplot(zone_entry_data, aes(x = X.Coordinate, y = Y.Coordinate)) +
        annotation_raster(pitch_image, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +  # Add the hockey pitch map image
        geom_point(aes(shape = Detail.1, fill = Detail.1), size = 4.5) +  # Use shape and fill based on entry type
        scale_shape_manual(values = c("Dumped" = 21, "Carried" = 24)) +  # Define specific shapes for types
        scale_fill_manual(values = c("Dumped" = "green", "Carried" = "blue")) +  # Define colors for types
        labs(title = "Zone Entry Attempts", x = NULL, y = NULL) +  # Remove axis labels with NULL
        theme_minimal() +
        theme(legend.title = element_blank(), 
              axis.title = element_blank(), 
              axis.text = element_blank(), 
              axis.ticks = element_blank(),
              ggtitle = element_text(family = "Arial", size = 18, face = "bold"))  # Remove the legend title
    } else {
      # Return an empty plot if no data is available
      ggplot() + 
        theme_void()  
    }
  })
  
  # Render the plot
  output$zone_entry_plot <- renderPlot({
    req(input$player_search, input$season_player)  # Ensure player name and season are selected
    zone_entryplot()  # Pass player_search as an argument to the reactive function
  })
  
}
shinyApp(ui, server)

