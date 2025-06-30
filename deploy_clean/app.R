############################
# Where Should You Live? - Shiny Dashboard
############################

library(shiny)
library(shinydashboard)
library(leaflet)
# library(leaflet.extras)  
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringdist)
library(shinyWidgets)
library(RColorBrewer)
# library(readxl)  
library(openxlsx)  
library(httr)
library(jsonlite)
library(promises)
library(future)
library(rsconnect)


# Load the pre-processed data
city_data <- read.xlsx("rankings_combined_FINAL.xlsx")  # Using openxlsx instead of readxl


# Diagnostic info about the dataset
print(paste("Loaded dataset with", nrow(city_data), "cities"))

# Check if file contains expected columns
print(paste("File contains Data_Completeness column:", "Data_Completeness" %in% colnames(city_data)))
print(paste("File contains Weighted_Rank column:", "Weighted_Rank" %in% colnames(city_data)))

# Print top cities to verify rankings
print("Top 5 cities by Weighted Score:")
top_weighted <- city_data %>% arrange(Weighted_Rank) %>% head(5)
print(paste(1:5, top_weighted$City, top_weighted$Country, "Score:", round(top_weighted$Weighted_Score, 1)))


# Calculate weighted score function (used ONLY when user changes weights)
calculate_weighted_score <- function(data, 
                                     econ_weight = 1, 
                                     env_weight = 1.5, 
                                     qol_weight = 1.5, 
                                     gov_weight = 1.2) {
  
  # Normalize weights to sum to 1
  total_weight <- econ_weight + env_weight + qol_weight + gov_weight
  econ_weight <- econ_weight / total_weight
  env_weight <- env_weight / total_weight
  qol_weight <- qol_weight / total_weight
  gov_weight <- gov_weight / total_weight
  
  # Calculate weighted score
  weighted_score <- data %>%
    mutate(
      Weighted_Score = 
        Economics_Score * econ_weight +
        Environment_Score * env_weight + 
        QualityOfLife_Score * qol_weight +
        Governance_Score * gov_weight
    ) %>%
    # Add rank based on weighted score
    arrange(desc(Weighted_Score)) %>%
    mutate(Weighted_Rank = row_number())
  
  return(weighted_score)
}


# Function to recalculate rankings based on user preferences
update_rankings <- function(data, weights) {
  # Recalculate weighted score with user-defined weights
  data <- calculate_weighted_score(
    data,
    econ_weight = weights$economics,
    env_weight = weights$environment,
    qol_weight = weights$quality_of_life,
    gov_weight = weights$governance
  )
  
  return(data)
}

# Function to find cities similar to user preferences
calculate_climate_match <- function(city_name, country_code, user_prefs, weather_data_cache) {
  city_key <- paste0(city_name, "_", country_code)
  
  # Get weather data from cache or return default score if not available
  city_weather <- weather_data_cache[[city_key]]
  if (is.null(city_weather)) {
    return(70) # Default score when weather data is unavailable
  }
  
  # Extract user preferences
  temp_range <- user_prefs$temp_preference
  humidity_pref <- user_prefs$humidity_preference
  
  # Calculate temperature match (0-50 points)
  temp_score <- 50
  if (!is.null(city_weather$temperature)) {
    if (city_weather$temperature < temp_range[1]) {
      # Too cold - reduce score based on how far below range
      temp_diff <- temp_range[1] - city_weather$temperature
      temp_score <- max(0, 50 - (temp_diff * 5))
    } else if (city_weather$temperature > temp_range[2]) {
      # Too hot - reduce score based on how far above range
      temp_diff <- city_weather$temperature - temp_range[2]
      temp_score <- max(0, 50 - (temp_diff * 5))
    }
  }
  
  # Calculate humidity match (0-30 points)
  humidity_score <- 30
  if (!is.null(city_weather$humidity) && humidity_pref) {
    if (city_weather$humidity > 70) {
      # Too humid - reduce score based on humidity level
      humidity_diff <- city_weather$humidity - 70
      humidity_score <- max(0, 30 - (humidity_diff * 0.6))
    }
  }
  
  # Calculate weather events match (0-20 points)
  weather_score <- 20
  if (!is.null(city_weather$weather_main) && 
      !is.null(user_prefs$weather_events_avoid) && 
      length(user_prefs$weather_events_avoid) > 0 && 
      user_prefs$weather_events_avoid[1] != "none") {
    
    # Map weather conditions to user preferences
    weather_mapping <- list(
      "hurricanes" = c("Tornado", "Hurricane", "Tropical Storm"),
      "heat" = c("Extreme Heat"),
      "snow" = c("Snow", "Blizzard"),
      "flooding" = c("Rain", "Thunderstorm")
    )
    
    # Check if current weather matches any avoided conditions
    for (event in user_prefs$weather_events_avoid) {
      if (event != "none" && event %in% names(weather_mapping)) {
        if (city_weather$weather_main %in% weather_mapping[[event]]) {
          weather_score <- 5 # Significantly reduce score if avoiding this weather
          break
        }
      }
    }
  }
  
  # Return total climate match score (0-100)
  return(temp_score + humidity_score + weather_score)
}

# Now modify the find_similar_cities function to incorporate climate scores
find_similar_cities <- function(data, preferences) {
  # [Keep all existing code up to the point where preference_distance is calculated]
  
  # Check for empty dataset
  if (nrow(data) == 0) {
    message("Empty dataset provided to find_similar_cities")
    return(data.frame(
      City = character(0),
      Country = character(0),
      Region = character(0),
      preference_distance = numeric(0),
      match_rank = integer(0),
      climate_match = numeric(0),
      Weighted_Rank = integer(0),
      Economics_Score = numeric(0),
      Environment_Score = numeric(0),
      QualityOfLife_Score = numeric(0),
      Governance_Score = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Create a copy to avoid modifying the original data
  result_data <- data
  
  # [Existing preference distance calculation code stays the same]
  
  # After calculating preference_distance, but before sorting:
  
  # Get weather data cache
  weather_cache <- weather_data_cache()
  
  # Calculate climate match scores for each city
  result_data$climate_match <- 70 # Default score
  
  if (!is.null(weather_cache)) {
    result_data$climate_match <- mapply(
      calculate_climate_match,
      result_data$City,
      result_data$Country,
      MoreArgs = list(
        user_prefs = preferences,
        weather_data_cache = weather_cache
      )
    )
  }
  
  # Integrate climate_match into the final ranking
  # Lower preference_distance is better, higher climate_match is better
  # We'll create a combined score where both factors matter
  
  # Normalize preference_distance to a 0-100 scale (inverted so higher is better)
  max_distance <- max(result_data$preference_distance, na.rm = TRUE)
  result_data$preference_score <- 100 - (result_data$preference_distance / max_distance * 100)
  
  # Create a weighted combined score (70% preference, 30% climate)
  # Adjust these weights based on how much you want climate to influence results
  result_data$combined_score <- (result_data$preference_score * 0.7) + 
    (result_data$climate_match * 0.3)
  
  # Sort by the combined score and assign match ranks
  result_data <- result_data %>%
    arrange(desc(combined_score)) %>%
    mutate(match_rank = row_number())
  
  # [Rest of the function stays the same, including region filtering]
  
  return(result_data)
}

############################
# UI Definition
############################

library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(DT)

# Example data frame used in some inputs:
# city_data <- data.frame(
#   Region = c("Europe", "Asia", "Europe", "Americas", "Africa"),
#   stringsAsFactors = FALSE
# )

ui <- dashboardPage(
  dashboardHeader(title = "Where Should You Live?"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("World Map", tabName = "world_map", icon = icon("globe")),
      menuItem("City Explorer", tabName = "city_explorer", icon = icon("building")),
      menuItem("City Comparison", tabName = "city_comparison", icon = icon("balance-scale")),
      menuItem("Find Your City", tabName = "predict_city", icon = icon("search")),
      menuItem("Weather Analysis", tabName = "weather_analysis", icon = icon("cloud-sun"))
    )
  ),
  
  dashboardBody(
    tabItems(
      #------------------------------------------------
      # 1) Dashboard Overview Tab
      #------------------------------------------------
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Welcome to the Where Should You Live? Dashboard", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  p("This dashboard helps you explore and compare cities worldwide to find your ideal place to live."),
                  p("Use the different tabs to:"),
                  tags$ul(
                    tags$li(strong("World Map:"), "Visualize all cities on a map with customizable ranking criteria"),
                    tags$li(strong("City Explorer:"), "Browse and filter cities based on various metrics"),
                    tags$li(strong("City Comparison:"), "Compare two cities side by side across all dimensions"),
                    tags$li(strong("Find Your City:"), "Get personalized recommendations based on your preferences")
                  )
                )
              ),
              fluidRow(
                valueBoxOutput("total_cities_box", width = 4),
                valueBoxOutput("top_city_box", width = 4),
                valueBoxOutput("top_sustainable_box", width = 4)
              ),
              fluidRow(
                box(
                  title = "Top 10 Cities Overall", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  DTOutput("dashboard_top_cities")
                ),
                box(
                  title = "Cities by Region", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("dashboard_region_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Category Distribution", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("dashboard_category_plot")
                )
              )
      ),
      
      #------------------------------------------------
      # 2) World Map Tab
      #------------------------------------------------
      tabItem(tabName = "world_map",
              fluidRow(
                box(
                  title = "Customize Your Ranking", width = 12, status = "primary",
                  solidHeader = TRUE,
                  fluidRow(
                    column(width = 3,
                           div(class = "well",
                               h4("Environment", style = "color: #3c8dbc;"),
                               sliderInput("env_weight", NULL, min = 0, max = 5, value = 1.5, step = 0.5)
                           )
                    ),
                    column(width = 3,
                           div(class = "well",
                               h4("Governance", style = "color: #3c8dbc;"),
                               sliderInput("gov_weight", NULL, min = 0, max = 5, value = 1.2, step = 0.5)
                           )
                    ),
                    column(width = 3,
                           div(class = "well",
                               h4("Economics", style = "color: #3c8dbc;"),
                               sliderInput("econ_weight", NULL, min = 0, max = 5, value = 1, step = 0.5)
                           )
                    ),
                    column(width = 3,
                           div(class = "well",
                               h4("Quality of Life", style = "color: #3c8dbc;"),
                               sliderInput("qol_weight", NULL, min = 0, max = 5, value = 1.5, step = 0.5)
                           )
                    )
                  ),
                  fluidRow(
                    column(width = 12, align = "center",
                           actionButton("update_map", "Update Ranking", class = "btn-lg btn-primary", 
                                        icon = icon("refresh"), style = "margin-top: 10px;")
                    )
                  )
                )
              ),
              fluidRow(
                valueBoxOutput("top_weighted_city", width = 4),
                valueBoxOutput("avg_city_score", width = 4),
                valueBoxOutput("cities_count_box", width = 4)
              ),
              fluidRow(
                box(
                  title = "Top Cities After Weighting", 
                  status = "info", 
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  fluidRow(
                    column(width = 8,
                           htmlOutput("top_cities_summary")
                    ),
                    column(width = 4,
                           plotlyOutput("weight_distribution_plot", height = "180px")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "World Map of Top Cities", width = 12, status = "primary",
                  leafletOutput("city_map", height = "60vh"), 
                  solidHeader = TRUE,
                  collapsible = TRUE
                )
              ),
              fluidRow(
                box(
                  title = "Top 20 Cities", width = 12, status = "primary",
                  DTOutput("top_cities_table")
                )
              )
      ),
      
      #------------------------------------------------
      # 3) City Explorer Tab
      #------------------------------------------------
      tabItem(tabName = "city_explorer",
              fluidRow(
                box(
                  title = "Filter Cities", width = 12, status = "primary",
                  fluidRow(
                    column(width = 3,
                           selectInput("region_filter", "Region", 
                                       choices = c("All", sort(unique(as.character(city_data$Region)))),
                                       selected = "All")
                    ),
                    column(width = 3,
                           sliderInput("score_filter", "Minimum Weighted Score", 
                                       min = 0, max = 100, value = 0)
                    ),
                    column(width = 3,
                           selectInput("sort_by", "Sort By",
                                       choices = c("Weighted Score" = "Weighted_Score",
                                                   "Overall Score" = "Overall_Score",
                                                   "Economics" = "Economics_Score",
                                                   "Environment" = "Environment_Score",
                                                   "Quality of Life" = "QualityOfLife_Score",
                                                   "Governance" = "Governance_Score"),
                                       selected = "Weighted_Score")
                    ),
                    column(width = 3,
                           checkboxGroupInput("data_source", "Data Source",
                                              choices = c("NUMBEO Cities" = "Source_Numbeo", 
                                                          "ARCADIS Cities" = "Source_Arcadis",
                                                          "Both Sources" = "both"),
                                              selected = c("Source_Numbeo", "Source_Arcadis", "both"))
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Cities", width = 12, status = "primary",
                  DTOutput("cities_table")
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = "Environment Rankings", width = 12, status = "info",
                         plotlyOutput("env_plot")
                       )
                ),
                column(width = 6,
                       box(
                         title = "Economics Rankings", width = 12, status = "success",
                         plotlyOutput("econ_plot")
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = "Quality of Life Rankings", width = 12, status = "warning",
                         plotlyOutput("qol_plot")
                       )
                ),
                column(width = 6,
                       box(
                         title = "Governance Rankings", width = 12, status = "danger",
                         plotlyOutput("gov_plot")
                       )
                )
              )
      ),
      
      #------------------------------------------------
      # 4) City Comparison Tab
      #------------------------------------------------
      tabItem(tabName = "city_comparison",
              fluidRow(
                column(width = 6,
                       selectizeInput("city1", "Select First City", choices = NULL, 
                                      options = list(placeholder = "Type to search for a city"))
                ),
                column(width = 6,
                       selectizeInput("city2", "Select Second City", choices = NULL,
                                      options = list(placeholder = "Type to search for a city"))
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = "City Profile", width = 12, status = "primary",
                         uiOutput("city1_profile")
                       )
                ),
                column(width = 6,
                       box(
                         title = "City Profile", width = 12, status = "primary",
                         uiOutput("city2_profile")
                       )
                )
              ),
              fluidRow(
                box(
                  title = "Comparative Analysis", width = 12, status = "primary",
                  plotlyOutput("comparison_radar_plot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Detailed Comparison", width = 12, status = "primary",
                  plotlyOutput("comparison_bar_plot", height = "400px")
                )
              )
      ),
      
      #------------------------------------------------
      # 5) Find Your City Tab
      #------------------------------------------------
      tabItem(tabName = "predict_city",
              fluidRow(
                box(
                  title = "Step 1: What Matters to You?", width = 12, status = "primary", solidHeader = TRUE,
                  p("For each category, indicate how important it is to you. This will affect both your ideal city profile and how cities are ranked."),
                  fluidRow(
                    column(width = 3,
                           wellPanel(
                             h4("Environment & Sustainability", style = "color: #00ba38; margin-top: 0;"),
                             sliderInput("pref_environment", NULL, min = 0, max = 100, value = 50),
                             div(style = "margin-top: 15px;"),
                             checkboxGroupInput("env_features", "Features that matter to you:",
                                                choices = c("Green spaces" = "green_spaces",
                                                            "Air quality" = "air_quality",
                                                            "Waste management" = "waste_mgmt",
                                                            "Public transport" = "pub_transport"),
                                                selected = c("air_quality"))
                           )
                    ),
                    column(width = 3,
                           wellPanel(
                             h4("Economics & Opportunity", style = "color: #f8766d; margin-top: 0;"),
                             sliderInput("pref_economics", NULL, min = 0, max = 100, value = 50),
                             div(style = "margin-top: 15px;"),
                             checkboxGroupInput("econ_features", "Features that matter to you:",
                                                choices = c("Job market" = "job_market",
                                                            "Cost of living" = "cost_living",
                                                            "Economic growth" = "econ_growth",
                                                            "Entrepreneurship" = "entrepreneurship"),
                                                selected = c("cost_living"))
                           )
                    ),
                    column(width = 3,
                           wellPanel(
                             h4("Quality of Life", style = "color: #619cff; margin-top: 0;"),
                             sliderInput("pref_qol", NULL, min = 0, max = 100, value = 50),
                             div(style = "margin-top: 15px;"),
                             checkboxGroupInput("qol_features", "Features that matter to you:",
                                                choices = c("Healthcare" = "healthcare",
                                                            "Education" = "education",
                                                            "Safety" = "safety",
                                                            "Cultural venues" = "culture"),
                                                selected = c("safety"))
                           )
                    ),
                    column(width = 3,
                           wellPanel(
                             h4("Governance & Services", style = "color: #f564e3; margin-top: 0;"),
                             sliderInput("pref_governance", NULL, min = 0, max = 100, value = 50),
                             div(style = "margin-top: 15px;"),
                             checkboxGroupInput("gov_features", "Features that matter to you:",
                                                choices = c("Political stability" = "stability",
                                                            "Transparency" = "transparency",
                                                            "Digital services" = "digital_services",
                                                            "Public safety" = "public_safety"),
                                                selected = c("stability"))
                           )
                    ),
                    
                    # Extra diagnostic info after user clicks 'Find My Ideal Cities'
                    conditionalPanel(
                      condition = "input.find_matches > 0",
                      fluidRow(
                        box(
                          title = "Diagnostic Information", width = 12, status = "primary", collapsed = TRUE, collapsible = TRUE,
                          verbatimTextOutput("diagnostic_output")
                        )
                      )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Step 2: Climate Preferences", width = 12, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(width = 4,
                           sliderInput("temp_preference", "Preferred Temperature (°C)",
                                       min = -10, max = 40, value = c(15, 25)),
                           radioButtons("seasons_preference", "Seasonal Variation:",
                                        choices = c("I prefer distinct seasons" = "distinct",
                                                    "I prefer consistent climate year-round" = "consistent"),
                                        selected = "distinct")
                    ),
                    column(width = 4,
                           sliderInput("rainfall_preference", "Annual Rainfall Preference (mm)",
                                       min = 0, max = 3000, value = c(500, 1500)),
                           checkboxInput("humidity_preference", "I prefer low humidity", value = FALSE)
                    ),
                    column(width = 4,
                           selectInput("weather_events_avoid", "Weather Events to Avoid:",
                                       choices = c("None" = "none",
                                                   "Hurricanes/Typhoons" = "hurricanes",
                                                   "Extreme Heat" = "heat",
                                                   "Heavy Snow" = "snow",
                                                   "Flooding" = "flooding"),
                                       selected = "none",
                                       multiple = TRUE),
                           helpText("Note: This demo uses simulated weather data. In production with API keys, real weather data from OpenWeatherMap would be used.")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Step 3: Additional Preferences", width = 6, status = "primary", solidHeader = TRUE,
                  fluidRow(
                    column(width = 6,
                           selectInput("pref_regions", "Preferred Regions",
                                       choices = c("All", sort(unique(as.character(city_data$Region)))),
                                       selected = "All",
                                       multiple = TRUE)
                    ),
                    column(width = 6,
                           checkboxGroupInput("pref_data_source", "Include cities from:",
                                              choices = c("NUMBEO database" = "Source_Numbeo", 
                                                          "ARCADIS database" = "Source_Arcadis"),
                                              selected = c("Source_Numbeo", "Source_Arcadis"))
                    )
                  ),
                  fluidRow(
                    column(width = 6,
                           selectInput("city_size", "City Size Preference:",
                                       choices = c("No preference" = "no_pref",
                                                   "Small cities (<500k)" = "small",
                                                   "Medium cities (500k-2M)" = "medium",
                                                   "Large cities (>2M)" = "large"),
                                       selected = "no_pref")
                    ),
                    column(width = 6,
                           selectInput("coastal", "Proximity to Coast:",
                                       choices = c("No preference" = "no_pref",
                                                   "Coastal cities only" = "coastal",
                                                   "Inland cities only" = "inland"),
                                       selected = "no_pref")
                    )
                  )
                ),
                
                box(
                  title = "Your Profile", width = 6, status = "primary", solidHeader = TRUE,
                  plotlyOutput("user_profile_radar", height = "250px"),
                  hr(),
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    actionButton("find_matches", "Find My Ideal Cities", 
                                 class = "btn-lg btn-success btn-block",
                                 icon = icon("search"))
                  )
                )
              ),
              
              fluidRow(
                uiOutput("matching_results")
              ),
              
              fluidRow(
                box(
                  title = "How Important Are These Categories?", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  p("Adjust the importance of each category. This affects how cities are ranked for you."),
                  fluidRow(
                    column(width = 3,
                           sliderInput("weight_environment", "Environment & Sustainability Weight", 
                                       min = 0.5, max = 5, value = 1.5, step = 0.5)
                    ),
                    column(width = 3,
                           sliderInput("weight_economics", "Economics & Opportunity Weight", 
                                       min = 0.5, max = 5, value = 1.0, step = 0.5)
                    ),
                    column(width = 3,
                           sliderInput("weight_qol", "Quality of Life Weight", 
                                       min = 0.5, max = 5, value = 1.5, step = 0.5)
                    ),
                    column(width = 3,
                           sliderInput("weight_governance", "Governance & Services Weight", 
                                       min = 0.5, max = 5, value = 1.2, step = 0.5)
                    )
                  )
                )
              ),
              
              # This conditional panel only appears after the user clicks "Find My Ideal Cities"
              conditionalPanel(
                condition = "input.find_matches > 0",
                fluidRow(
                  tabBox(
                    title = "Your Top City Matches", width = 12, id = "match_tabs",
                    
                    tabPanel("Map View", 
                             leafletOutput("matches_map", height = "400px")),
                    
                    tabPanel("Table View", 
                             DTOutput("matches_table")),
                    
                    tabPanel("Charts",
                             fluidRow(
                               column(width = 6,
                                      box(
                                        title = "Match Distribution by Score", width = 12, status = "info",
                                        plotlyOutput("matches_score_dist")
                                      )
                               ),
                               column(width = 6,
                                      box(
                                        title = "Match Distribution by Region", width = 12, status = "info",
                                        plotlyOutput("matches_region_dist")
                                      )
                               )
                             )
                    ),
                    
                    conditionalPanel(
                      condition = "input.find_matches > 0",
                      fluidRow(
                        box(
                          title = "Climate Match Summary", 
                          width = 12, 
                          status = "info", 
                          solidHeader = TRUE,
                          div(
                            class = "climate-summary",
                            htmlOutput("climate_match_summary")
                          )
                        )
                      )
                    ),
                    
                    tabPanel("Weather Comparison",
                             plotlyOutput("weather_comparison_plot", height = "400px"))
                  )
                )
              )
      ),  # <--- End of tabItem "predict_city"
      
      #------------------------------------------------
      # 6) Weather Analysis Tab (moved out of predict_city)
      #------------------------------------------------
      tabItem(tabName = "weather_analysis",
              fluidRow(
                box(
                  title = "Climate Profiles by Region", 
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("This tab allows you to explore typical climate conditions across different regions and compare them with your preferences."),
                  p("Use the controls below to analyze how well different regions match your climate preferences.")
                )
              ),
              
              fluidRow(
                box(
                  title = "Your Climate Preferences", 
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  # This will render the user's climate preferences
                  uiOutput("climate_preferences_display")
                ),
                
                box(
                  title = "Regional Climate Analysis", 
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  
                  selectInput("weather_analysis_region", "Select Region to Analyze",
                              choices = c("All", sort(unique(as.character(city_data$Region)))),
                              selected = "All"),
                  
                  plotlyOutput("region_climate_plot", height = "300px"),
                  
                  div(class = "well",
                      h4("Climate Compatibility Analysis", class = "text-center"),
                      htmlOutput("region_climate_analysis")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Top Climate Matches for You", 
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  p("Based on your climate preferences, these cities offer the best weather conditions:"),
                  uiOutput("climate_top_matches")
                )
              ),
              fluidRow(
                box(
                  title = "Chat with Groq AI",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  # Use textAreaInput instead of textInput
                  textAreaInput("user_input", "Ask Groq AI:", 
                                value = "", 
                                placeholder = "Type your question...",
                                width = "100%", 
                                height = "80px"),
                  
                  actionButton("ask_button", "Ask AI", icon = icon("paper-plane")),
                  
                  br(), br(),
                  
                  # Instead of verbatimTextOutput, we’ll render HTML
                  uiOutput("response_ui"),
                  
                  hr(),
                  
                  # Debug text (no status code)
                  verbatimTextOutput("debug_output")
                )
              )
      
      )
    )
  )
)


############################
# Server Definition
############################

server <- function(input, output, session) {
  
  
  future::plan(future::multisession)
  # Initialize reactive values with UNMODIFIED data from the file
  ranked_cities <- reactiveVal(city_data)
  
  # 🛡️ RATE LIMITING SETUP
  rate_limit_storage <- reactiveVal(list())
  
  # Get client IP function
  get_client_ip <- function() {
    ip <- session$request$HTTP_X_FORWARDED_FOR %||% 
          session$request$HTTP_X_REAL_IP %||%
          session$request$REMOTE_ADDR %||%
          "unknown"
    if (grepl(",", ip)) ip <- trimws(strsplit(ip, ",")[[1]][1])
    return(ip)
  }
  
  # Rate limiting check function
  check_rate_limit <- function(api_name, max_requests = 10, window_minutes = 15) {
    ip <- get_client_ip()
    current_time <- Sys.time()
    window_start <- current_time - (window_minutes * 60)
    
    storage <- rate_limit_storage()
    key <- paste0(ip, "_", api_name)
    
    if (!key %in% names(storage)) storage[[key]] <- list()
    storage[[key]] <- storage[[key]][storage[[key]] > window_start]
    
    if (length(storage[[key]]) >= max_requests) {
      return(list(allowed = FALSE, remaining = 0))
    }
    
    storage[[key]] <- c(storage[[key]], current_time)
    rate_limit_storage(storage)
    
    return(list(allowed = TRUE, remaining = max_requests - length(storage[[key]])))
  }
  
  # Cleanup old rate limit entries every 5 minutes
  observe({
    invalidateLater(300000)
    storage <- rate_limit_storage()
    cutoff_time <- Sys.time() - (60 * 60)
    for (key in names(storage)) {
      storage[[key]] <- storage[[key]][storage[[key]] > cutoff_time]
      if (length(storage[[key]]) == 0) storage[[key]] <- NULL
    }
    rate_limit_storage(storage)
  })
  
  # Add this near the beginning of your server function
  message("API Key Status: ", ifelse(Sys.getenv("OPENWEATHER_API_KEY") == "", "MISSING", "PRESENT"))
  message("API Key Value (first 5 chars): ", substr(Sys.getenv("OPENWEATHER_API_KEY"), 1, 5), "...")
  
  # Update city selection inputs
  observe({
    cities <- city_data$City
    updateSelectizeInput(session, "city1", choices = cities)
    updateSelectizeInput(session, "city2", choices = cities)
  })
  
  # Dashboard tab outputs
  output$total_cities_box <- renderValueBox({
    valueBox(
      nrow(city_data), 
      "Cities in Database", 
      icon = icon("city"), 
      color = "blue"
    )
  })
  
  # Use the precalculated Weighted_Score for top city box
  output$top_city_box <- renderValueBox({
    top_city <- city_data %>%
      arrange(Weighted_Rank) %>%
      slice(1)
    
    valueBox(
      top_city$City,
      "Top Rated City",
      icon = icon("trophy"), 
      color = "yellow"
    )
  })
  
  output$top_sustainable_box <- renderValueBox({
    sustainable_city <- city_data %>%
      arrange(desc(Environment_Score)) %>%
      slice(1)
    
    valueBox(
      sustainable_city$City,
      "Most Sustainable City",
      icon = icon("leaf"), 
      color = "green"
    )
  })
  
  # Weighted Scores
  output$top_weighted_city <- renderValueBox({
    data <- ranked_cities()
    top_city <- data %>% arrange(Weighted_Rank) %>% slice(1)
    
    valueBox(
      paste0(top_city$City, ", ", top_city$Country),
      "Top Ranked City Based on Your Weights",
      icon = icon("trophy"),
      color = "yellow"
    )
  })
  
  output$avg_city_score <- renderValueBox({
    data <- ranked_cities()
    avg_score <- mean(data$Weighted_Score, na.rm = TRUE)
    
    valueBox(
      round(avg_score, 1),
      "Average City Score",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  output$cities_count_box <- renderValueBox({
    data <- ranked_cities()
    
    valueBox(
      nrow(data),
      "Cities in Database",
      icon = icon("city"),
      color = "blue"
    )
  })
  
  # FIX: Use row_number() for creating ranks in tables
  output$dashboard_top_cities <- renderDT({
    city_data %>%
      arrange(Weighted_Rank) %>%
      select(City, Country, Weighted_Score, Data_Completeness) %>%
      head(10) %>%
      # Create a new rank based on the current order
      mutate(Rank = row_number()) %>%
      select(Rank, City, Country, "Score" = Weighted_Score, "Data Quality" = Data_Completeness) %>%
      datatable(options = list(dom = 't', ordering = FALSE))
  })
  
  output$dashboard_region_plot <- renderPlotly({
    region_count <- city_data %>%
      group_by(Region) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    
    p <- ggplot(region_count, aes(x = reorder(Region, -Count), y = Count, fill = Region)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "", y = "Number of Cities") +
      scale_fill_brewer(palette = "Set3")
    
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  output$dashboard_category_plot <- renderPlotly({
    # Get top 5 cities by weighted rank
    top_cities <- city_data %>%
      arrange(Weighted_Rank) %>%
      head(5)
    
    # Reshape data for plotting
    plot_data <- top_cities %>%
      select(City, Economics_Score, Environment_Score, QualityOfLife_Score, Governance_Score) %>%
      rename(
        "Economics" = Economics_Score,
        "Environment" = Environment_Score,
        "Quality of Life" = QualityOfLife_Score,
        "Governance" = Governance_Score
      ) %>%
      tidyr::pivot_longer(cols = -City, names_to = "Category", values_to = "Score")
    
    p <- ggplot(plot_data, aes(x = Category, y = Score, fill = City, group = City)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(x = "", y = "Score") +
      scale_fill_brewer(palette = "Set1")
    
    ggplotly(p)
  })
  
  # Update rankings based on user inputs
  observeEvent(input$update_map, {
    # Get the weights
    weights <- list(
      environment = input$env_weight,
      governance = input$gov_weight,
      economics = input$econ_weight,
      quality_of_life = input$qol_weight
    )
    
    # Add debugging to verify weights are captured
    message("Updating with weights: Env=", weights$environment, 
            ", Gov=", weights$governance, 
            ", Econ=", weights$economics, 
            ", QoL=", weights$quality_of_life)
    
    # Update the rankings with the new weights
    updated_data <- update_rankings(city_data, weights)
    
    # Print top city to verify rankings changed
    top_city <- updated_data %>% 
      arrange(Weighted_Rank) %>% 
      slice(1)
    message("New top city: ", top_city$City, ", Score: ", round(top_city$Weighted_Score, 1))
    
    # Updating reactive value
    ranked_cities(updated_data)
  })
  
  # World Map tab outputs
  output$city_map <- renderLeaflet({
    data <- ranked_cities()
    
    # Color palette based on ranking
    pal <- colorNumeric(
      palette = "RdYlGn",  # Red-Yellow-Green palette (standard for maps)
      domain = data$Weighted_Score,
      reverse = FALSE  # Higher scores get green, lower scores get red
    )
    
    # Format popup content
    popup_content <- paste(
      "<b>", data$City, ", ", data$Country, "</b><br>",
      "Rank: ", data$Weighted_Rank, "<br>",
      "Weighted Score: ", round(data$Weighted_Score, 1), "<br>",
      "Economics: ", round(data$Economics_Score, 1), "<br>",
      "Environment: ", round(data$Environment_Score, 1), "<br>",
      "Quality of Life: ", round(data$QualityOfLife_Score, 1), "<br>",
      "Governance: ", round(data$Governance_Score, 1)
    )
    
    # Create the map without the problematic functions
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        color = ~pal(Weighted_Score),
        radius = ~pmin(12, 8 + (Weighted_Score/20)),
        fillOpacity = 0.8,
        weight = 1,
        stroke = TRUE,
        popup = popup_content,
        label = ~City
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~Weighted_Score,
        title = "City Score",
        opacity = 1
      ) %>%
      # Simple reset view control (replaces addEasyButton from leaflet.extras)
      # Using basic leaflet addControl instead
      addControl(html = "<button onclick='map.setView([20, 0], 2);' style='background: white; border: 1px solid #ccc; padding: 5px; border-radius: 3px; cursor: pointer;' title='Reset View'>🏠</button>", position = "topleft")
  })
  
  output$top_cities_summary <- renderUI({
    data <- ranked_cities()
    top_5 <- data %>% 
      arrange(Weighted_Rank) %>% 
      head(5)
    
    # Define score color functions for gradient bars
    get_score_style <- function(score, color) {
      width_percent <- score
      paste0(
        "padding: 10px; text-align: center; border-bottom: 1px solid #ddd; ",
        "background: linear-gradient(to right, ", color, " 0%, ", color, " ", width_percent, "%, ",
        "rgba(255,255,255,0) ", width_percent, "%, rgba(255,255,255,0) 100%);"
      )
    }
    
    # Define rank background colors
    rank_colors <- c("#FFD700", "#C0C0C0", "#CD7F32", "#FFFFFF", "#FFFFFF")
    
    tagList(
      p("The table below shows how your weighting choices affect city rankings. Here are your top cities based on your current weights:"),
      tags$div(
        tags$table(
          style = "width: 100%; border-collapse: collapse; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
          tags$thead(
            tags$tr(
              tags$th(style = "padding: 10px; text-align: center; border-bottom: 2px solid #ddd; background-color: #f2f2f2;", "Rank"),
              tags$th(style = "padding: 10px; text-align: left; border-bottom: 2px solid #ddd; background-color: #f2f2f2;", "City"),
              tags$th(style = "padding: 10px; text-align: left; border-bottom: 2px solid #ddd; background-color: #f2f2f2;", "Country"),
              tags$th(style = "padding: 10px; text-align: center; border-bottom: 2px solid #ddd; background-color: #f2f2f2;", "Score"),
              tags$th(style = "padding: 10px; text-align: center; border-bottom: 2px solid #ddd; background-color: #f2f2f2;", "Econ"),
              tags$th(style = "padding: 10px; text-align: center; border-bottom: 2px solid #ddd; background-color: #f2f2f2;", "Env"),
              tags$th(style = "padding: 10px; text-align: center; border-bottom: 2px solid #ddd; background-color: #f2f2f2;", "QoL"),
              tags$th(style = "padding: 10px; text-align: center; border-bottom: 2px solid #ddd; background-color: #f2f2f2;", "Gov")
            )
          ),
          tags$tbody(
            lapply(1:nrow(top_5), function(i) {
              tags$tr(
                # Full color for rank cell
                tags$td(style = paste0("padding: 10px; text-align: center; border-bottom: 1px solid #ddd; font-weight: bold; background-color: ", 
                                       ifelse(i <= 3, rank_colors[i], "#f8f9fa"), ";"), i),
                # Apply rank background ONLY to city cell
                tags$td(style = paste0("padding: 10px; text-align: left; border-bottom: 1px solid #ddd; font-weight: bold; ", 
                                       ifelse(i <= 3, paste0("background-color: ", rank_colors[i], "50;"), "")), 
                        top_5$City[i]),
                # Apply rank background ONLY to country cell
                tags$td(style = paste0("padding: 10px; text-align: left; border-bottom: 1px solid #ddd; ", 
                                       ifelse(i <= 3, paste0("background-color: ", rank_colors[i], "50;"), "")), 
                        top_5$Country[i]),
                # Regular styling for score columns with gradient bars
                tags$td(style = paste0("padding: 10px; text-align: center; border-bottom: 1px solid #ddd; font-weight: bold; ",
                                       "background: linear-gradient(to right, lightblue 0%, lightblue ", 
                                       top_5$Weighted_Score[i], "%, rgba(255,255,255,0) ", 
                                       top_5$Weighted_Score[i], "%, rgba(255,255,255,0) 100%);"), 
                        round(top_5$Weighted_Score[i], 1)),
                tags$td(style = get_score_style(top_5$Economics_Score[i], "#f8766d"), 
                        round(top_5$Economics_Score[i], 1)),
                tags$td(style = get_score_style(top_5$Environment_Score[i], "#00ba38"), 
                        round(top_5$Environment_Score[i], 1)),
                tags$td(style = get_score_style(top_5$QualityOfLife_Score[i], "#619cff"), 
                        round(top_5$QualityOfLife_Score[i], 1)),
                tags$td(style = get_score_style(top_5$Governance_Score[i], "#f564e3"), 
                        round(top_5$Governance_Score[i], 1))
              )
            })
          )
        )
      )
    )
  })
  
  output$weight_distribution_plot <- renderPlotly({
    # Create a small pie chart showing the weight distribution
    weights <- data.frame(
      Category = c("Economics", "Environment", "Quality of Life", "Governance"),
      Weight = c(input$econ_weight, input$env_weight, input$qol_weight, input$gov_weight)
    )
    
    # Calculate percentages
    total <- sum(weights$Weight)
    weights$Percentage <- weights$Weight / total * 100
    
    # Create pie chart
    plot_ly(weights, labels = ~Category, values = ~Percentage, type = 'pie',
            textinfo = 'label+percent',
            insidetextorientation = 'radial',
            marker = list(colors = c('#f8766d', '#00ba38', '#619cff', '#f564e3'))) %>%
      layout(title = "Weight Distribution",
             showlegend = FALSE,
             margin = list(l=0, r=0, b=0, t=30))
  })
  
  output$top_cities_table <- renderDT({
    data <- ranked_cities()
    
    formatted_data <- data %>%
      arrange(Weighted_Rank) %>%
      select(
        Rank = Weighted_Rank,
        City,
        Country,
        Region,
        Score = Weighted_Score,
        Economics = Economics_Score,
        Environment = Environment_Score,
        `Quality of Life` = QualityOfLife_Score,
        Governance = Governance_Score
      ) %>%
      head(20)
    
    # Format scores to have consistent decimal places
    formatted_data <- formatted_data %>%
      mutate(across(c(Score, Economics, Environment, `Quality of Life`, Governance), 
                    ~round(., 1)))
    
    datatable(
      formatted_data, 
      options = list(
        pageLength = 10,
        dom = '<"top"f>rt<"bottom"ip>',
        scrollY = "400px",
        scrollCollapse = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = '50px', targets = 0),  # Rank column
          list(width = '140px', targets = 1), # City column
          list(width = '100px', targets = 2), # Country column
          list(width = '120px', targets = 3), # Region column
          list(width = '80px', targets = c(4, 5, 6, 7, 8))  # Score columns
        )
      ),
      rownames = FALSE,
      selection = 'single',
      class = 'cell-border stripe'
    ) %>%
      # Gold, silver, bronze for rank column
      formatStyle(
        'Rank', 
        backgroundColor = styleEqual(1:3, c('#FFD700', '#C0C0C0', '#CD7F32')),
        fontWeight = styleEqual(1:3, 'bold')
      ) %>%
      # Apply medal colors with transparency to City column
      formatStyle(
        'City', 
        backgroundColor = styleEqual(
          c(1, 2, 3), 
          c('#FFD70080', '#C0C0C080', '#CD7F3280')
        ),
        fontWeight = styleEqual(1:3, 'bold')
      ) %>%
      # Apply medal colors with transparency to Country column
      formatStyle(
        'Country', 
        backgroundColor = styleEqual(
          c(1, 2, 3), 
          c('#FFD70080', '#C0C0C080', '#CD7F3280')
        )
      ) %>%
      # Score visualization bars
      formatStyle('Score',
                  background = styleColorBar(c(0, max(formatted_data$Score)), 'lightblue'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle('Economics',
                  background = styleColorBar(c(0, 100), '#f8766d'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle('Environment',
                  background = styleColorBar(c(0, 100), '#00ba38'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle('Quality of Life',
                  background = styleColorBar(c(0, 100), '#619cff'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle('Governance',
                  background = styleColorBar(c(0, 100), '#f564e3'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  # City Explorer tab outputs
  filtered_cities <- reactive({
    data <- city_data
    
    # Apply region filter
    if (input$region_filter != "All") {
      data <- data %>% filter(Region == input$region_filter)
    }
    
    # Apply score filter to Weighted_Score
    data <- data %>% filter(Weighted_Score >= input$score_filter)
    
    # Apply data source filter
    if (!"both" %in% input$data_source) {
      if ("Source_Numbeo" %in% input$data_source) {
        data <- data %>% filter(Source_Numbeo == TRUE)
      }
      if ("Source_Arcadis" %in% input$data_source) {
        data <- data %>% filter(Source_Arcadis == TRUE)
      }
    }
    
    # Sort data
    data <- data %>% arrange(desc(!!sym(input$sort_by)))
    
    return(data)
  })
  
  output$cities_table <- renderDT({
    filtered_cities() %>%
      select(
        City,
        Country,
        Region,
        `Weighted Score` = Weighted_Score,
        `Weighted Rank` = Weighted_Rank,
        `Data Quality` = Data_Completeness,
        Economics = Economics_Score,
        Environment = Environment_Score,
        `Quality of Life` = QualityOfLife_Score,
        Governance = Governance_Score,
        `NUMBEO` = Source_Numbeo,
        `ARCADIS` = Source_Arcadis
      ) %>%
      datatable(options = list(pageLength = 15, scrollY = "400px"))
  })
  
  # Category plots
  output$env_plot <- renderPlotly({
    data <- filtered_cities() %>%
      head(10) %>%
      arrange(desc(Environment_Score))
    
    p <- ggplot(data, aes(x = reorder(City, Environment_Score), y = Environment_Score, fill = Environment_Score)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(x = "", y = "Environment Score") +
      scale_fill_viridis_c()
    
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  output$econ_plot <- renderPlotly({
    data <- filtered_cities() %>%
      head(10) %>%
      arrange(desc(Economics_Score))
    
    p <- ggplot(data, aes(x = reorder(City, Economics_Score), y = Economics_Score, fill = Economics_Score)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(x = "", y = "Economics Score") +
      scale_fill_viridis_c(option = "plasma")
    
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  output$qol_plot <- renderPlotly({
    data <- filtered_cities() %>%
      head(10) %>%
      arrange(desc(QualityOfLife_Score))
    
    p <- ggplot(data, aes(x = reorder(City, QualityOfLife_Score), y = QualityOfLife_Score, fill = QualityOfLife_Score)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(x = "", y = "Quality of Life Score") +
      scale_fill_viridis_c(option = "inferno")
    
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  output$gov_plot <- renderPlotly({
    data <- filtered_cities() %>%
      head(10) %>%
      arrange(desc(Governance_Score))
    
    p <- ggplot(data, aes(x = reorder(City, Governance_Score), y = Governance_Score, fill = Governance_Score)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(x = "", y = "Governance Score") +
      scale_fill_viridis_c(option = "magma")
    
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  # City Comparison tab outputs
  city1_data <- reactive({
    req(input$city1)
    rows <- city_data %>% filter(City == input$city1)
    message("City1 rows found: ", nrow(rows))
    rows
  })
  
  city2_data <- reactive({
    req(input$city2)
    result <- city_data %>% filter(City == input$city2)
    message("City2: ", input$city2, " => nrow=", nrow(result))
    
    # Show the actual rows for debugging
    if (nrow(result) > 0) {
      print(result)
    }
    
    result
  })
  
  # FIX: Properly handle text rendering in UI components
  output$city1_profile <- renderUI({
    req(city1_data())
    city <- city1_data()
    
    # Ensure all values are properly formatted as strings
    city_name <- as.character(city$City)
    country_name <- as.character(city$Country)
    region_name <- as.character(city$Region)
    weighted_score <- round(as.numeric(city$Weighted_Score), 1)
    weighted_rank <- as.integer(city$Weighted_Rank)
    eco_score <- round(as.numeric(city$Economics_Score), 1)
    env_score <- round(as.numeric(city$Environment_Score), 1)
    qol_score <- round(as.numeric(city$QualityOfLife_Score), 1)
    gov_score <- round(as.numeric(city$Governance_Score), 1)
    
    # Construct UI
    tagList(
      h4(paste0(city_name, ", ", country_name)),
      p(paste0("Region: ", region_name)),
      p(paste0("Weighted Score: ", weighted_score, " (Rank: ", weighted_rank, ")")),
      
      h5("Category Scores:"),
      tags$div(
        tags$div(style = "display: flex; justify-content: space-between;",
                 tags$span("Economics:"),
                 tags$span(style = paste0("font-weight: bold; color: ", 
                                          ifelse(eco_score > 75, "green", 
                                                 ifelse(eco_score > 50, "orange", "red"))),
                           paste0(eco_score))
        ),
        tags$div(style = "display: flex; justify-content: space-between;",
                 tags$span("Environment:"),
                 tags$span(style = paste0("font-weight: bold; color: ", 
                                          ifelse(env_score > 75, "green", 
                                                 ifelse(env_score > 50, "orange", "red"))),
                           paste0(env_score))
        ),
        tags$div(style = "display: flex; justify-content: space-between;",
                 tags$span("Quality of Life:"),
                 tags$span(style = paste0("font-weight: bold; color: ", 
                                          ifelse(qol_score > 75, "green", 
                                                 ifelse(qol_score > 50, "orange", "red"))),
                           paste0(qol_score))
        ),
        tags$div(style = "display: flex; justify-content: space-between;",
                 tags$span("Governance:"),
                 tags$span(style = paste0("font-weight: bold; color: ", 
                                          ifelse(gov_score > 75, "green", 
                                                 ifelse(gov_score > 50, "orange", "red"))),
                           paste0(gov_score))
        )
      ),
      
      h5("Data Sources:"),
      tags$div(
        tags$div(style = "display: flex; justify-content: space-between;",
                 tags$span("NUMBEO:"),
                 tags$span(ifelse(city$Source_Numbeo, "Yes", "No"))
        ),
        tags$div(style = "display: flex; justify-content: space-between;",
                 tags$span("ARCADIS:"),
                 tags$span(ifelse(city$Source_Arcadis, "Yes", "No"))
        ),
        if("Data_Completeness" %in% names(city)) {
          tags$div(style = "display: flex; justify-content: space-between;",
                   tags$span("Data Quality:"),
                   tags$span(paste0(round(city$Data_Completeness * 100, 0), "%"))
          )
        }
      )
    )
  })
  
  # Handle text rendering in UI components
  output$city2_profile <- renderUI({
    req(city2_data())
    city <- city2_data()
    
    # Ensure all values are properly formatted as strings
    city_name <- as.character(city$City)
    country_name <- as.character(city$Country)
    region_name <- as.character(city$Region)
    weighted_score <- round(as.numeric(city$Weighted_Score), 1)
    weighted_rank <- as.integer(city$Weighted_Rank)
    eco_score <- round(as.numeric(city$Economics_Score), 1)
    env_score <- round(as.numeric(city$Environment_Score), 1)
    qol_score <- round(as.numeric(city$QualityOfLife_Score), 1)
    gov_score <- round(as.numeric(city$Governance_Score), 1)
    
    # Construct UI
    tagList(
      h4(paste0(city_name, ", ", country_name)),
      p(paste0("Region: ", region_name)),
      p(paste0("Weighted Score: ", weighted_score, " (Rank: ", weighted_rank, ")")),
      
      h5("Category Scores:"),
      tags$div(
        tags$div(style = "display: flex; justify-content: space-between;",
                 tags$span("Economics:"),
                 tags$span(style = paste0("font-weight: bold; color: ", 
                                          ifelse(eco_score > 75, "green", 
                                                 ifelse(eco_score > 50, "orange", "red"))),
                           paste0(eco_score))
        ),
        tags$div(style = "display: flex; justify-content: space-between;",
                 tags$span("Environment:"),
                 tags$span(style = paste0("font-weight: bold; color: ", 
                                          ifelse(env_score > 75, "green", 
                                                 ifelse(env_score > 50, "orange", "red"))),
                           paste0(env_score))
        ),
        tags$div(style = "display: flex; justify-content: space-between;",
                 tags$span("Quality of Life:"),
                 tags$span(style = paste0("font-weight: bold; color: ", 
                                          ifelse(qol_score > 75, "green", 
                                                 ifelse(qol_score > 50, "orange", "red"))),
                           paste0(qol_score))
        ),
        tags$div(style = "display: flex; justify-content: space-between;",
                 tags$span("Governance:"),
                 tags$span(style = paste0("font-weight: bold; color: ", 
                                          ifelse(gov_score > 75, "green", 
                                                 ifelse(gov_score > 50, "orange", "red"))),
                           paste0(gov_score))
        )
      ),
      
      h5("Data Sources:"),
      tags$div(
        tags$div(style = "display: flex; justify-content: space-between;",
                 tags$span("NUMBEO:"),
                 tags$span(ifelse(city$Source_Numbeo, "Yes", "No"))
        ),
        tags$div(style = "display: flex; justify-content: space-between;",
                 tags$span("ARCADIS:"),
                 tags$span(ifelse(city$Source_Arcadis, "Yes", "No"))
        ),
        if("Data_Completeness" %in% names(city)) {
          tags$div(style = "display: flex; justify-content: space-between;",
                   tags$span("Data Quality:"),
                   tags$span(paste0(round(city$Data_Completeness * 100, 0), "%"))
          )
        }
      )
    )
  })
  
  output$comparison_radar_plot <- renderPlotly({
    req(city1_data(), city2_data())
    
    # Error handling - ensure all data is available
    tryCatch({
      city1 <- city1_data()
      city2 <- city2_data()
      
      # Define categories consistently for both cities
      categories <- c("Economics", "Environment", "Quality of Life", "Governance")
      
      # Create the figure with error handling
      fig <- plot_ly() %>%
        add_trace(
          type = 'scatterpolar',
          r = c(city1$Economics_Score, city1$Environment_Score, 
                city1$QualityOfLife_Score, city1$Governance_Score),
          theta = categories,
          fill = 'toself',
          mode = 'lines',  # Explicitly set mode
          name = as.character(city1$City)
        ) %>%
        add_trace(
          type = 'scatterpolar',
          r = c(city2$Economics_Score, city2$Environment_Score, 
                city2$QualityOfLife_Score, city2$Governance_Score),
          theta = categories,
          fill = 'toself',
          mode = 'lines',  # Explicitly set mode
          name = as.character(city2$City)
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(0, 100)
            )
          )
        )
      
      return(fig)
    }, error = function(e) {
      # Return a message plot on error
      plot_ly() %>%
        add_annotations(
          text = paste("Could not generate comparison chart:", e$message),
          showarrow = FALSE,
          font = list(size = 14)
        )
    })
  })
  
  output$comparison_bar_plot <- renderPlotly({
    req(city1_data(), city2_data())
    
    # Error handling
    tryCatch({
      city1 <- city1_data()
      city2 <- city2_data()
      
      # Define categories and ensure consistent data preparation
      categories <- c("Economics", "Environment", "Quality of Life", "Governance")
      
      # Prepare data safely
      city1_scores <- c(city1$Economics_Score, city1$Environment_Score, 
                        city1$QualityOfLife_Score, city1$Governance_Score)
      city2_scores <- c(city2$Economics_Score, city2$Environment_Score, 
                        city2$QualityOfLife_Score, city2$Governance_Score)
      
      # Validate data
      if(length(city1_scores) != 4 || length(city2_scores) != 4) {
        stop("Inconsistent data structure detected")
      }
      
      # Create comparison data frame
      comparison_data <- data.frame(
        Category = rep(categories, 2),
        City = c(rep(as.character(city1$City), 4), rep(as.character(city2$City), 4)),
        Score = c(city1_scores, city2_scores),
        stringsAsFactors = FALSE
      )
      
      # Create the plot
      p <- ggplot(comparison_data, aes(x = Category, y = Score, fill = City)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal() +
        labs(x = "", y = "Score") +
        scale_fill_brewer(palette = "Set1")
      
      ggplotly(p)
    }, error = function(e) {
      # Return a message plot on error
      plot_ly() %>%
        add_annotations(
          text = paste("Could not generate comparison chart:", e$message),
          showarrow = FALSE,
          font = list(size = 14)
        )
    })
  })
  
  # Find Your City tab outputs
  # OpenWeatherMap API configuration - secure version
  weather_api_key <- Sys.getenv("OPENWEATHER_API_KEY")
  
  # Check if API key is available, but don't fall back to hardcoded keys
  if (weather_api_key == "") {
    message("INFO: No OpenWeatherMap API key found. Using simulated weather data for demo.")
    use_fake_weather_data <- TRUE
  } else if (nchar(weather_api_key) < 10) {
    message("WARNING: API key appears invalid. Using simulated weather data for demo.")
    use_fake_weather_data <- TRUE
  } else {
    message("INFO: OpenWeatherMap API key found. Real weather data available.")
    use_fake_weather_data <- FALSE
  }
  
  # Store weather data cache
  weather_data_cache <- reactiveVal(NULL)
  
  # Function to fetch weather data for a city
  # Add a diagnostic version of the fetch_weather_data function
  # Use fake weather data by default for public demo (secure deployment)
  use_fake_weather_data <- TRUE  # Set to FALSE only when real API key is properly configured
  
  # Add this function to generate test data for development
  generate_fake_weather_data <- function(city_name) {
    list(
      temperature = runif(1, 5, 30),
      humidity = runif(1, 30, 90),
      wind_speed = runif(1, 1, 15),
      weather_main = sample(c("Clear", "Clouds", "Rain"), 1)
    )
  }
  
  # Replace your existing fetch_weather_data function
  fetch_weather_data <- function(city_name, country_code) {
    # 🛡️ Check rate limit first (20 requests per 15 minutes)
    rate_check <- check_rate_limit("weather", max_requests = 20, window_minutes = 15)
    
    if (!rate_check$allowed) {
      message("Weather API rate limit exceeded for IP: ", get_client_ip())
      showNotification(
        "Weather API rate limit reached. Please wait before requesting more weather data.",
        type = "warning",
        duration = 10
      )
      return(generate_fake_weather_data(city_name))  # Fallback to demo data
    }
    
    message("Weather API call allowed. Remaining: ", rate_check$remaining, " for IP: ", get_client_ip())
    
    # For testing/development, use fake data if flag is set
    if (use_fake_weather_data) {
      # Create more realistic fake data with all required fields
      weather_conditions <- c("Clear", "Clouds", "Rain", "Snow", "Mist", "Thunderstorm")
      
      # Use weighted random selection to make certain conditions more common
      condition_weights <- c(0.4, 0.3, 0.15, 0.05, 0.05, 0.05)
      selected_condition <- sample(weather_conditions, 1, prob = condition_weights)
      
      # Temperature varies by condition
      temp_range <- switch(selected_condition,
                           "Clear" = c(15, 35),
                           "Clouds" = c(10, 25),
                           "Rain" = c(5, 20),
                           "Snow" = c(-10, 5),
                           "Mist" = c(5, 15),
                           "Thunderstorm" = c(15, 30),
                           c(10, 25))  # default
      
      # Humidity varies by condition
      humidity_range <- switch(selected_condition,
                               "Clear" = c(30, 60),
                               "Clouds" = c(50, 75),
                               "Rain" = c(70, 95),
                               "Snow" = c(60, 90),
                               "Mist" = c(80, 95),
                               "Thunderstorm" = c(75, 95),
                               c(50, 70))  # default
      
      fake_data <- list(
        temperature = runif(1, temp_range[1], temp_range[2]),
        humidity = runif(1, humidity_range[1], humidity_range[2]),
        wind_speed = runif(1, 1, 20),
        weather_main = selected_condition,
        temp_min = runif(1, temp_range[1] - 5, temp_range[1]),
        temp_max = runif(1, temp_range[2], temp_range[2] + 5),
        rate_limited = FALSE,
        remaining_requests = rate_check$remaining
      )
      
      message("Using enhanced fake weather data for ", city_name, ": ", 
              selected_condition, ", ", round(fake_data$temperature, 1), "°C")
      
      return(fake_data)
    }
    
    # Real API implementation with improved error handling
    message("Fetching weather for: ", city_name, ", ", country_code)
    
    # Safe URL creation
    safe_city <- URLencode(city_name)
    safe_country <- URLencode(country_code)
    
    url <- paste0(
      "https://api.openweathermap.org/data/2.5/weather?q=",
      safe_city, ",", safe_country,
      "&units=metric&appid=", weather_api_key
    )
    
    # Validate API key before making request
    if (nchar(weather_api_key) < 10) {
      message("INFO: Invalid API key, using simulated data for ", city_name)
      return(generate_fake_weather_data(city_name))
    }
    
    # Make API request with better error handling and timeout
    response <- tryCatch({
      GET(url, timeout(10))
    }, error = function(e) {
      message("INFO: API request failed, using simulated data for ", city_name)
      return(NULL)
    })
    
    if (is.null(response) || status_code(response) != 200) {
      message("INFO: Weather API unavailable, using simulated data for ", city_name)
      return(generate_fake_weather_data(city_name))
    }
    
    # Parse the response with better error handling
    # Parse the response with better error handling
    tryCatch({
      # Get the content as text first
      text_content <- content(response, "text", encoding = "UTF-8")
      
      # Parse the JSON with explicit settings to ensure proper list structure
      weather_data <- fromJSON(text_content, simplifyVector = FALSE)
      
      # Debug output
      message("Response structure: ", paste(names(weather_data), collapse=", "))
      
      # Safer extraction using [[ ]] instead of $ and with explicit checks
      result <- list(
        temperature = if("main" %in% names(weather_data) && "temp" %in% names(weather_data[["main"]])) {
          as.numeric(weather_data[["main"]][["temp"]])
        } else NA,
        
        humidity = if("main" %in% names(weather_data) && "humidity" %in% names(weather_data[["main"]])) {
          as.numeric(weather_data[["main"]][["humidity"]])
        } else NA,
        
        wind_speed = if("wind" %in% names(weather_data) && "speed" %in% names(weather_data[["wind"]])) {
          as.numeric(weather_data[["wind"]][["speed"]])
        } else NA,
        
        temp_min = if("main" %in% names(weather_data) && "temp_min" %in% names(weather_data[["main"]])) {
          as.numeric(weather_data[["main"]][["temp_min"]])
        } else NA,
        
        temp_max = if("main" %in% names(weather_data) && "temp_max" %in% names(weather_data[["main"]])) {
          as.numeric(weather_data[["main"]][["temp_max"]])
        } else NA,
        
        weather_main = if("weather" %in% names(weather_data) && length(weather_data[["weather"]]) > 0 && 
                          "main" %in% names(weather_data[["weather"]][[1]])) {
          as.character(weather_data[["weather"]][[1]][["main"]])
        } else "Unknown"
      )
      
      message("Successfully fetched weather for ", city_name, ": ", 
              result$weather_main, ", ", round(result$temperature, 1), "°C")
      
      return(result)
    }, error = function(e) {
      message("INFO: Weather data parsing failed, using simulated data for ", city_name)
      return(generate_fake_weather_data(city_name))
    })
  }
  
  # Add this observeEvent to improve weather data fetching for top matches
  observeEvent(input$find_matches, {
    # Synchronous version without futures
    matches <- matched_cities()
    if (nrow(matches) > 0) {
      message("Fetching weather data for top ", min(nrow(matches), 10), " cities...")
      
      weather_data <- list()
      
      # Fetch for top cities
      for (i in 1:min(nrow(matches), 10)) {
        city_name <- matches$City[i]
        country_code <- matches$Country[i]
        city_key <- paste0(city_name, "_", country_code)
        
        # Use the fetch_weather_data function
        weather_data[[city_key]] <- fetch_weather_data(city_name, country_code)
      }
      
      # Update the cache directly
      old_cache <- weather_data_cache()
      
      # Merge data if needed
      if (!is.null(old_cache)) {
        merged_data <- c(old_cache, weather_data)
        merged_data <- merged_data[!duplicated(names(merged_data))]
        weather_data_cache(merged_data)
      } else {
        weather_data_cache(weather_data)
      }
      
      message("Updated weather cache with data for ", length(weather_data), " cities")
    }
  })
  
  # Function to check if city climate matches user preferences
  evaluate_climate_match <- function(weather_data, user_prefs) {
    if (is.null(weather_data)) {
      return(NA)  # Weather data unavailable
    }
    
    # Calculate a climate match score (0-100)
    temp_match <- 100
    humidity_match <- 100
    
    # Temperature preference match
    if (weather_data$temp_max < user_prefs$temp_min || weather_data$temp_min > user_prefs$temp_max) {
      temp_match <- 50  # Temperature outside preferred range
    }
    
    # Humidity preference match
    if (user_prefs$low_humidity && weather_data$humidity > 70) {
      humidity_match <- 50
    }
    
    # Combine scores
    climate_score <- mean(c(temp_match, humidity_match))
    
    return(climate_score)
  }
  
  # 1. Update the user_preferences function to correctly map UI inputs
  user_preferences <- eventReactive(input$find_matches, {
    # Create a list with all required fields
    list(
      environment = input$pref_environment,
      economics = input$pref_economics,
      quality_of_life = input$pref_qol,
      governance = input$pref_governance,
      # Use the weight sliders that are still in the UI
      environment_weight = input$weight_environment,
      economics_weight = input$weight_economics,
      quality_of_life_weight = input$weight_qol,
      governance_weight = input$weight_governance,
      regions = if (length(input$pref_regions) > 0 && input$pref_regions[1] != "All") input$pref_regions else NULL,
      # Store detailed preferences for potential future use
      env_features = input$env_features,
      econ_features = input$econ_features,
      qol_features = input$qol_features,
      gov_features = input$gov_features,
      # Climate preferences
      temp_preference = input$temp_preference,
      humidity_preference = input$humidity_preference,
      seasons_preference = input$seasons_preference,
      rainfall_preference = input$rainfall_preference,
      weather_events_avoid = input$weather_events_avoid
    )
  })
  
  # 2. Matched_cities function to handle data source filtering properly
  matched_cities <- eventReactive(input$find_matches, {
    # Get user preferences
    prefs <- user_preferences()
    
    # Start with full dataset
    data <- city_data
    
    # Debug message
    message("Starting city matching with ", nrow(data), " cities")
    
    # Initialize filter conditions with proper checking
    use_numbeo <- "Source_Numbeo" %in% input$pref_data_source
    use_arcadis <- "Source_Arcadis" %in% input$pref_data_source
    
    # Apply filtering based on selected data sources
    if (length(input$pref_data_source) > 0) {
      if (use_numbeo && !use_arcadis) {
        data <- data %>% filter(Source_Numbeo == TRUE)
        message("Filtered to ", nrow(data), " NUMBEO cities")
      } else if (!use_numbeo && use_arcadis) {
        data <- data %>% filter(Source_Arcadis == TRUE)
        message("Filtered to ", nrow(data), " ARCADIS cities")
      } else if (use_numbeo && use_arcadis) {
        # Both are selected, keep cities from either source
        data <- data %>% filter(Source_Numbeo == TRUE | Source_Arcadis == TRUE)
        message("Keeping cities from both sources: ", nrow(data), " cities")
      } else {
        # Neither source selected - keep all with a warning
        message("WARNING: No data sources selected. Keeping all cities.")
      }
    }
    
    # Handle potential empty dataset
    if (nrow(data) == 0) {
      message("WARNING: Empty dataset after source filtering")
      return(data.frame(
        City = character(0),
        Country = character(0),
        Region = character(0),
        preference_distance = numeric(0),
        match_rank = integer(0),
        climate_match = numeric(0),
        Weighted_Rank = integer(0),
        Economics_Score = numeric(0),
        Environment_Score = numeric(0),
        QualityOfLife_Score = numeric(0),
        Governance_Score = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
    
    # Find matches with error handling
    tryCatch({
      matches <- find_similar_cities(data, prefs)
      message("Found ", nrow(matches), " matches")
      return(matches)
    }, error = function(e) {
      message("Error finding matches: ", e$message)
      # Return an empty dataframe with proper structure
      return(data.frame(
        City = character(0),
        Country = character(0),
        Region = character(0),
        preference_distance = numeric(0),
        match_rank = integer(0),
        climate_match = numeric(0),
        Weighted_Rank = integer(0),
        Economics_Score = numeric(0),
        Environment_Score = numeric(0),
        QualityOfLife_Score = numeric(0),
        Governance_Score = numeric(0),
        stringsAsFactors = FALSE
      ))
    })
  })
  
  # Add this after matched_cities is defined
  observeEvent(input$find_matches, {
    matches <- matched_cities()
    if (nrow(matches) > 0) {
      weather_data <- list()
      
      for (i in 1:min(nrow(matches), 5)) {
        city_name <- matches$City[i]
        country_code <- matches$Country[i]
        city_key <- paste0(city_name, "_", country_code)
        
        # Use fake data for testing - much more reliable
        if (use_fake_weather_data) {
          weather_data[[city_key]] <- generate_fake_weather_data(city_name)
        } else {
          weather_data[[city_key]] <- fetch_weather_data(city_name, country_code)
        }
      }
      
      # Update the cache
      weather_data_cache(weather_data)
    }
  })
  
  # 3. Update the find_similar_cities function to handle edge cases
  find_similar_cities <- function(data, preferences) {
    # Check for empty dataset
    if (nrow(data) == 0) {
      message("Empty dataset provided to find_similar_cities")
      return(data.frame(
        City = character(0),
        Country = character(0),
        Region = character(0),
        preference_distance = numeric(0),
        match_rank = integer(0),
        climate_match = numeric(0),
        Weighted_Rank = integer(0),
        Economics_Score = numeric(0),
        Environment_Score = numeric(0),
        QualityOfLife_Score = numeric(0),
        Governance_Score = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
    
    result_data <- data
    
    #Set default weights if missing
    if(is.null(preferences$environment_weight) || !is.numeric(preferences$environment_weight)) {
      preferences$environment_weight <- 1.5
    }
    if(is.null(preferences$economics_weight) || !is.numeric(preferences$economics_weight)) {
      preferences$economics_weight <- 1.0
    }
    if(is.null(preferences$quality_of_life_weight) || !is.numeric(preferences$quality_of_life_weight)) {
      preferences$quality_of_life_weight <- 1.5
    }
    if(is.null(preferences$governance_weight) || !is.numeric(preferences$governance_weight)) {
      preferences$governance_weight <- 1.2
    }
    
    # Ensure all preference values are numeric with defaults
    preferences$environment <- as.numeric(preferences$environment)
    if(is.na(preferences$environment)) preferences$environment <- 50
    
    preferences$economics <- as.numeric(preferences$economics)
    if(is.na(preferences$economics)) preferences$economics <- 50
    
    preferences$quality_of_life <- as.numeric(preferences$quality_of_life)
    if(is.na(preferences$quality_of_life)) preferences$quality_of_life <- 50
    
    preferences$governance <- as.numeric(preferences$governance)
    if(is.na(preferences$governance)) preferences$governance <- 50
    
    
    tryCatch({
      # Handle potential NA or NULL values in the calculation
      env_diff <- result_data$Environment_Score - preferences$environment
      env_diff[is.na(env_diff)] <- 0
      env_component <- (env_diff^2) * preferences$environment_weight
      
      econ_diff <- result_data$Economics_Score - preferences$economics
      econ_diff[is.na(econ_diff)] <- 0
      econ_component <- (econ_diff^2) * preferences$economics_weight
      
      qol_diff <- result_data$QualityOfLife_Score - preferences$quality_of_life
      qol_diff[is.na(qol_diff)] <- 0
      qol_component <- (qol_diff^2) * preferences$quality_of_life_weight
      
      gov_diff <- result_data$Governance_Score - preferences$governance
      gov_diff[is.na(gov_diff)] <- 0
      gov_component <- (gov_diff^2) * preferences$governance_weight
      
      # Calculate total distance
      sum_components <- env_component + econ_component + qol_component + gov_component
      result_data$preference_distance <- sqrt(sum_components)
      
      # Verify calculation succeeded
      message("Successfully calculated preference distances. Range: ", 
              min(result_data$preference_distance, na.rm = TRUE), " to ", 
              max(result_data$preference_distance, na.rm = TRUE))
      
    }, error = function(e) {
      # On error, log message and create default values
      message("Error in preference distance calculation: ", e$message)
      
      # Set default distances based on a simple Euclidean distance without weights
      result_data$preference_distance <- sqrt(
        (result_data$Environment_Score - 50)^2 + 
          (result_data$Economics_Score - 50)^2 + 
          (result_data$QualityOfLife_Score - 50)^2 + 
          (result_data$Governance_Score - 50)^2
      )
      
      # Handle NAs in the fallback calculation
      result_data$preference_distance[is.na(result_data$preference_distance)] <- 100
    })
    
    # Always sort and rank here, after either successful calculation or fallback
    result_data <- result_data %>%
      arrange(preference_distance) %>%
      mutate(match_rank = row_number())
    
    # Debug
    message("After initial ranking, top city: ", result_data$City[1], 
            ", distance: ", result_data$preference_distance[1])
    
    # Filter by region if specified
    if (!is.null(preferences$regions) && length(preferences$regions) > 0 && 
        all(preferences$regions != "All")) {
      message("Filtering by regions: ", paste(preferences$regions, collapse = ", "))
      result_data <- result_data %>% filter(Region %in% preferences$regions)
      
      # Re-rank after filtering
      if (nrow(result_data) > 0) {
        result_data <- result_data %>%
          arrange(preference_distance) %>%
          mutate(match_rank = row_number())
      }
    }
    
    # Handle potential empty result after filtering
    if (nrow(result_data) == 0) {
      message("No matches found after filtering")
      return(data.frame(
        City = character(0),
        Country = character(0),
        Region = character(0),
        preference_distance = numeric(0),
        match_rank = integer(0),
        climate_match = numeric(0),
        Weighted_Rank = integer(0),
        Economics_Score = numeric(0),
        Environment_Score = numeric(0),
        QualityOfLife_Score = numeric(0),
        Governance_Score = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
    
    message("Found ", nrow(result_data), " matching cities")
    return(result_data)
  }

  output$matching_results <- renderUI({
    if (input$find_matches == 0) {
      return(NULL)
    }
    
    # Try to get matched cities
    matches <- tryCatch({
      matched_cities()
    }, error = function(e) {
      message("Error getting matched cities: ", e$message)
      return(NULL)
    })
    
    # Safety check before proceeding
    if (is.null(matches) || nrow(matches) == 0) {
      return(
        fluidRow(
          box(
            title = "No Matches Found", width = 12, status = "warning", solidHeader = TRUE,
            p("No cities match your current criteria. Try adjusting your preferences or expanding your region selection."),
            div(
              style = "text-align: center; margin-top: 20px;",
              actionButton("reset_preferences", "Reset Preferences", class = "btn btn-primary")
            )
          )
        )
      )
    }
    
    # Top matches preview
    top_matches <- matches %>% head(3)
    
    
    
    # Create UI elements when matches are found
    fluidRow(
      box(
        title = "Your Matching Results", width = 12, status = "success", solidHeader = TRUE,
        p("Based on your preferences, we've found", strong(nrow(matches)), "matching cities."),
        p("Here are your top 3 matches:"),
        
        # Top 3 matches preview table with styling
        tags$div(
          style = "overflow-x: auto;",
          tags$table(
            class = "table table-bordered table-striped",
            tags$thead(
              tags$tr(
                tags$th("Rank"),
                tags$th("City"),
                tags$th("Country"),
                tags$th("Region"),
                tags$th("Match Score")
              )
            ),
            tags$tbody(
              lapply(1:nrow(top_matches), function(i) {
                # Calculate a match score from 0-100 where higher is better
                match_score <- 100 - pmin(100, round(top_matches$preference_distance[i], 1))
                
                tags$tr(
                  tags$td(i),
                  tags$td(strong(top_matches$City[i])),
                  tags$td(top_matches$Country[i]),
                  tags$td(top_matches$Region[i]),
                  tags$td(
                    div(
                      class = "progress",
                      style = "margin-bottom: 0;",
                      div(
                        class = "progress-bar",
                        style = paste0("width: ", match_score, "%; background-color: ", 
                                       if(match_score > 75) "#5cb85c" else if(match_score > 50) "#f0ad4e" else "#d9534f"),
                        match_score, "%"
                      )
                    )
                  )
                )
              })
            )
          )
        ),
        
        p("Explore all your matches in the tabs below.")
      )
    )
  })
  
  output$matches_map <- renderLeaflet({
    # Add error handling
    tryCatch({
      req(matched_cities())
      matches <- matched_cities()
      
      # Check if we have matches
      if(nrow(matches) == 0) {
        return(leaflet() %>% 
                 addTiles() %>%
                 setView(lng = 0, lat = 0, zoom = 2) %>%
                 addControl("No matching cities found. Try adjusting your preferences.", 
                            position = "topright"))
      }
      
      # Get top matches
      top_cities <- matches %>% head(10)
      
      # Ensure we have all needed columns
      if(!"lng" %in% names(top_cities) || !"lat" %in% names(top_cities)) {
        return(leaflet() %>% 
                 addTiles() %>%
                 setView(lng = 0, lat = 0, zoom = 2) %>%
                 addControl("Missing location data for cities. Check your dataset.", 
                            position = "topright"))
      }
      
      # Ensure preference_distance exists and is numeric
      if(!"preference_distance" %in% names(top_cities) || 
         !is.numeric(top_cities$preference_distance)) {
        top_cities$preference_distance <- 100
      }
      
      # Create domain with protection against NA/NULL
      domain_min <- min(top_cities$preference_distance, na.rm = TRUE)
      domain_max <- max(top_cities$preference_distance, na.rm = TRUE)
      
      # Use safe domain values
      if(!is.finite(domain_min)) domain_min <- 0
      if(!is.finite(domain_max)) domain_max <- 100
      if(domain_min == domain_max) {
        domain_min <- domain_min - 1
        domain_max <- domain_max + 1
      }
      
      # Create color palette
      pal <- colorNumeric(
        palette = "RdYlGn",
        domain = c(domain_min, domain_max),
        reverse = TRUE  # Lower distance (better match) = greener
      )
      
      # Create labels
      labels <- paste0(
        "#", top_cities$match_rank, ": ", 
        top_cities$City, ", ", 
        top_cities$Country
      )
      
      # Create the map
      leaflet(top_cities) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~lng,
          lat = ~lat,
          color = ~pal(preference_distance),
          radius = 12,  # Fixed radius for simplicity
          fillOpacity = 0.8,
          weight = 2,
          stroke = TRUE,
          label = labels
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~preference_distance,
          title = "Match Quality",
          opacity = 1
        ) %>%
        setView(lng = mean(top_cities$lng), lat = mean(top_cities$lat), zoom = 2)
    }, error = function(e) {
      # Return a basic map with error message on failure
      message("Error rendering map: ", e$message)
      return(leaflet() %>% 
               addTiles() %>%
               setView(lng = 0, lat = 0, zoom = 2) %>%
               addControl(paste("Error loading map:", e$message), 
                          position = "topright"))
    })
  })
  
  # Add this to your server
  output$diagnostic_output <- renderPrint({
    req(input$find_matches > 0)
    
    # Collect diagnostic information
    cat("Diagnostic Information:\n\n")
    
    # Check matched cities
    matches <- tryCatch({
      m <- matched_cities()
      cat("Matched cities count:", nrow(m), "\n")
      if(nrow(m) > 0) {
        cat("First match:", m$City[1], ",", m$Country[1], "\n")
        cat("Preference distance:", m$preference_distance[1], "\n")
      }
      m
    }, error = function(e) {
      cat("Error getting matched cities:", e$message, "\n")
      NULL
    })
    
    # Check weather data
    weather_cache <- weather_data_cache()
    cat("\nWeather cache status:", if(is.null(weather_cache)) "NULL" else "Initialized", "\n")
    if(!is.null(weather_cache)) {
      cat("Weather cache entries:", length(weather_cache), "\n")
      if(length(weather_cache) > 0) {
        for(name in names(weather_cache)) {
          cat(" -", name, ":", if(is.null(weather_cache[[name]])) "NULL" else "Data available", "\n")
        }
      }
    }
    
    # Print reactive dependencies
    cat("\nReactive dependencies status:\n")
    cat(" - user_preferences:", if(is.function(user_preferences)) "Function" else "Not a function", "\n")
    cat(" - matched_cities:", if(is.function(matched_cities)) "Function" else "Not a function", "\n")
    
    invisible()
  })
  
  output$matches_table <- renderDT({
    req(matched_cities())
    matches <- matched_cities() %>% head(20)
    
    if (nrow(matches) == 0) {
      return(datatable(data.frame(Message = "No matching cities found. Try adjusting your preferences."),
                       options = list(dom = 't'), rownames = FALSE))
    }
    
    # Format data with simplified scores and better structure
    display_data <- matches %>%
      select(
        Rank = match_rank,
        City,
        Country,
        Region,
        preference_distance,
        "Weighted Rank" = Weighted_Rank,
        Economics = Economics_Score,
        Environment = Environment_Score,
        "Quality of Life" = QualityOfLife_Score,
        Governance = Governance_Score
      ) %>%
      # Round scores first, then calculate match score
      mutate(
        Economics = round(Economics, 0),  # Use whole numbers for clarity
        Environment = round(Environment, 0),
        "Quality of Life" = round(`Quality of Life`, 0),
        Governance = round(Governance, 0),
        "Match Score" = round(100 - pmin(100, preference_distance), 0)
      ) %>%
      select(-preference_distance)  # Remove the original distance column
    
    # Create a more visually appealing datatable
    datatable(
      display_data,
      options = list(
        pageLength = 10,
        dom = '<"top"f>rt<"bottom"ip>',
        scrollX = TRUE,
        columnDefs = list(
          # Center align all numeric columns
          list(className = 'dt-center', targets = c(0, 4, 5, 6, 7, 8, 9)),
          # Left align text columns
          list(className = 'dt-left', targets = c(1, 2, 3))
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      # Format using more meaningful colors
      formatStyle(
        'Match Score',
        background = styleColorBar(c(0, 100), 'rgba(66, 139, 202, 0.8)'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        fontWeight = 'bold',
        color = styleInterval(c(75, 90), c('#333333', '#333333', 'white'))
      ) %>%
      formatStyle(columns = 6:9, fontSize = '90%')  # Slightly reduce font size for score columns
  })
  
  output$matches_score_dist <- renderPlotly({
    req(matched_cities())
    matches <- matched_cities()
    
    message("Score distribution data: ", nrow(matches), " rows")
    
    if (nrow(matches) == 0) {
      return(plot_ly() %>% 
               add_annotations(text = "No matching cities to display",
                               showarrow = FALSE, font = list(size = 16)))
    }
    
    # Create score bins
    score_bins <- cut(
      matches$Weighted_Score,
      breaks = seq(0, 100, by = 10),
      labels = c("0-10", "10-20", "20-30", "30-40", "40-50", 
                 "50-60", "60-70", "70-80", "80-90", "90-100"),
      include.lowest = TRUE
    )
    
    # Count cities in each bin
    score_counts <- table(score_bins)
    score_df <- data.frame(
      Score = names(score_counts),
      Count = as.numeric(score_counts)
    )
    
    # Create the plot
    p <- ggplot(score_df, aes(x = Score, y = Count, fill = Score)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Score Range", y = "Number of Cities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  output$matches_region_dist <- renderPlotly({
    req(matched_cities())
    matches <- matched_cities()
    
    message("Region distribution data: ", nrow(matches), " rows")
    if (nrow(matches) == 0 || !"Region" %in% colnames(matches)) {
      return(plot_ly() %>% 
               add_annotations(text = "No region data available",
                               showarrow = FALSE, font = list(size = 16)))
    }
    
    # Count cities by region
    region_counts <- matches %>%
      count(Region) %>%
      arrange(desc(n))
    
    # Limit to top 10 regions for clarity
    if (nrow(region_counts) > 10) {
      region_counts <- head(region_counts, 10)
    }
    
    # Create the plot
    p <- ggplot(region_counts, aes(x = reorder(Region, -n), y = n, fill = Region)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "", y = "Number of Cities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  output$user_profile_radar <- renderPlotly({
    # Create a radar chart showing user preferences
    
    # Get current preferences from sliders
    env_pref <- input$pref_environment
    econ_pref <- input$pref_economics
    qol_pref <- input$pref_qol
    gov_pref <- input$pref_governance
    
    # Ensure values are numeric
    if(is.null(env_pref) || !is.numeric(env_pref)) env_pref <- 50
    if(is.null(econ_pref) || !is.numeric(econ_pref)) econ_pref <- 50
    if(is.null(qol_pref) || !is.numeric(qol_pref)) qol_pref <- 50
    if(is.null(gov_pref) || !is.numeric(gov_pref)) gov_pref <- 50
    
    # Create a plotly radar chart
    plot_ly(
      type = 'scatterpolar',
      mode = 'lines',
      fill = 'toself',
      r = c(env_pref, econ_pref, qol_pref, gov_pref),
      theta = c('Environment', 'Economics', 'Quality of Life', 'Governance'),
      line = list(color = 'rgb(67, 160, 71)'),
      fillcolor = 'rgba(67, 160, 71, 0.5)'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100)
          )
        ),
        showlegend = FALSE
      )
  })
  


  # 5. Add a reset preferences function (optional)
  observeEvent(input$reset_preferences, {
    updateSliderInput(session, "pref_environment", value = 50)
    updateSliderInput(session, "pref_economics", value = 50)
    updateSliderInput(session, "pref_qol", value = 50)
    updateSliderInput(session, "pref_governance", value = 50)
    
    updateSliderInput(session, "weight_environment", value = 1.5)
    updateSliderInput(session, "weight_economics", value = 1.0)
    updateSliderInput(session, "weight_qol", value = 1.5)
    updateSliderInput(session, "weight_governance", value = 1.2)
    
    updateSelectInput(session, "pref_regions", selected = "All")
    updateCheckboxGroupInput(session, "pref_data_source", 
                             selected = c("Source_Numbeo", "Source_Arcadis"))
    
    updateSliderInput(session, "temp_preference", value = c(15, 25))
    updateCheckboxInput(session, "humidity_preference", value = FALSE)
  })
  
  # Weather comparison chart
  # Replace the existing weather_comparison_plot output with this cleaner version
  output$weather_comparison_plot <- renderUI({
    req(matched_cities(), weather_data_cache())
    
    # Get top cities
    top_cities <- matched_cities() %>% head(5)
    
    # User preferences
    temp_range <- input$temp_preference
    humidity_pref <- input$humidity_preference
    
    # Create weather cards for each city
    weather_cards <- lapply(1:nrow(top_cities), function(i) {
      city_name <- top_cities$City[i]
      country_code <- top_cities$Country[i]
      city_key <- paste0(city_name, "_", country_code)
      
      # Get weather data
      city_weather <- weather_data_cache()[[city_key]]
      if (is.null(city_weather)) {
        # Create placeholder data if none available
        city_weather <- list(
          temperature = runif(1, 5, 30),
          humidity = runif(1, 30, 90),
          weather_main = sample(c("Clear", "Clouds", "Rain"), 1)
        )
      }
      
      # Check if temperature is in preferred range
      temp_match <- city_weather$temperature >= temp_range[1] && 
        city_weather$temperature <= temp_range[2]
      
      # Check if humidity meets preferences
      humidity_match <- (!humidity_pref) || 
        (humidity_pref && city_weather$humidity < 70)
      
      # Determine overall climate match score (0-100)
      match_score <- (temp_match * 50) + (humidity_match * 50)
      
      # Weather icon
      weather_icon <- switch(
        city_weather$weather_main,
        "Clear" = "sun",
        "Clouds" = "cloud",
        "Rain" = "cloud-showers-heavy",
        "Snow" = "snowflake",
        "Thunderstorm" = "bolt",
        "sun" # default
      )
      
      # Create a card for each city
      div(
        class = "col-md-6 col-lg-4",
        div(
          class = paste0("panel panel-", 
                         ifelse(match_score >= 80, "success", 
                                ifelse(match_score >= 50, "info", "warning"))),
          div(
            class = "panel-heading",
            h4(class = "panel-title", paste0(city_name, ", ", country_code))
          ),
          div(
            class = "panel-body",
            
            # Current conditions
            div(class = "row text-center",
                div(class = "col-xs-4",
                    tags$i(class = paste0("fa fa-", weather_icon), 
                           style = "font-size: 2.5em; color: #5bc0de;"),
                    h4(city_weather$weather_main),
                    tags$small("Conditions")
                ),
                div(class = "col-xs-4",
                    tags$i(class = "fa fa-thermometer-half", 
                           style = "font-size: 2.5em; color: #f0ad4e;"),
                    h3(paste0(round(city_weather$temperature, 1), "°C")),
                    tags$small("Temperature")
                ),
                div(class = "col-xs-4",
                    tags$i(class = "fa fa-tint", 
                           style = "font-size: 2.5em; color: #5bc0de;"),
                    h3(paste0(round(city_weather$humidity), "%")),
                    tags$small("Humidity")
                )
            ),
            
            # Preference matching indicators
            div(class = "row", style = "margin-top: 15px;",
                div(class = "col-xs-6",
                    div(class = "well well-sm text-center",
                        style = paste0("background-color: ", 
                                       ifelse(temp_match, "#dff0d8", "#fcf8e3"), ";"),
                        tags$i(class = paste0("fa fa-", 
                                              ifelse(temp_match, "check", "exclamation-triangle")),
                               style = paste0("margin-right: 5px; color: ", 
                                              ifelse(temp_match, "#5cb85c", "#f0ad4e"), ";")),
                        tags$strong(
                          ifelse(temp_match, 
                                 "Temperature in range", 
                                 "Temperature outside range")
                        )
                    )
                ),
                div(class = "col-xs-6",
                    div(class = "well well-sm text-center",
                        style = paste0("background-color: ", 
                                       ifelse(humidity_match, "#dff0d8", "#fcf8e3"), ";"),
                        tags$i(class = paste0("fa fa-", 
                                              ifelse(humidity_match, "check", "exclamation-triangle")),
                               style = paste0("margin-right: 5px; color: ", 
                                              ifelse(humidity_match, "#5cb85c", "#f0ad4e"), ";")),
                        tags$strong(
                          ifelse(humidity_match, 
                                 ifelse(humidity_pref, "Low humidity", "Humidity OK"), 
                                 "Humidity higher than preferred")
                        )
                    )
                )
            ),
            
            # Climate match score
            div(style = "margin-top: 15px;",
                h4("Climate Match Score", class = "text-center"),
                div(class = "progress",
                    div(class = "progress-bar",
                        role = "progressbar",
                        style = paste0("width: ", match_score, "%; background-color: ", 
                                       ifelse(match_score >= 80, "#5cb85c", 
                                              ifelse(match_score >= 50, "#5bc0de", "#f0ad4e"))),
                        paste0(match_score, "%")
                    )
                ),
                p(class = "text-center text-muted",
                  ifelse(match_score >= 80, 
                         "Excellent climate match for your preferences!",
                         ifelse(match_score >= 50, 
                                "Good climate match for your preferences",
                                "Fair climate match - consider your priorities"))
                )
            )
          )
        )
      )
    })
    
    fluidRow(
      div(class = "col-xs-12",
          h3("Weather Comparison for Top Matching Cities", class = "text-center"),
          p("This comparison shows how well each city's climate aligns with your stated preferences.", 
            class = "text-center")
      ),
      div(class = "weather-cards-container", 
          style = "display: flex; flex-wrap: wrap;",
          weather_cards
      )
    )
  })
  
  output$climate_preferences_display <- renderUI({
    fluidRow(
      column(width = 6,
             h4("Temperature"),
             div(class = "well well-sm text-center",
                 h3(paste0(input$temp_preference[1], "°C - ", input$temp_preference[2], "°C")),
                 p("Your preferred range")
             )
      ),
      column(width = 6,
             h4("Humidity"),
             div(class = "well well-sm text-center",
                 h3(ifelse(input$humidity_preference, "Low Humidity", "Any Humidity")),
                 p("Your preference")
             )
      ),
      column(width = 12,
             h4("Seasonal Variation"),
             div(class = "well well-sm",
                 p(ifelse(input$seasons_preference == "distinct", 
                          "You prefer distinct seasons with temperature variation throughout the year.",
                          "You prefer consistent climate conditions year-round."))
             )
      ),
      column(width = 12,
             h4("Weather Events to Avoid"),
             div(class = "well well-sm",
                 if (is.null(input$weather_events_avoid) || 
                     length(input$weather_events_avoid) == 0 || 
                     input$weather_events_avoid[1] == "none") {
                   p("You haven't specified any weather events to avoid.")
                 } else {
                   tagList(
                     p("You prefer to avoid:"),
                     tags$ul(
                       lapply(input$weather_events_avoid, function(event) {
                         tags$li(
                           switch(event,
                                  "hurricanes" = "Hurricanes and Typhoons",
                                  "heat" = "Extreme Heat",
                                  "snow" = "Heavy Snow",
                                  "flooding" = "Flooding",
                                  event)
                         )
                       })
                     )
                   )
                 }
             )
      )
    )
  })
  
  output$climate_match_summary <- renderUI({
    req(matched_cities(), weather_data_cache())
    
    # Get top 3 cities from matched_cities
    top_cities <- matched_cities() %>% head(3)
    
    # If there are no matched cities, show a message
    if(nrow(top_cities) == 0) {
      return(div("No cities matched your preferences."))
    }
    
    # Ensure the 'climate_match' column exists; if not, default it to 70
    if (!("climate_match" %in% names(top_cities))) {
      top_cities$climate_match <- 70
    }
    
    # Get user preferences for temperature and humidity
    user_prefs <- user_preferences()
    temp_range <- user_prefs$temp_preference  # Expecting a numeric vector of length 2
    humidity_pref <- user_prefs$humidity_preference
    
    # Create a summary card for each city
    city_cards <- lapply(seq_len(nrow(top_cities)), function(i) {
      city_name <- top_cities$City[i]
      country_code <- top_cities$Country[i]
      city_key <- paste0(city_name, "_", country_code)
      climate_match_score <- top_cities$climate_match[i]
      
      # Get weather data for the city
      city_weather <- weather_data_cache()[[city_key]]
      if (is.null(city_weather)) {
        return(
          div(class = "col-md-4",
              div(class = "panel panel-default",
                  div(class = "panel-heading", 
                      h4(class = "panel-title", paste0("#", i, ": ", city_name, ", ", country_code))),
                  div(class = "panel-body text-center",
                      p("Weather data not available"),
                      div(class = "well well-sm",
                          "Ranking based on city preferences only")
                  )
              )
          )
        )
      }
      
      # Safely compute temperature match only if data exists
      temp_value <- city_weather$temperature
      temp_ok <- FALSE
      if (!is.null(temp_value) && length(temp_range) == 2) {
        temp_ok <- isTRUE(temp_value >= temp_range[1] && temp_value <= temp_range[2])
      }
      
      # Safely compute humidity match
      humidity_value <- city_weather$humidity
      humidity_ok <- FALSE
      if (!is.null(humidity_value)) {
        if (!humidity_pref) {
          humidity_ok <- TRUE
        } else {
          humidity_ok <- isTRUE(humidity_value < 70)
        }
      }
      
      # Create status indicators
      temp_status <- if(temp_ok) {
        tags$span(class = "label label-success", 
                  tags$i(class = "fa fa-check"), " Within range")
      } else {
        tags$span(class = "label label-warning", 
                  tags$i(class = "fa fa-exclamation-triangle"), " Outside range")
      }
      
      humidity_status <- if(humidity_ok) {
        tags$span(class = "label label-success", 
                  tags$i(class = "fa fa-check"), " Matches preference")
      } else {
        tags$span(class = "label label-warning", 
                  tags$i(class = "fa fa-exclamation-triangle"), " Higher than preferred")
      }
      
      # Determine panel color based on climate_match_score
      panel_class <- if(climate_match_score >= 80) {
        "panel-success"
      } else if(climate_match_score >= 60) {
        "panel-info"
      } else {
        "panel-warning"
      }
      
      # Create the UI card for the city
      div(class = "col-md-4",
          div(class = paste0("panel ", panel_class),
              div(class = "panel-heading", 
                  h4(class = "panel-title", paste0("#", i, ": ", city_name, ", ", country_code))),
              div(class = "panel-body",
                  div(class = "row",
                      div(class = "col-xs-4 text-center",
                          tags$i(class = "fa fa-thermometer-half fa-3x"),
                          h4(paste0(round(temp_value, 1), "°C")),
                          temp_status
                      ),
                      div(class = "col-xs-4 text-center",
                          tags$i(class = "fa fa-tint fa-3x"),
                          h4(paste0(round(humidity_value, 0), "%")),
                          humidity_status
                      ),
                      div(class = "col-xs-4 text-center",
                          tags$i(class = "fa fa-sun fa-3x"),
                          h4(city_weather$weather_main),
                          tags$small("Condition")
                      )
                  ),
                  hr(),
                  div(class = "text-center",
                      h4("Climate Match Score"),
                      div(class = "progress",
                          div(class = "progress-bar", 
                              role = "progressbar",
                              style = paste0("width: ", climate_match_score, "%; ",
                                             "background-color: ", 
                                             ifelse(climate_match_score >= 80, "#5cb85c", 
                                                    ifelse(climate_match_score >= 60, "#5bc0de", "#f0ad4e"))),
                              paste0(climate_match_score, "%")
                          )
                      ),
                      p(class = "text-muted", 
                        ifelse(climate_match_score >= 80,
                               "Excellent climate match!",
                               ifelse(climate_match_score >= 60,
                                      "Good climate match",
                                      "Fair climate match - consider your preferences")))
                  )
              )
          )
      )
    })
    
    # Return the fluidRow containing all city cards
    fluidRow(city_cards)
  })
  
  
  output$region_climate_plot <- renderPlotly({
    selected_region <- input$weather_analysis_region
    
    # Filter cities by region
    cities <- if (selected_region == "All") {
      city_data
    } else {
      city_data %>% filter(Region == selected_region)
    }
    
    # Get weather data for cities in this region
    weather_cache <- weather_data_cache()
    
    # Prepare data for plotting
    plot_data <- data.frame(
      City = character(),
      Temperature = numeric(),
      Humidity = numeric(),
      Region = character(),
      Country = character(),
      stringsAsFactors = FALSE
    )
    
    # Add data for cities with weather information
    if (!is.null(weather_cache)) {
      for (i in 1:nrow(cities)) {
        city_name <- cities$City[i]
        country_code <- cities$Country[i]
        region <- cities$Region[i]
        city_key <- paste0(city_name, "_", country_code)
        
        if (city_key %in% names(weather_cache) && !is.null(weather_cache[[city_key]])) {
          city_weather <- weather_cache[[city_key]]
          
          # Add to plot data
          plot_data <- rbind(
            plot_data,
            data.frame(
              City = city_name,
              Temperature = city_weather$temperature,
              Humidity = city_weather$humidity,
              Region = region,
              Country = country_code,
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
    
    # If no data, generate placeholder data
    if (nrow(plot_data) == 0) {
      # Generate representative data for the region
      set.seed(123) # For reproducibility
      
      # Different temperature ranges for different regions
      temp_ranges <- list(
        "North America" = c(5, 25),
        "Europe" = c(0, 20),
        "Asia" = c(10, 35),
        "Africa" = c(15, 40),
        "Oceania" = c(15, 30),
        "South America" = c(10, 30)
      )
      
      # Get temperature range for this region
      temp_range <- if (selected_region %in% names(temp_ranges)) {
        temp_ranges[[selected_region]]
      } else {
        c(10, 30) # Default range
      }
      
      # Generate data for top cities in this region
      top_region_cities <- cities %>% 
        arrange(Weighted_Rank) %>% 
        head(10)
      
      for (i in 1:nrow(top_region_cities)) {
        plot_data <- rbind(
          plot_data,
          data.frame(
            City = top_region_cities$City[i],
            Temperature = runif(1, temp_range[1], temp_range[2]),
            Humidity = runif(1, 40, 80),
            Region = top_region_cities$Region[i],
            Country = top_region_cities$Country[i],
            stringsAsFactors = FALSE
          )
        )
      }
    }
    
    # Get user preferences for visualization
    temp_min <- input$temp_preference[1]
    temp_max <- input$temp_preference[2]
    
    # Create a scatter plot with temperature vs. humidity
    p <- plot_ly(plot_data, 
                 x = ~Temperature, 
                 y = ~Humidity,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 10),
                 text = ~paste(City, Country, 
                               "<br>Temperature:", round(Temperature, 1), "°C",
                               "<br>Humidity:", round(Humidity, 0), "%"),
                 hoverinfo = 'text') %>%
      layout(
        title = paste("Temperature vs. Humidity in", 
                      ifelse(selected_region == "All", "All Regions", selected_region)),
        xaxis = list(title = "Temperature (°C)"),
        yaxis = list(title = "Humidity (%)"),
        shapes = list(
          # Add rectangle for preferred temperature range
          list(
            type = "rect",
            x0 = temp_min,
            x1 = temp_max,
            y0 = 0,
            y1 = 100,
            fillcolor = "rgba(144, 238, 144, 0.3)",
            line = list(width = 0),
            layer = "below"
          ),
          # Add horizontal line for humidity preference if selected
          if (input$humidity_preference) {
            list(
              type = "line",
              x0 = -20,
              x1 = 50,
              y0 = 70,
              y1 = 70,
              line = list(
                color = "rgba(255, 0, 0, 0.5)",
                width = 2,
                dash = "dash"
              )
            )
          }
        )
      )
    
    # Add annotation for humidity preference if selected
    if (input$humidity_preference) {
      p <- p %>% add_annotations(
        x = max(plot_data$Temperature, na.rm = TRUE),
        y = 70,
        text = "Preferred Max Humidity",
        showarrow = FALSE,
        xanchor = "right",
        yanchor = "bottom",
        font = list(color = "red")
      )
    }
    
    return(p)
  })
  
  output$region_climate_analysis <- renderUI({
    selected_region <- input$weather_analysis_region
    temp_range <- input$temp_preference
    humidity_pref <- input$humidity_preference
    
    # Get weather data
    weather_cache <- weather_data_cache()
    
    # If no weather data available, provide a generic message
    if (is.null(weather_cache) || length(weather_cache) == 0) {
      return(
        div(
          p("Insufficient weather data available for analysis. Try adjusting your search or wait for data to load."),
          p("Based on general climate patterns, here are some observations:"),
          tags$ul(
            tags$li(strong("Temperature considerations:"), 
                    "Your preferred range is ", temp_range[1], "°C to ", temp_range[2], "°C. ",
                    "This range is typical for ", 
                    ifelse(mean(temp_range) > 25, "warm tropical or subtropical regions.",
                           ifelse(mean(temp_range) > 15, "temperate regions with moderate climates.",
                                  "cooler regions or higher altitudes."))),
            tags$li(strong("Humidity considerations:"), 
                    ifelse(humidity_pref, 
                           "You prefer low humidity environments, which are typically found in inland, arid, or desert regions, as well as some Mediterranean climates.",
                           "You have no specific humidity preference, which gives you more flexibility in choosing locations."))
          )
        )
      )
    }
    
    # Filter cities by region
    cities <- if (selected_region == "All") {
      city_data
    } else {
      city_data %>% filter(Region == selected_region)
    }
    
    # Check for climate compatibility
    cities_with_weather <- 0
    cities_in_temp_range <- 0
    cities_with_good_humidity <- 0
    
    for (i in 1:nrow(cities)) {
      city_name <- cities$City[i]
      country_code <- cities$Country[i]
      city_key <- paste0(city_name, "_", country_code)
      
      if (city_key %in% names(weather_cache) && !is.null(weather_cache[[city_key]])) {
        cities_with_weather <- cities_with_weather + 1
        
        city_weather <- weather_cache[[city_key]]
        
        # Check temperature
        if (city_weather$temperature >= temp_range[1] && 
            city_weather$temperature <= temp_range[2]) {
          cities_in_temp_range <- cities_in_temp_range + 1
        }
        
        # Check humidity
        if (!humidity_pref || (humidity_pref && city_weather$humidity < 70)) {
          cities_with_good_humidity <- cities_with_good_humidity + 1
        }
      }
    }
    
    # Calculate percentages
    temp_match_pct <- ifelse(cities_with_weather > 0,
                             round(cities_in_temp_range / cities_with_weather * 100),
                             0)
    
    humidity_match_pct <- ifelse(cities_with_weather > 0,
                                 round(cities_with_good_humidity / cities_with_weather * 100),
                                 0)
    
    # Create analysis text
    div(
      p(strong("Regional Climate Analysis:"), "Based on data from", 
        strong(cities_with_weather), "cities in", 
        ifelse(selected_region == "All", "all regions", selected_region), "."),
      
      h4("Temperature Compatibility"),
      div(class = "progress",
          div(class = "progress-bar", 
              role = "progressbar",
              style = paste0("width:", temp_match_pct, "%; background-color:", 
                             ifelse(temp_match_pct >= 75, "#5cb85c", 
                                    ifelse(temp_match_pct >= 50, "#f0ad4e", "#d9534f"))),
              paste0(temp_match_pct, "% of cities in temperature range")
          )
      ),
      p(ifelse(temp_match_pct >= 75,
               paste0("The climate in ", ifelse(selected_region == "All", "most regions", selected_region), 
                      " is very compatible with your temperature preferences."),
               ifelse(temp_match_pct >= 50,
                      paste0("About half of the cities in ", ifelse(selected_region == "All", "the analyzed regions", selected_region), 
                             " match your temperature preferences."),
                      paste0("Few cities in ", ifelse(selected_region == "All", "the analyzed regions", selected_region), 
                             " match your temperature preferences. Consider adjusting your preferences or exploring other regions.")))),
      
      h4("Humidity Compatibility"),
      div(class = "progress",
          div(class = "progress-bar", 
              role = "progressbar",
              style = paste0("width:", humidity_match_pct, "%; background-color:", 
                             ifelse(humidity_match_pct >= 75, "#5cb85c", 
                                    ifelse(humidity_match_pct >= 50, "#f0ad4e", "#d9534f"))),
              paste0(humidity_match_pct, "% of cities match humidity preference")
          )
      ),
      p(ifelse(!humidity_pref,
               "You have no specific humidity preference.",
               ifelse(humidity_match_pct >= 75,
                      paste0("Most cities in ", ifelse(selected_region == "All", "the analyzed regions", selected_region), 
                             " have humidity levels that match your preference for low humidity."),
                      ifelse(humidity_match_pct >= 50,
                             paste0("About half of the cities in ", ifelse(selected_region == "All", "the analyzed regions", selected_region), 
                                    " match your preference for low humidity."),
                             paste0("Few cities in ", ifelse(selected_region == "All", "the analyzed regions", selected_region), 
                                    " match your preference for low humidity. Consider adjusting your preferences or exploring other regions.")))))
    )
  })
  
  output$climate_top_matches <- renderUI({
    # Get all cities
    all_cities <- city_data
    
    # Get user preferences
    user_prefs <- user_preferences()
    temp_range <- user_prefs$temp_preference
    humidity_pref <- user_prefs$humidity_preference
    
    # Get weather data
    weather_cache <- weather_data_cache()
    
    # If no weather data, return a message
    if (is.null(weather_cache) || length(weather_cache) == 0) {
      return(
        p("Weather data is still loading. Please check back in a moment.")
      )
    }
    
    # Calculate climate scores for all cities
    climate_scores <- data.frame(
      City = character(),
      Country = character(),
      Region = character(),
      Temperature = numeric(),
      Humidity = numeric(),
      ClimateScore = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:nrow(all_cities)) {
      city_name <- all_cities$City[i]
      country_code <- all_cities$Country[i]
      region <- all_cities$Region[i]
      city_key <- paste0(city_name, "_", country_code)
      
      if (city_key %in% names(weather_cache) && !is.null(weather_cache[[city_key]])) {
        city_weather <- weather_cache[[city_key]]
        
        # Calculate temperature score (0-50 points)
        temp_score <- 50
        if (!is.null(city_weather$temperature)) {
          if (city_weather$temperature < temp_range[1]) {
            # Too cold - reduce score based on how far below range
            temp_diff <- temp_range[1] - city_weather$temperature
            temp_score <- max(0, 50 - (temp_diff * 5))
          } else if (city_weather$temperature > temp_range[2]) {
            # Too hot - reduce score based on how far above range
            temp_diff <- city_weather$temperature - temp_range[2]
            temp_score <- max(0, 50 - (temp_diff * 5))
          }
        }
        
        # Calculate humidity score (0-50 points)
        humidity_score <- 50
        if (!is.null(city_weather$humidity) && humidity_pref) {
          if (city_weather$humidity > 70) {
            # Too humid - reduce score based on humidity level
            humidity_diff <- city_weather$humidity - 70
            humidity_score <- max(0, 50 - (humidity_diff))
          }
        }
        
        # Calculate total climate score
        climate_score <- temp_score + humidity_score
        
        # Add to dataframe
        climate_scores <- rbind(
          climate_scores,
          data.frame(
            City = city_name,
            Country = country_code,
            Region = region,
            Temperature = city_weather$temperature,
            Humidity = city_weather$humidity,
            ClimateScore = climate_score,
            stringsAsFactors = FALSE
          )
        )
      }
    }
    
    # Sort by climate score and get top 10
    top_climate_cities <- climate_scores %>%
      arrange(desc(ClimateScore)) %>%
      head(5)
    
    # If no cities found, return a message
    if (nrow(top_climate_cities) == 0) {
      return(
        p("No cities with climate data available. Try selecting different preferences or regions.")
      )
    }
    
    # Create cards for top cities
    city_cards <- lapply(1:nrow(top_climate_cities), function(i) {
      city <- top_climate_cities[i, ]
      
      # Card color based on score
      card_color <- if (city$ClimateScore >= 90) {
        "success"
      } else if (city$ClimateScore >= 75) {
        "info"
      } else {
        "warning"
      }
      
      # Temperature status
      temp_status <- if (city$Temperature >= temp_range[1] && city$Temperature <= temp_range[2]) {
        tags$span(class = "label label-success", "Ideal Temperature")
      } else if (abs(city$Temperature - mean(temp_range)) <= 5) {
        tags$span(class = "label label-warning", "Near Range")
      } else {
        tags$span(class = "label label-danger", "Outside Range")
      }
      
      # Humidity status
      humidity_status <- if (!humidity_pref) {
        tags$span(class = "label label-default", "No Preference")
      } else if (city$Humidity < 70) {
        tags$span(class = "label label-success", "Low Humidity")
      } else {
        tags$span(class = "label label-warning", "Higher Humidity")
      }
      
      div(class = "col-md-2",
          div(class = paste0("panel panel-", card_color),
              div(class = "panel-heading", 
                  h4(class = "panel-title", paste0("#", i))),
              div(class = "panel-body text-center",
                  h4(city$City),
                  p(city$Country),
                  div(class = "well well-sm",
                      p(strong(paste0(round(city$Temperature, 1), "°C")), br(), temp_status),
                      p(strong(paste0(round(city$Humidity), "%")), br(), humidity_status)
                  ),
                  div(class = "text-center",
                      p(strong("Climate Score:")),
                      div(class = "progress",
                          div(class = "progress-bar",
                              style = paste0("width: ", city$ClimateScore, "%;"),
                              paste0(round(city$ClimateScore), "%")
                          )
                      )
                  )
              )
          )
      )
    })
    
    # Return all cards in a row
    div(class = "row", city_cards)
  })
  
    # Groq AI Chat Feature - Secured for Production
  # Note: This feature is disabled in the public demo for security
  # To enable: Set GROQ_API_KEY environment variable
  
  observeEvent(input$ask_button, {
    req(input$user_input)
    
    # 🛡️ Check rate limit first (5 requests per 10 minutes)
    rate_check <- check_rate_limit("groq", max_requests = 5, window_minutes = 10)
    
    if (!rate_check$allowed) {
      message("Groq API rate limit exceeded for IP: ", get_client_ip())
      
      output$response_ui <- renderUI({
        div(
          style = "padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px;",
          h4("🛡️ Rate Limit Reached", style = "color: #856404;"),
          p("You've reached the AI chat limit to help keep the service available for everyone."),
          p("Please wait a few minutes before asking another question."),
          p(strong("Current question:"), input$user_input),
          p(strong("Demo response:"), "Based on your question, I'd recommend exploring the personalized city matches above. The top-ranked cities from your preferences would be excellent starting points for your research!")
        )
      })
      
      output$debug_output <- renderText({
        paste("Rate limited. Remaining requests: 0")
      })
      
      showNotification(
        "AI chat rate limit reached. Please wait before asking another question.",
        type = "warning",
        duration = 10
      )
      
      return()
    }
    
    message("Groq API call allowed. Remaining: ", rate_check$remaining, " for IP: ", get_client_ip())
    
    # Check for API key in environment variables
    groq_api_key <- Sys.getenv("GROQ_API_KEY")
    
    if (groq_api_key == "") {
      # Show demo message when no API key is available
      output$response_ui <- renderUI({
        div(
          style = "padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;",
          h4("AI Chat Feature - Demo Mode", style = "color: #495057;"),
          p("This feature demonstrates AI-powered city recommendations."),
          p("In production with proper API keys, this would provide:"),
          tags$ul(
            tags$li("Personalized city advice based on your preferences"),
            tags$li("Real-time answers about climate and living conditions"),
            tags$li("Interactive guidance for your relocation decisions")
          ),
          p(strong("Demo Response to your question:"), style = "margin-top: 15px;"),
          p(paste0("Based on your interest in '", input$user_input, 
                   "', I'd recommend exploring cities that excel in the categories ",
                   "you prioritized in the dashboard above. The top-ranked cities ",
                   "from your personalized matches would be excellent starting points ",
                   "for your research!"))
        )
      })
      
      output$debug_output <- renderText({
        paste("Demo mode - Remaining API requests:", rate_check$remaining)
      })
      
      return()
    }
    
    # Real API implementation (only when API key is provided)
    output$response_ui <- renderUI({
      div(style = "white-space: pre-wrap;", "Fetching response from AI...")
    })
    
    url <- "https://api.groq.com/openai/v1/chat/completions"
    
    headers <- httr::add_headers(
      "Authorization" = paste("Bearer", groq_api_key),
      "Content-Type" = "application/json"
    )
    
    body <- list(
      model = "mixtral-8x7b-32768",
      messages = list(
        list(role = "user", content = input$user_input)
      )
    )
    
    response <- tryCatch({
      httr::POST(url, headers, body = jsonlite::toJSON(body, auto_unbox = TRUE))
    }, error = function(e) {
      return(NULL)
    })
    
    # Debug output
    output$debug_output <- renderText({
      if (is.null(response)) {
        paste("Error: API request failed - Remaining requests:", rate_check$remaining)
      } else {
        paste("Response received from Groq API - Remaining requests:", rate_check$remaining)
      }
    })
    
    # Check if request succeeded
    if (!is.null(response) && httr::http_status(response)$category == "Success") {
      result <- httr::content(response, as = "parsed")
      
      # Render the AI response in multi-line format
      output$response_ui <- renderUI({
        div(
          style = "white-space: pre-wrap;", 
          result$choices[[1]]$message$content
        )
      })
      
    } else {
      # Show error
      output$response_ui <- renderUI({
        div(style = "white-space: pre-wrap;", "Error: AI service temporarily unavailable. Please try again later.")
      })
    }
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)