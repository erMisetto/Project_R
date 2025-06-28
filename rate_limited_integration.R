# Rate-Limited API Integration for Your Shiny App
# Add these modifications to your app.R server function

# Add at the beginning of your server function:
source("rate_limiting_utils.R")  # Include the rate limiting functions

# Get client IP once per session
client_ip <- reactive({
  get_client_ip(session)
})

# Periodic cleanup of old rate limit entries
observe({
  invalidateLater(300000)  # Every 5 minutes
  cleanup_rate_limits()
})

# RATE-LIMITED WEATHER API FUNCTION
fetch_weather_data_with_rate_limit <- function(city_name, country_code, client_ip) {
  # Check rate limit: 20 requests per 15 minutes per IP
  rate_check <- check_rate_limit(client_ip, "weather", max_requests = 20, window_minutes = 15)
  
  if (!rate_check$allowed) {
    reset_time <- format(rate_check$reset_time, "%H:%M:%S")
    message("Weather API rate limit exceeded for IP: ", client_ip, ". Reset at: ", reset_time)
    
    # Return rate limit info instead of weather data
    return(list(
      rate_limited = TRUE,
      reset_time = reset_time,
      message = paste("Weather API rate limit reached. Try again at", reset_time)
    ))
  }
  
  message("Weather API call allowed. Remaining: ", rate_check$remaining, " for IP: ", client_ip)
  
  # Proceed with normal weather fetch
  # Use your existing fetch_weather_data function but add rate limit info
  result <- fetch_weather_data(city_name, country_code)
  
  if (!is.null(result)) {
    result$rate_limited <- FALSE
    result$remaining_requests <- rate_check$remaining
  }
  
  return(result)
}

# RATE-LIMITED GROQ AI FUNCTION  
call_groq_api_with_rate_limit <- function(user_input, groq_api_key, client_ip) {
  # Check rate limit: 5 requests per 10 minutes per IP (stricter for AI)
  rate_check <- check_rate_limit(client_ip, "groq", max_requests = 5, window_minutes = 10)
  
  if (!rate_check$allowed) {
    reset_time <- format(rate_check$reset_time, "%H:%M:%S")
    message("Groq API rate limit exceeded for IP: ", client_ip, ". Reset at: ", reset_time)
    
    return(list(
      rate_limited = TRUE,
      reset_time = reset_time,
      message = paste("AI chat rate limit reached. You can ask again at", reset_time),
      content = paste("I'm currently rate-limited to prevent overuse.", 
                     "This helps keep the service available for everyone.", 
                     "Please try again at", reset_time)
    ))
  }
  
  message("Groq API call allowed. Remaining: ", rate_check$remaining, " for IP: ", client_ip)
  
  # Proceed with normal API call
  url <- "https://api.groq.com/openai/v1/chat/completions"
  
  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", groq_api_key),
    "Content-Type" = "application/json"
  )
  
  body <- list(
    model = "mixtral-8x7b-32768",
    messages = list(
      list(role = "user", content = user_input)
    )
  )
  
  response <- tryCatch({
    httr::POST(url, headers, body = jsonlite::toJSON(body, auto_unbox = TRUE))
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(response) && httr::http_status(response)$category == "Success") {
    result <- httr::content(response, as = "parsed")
    
    return(list(
      rate_limited = FALSE,
      remaining_requests = rate_check$remaining,
      content = result$choices[[1]]$message$content,
      success = TRUE
    ))
  } else {
    return(list(
      rate_limited = FALSE,
      remaining_requests = rate_check$remaining,
      content = "AI service temporarily unavailable. Please try again later.",
      success = FALSE
    ))
  }
}

# EXAMPLE: How to use in your existing observeEvent for weather
observeEvent(input$find_matches, {
  matches <- matched_cities()
  if (nrow(matches) > 0) {
    weather_data <- list()
    user_ip <- client_ip()
    
    for (i in 1:min(nrow(matches), 5)) {
      city_name <- matches$City[i]
      country_code <- matches$Country[i]
      city_key <- paste0(city_name, "_", country_code)
      
      # Use rate-limited weather function
      result <- fetch_weather_data_with_rate_limit(city_name, country_code, user_ip)
      
      if (!is.null(result) && !isTRUE(result$rate_limited)) {
        weather_data[[city_key]] <- result
      } else if (isTRUE(result$rate_limited)) {
        # Show rate limit message to user
        showNotification(
          result$message,
          type = "warning",
          duration = 10
        )
        break  # Stop fetching more weather data
      }
    }
    
    weather_data_cache(weather_data)
  }
})

# EXAMPLE: How to use in your Groq AI observeEvent
observeEvent(input$ask_button, {
  req(input$user_input)
  user_ip <- client_ip()
  
  groq_api_key <- Sys.getenv("GROQ_API_KEY")
  
  if (groq_api_key == "") {
    # Your existing demo mode code here
    # ... 
    return()
  }
  
  # Show loading message
  output$response_ui <- renderUI({
    div(style = "white-space: pre-wrap;", "Fetching response from AI...")
  })
  
  # Make rate-limited API call
  result <- call_groq_api_with_rate_limit(input$user_input, groq_api_key, user_ip)
  
  if (result$rate_limited) {
    # Show rate limit message
    output$response_ui <- renderUI({
      div(
        style = "padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px;",
        h4("Rate Limit Reached", style = "color: #856404;"),
        p(result$content),
        p(paste("You can make another request at:", result$reset_time))
      )
    })
    
    output$debug_output <- renderText({
      paste("Rate limited. Reset at:", result$reset_time, "- Remaining: 0")
    })
  } else {
    # Show normal response
    output$response_ui <- renderUI({
      div(style = "white-space: pre-wrap;", result$content)
    })
    
    output$debug_output <- renderText({
      paste("Request successful. Remaining requests:", result$remaining_requests)
    })
  }
})
