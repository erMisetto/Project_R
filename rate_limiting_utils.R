# Rate Limiting Implementation for Shiny Apps
# Add this to your server function

# Initialize rate limiting storage
rate_limit_storage <- reactiveVal(list())

# Function to get client IP address
get_client_ip <- function(session) {
  # Try different headers in order of preference
  ip <- session$request$HTTP_X_FORWARDED_FOR %||% 
        session$request$HTTP_X_REAL_IP %||%
        session$request$HTTP_CF_CONNECTING_IP %||%  # Cloudflare
        session$request$REMOTE_ADDR %||%
        "unknown"
  
  # Handle comma-separated IPs (proxy chains)
  if (grepl(",", ip)) {
    ip <- trimws(strsplit(ip, ",")[[1]][1])
  }
  
  return(ip)
}

# Rate limiting function with time windows
check_rate_limit <- function(ip, api_name, max_requests = 10, window_minutes = 15) {
  current_time <- Sys.time()
  window_start <- current_time - (window_minutes * 60)
  
  # Get current storage
  storage <- rate_limit_storage()
  
  # Create key for this IP and API
  key <- paste0(ip, "_", api_name)
  
  # Initialize if not exists
  if (!key %in% names(storage)) {
    storage[[key]] <- list()
  }
  
  # Remove old requests outside the time window
  storage[[key]] <- storage[[key]][storage[[key]] > window_start]
  
  # Check if limit exceeded
  if (length(storage[[key]]) >= max_requests) {
    return(list(
      allowed = FALSE, 
      remaining = 0,
      reset_time = min(storage[[key]]) + (window_minutes * 60)
    ))
  }
  
  # Add current request
  storage[[key]] <- c(storage[[key]], current_time)
  
  # Update storage
  rate_limit_storage(storage)
  
  return(list(
    allowed = TRUE, 
    remaining = max_requests - length(storage[[key]]),
    reset_time = window_start + (window_minutes * 60)
  ))
}

# Clean up old entries periodically (call this in a reactive timer)
cleanup_rate_limits <- function() {
  current_time <- Sys.time()
  storage <- rate_limit_storage()
  
  # Remove entries older than 1 hour
  cutoff_time <- current_time - (60 * 60)
  
  for (key in names(storage)) {
    storage[[key]] <- storage[[key]][storage[[key]] > cutoff_time]
    if (length(storage[[key]]) == 0) {
      storage[[key]] <- NULL
    }
  }
  
  rate_limit_storage(storage)
}
