# FINAL TERRA-FREE DEPLOYMENT SCRIPT
# Run this after making the readxl -> openxlsx changes
# ====================================================

setwd("C:/Users/bmat2/Project_R")

cat("🎯 FINAL TERRA-FREE DEPLOYMENT\n")
cat("="*40, "\n\n")

# Step 1: Install openxlsx if needed
if (!require(openxlsx, quietly = TRUE)) {
  cat("📦 Installing openxlsx...\n")
  install.packages("openxlsx")
}

# Step 2: Test the app briefly
cat("🧪 Quick app test...\n")
tryCatch({
  source("app.R", local = TRUE)
  cat("✅ App syntax is valid\n")
}, error = function(e) {
  cat("❌ App error:", e$message, "\n")
  stop("Fix app errors first")
})

# Step 3: Deploy with new name
cat("🚀 Deploying terra-free app...\n")
rsconnect::deployApp(
  appDir = ".",
  appName = "city-recommendation-terra-free",
  appTitle = "Where Should You Live? - Terra-Free Version",
  appFiles = c("app.R", "rankings_combined_FINAL.xlsx"),
  forceUpdate = TRUE,
  launch.browser = TRUE
)

cat("🎉 Deployment complete!\n")
cat("✅ Check that your app works without terra errors\n")
