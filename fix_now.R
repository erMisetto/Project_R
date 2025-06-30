# IMMEDIATE FIX - Run this now!
# ===============================

setwd("C:/Users/bmat2/Project_R")

# Create clean deployment folder
if (dir.exists("deploy_clean")) unlink("deploy_clean", recursive = TRUE)
dir.create("deploy_clean")

# Copy only essential files (no renv)
file.copy("app.R", "deploy_clean/app.R")
file.copy("rankings_combined_FINAL.xlsx", "deploy_clean/rankings_combined_FINAL.xlsx")

# Create .rscignore to exclude renv and spatial packages
writeLines(c(
  "renv/",
  "renv.lock",
  ".Rprofile", 
  "terra/",
  "raster/",
  "sf/",
  "sp/"
), "deploy_clean/.rscignore")

# Deploy from clean folder (bypasses renv)
rsconnect::deployApp(
  appDir = "deploy_clean",
  appName = "city-dashboard-fixed",
  appTitle = "Where Should You Live? - Fixed Dashboard",
  forceUpdate = TRUE,
  launch.browser = TRUE
)

cat("🎉 DEPLOYMENT SUCCESSFUL!\n")
cat("Your app should now work without terra/raster issues\n")
