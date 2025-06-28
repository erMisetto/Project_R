# Where Should You Live? - Shiny Dashboard

A comprehensive R Shiny application that helps users discover their ideal city to live in based on personal preferences across economics, environment, quality of life, and governance metrics.

## 🌟 Features

- **Interactive World Map**: Visualize cities with customizable ranking criteria
- **City Explorer**: Browse and filter cities based on various metrics  
- **City Comparison**: Compare two cities side-by-side across all dimensions
- **Personalized Recommendations**: Get city matches based on your preferences
- **Climate Analysis**: Weather-based city recommendations (demo mode)
- **AI-Powered Insights**: Interactive chat for city advice (demo mode)

## 🚀 Live Demo

- **Shiny Apps**: [Your App URL Here]
- **GitHub Repository**: [Your GitHub URL Here]

## 📊 Data Sources

- **NUMBEO**: Cost of living, crime, and quality of life data
- **ARCADIS**: Sustainable cities index
- **OpenWeatherMap**: Weather data (when API key is provided)
- **Groq AI**: AI-powered city insights (when API key is provided)

## 🔧 Local Development Setup

### Prerequisites

```r
# Required R packages
install.packages(c(
  \"shiny\", \"shinydashboard\", \"leaflet\", \"leaflet.extras\",
  \"DT\", \"plotly\", \"dplyr\", \"tidyr\", \"ggplot2\", \"scales\",
  \"stringdist\", \"shinyWidgets\", \"RColorBrewer\", \"readxl\",
  \"httr\", \"jsonlite\", \"promises\", \"future\", \"rsconnect\", \"terra\"
))
```

### Environment Setup

1. **Clone the repository**:
   ```bash
   git clone [your-repo-url]
   cd Project_R
   ```

2. **Set up environment variables** (optional for enhanced features):
   ```bash
   # Linux/Mac
   echo 'OPENWEATHER_API_KEY=your_weather_key_here' >> ~/.Renviron
   echo 'GROQ_API_KEY=your_groq_key_here' >> ~/.Renviron
   
   # Windows
   # Add to your .Renviron file in your home directory
   # OPENWEATHER_API_KEY=your_weather_key_here
   # GROQ_API_KEY=your_groq_key_here
   ```

3. **Run the application**:
   ```r
   # In R/RStudio
   shiny::runApp(\"app.R\")
   ```

## 🌐 Deployment Guide

### GitHub Deployment

1. **Prepare your repository**:
   ```bash
   # Ensure .gitignore is in place (already included)
   git add .
   git commit -m \"Secure deployment-ready version\"
   git push origin main
   ```

2. **Environment variables are excluded** - the app runs in demo mode by default

### Shinyapps.io Deployment

1. **Install rsconnect**:
   ```r
   install.packages(\"rsconnect\")
   ```

2. **Configure your account**:
   ```r
   library(rsconnect)
   rsconnect::setAccountInfo(
     name = \"your-account-name\",
     token = \"your-token\",
     secret = \"your-secret\"
   )
   ```

3. **Deploy the application**:
   ```r
   # Deploy with demo data (secure - no API keys exposed)
   rsconnect::deployApp(
     appDir = \".\",
     appName = \"where-should-you-live\",
     appTitle = \"Where Should You Live? Dashboard\"
   )
   ```

4. **Add environment variables** (optional for enhanced features):
   - Go to your app dashboard on shinyapps.io
   - Navigate to **Settings → Environment Variables**
   - Add: `OPENWEATHER_API_KEY = your_weather_key`
   - Add: `GROQ_API_KEY = your_groq_key`
   - The app will automatically enable enhanced features when keys are available

## 🔒 Security & Rate Limiting Features

✅ **No hardcoded API keys** - uses environment variables only  
✅ **Demo mode by default** - works without any API keys  
✅ **Secure error handling** - no sensitive info in error messages  
✅ **Comprehensive .gitignore** - prevents secrets in version control  
✅ **Input validation** - protects against malicious inputs  
✅ **IP-based rate limiting** - prevents API abuse and controls costs:
   - Weather API: 20 requests per 15 minutes per IP
   - AI Chat API: 5 requests per 10 minutes per IP
✅ **Graceful degradation** - fallback to demo data when limits reached
✅ **User notifications** - clear feedback when rate limits are hit  

## 💺 File Structure

```
Project_R/
├── app.R                           # Main Shiny application (with rate limiting)
├── rankings_combined_FINAL.xlsx    # City rankings dataset
├── .gitignore                      # Security exclusions
├── README.md                       # This documentation
├── DEPLOYMENT_CHECKLIST.md         # Step-by-step deployment guide
├── RATE_LIMITING_GUIDE.md          # Comprehensive rate limiting documentation
├── rate_limiting_utils.R           # Rate limiting utility functions
├── rate_limited_integration.R      # Integration examples
└── FINAL_IMPLEMENTATION_SUMMARY.md # Complete feature overview
```  

## 📁 File Structure

```
Project_R/
├── app.R                           # Main Shiny application
├── rankings_combined_FINAL.xlsx    # City rankings dataset
├── .gitignore                      # Security exclusions
└── README.md                       # This file
```

## 🌡️ Enhanced Features Configuration

The application supports two modes for enhanced features:

### Demo Mode (Default - Secure)
- Uses simulated weather data
- Shows demo AI responses
- No API keys required
- Perfect for public deployment
- Showcases all functionality

### Production Mode (Optional)
- Requires OpenWeatherMap API key for real weather data
- Requires Groq API key for AI-powered insights
- Set environment variables: `OPENWEATHER_API_KEY` and `GROQ_API_KEY`
- Real-time data and interactive AI features

## 📝 Usage for CV/Portfolio

This project demonstrates:

- **Full-stack R Shiny development**
- **Interactive data visualization** with Leaflet, Plotly, DT
- **Secure deployment practices** with API key management
- **Rate limiting implementation** for cost control and abuse prevention
- **Multi-API integration** with proper error handling
- **AI/ML integration** with language models
- **User experience design** with responsive dashboards
- **Data science workflows** with real-world datasets

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature-name`
3. Commit your changes: `git commit -am 'Add feature'`
4. Push to the branch: `git push origin feature-name`
5. Submit a pull request

## 📜 License

This project is available under the MIT License. See LICENSE file for details.

## 🙋‍♂️ Contact

**Your Name**  
- Portfolio: [Your Portfolio URL]
- LinkedIn: [Your LinkedIn]
- Email: [Your Email]

---

*Built with ❤️ using R Shiny, Leaflet, and Plotly*
