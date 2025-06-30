# 🌍 Global City Analytics Dashboard
*An Interactive Data Science Platform for Location Intelligence*

[![Live Demo](https://img.shields.io/badge/🌐_Live_Demo-Visit_App-blue?style=for-the-badge)]([your-app-url-here](https://bianchi-mattia.shinyapps.io/cities-recommendations-dash/))
[![GitHub](https://img.shields.io/badge/📊_Source_Code-GitHub-black?style=for-the-badge)]([your-github-url-here](https://github.com/erMisetto))

## 🎯 **Project Overview**

A comprehensive **data analytics platform** that processes and visualizes global city data to provide data-driven recommendations for optimal living locations. This project demonstrates advanced skills in **data science, interactive visualization, API integration, and production deployment** - key competencies for modern analyst roles.

### **Business Impact**
- **Data-Driven Decision Making**: Transforms complex multi-dimensional city data into actionable insights
- **User-Centric Analytics**: Implements preference-based recommendation algorithms
- **Scalable Architecture**: Production-ready deployment with rate limiting and security features
- **Real-Time Integration**: Live weather data and AI-powered insights

---

## 🔧 **Technical Skills Demonstrated**

### **Data Analysis & Statistics**
- ✅ **Multi-dimensional data analysis** across 4+ key metrics (Economics, Environment, Quality of Life, Governance)
- ✅ **Statistical modeling** with weighted scoring algorithms and preference matching
- ✅ **Data quality assessment** and completeness scoring
- ✅ **Comparative analysis** with percentile rankings and outlier detection

### **Data Visualization & Reporting**
- ✅ **Interactive dashboards** with multiple analytical views
- ✅ **Geospatial visualization** using Leaflet maps with custom markers and clustering
- ✅ **Advanced charting** with Plotly (radar charts, bar plots, scatter plots, histograms)
- ✅ **Responsive design** optimized for desktop and mobile viewing

### **Programming & Development**
- ✅ **R Shiny framework** for full-stack web application development
- ✅ **RESTful API integration** (OpenWeatherMap, Groq AI)
- ✅ **Data manipulation** with dplyr, tidyr for ETL processes
- ✅ **Production deployment** with security best practices

### **Database & Data Management**
- ✅ **Multi-source data integration** (NUMBEO, ARCADIS datasets)
- ✅ **Data filtering and aggregation** with complex business rules
- ✅ **Caching strategies** for performance optimization
- ✅ **Data validation** and error handling

---

## 📊 **Key Features & Analytics**

### **1. Interactive World Map Analytics**
- **Custom weighting algorithms** for personalized city rankings
- **Real-time recalculation** of scores based on user preferences
- **Geospatial clustering** and interactive filtering
- **Color-coded performance indicators**

### **2. Comparative Analysis Tools**
- **Side-by-side city comparisons** across multiple dimensions
- **Radar chart visualizations** for multi-variate analysis
- **Percentile rankings** and benchmarking
- **Statistical significance testing**

### **3. Recommendation Engine**
- **Machine learning-style preference matching** using Euclidean distance algorithms
- **Climate compatibility scoring** with real-time weather integration
- **Multi-criteria decision analysis** with weighted preferences
- **Top-N recommendations** with confidence scoring

### **4. Advanced Data Filtering**
- **Dynamic filtering** by region, data quality, and source
- **Range-based selections** for continuous variables
- **Boolean logic** for complex filter combinations
- **Real-time data updates** with reactive programming

### **5. AI-Powered Insights** *(Production Mode)*
- **Natural language processing** for user queries
- **Conversational analytics** with context awareness
- **Rate-limited API integration** for cost control

---

## 🏗️ **Architecture & Production Features**

### **Scalability & Performance**
- **Modular design** with separated UI and server logic
- **Efficient data processing** with vectorized operations
- **Caching mechanisms** for improved response times
- **Asynchronous processing** for API calls

### **Security & Deployment**
- **Environment variable management** for API keys
- **Rate limiting implementation** (20 requests/15min for weather, 5 requests/10min for AI)
- **Input validation** and sanitization
- **Comprehensive error handling**
- **Git security** with proper .gitignore patterns

### **Monitoring & Analytics**
- **User session tracking** with IP-based rate limiting
- **API usage monitoring** and reporting
- **Performance metrics** collection
- **Automated cleanup** of temporary data

---

## 📈 **Business Value & Use Cases**

### **For HR & Talent Management**
- **Relocation analysis** for employee transfers
- **Cost of living comparisons** for salary benchmarking
- **Quality of life assessments** for retention strategies

### **For Real Estate & Investment**
- **Market analysis** across global cities
- **Risk assessment** using governance and economic indicators
- **Investment opportunity identification**

### **For Urban Planning & Research**
- **Sustainability benchmarking** across cities
- **Policy impact analysis** using governance scores
- **Climate adaptation planning**

---

## 🚀 **Getting Started**

### **Live Demo**
Experience the full application: **[View Live Dashboard]([your-app-url-here](https://bianchi-mattia.shinyapps.io/cities-recommendations-dash/))**

### **Local Development**
```r
# Install dependencies
packages <- c("shiny", "shinydashboard", "leaflet", "plotly", 
              "dplyr", "DT", "httr", "jsonlite")
install.packages(packages)

# Run application
shiny::runApp("app.R")
```

### **Data Sources**
- **NUMBEO**: Cost of living, crime, and quality metrics
- **ARCADIS**: Sustainable cities index
- **OpenWeatherMap**: Real-time climate data
- **Custom calculations**: Weighted scoring algorithms

---

## 🏆 **Professional Highlights**

### **What This Project Demonstrates**
- **End-to-end data science workflow** from raw data to deployed insights
- **Full-stack development capabilities** with modern web technologies
- **Production-ready coding practices** with security and scalability
- **User experience design** for complex analytical interfaces
- **API integration expertise** with external data sources
- **Statistical analysis skills** with custom algorithms
- **Project management** with comprehensive documentation

### **Relevant for Analyst Roles**
- ✅ **Business Intelligence**: Transforms data into actionable recommendations
- ✅ **Data Visualization**: Creates compelling, interactive reports
- ✅ **Statistical Analysis**: Implements complex analytical models
- ✅ **Tool Development**: Builds reusable analytical frameworks
- ✅ **Stakeholder Communication**: Designs user-friendly interfaces for non-technical users

---

## 📝 **Technical Documentation**

### **File Structure**
```
├── app.R                          # Main Shiny application
├── rankings_combined_FINAL.xlsx   # Processed city dataset
├── rate_limiting_utils.R          # Production security features
├── DEPLOYMENT_CHECKLIST.md        # Production deployment guide
└── README.md                      # Project documentation
```

### **Key Algorithms**
- **Weighted Scoring**: Multi-criteria decision analysis with user-defined weights
- **Preference Matching**: Euclidean distance calculation for similarity scoring
- **Climate Compatibility**: Temperature and humidity matching algorithms
- **Rate Limiting**: IP-based request throttling for API protection

---

## 🌟 **Contact & Portfolio**

**Looking for Data Analyst Opportunities**

This project showcases practical experience in:
- **Business Intelligence & Reporting**
- **Interactive Dashboard Development** 
- **Statistical Analysis & Modeling**
- **API Integration & Data Pipeline Development**
- **Production Deployment & Security**

**[Mattia Bianchi]**
- 📧 Email: [mattiabianchi.work@gmail.com]
- 💼 LinkedIn: [[LinkedIn Profile](https://www.linkedin.com/in/bianchi-mattia/)]
- 🐙 GitHub: [[GitHub Profile](https://github.com/erMisetto)]

---

*Built with R Shiny, demonstrating enterprise-level data science and analytics capabilities*
