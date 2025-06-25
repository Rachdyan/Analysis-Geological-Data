# 🔬 Rock Sample Analysis Dashboard

An interactive web application built with R and the Shiny framework to visualize geochemical and mineralogical data from rock sample analysis.

### 🚀 [View Live Demo](https://rachdyan.shinyapps.io/analysis_geological_data/)

---

## 📋 Table of Contents

- [About The Project](#about-the-project)
- [Key Features](#key-features)
- [Dashboard Sections](#dashboard-sections)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
  - [Usage](#usage)
- [File Structure](#file-structure)
- [License](#license)

---

## 🎯 About The Project

The Rock Sample Analysis Dashboard is an interactive web application built with R and the Shiny framework. It is designed to provide a comprehensive suite of tools for the visualization and exploration of data from geological or materials science analyses. 

The application is organized into multiple tabs, each dedicated to a specific type of analysis, such as particle screen sizing, elemental grade concentrations, mineral recovery rates, and results from X-Ray Fluorescence (XRF) and X-Ray Diffraction (XRD). Using interactive Plotly charts, users can dynamically filter and view the data to gain insights into the composition and properties of the rock samples.

---

## ✨ Key Features

- **📊 Interactive Visualizations:** All plots are built with Plotly, allowing users to hover over data points for detailed information, zoom, and pan
- **📑 Multi-Tab Interface:** The app is organized into seven distinct tabs, each focusing on a specific analysis to provide a clear and comprehensive overview
- **🔍 Dynamic Filtering:** Each analysis tab includes its own set of controls, allowing users to dynamically filter the data and update the visualizations in real-time
- **🧩 Modular Codebase:** The application uses Shiny Modules, which organizes the code logically and makes it easier to maintain and scale
- **🎨 Modern UI:** Built with the `bslib` package for a clean, modern, and responsive user interface

---

## 📊 Dashboard Sections

The dashboard is divided into the following analysis tabs:

### 1. **Screen Sizing**
Visualizes the particle size distribution of the rock samples.

### 2. **Grade**
Displays elemental grade concentrations using interactive radar charts for different sample fractions.

### 3. **Recovery**
Shows the recovery percentage of various minerals or elements across different sample sizes.

### 4. **XRF (X-Ray Fluorescence)**
Presents elemental composition data obtained from XRF analysis.

### 5. **XRD Summary (X-Ray Diffraction)**
Provides a summary of the mineralogical composition determined by XRD.

### 6. **Stacked QXRD**
Offers a stacked bar chart view of Quantitative XRD results, showing the relative abundance of different minerals.

### 7. **Mass Recovery**
Illustrates the mass recovery for different experimental runs or sample fractions.

---

## 🚀 Getting Started

To run this application locally, follow these steps.

### Prerequisites

You will need R and RStudio (recommended) installed.

**Required R version:** R >= 4.0.0

### Installation

1. **Clone the repository:**
   ```bash
   git clone https://github.com/Rachdyan/rock_sample_dashboard.git
   cd rock-sample-dashboard
   ```

2. **Install required R packages:**
   ```r
   # Install all required packages for the app
   install.packages(c(
     "shiny", 
     "dplyr", 
     "plotly", 
     "scales", 
     "glue", 
     "shinyWidgets", 
     "strex", 
     "tidyr", 
     "purrr", 
     "bslib"
   ))
   ```

3. **Set up the project directory:**
   Ensure your project folder contains the necessary `data/` and `R/` sub-directories as required by the application.

### Usage

1. **Run the application:**
   - Open the `app.R` file in RStudio
   - Click the "Run App" button that appears at the top of the editor
   
   OR
   
   - Run from R console:
   ```r
   shiny::runApp()
   ```

2. **Access the dashboard:**
   The application will open in your default web browser, typically at `http://127.0.0.1:port`

---

## 📁 File Structure

The application expects the following file and directory structure to function correctly:

```
rock-analysis-dashboard/
├── app.R                       # Main application file (UI and Server)
├── R/
│   └── modules.R               # Contains all Shiny modules
├── data/
│   ├── screen-sizing-report.csv
│   ├── assay-by-size.csv
│   └── ... (other data files)
├── www/                        # Static web assets (optional)
├── README.md                   # This file
└── LICENSE                     # License file
```

---

## 📄 License

Distributed under the MIT License
