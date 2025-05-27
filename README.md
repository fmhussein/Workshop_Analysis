# Workshop Analysis

[Workshop Registrant Analysis App](https://fmh7pv.shinyapps.io/registrant_analysis/)

# 📊 Workshop Analysis

An interactive R project for analyzing university workshop registrations, attendance trends, and thematic categories. This project integrates registration records, workshop metadata, and topic mappings to produce insights for planning and engagement tracking — enhanced through a Shiny dashboard.

---

## 🎯 Goal

To explore and visualize trends in workshop participation, identify topic popularity, and enhance decision-making for future events using structured analysis and interactivity.

---

## 📦 Dataset Summary

- **WorkshopRegistrations.csv** – Registration-level records with attendee info  
- **WorkshopDetails.csv** – Workshop titles, dates, presenters, and descriptions  
- **workshop_categories.csv** – Manually created mappings of `title` to `Category`  
- **Derived Merged Data** – Combined and enriched data with date parsing and category imputation  

### Key Features
- `title`, `presenter`, `registered_date`, `attendance`, `category`, `start`, `seats_taken`, etc.

---

## 🔍 Key Steps

- **Data Cleaning & Preparation**
  - Standardize titles and categories using `stringr` and `dplyr`
  - Parse and format dates with `lubridate`
  - Handle nested columns (e.g., JSON-style category lists) with `tidyr`

- **Merging & Enrichment**
  - Combine workshop details and participant data on multiple keys  
  - Impute or override missing `Category` fields using curated mapping  

- **Interactive Visualization**
  - Shiny dashboard for real-time exploration
  - Interactive attendance plots, trend lines, and filters by date/category

---

## 🧰 Frameworks / Libraries I Used

### 🧮 Data Manipulation & Cleaning

- `dplyr` – Fast and readable data transformation (`filter`, `mutate`, `group_by`)  
- `tidyr` – Reshaping and flattening messy data structures (`unnest`, `separate`)  
- `tidyverse` – A collection of core tidy tools including `ggplot2`, `readr`, `stringr`, `dplyr`  
- `stringr` – Regular expressions and string normalization  
- `lubridate` – Date/time parsing, feature extraction (e.g. `month`, `floor_date`)  
- `readr` – Efficient reading of `.csv` and text files  
- `readxl` – Read data from Excel files (`.xlsx`, `.xls`)

### 📊 Data Visualization

- `ggplot2` – Grammar of graphics for elegant, layered plotting  
- `RColorBrewer` – Color palettes for categorical and sequential visualizations  
- `plotly` – Interactive charts compatible with `ggplot2` and standalone

### 🖥️ Interactive Web App

- `shiny` – Builds the web application interface  
- `plotly` – Powers dynamic, zoomable plots inside the dashboard

---

## 📈 Key Insights

- 📌 **Topic Popularity**: Most attended topics include *Python*, *HPC*, and *Bioinformatics*  
- 📈 **Seasonal Trends**: Clear peaks in registration afternoons and spring academic semesters   

---



