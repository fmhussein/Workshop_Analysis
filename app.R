library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# ---- Load or create your data ----
# You need to load `df_updated` and `transformed_data` before running the app
# Example:
df_updated <- read.csv("df_updated.csv")
transformed_data <- read.csv("transformed_data.csv")

ui <- fluidPage(
  titlePanel("Workshop Dashboards"),
  
  tabsetPanel(
    tabPanel("Registrants by Time & Category",
             sidebarLayout(
               sidebarPanel(
                 selectInput("year1", "Year:", choices = NULL, selected = NULL, multiple = TRUE),
                 selectInput("semester1", "Semester:", 
                             choices = c("Spring", "Summer", "Fall"), 
                             selected = NULL, multiple = TRUE),
                 selectInput("category1", "Category:", choices = NULL, selected = NULL, multiple = TRUE),
                 selectInput("title1", "Workshop Title:", choices = NULL, selected = NULL, multiple = TRUE),
                 selectInput("school1", "School:", choices = NULL, selected = NULL, multiple = TRUE),
                 selectInput("status1", "Status:", choices = NULL, selected = NULL, multiple = TRUE)
               ),
               mainPanel(
                 plotlyOutput("workshopPlot1")
               )
             )
    ),
    
    tabPanel("Demographics by School",
             sidebarLayout(
               sidebarPanel(
                 selectInput("year2", "Select Year", choices = NULL, multiple = TRUE),
                 selectInput("semester2", "Select Semester", 
                             choices = c("Spring", "Summer", "Fall"), multiple = TRUE),
                 uiOutput("categoryUI2"),
                 uiOutput("titleUI2"),
                 selectInput("school2", "Select School or Organization", choices = NULL, multiple = TRUE)
               ),
               mainPanel(
                 plotlyOutput("barPlot2")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  # ---- App 1: Workshop Registrants ----
  observe({
    updateSelectInput(session, "year1", choices = unique(df_updated$year_value))
    updateSelectInput(session, "category1", choices = unique(df_updated$Category))
    updateSelectInput(session, "title1", choices = unique(df_updated$title))
    
    school_choices <- unique(df_updated$School[!is.na(df_updated$School) & df_updated$School != "NA" & df_updated$School != "OTHER"])
    updateSelectInput(session, "school1", choices = school_choices)
    
    status_choices <- c("Faculty", "Staff", "Undergraduate Student", "Graduate Student")
    updateSelectInput(session, "status1", choices = status_choices)
  })
  
  filtered_data1 <- reactive({
    df <- df_updated %>%
      filter(!is.na(School) & School != "NA" & School != "OTHER") %>%
      mutate(Status = case_when(
        Status %in% c("Student Worker", "Graduate Student") ~ "Graduate Student",
        Status %in% c("Faculty", "Staff", "Undergraduate Student") ~ Status,
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Status)) %>%
      mutate(
        year_value = year(start),
        month_value = month(start),
        semester = case_when(
          month_value %in% 1:5 ~ "Spring",
          month_value %in% 6:7 ~ "Summer",
          month_value %in% 8:12 ~ "Fall"
        )
      )
    
    if (length(input$status1) > 0) df <- df %>% filter(Status %in% input$status1)
    if (length(input$year1) > 0) df <- df %>% filter(year_value %in% input$year1)
    if (length(input$category1) > 0) df <- df %>% filter(Category %in% input$category1)
    if (length(input$title1) > 0) df <- df %>% filter(title %in% input$title1)
    if (length(input$school1) > 0) df <- df %>% filter(School %in% input$school1)
    if (length(input$semester1) > 0) df <- df %>% filter(semester %in% input$semester1)
    
    df
  })
  
  output$workshopPlot1 <- renderPlotly({
    df <- filtered_data1()
    df <- df %>% mutate(time_of_day = format(as.POSIXct(start), "%H:%M"))
    
    grouped <- df %>%
      group_by(time_of_day, Category) %>%
      summarise(num_registrants = n_distinct(booking_id), .groups = "drop")
    
    num_categories <- n_distinct(grouped$Category)
    color_palette <- brewer.pal(min(num_categories, 12), "Set2")
    
    plot_ly(data = grouped, 
            x = ~time_of_day, 
            y = ~num_registrants, 
            color = ~Category, 
            colors = color_palette,
            type = 'bar',
            text = ~paste("Category:", Category, "<br>Time:", time_of_day, "<br>Registrants:", num_registrants),
            hoverinfo = 'text') %>%
      layout(title = "Number of Registrants by Time of Day and Category",
             xaxis = list(title = "Time of Day"),
             yaxis = list(title = "Number of Registrants"),
             barmode = 'stack')
  })
  
  # ---- App 2: Demographics ----
  observe({
    updateSelectInput(session, "year2", choices = sort(unique(transformed_data$year_value)))
    updateSelectInput(session, "school2", choices = sort(unique(transformed_data$School[transformed_data$School != "OTHER"])))
  })
  
  output$categoryUI2 <- renderUI({
    filtered_data <- transformed_data
    
    if (length(input$year2) > 0) {
      filtered_data <- filtered_data %>% filter(year_value %in% input$year2)
    }
    if (length(input$semester2) > 0) {
      filtered_data <- filtered_data %>%
        filter(
          (input$semester2 %in% "Spring" & month(as.POSIXct(start)) %in% 1:5) |
            (input$semester2 %in% "Summer" & month(as.POSIXct(start)) %in% 6:7) |
            (input$semester2 %in% "Fall" & month(as.POSIXct(start)) %in% 8:12)
        )
    }
    
    categories <- unique(filtered_data$Category)
    selectInput("category2", "Select Category", choices = sort(categories), multiple = TRUE)
  })
  
  output$titleUI2 <- renderUI({
    filtered_data <- transformed_data
    if (length(input$category2) > 0) {
      filtered_data <- filtered_data %>% filter(Category %in% input$category2)
    }
    if (length(input$year2) > 0) {
      filtered_data <- filtered_data %>% filter(year_value %in% input$year2)
    }
    if (length(input$semester2) > 0) {
      filtered_data <- filtered_data %>%
        filter(
          (input$semester2 %in% "Spring" & month(as.POSIXct(start)) %in% 1:5) |
            (input$semester2 %in% "Summer" & month(as.POSIXct(start)) %in% 6:7) |
            (input$semester2 %in% "Fall" & month(as.POSIXct(start)) %in% 8:12)
        )
    }
    titles <- unique(filtered_data$title)
    selectInput("title2", "Select Workshop Title", choices = sort(titles), multiple = TRUE)
  })
  
  data_filtered2 <- reactive({
    df <- transformed_data %>%
      filter((year_value %in% input$year2) | is.null(input$year2)) %>%
      filter((Category %in% input$category2) | is.null(input$category2)) %>%
      filter((title %in% input$title2) | is.null(input$title2)) %>%
      filter((School %in% input$school2) | is.null(input$school2)) %>%
      filter(School != "OTHER") %>%
      filter(!grepl("\\(unknown\\)", Status)) %>%
      filter(!grepl("student worker", Status, ignore.case = TRUE))
    
    df$start <- as.POSIXct(df$start)
    
    if (length(input$semester2) > 0) {
      df <- df %>%
        filter(
          (input$semester2 %in% "Spring" & month(start) %in% 1:5) |
            (input$semester2 %in% "Summer" & month(start) %in% 6:7) |
            (input$semester2 %in% "Fall" & month(start) %in% 8:12)
        )
    }
    
    if ("Status" %in% names(df)) {
      df$Status <- as.character(df$Status)
      df %>%
        separate_rows(Status, sep = ",") %>%
        filter(!grepl("student worker", Status, ignore.case = TRUE)) %>%
        group_by(School, Status) %>%
        summarise(count = n_distinct(email), .groups = 'drop')
    } else {
      data.frame()
    }
  })
  
  output$barPlot2 <- renderPlotly({
    df <- data_filtered2()
    if (nrow(df) == 0) return(NULL)
    
    p <- ggplot(df, aes(x = School, y = count, fill = Status)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Demographics by School and Status", x = "School", y = "Count") +
      theme_minimal() +
      theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
}

shinyApp(ui, server)
