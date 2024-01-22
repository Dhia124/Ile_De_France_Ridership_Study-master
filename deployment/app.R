library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(DT)
library(ggplot2)
library(corrplot)


# Read in the data, assuming the CSV file is in the same directory as the app.R script
data_NB_FER <- read_delim("data_NB_FER_finale.csv")
view(data_NB_FER)
# Prepare the data
data_NB_FER <- data_NB_FER %>%
  mutate(
    Month = lubridate::month(JOUR, label = TRUE),
    Year = lubridate::year(JOUR),
    Weekday = lubridate::wday(JOUR, label = TRUE),
    carte_imaginaire = ifelse(CATEGORIE_TITRE == "IMAGINE R", 1, 0),
    is_holiday_numeric = as.numeric(is_holiday == TRUE)
  )
baseline_week <- data_NB_FER %>%
  group_by(day_of_week = weekdays(JOUR)) %>%
  summarise(avg_vald = mean(NB_VALD, na.rm = TRUE))

holiday_data <- data_NB_FER %>%
  filter(is_holiday == TRUE) %>%
  summarise(avg_vald_holiday = mean(NB_VALD, na.rm = TRUE))

non_holiday_data <- data_NB_FER %>%
  filter(is_holiday == FALSE) %>%
  summarise(avg_vald_non_holiday = mean(NB_VALD, na.rm = TRUE))

deviations_df <- data.frame(
  period = c("Holidays", "Non-Holidays"),
  avg_vald = c(holiday_data$avg_vald_holiday, non_holiday_data$avg_vald_non_holiday)
)
# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Ridership Dashboard"),
  dashboardSidebar(
    selectInput("yearInput", "Select Year", choices = unique(data_NB_FER$Year)),
    selectInput("weekdayInput", "Select Day of Week", choices = unique(data_NB_FER$Weekday))
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("overall_trend"), title = "Overall Ridership Trend"),
      box(plotOutput("monthly_trends"), title = "Monthly Ridership Trends"),
      box(plotOutput("weekday_vs_weekend"), title = "Weekday vs. Weekend Ridership"),
      box(
        plotOutput("comparisons_with_norm"),
        title = "Normal week."
      ),
      box(
        plotOutput("comparisons_with_norm2"),
        title = "Average Validations During Holidays and Non-Holidays."
      ),
      box(plotOutput("correlation_heatmap"), title = "Correlation Heatmap")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  output$overall_trend <- renderPlot({
    data_filtered <- data_NB_FER %>% filter(Year == input$yearInput)
    ggplot(data_filtered, aes(x = JOUR, y = NB_VALD)) +
      geom_line() +
      ggtitle("Overall Ridership Trend") +
      xlab("Date") +
      ylab("Number of Validations")
  })
  
  output$monthly_trends <- renderPlot({
    data_filtered <- data_NB_FER %>% filter(Year == input$yearInput)
    ggplot(data_filtered, aes(x = Month, y = NB_VALD)) +
      geom_boxplot() +
      labs(title = paste("Monthly Ridership Distribution for", input$yearInput), x = "Month", y = "Ridership")
  })
  
  output$weekday_vs_weekend <- renderPlot({
    data_filtered <- data_NB_FER %>% filter(Weekday == input$weekdayInput)
    ggplot(data_filtered, aes(x = Weekday, y = NB_VALD)) +
      geom_boxplot() +
      theme_minimal() +
      ggtitle("Ridership on Weekdays vs. Weekends") +
      xlab("Day of Week") +
      ylab("Ridership")
  })
  
  output$comparisons_with_norm <- renderPlot({
    ggplot(baseline_week, aes(x = day_of_week, y = avg_vald)) +
      geom_bar(stat = "identity", position = "dodge", fill = "skyblue", color = "black") +
      labs(title = "Baseline Average Validations by Day of the Week",
           x = "Day of the Week",
           y = "Average Validations") +
      theme_minimal()
  })
  output$comparisons_with_norm2 <- renderPlot({
    ggplot(deviations_df, aes(x = period, y = avg_vald, fill = period)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      labs(title = "Average Validations During Holidays and Non-Holidays",
           x = "Period",
           y = "Average Validations") +
      theme_minimal()
  })
  output$correlation_heatmap <- renderPlot({
    cor_matrix <- cor(data_NB_FER[, c("is_holiday_numeric", "NB_VALD", "carte_imaginaire")])
    corrplot(cor_matrix, method = "color", addCoef.col = "black",
             tl.col = "black", tl.srt = 45,
             col = colorRampPalette(c("blue", "white", "red"))(200),
             type = "upper", order = "hclust",
             title = "Correlation Heatmap Including Holiday Flag",
             cl.lim = c(-1, 1))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
