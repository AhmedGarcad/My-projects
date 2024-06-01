
********************************************************************
  THIS IS my PROJECT CODE
**********************************************************************


# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(forecast)
library(shinydashboard)

# Read data for all years
QETC2018_Addmissions <- read_csv("C:/Users/ahmed/Desktop/QETC- DATA/QETC2018 Addmissions.csv")
QETC2019_Admissions <- read_csv("C:/Users/ahmed/Desktop/QETC- DATA/QETC2019 Admissions.csv")
QETC2020_Admissions <- read_csv("C:/Users/ahmed/Desktop/QETC- DATA/QETC2020 Admissions.csv")
QETC2021_Admission <- read_csv("C:/Users/ahmed/Desktop/QETC- DATA/QETC2021 Admission.csv")
QETC_2022_Admmissions <- read_csv("C:/Users/ahmed/Desktop/QETC- DATA/QETC 2022 Admmissions.csv")
QETC_2023_Admissions_ <- read_csv("C:/Users/ahmed/Desktop/QETC- DATA/QETC 2023 Admissions .csv")

# Combine all data into one data frame
all_data <- bind_rows(
  data2019clean ,
  data2022clean ,
  data2018clean ,
  data2020clean,
  data2021clean,
  data2023clean
)

# Combine all data into one data frame
data2019clean <- na.omit(QETC2019_Admissions)
data2018clean <- na.omit(QETC2018_Addmissions)
data2020clean <- na.omit(QETC2020_Admissions)
data2021clean <- na.omit(QETC2021_Admission)
data2022clean <- na.omit(QETC_2022_Admmissions)
data2023clean <- na.omit(QETC_2023_Admissions_)

all_data

all_data$Gender <- ifelse(all_data$Gender == "M", "Male", all_data$Gender)
all_data$Gender[all_data$Gender == "M"] <- "Male"
summary(all_data)
unique(all_data$Gender)


# Rename variables
names(all_data) <- c("Name", "PassportID", "Gender", "DateOfBirth", "Age", "Nationality", "University", "Commission", "YearOfAdmission")
# Filter data for specified years
years <- 2018:2023
all_data <- all_data %>%
  filter(YearOfAdmission %in% years)

# Create Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "QETC Data Analytics Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    tabsetPanel(
      tabPanel("Overview",
               dataTableOutput("overview_table"),
               verbatimTextOutput("summary_stats")
      ),
      tabPanel("Enrollment by Year",
               plotOutput("enrollment_chart"),
               plotOutput("year_histogram")
      ),
      tabPanel("Country Commission Range",
               plotOutput("country_commission_graph")
      ),
      tabPanel("Country of Origin",
               plotOutput("country_pie_chart")
      ),
      tabPanel("University Distribution",
               plotOutput("university_pie_chart")
      ),
      tabPanel("Age Distribution",
               plotOutput("age_histogram")
      ),
      tabPanel("Gender Composition",
               plotOutput("gender_pie_chart")
      ),
      tabPanel("Commission Distribution",
               plotOutput("commission_histogram")
      ),
      tabPanel("Enrollment Trend",
               plotOutput("enrollment_trend")
      ),
      tabPanel("Nationality Distribution",
               plotOutput("nationality_bar_chart")
      ),
      tabPanel("Top Universities",
               plotOutput("top_universities_bar_chart")
      ),
      tabPanel("Admission by Month",
               plotOutput("admission_month_line_chart")
      )
    )
  )
)

# Create Shiny server logic
server <- function(input, output) {
  # Overview table
  output$overview_table <- renderDataTable({
    all_data
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    summary(all_data)
  })
  
  # Enrollment chart
  output$enrollment_chart <- renderPlot({
    enrollment_counts <- all_data %>%
      group_by(YearOfAdmission) %>%
      count(Gender)
    
    ggplot(enrollment_counts, aes(x = YearOfAdmission, y = n, fill = Gender)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Enrollment by Year", x = "Year of Admission", y = "Number of Students") +
      theme_minimal()
  })
  
  # Year histogram
  output$year_histogram <- renderPlot({
    ggplot(all_data, aes(x = factor(YearOfAdmission))) +
      geom_bar() +
      labs(title = "Student Distribution by Year", x = "Year", y = "Number of Students") +
      theme_minimal()
  })
  
  # Country commission range line graph
  output$country_commission_graph <- renderPlot({
    top_countries <- all_data %>%
      group_by(Nationality) %>%
      summarise(CommissionRange = max(Commission) - min(Commission)) %>%
      top_n(10, CommissionRange)
    
    top_countries_data <- all_data %>%
      filter(Nationality %in% top_countries$Nationality)
    
    ggplot(top_countries_data, aes(x = YearOfAdmission, y = Commission, color = Nationality, group = Nationality)) +
      geom_line() +
      labs(title = "Commission Range by Country", x = "Year of Admission", y = "Commission (RM)") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      guides(color = guide_legend(title = "Nationality"))
  })
  
  # Country of origin pie chart
  output$country_pie_chart <- renderPlot({
    country_counts <- all_data %>%
      group_by(Nationality) %>%
      count()
    
    ggplot(country_counts, aes(x = "", y = n, fill = Nationality)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Country of Origin", fill = "Nationality") +
      theme_minimal() +
      theme(legend.position = "right") +
      guides(fill = guide_legend(title = "Nationality"))
  })
  
  # University distribution pie chart
  output$university_pie_chart <- renderPlot({
    university_counts <- all_data %>%
      group_by(Nationality, University) %>%
      count() %>%
      group_by(Nationality) %>%
      slice_max(n, with_ties = FALSE)
    
    ggplot(university_counts, aes(x = "", y = n, fill = University)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "University Distribution", fill = "University") +
      theme_minimal() +
      theme(legend.position = "right") +
      guides(fill = guide_legend(title = "University"))
  })
  
  # Age histogram
  output$age_histogram <- renderPlot({
    ggplot(all_data, aes(x = Age)) +
      geom_histogram() +
      labs(title = "Age Distribution", x = "Age", y = "Count") +
      theme_minimal()
  })
  
  # Gender composition pie chart
  output$gender_pie_chart <- renderPlot({
    gender_counts <- all_data %>%
      count(Gender)
    
    ggplot(gender_counts, aes(x = "", y = n, fill = Gender)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Gender Composition", fill = "Gender") +
      theme_minimal() +
      theme(legend.position = "right") +
      guides(fill = guide_legend(title = "Gender"))
  })
  
  # Commission distribution histogram
  output$commission_histogram <- renderPlot({
    ggplot(all_data, aes(x = Commission)) +
      geom_histogram() +
      labs(title = "Commission Distribution", x = "Commission", y = "Count") +
      theme_minimal()
  })
  
  # Enrollment trend line chart
  output$enrollment_trend <- renderPlot({
    enrollment_trend <- all_data %>%
      group_by(YearOfAdmission) %>%
      count() %>%
      mutate(YearOfAdmission = as.character(YearOfAdmission))
    
    ggplot(enrollment_trend, aes(x = YearOfAdmission, y = n)) +
      geom_line() +
      labs(title = "Enrollment Trend", x = "Year of Admission", y = "Number of Students") +
      theme_minimal()
  })
  
  # Nationality distribution bar chart
  output$nationality_bar_chart <- renderPlot({
    nationality_counts <- all_data %>%
      group_by(Nationality) %>%
      count() %>%
      arrange(desc(n))
    
    ggplot(nationality_counts, aes(x = reorder(Nationality, -n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Nationality Distribution", x = "Nationality", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Top universities bar chart
  output$top_universities_bar_chart <- renderPlot({
    top_universities <- all_data %>%
      group_by(University) %>%
      count() %>%
      arrange(desc(n)) %>%
      top_n(10, n)
    
    ggplot(top_universities, aes(x = reorder(University, -n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Top Universities", x = "University", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Admission by month line chart
  output$admission_month_line_chart <- renderPlot({
    all_data$Month <- format(as.Date(all_data$DateOfBirth), "%b")
    admission_by_month <- all_data %>%
      group_by(Month) %>%
      count()
    
    ggplot(admission_by_month, aes(x = Month, y = n, group = 1)) +
      geom_line() +
      labs(title = "Admission by Month", x = "Month", y = "Number of Students") +
      theme_minimal()
  })
}

# Run the Shiny app
shinyApp(ui, server)
