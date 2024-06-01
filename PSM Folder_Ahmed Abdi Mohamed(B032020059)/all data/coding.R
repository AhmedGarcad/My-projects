library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(forecast)
library(shinydashboard)
library(RColorBrewer)

# Read data for all years
QETC2018_Addmissions <- read.csv("C:/Users/ahmed/Desktop/QETC- DATA/QETC2018 Addmissions.csv")
QETC2019_Admissions <- read.csv("C:/Users/ahmed/Desktop/QETC- DATA/QETC2019 Admissions.csv")
QETC2020_Admissions <- read.csv("C:/Users/ahmed/Desktop/QETC- DATA/QETC2020 Admissions.csv")
QETC2021_Admission <- read.csv("C:/Users/ahmed/Desktop/QETC- DATA/QETC2021 Admission.csv")
QETC_2022_Admmissions <- read.csv("C:/Users/ahmed/Desktop/QETC- DATA/QETC 2022 Admmissions.csv")
QETC_2023_Admissions_ <- read.csv("C:/Users/ahmed/Desktop/QETC- DATA/QETC 2023 Admissions .csv")

# Combine all data into one data frame
data2019clean <- na.omit(QETC2019_Admissions)
data2018clean <- na.omit(QETC2018_Addmissions)
data2020clean <- na.omit(QETC2020_Admissions)
data2021clean <- na.omit(QETC2021_Admission)
data2022clean <- na.omit(QETC_2022_Admmissions)
data2023clean <- na.omit(QETC_2023_Admissions_)

summary(all_data)
all_data <- bind_rows(
  data2019clean,
  data2022clean,
  data2018clean,
  data2020clean,
  data2021clean,
  data2023clean
)


# Rename variables
names(all_data) <- c("Name", "PassportID", "Gender", "DateOfBirth", "Age", "Nationality", "University", "Commission", "YearOfAdmission")
# Filter data for specified years
years <- 201:2023
all_data <- all_data %>%
  filter(YearOfAdmission %in% years)
summary(all_data)

# Create a mapping between "M" and "Male" and "F" and "Female"
gender_mapping <- c("M" = "Male", "F" = "Female")

# Use the mapping to update the "Gender" column
all_data$Gender <- gender_mapping[all_data$Gender]

all_data
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
      tabPanel("University Distribution",
               plotlyOutput("university_public"),
               plotlyOutput("university_private")
      ),
      tabPanel("Country of Origin",
               plotOutput("country_pie_chart")
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
  
  # University distribution (Public vs Private) - Public
  output$university_public <- renderPlotly({
    # Calculate average commission based on university
    university_commission <- all_data %>%
      ungroup() %>%
      group_by(University) %>%
      summarise(AverageCommission = mean(Commission, na.rm = TRUE))
    
    # Determine the threshold for categorizing as public or private
    commission_threshold <- median(university_commission$AverageCommission)
    
    # Categorize universities as public or private based on average commission
    university_commission$UniversityType <- ifelse(university_commission$AverageCommission >= commission_threshold, "Private", "Public")
    
    # Filter the data for public universities
    public_universities <- university_commission %>%
      filter(UniversityType == "Public")
    
    plot_ly(data = public_universities, x = ~University, y = ~AverageCommission, type = "bar",
            text = ~paste("Avg. Commission:", round(AverageCommission, 2)), hoverinfo = "text",
            marker = list(color = ~UniversityType, colors = brewer.pal(3, name = "Set2"))) %>%
      layout(title = "Public Universities",
             xaxis = list(title = "University"),
             yaxis = list(title = "Average Commission (RM)"),
             hoverlabel = list(namelength = -1))
  })
  
  # University distribution (Public vs Private) - Private
  output$university_private <- renderPlotly({
    # Calculate average commission based on university
    university_commission <- all_data %>%
      ungroup() %>%
      group_by(University) %>%
      summarise(AverageCommission = mean(Commission, na.rm = TRUE))
    
    # Determine the threshold for categorizing as public or private
    commission_threshold <- median(university_commission$AverageCommission)
    
    # Categorize universities as public or private based on average commission
    university_commission$UniversityType <- ifelse(university_commission$AverageCommission >= commission_threshold, "Private", "Public")
    
    # Filter the data for private universities
    private_universities <- university_commission %>%
      filter(UniversityType == "Private")
    
    plot_ly(data = private_universities, x = ~University, y = ~AverageCommission, type = "bar",
            text = ~paste("Avg. Commission:", round(AverageCommission, 2)), hoverinfo = "text",
            marker = list(color = ~UniversityType, colors = brewer.pal(3, name = "Set2"))) %>%
      layout(title = "Private Universities",
             xaxis = list(title = "University"),
             yaxis = list(title = "Average Commission (RM)"),
             hoverlabel = list(namelength = -1))
  })
  
  # Nationality bar chart
  output$nationality_bar_chart <- renderPlot({
    nationality_counts <- all_data %>%
      group_by(Nationality) %>%
      count() %>%
      arrange(desc(n)) %>%
      top_n(10)
    
    ggplot(nationality_counts, aes(x = reorder(Nationality, n), y = n)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 Nationalities", x = "Nationality", y = "Number of Students") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Top universities bar chart
  output$top_universities_bar_chart <- renderPlot({
    top_universities <- all_data %>%
      group_by(University) %>%
      count() %>%
      arrange(desc(n)) %>%
      top_n(10)
    
    ggplot(top_universities, aes(x = reorder(University, n), y = n)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 Universities", x = "University", y = "Number of Students") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Average commission by university bar chart
  output$average_commission_by_university_bar_chart <- renderPlot({
    average_commission_by_university <- all_data %>%
      group_by(University) %>%
      summarise(AverageCommission = mean(Commission, na.rm = TRUE)) %>%
      arrange(desc(AverageCommission)) %>%
      top_n(10)
    
    ggplot(average_commission_by_university, aes(x = reorder(University, AverageCommission), y = AverageCommission)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 Universities by Average Commission", x = "University", y = "Average Commission (RM)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}

# Run the Shiny app
shinyApp(ui, server)
