library(shiny)
library(googlesheets4)
library(dplyr)
library(lubridate)
library(DT)
library(tidyr)
library(stringr)
library(rsconnect)
library(ggmap)
library(leaflet)

#rsconnect::setAccountInfo(name='rroday-noaa', token='A323A71A26EB9A5CBADD81BA1874C9BA', secret='AiQGUCq4Ds3egzMYfkyShGhZitERGJyQfYxDQ1eJ')
# rsconnect::deployApp("C:/Users/rachel.roday/Documents/")

#JSON 405422767782-j0aoik7rr6ntct6badu6va920lqg0q8s.apps.googleusercontent.com
# rsconnect::deployments("MSR Shiny App.R")

rsconnect::forgetDeployment("MSR Shiny App.R")

rsconnect::setAccountInfo(
  name = Sys.getenv("SHINY_ACC_NAME"),
  token = Sys.getenv("SHINY_ACC_TOKEN"),
  secret = Sys.getenv("SHINY_ACC_SECRET")
)

# Deauthenticate and authenticate to Google Sheets
#gs4_auth(scope = "https://www.googleapis.com/auth/spreadsheets.readonly")
gs4_auth(path = "nmfs-stt-api-MSR.json",  scope = "https://www.googleapis.com/auth/spreadsheets.readonly")

# Specify the Google Sheet URL or ID
#sheet_id <- "https://docs.google.com/spreadsheets/d/1SVkU-cYaubia63LDyQBN3PWohPtmEr5gNrtfBH6ur9g/edit?usp=sharing"
sheet_id <- "https://docs.google.com/spreadsheets/d/1s64uxb6fwUakm22RQAsf4fPtC3rWeV7hZw9y3TmdUmc/edit?usp=sharing"

# Get the names of all sheets
sheet_names <- sheet_names(sheet_id)

# Filter for sheets named "MSR 2025" to "MSR 2020" (assuming this pattern exists)
sheet_names_to_use <- sheet_names[grepl("^MSR 20(25|24)$", sheet_names)]

# Initialize an empty list to store each sheet
sheet_data_list <- list()

# Loop through each filtered sheet
for (sheet_name in sheet_names_to_use) {
  # Read the sheet
  sheet_data <- read_sheet(sheet_id, sheet = sheet_name)
  
  # Data Cleaning: Removing unnecessary columns
  sheet_data <- sheet_data %>%
    select(-`Submission to DoS`, -`NOAA Response Due Date (NOAA internal)`, -`State Department Deadline`) %>%
    mutate(
      # Ensure 'Start Date' and 'End Date' are properly formatted as Date objects
      `Start Date` = as.Date(`Start Date`),
      `End Date` = as.Date(`End Date`),
      # Check if cruises are active
      ActiveCruise = ifelse(Sys.Date() >= `Start Date` & Sys.Date() <= `End Date`, TRUE, FALSE)
    )
  
  # Add the sheet data to the list
  sheet_data_list[[sheet_name]] <- sheet_data
}

# Combine all the filtered sheets into one data frame and make sure research categories are separated
combined_data <- bind_rows(sheet_data_list, .id = "sheet_name") 
combined_data2 <- combined_data %>% 
  separate_rows(`Category of Research`, sep = ",\\s*") %>% 
  mutate(`Category of Research` = str_trim(`Category of Research`)) %>% 
  dplyr::select(sheet_name:Status, ActiveCruise)


#make coordinates of research cruises (approx)
# locations <- unique(combined_data2$`Location of MSR`)
# lat <- c(61.567296, 61.567296, 58.756030, 0.228091, 58.756030, 
#          43.548139, 69.554296, 47.118778, 25.374147, 48.377102, 
#          19.259203, 13.370032, 40.929013, 17.772581, 41.154629, 
#          13.370032, 36.473959, 48.377102, 41.194944, 18.400912)
# 
# lon <- c(-170.842372, -170.842372, -143.534446, -176.478164, -143.534446, 
#          -68.468435, -164.335578, -124.556759, -79.999893, -124.262131, 
#          166.641046, 144.879217, -68.558870, -64.695810, -70.045530, 
#          144.879217, -122.491419, -124.262131, -71.590683, -75.029840)
# Coords <- data.frame(Location = locations, lat = lat, lon = lon)
Coords <- data.frame(
  Location = c(
    "Bering/Chukchi Seas (AK)", "Alaska and Pacific Coast", "USVI", "Bering Sea (AK)",
    "Gulf of Alaska (AK)", "Baker, Howland and the Northern Mariana Islands (PI)", "Alaska", "Gulf of Maine",
    "Chukchi Sea", "PNW", "Florida", "Juan de Fuca", "Wake Island", "Guam and CNMI",
    "Georges Bank", "U.S. Virgin Islands", "Nantucket, Mass", "Guam", "California",
    "Juan de Fuca Strait", "New England", "Navassa Island"
  ),
  lat = c(
    61.567296, 57.0, 18.341900, 58.756030, 58.756030,
    0.228091, 69.554296, 43.548139, 47.118778, 48.377102,
    25.374147, 48.377102, 19.259203, 13.370032, 40.929013,
    17.772581, 41.154629, 13.370032, 36.473959,
    48.377102, 41.194944, 18.400912
  ),
  lon = c(
    -170.842372, -150.0, -64.930700, -143.534446, -143.534446,
    -176.478164, -164.335578, -68.468435, -124.556759, -124.262131,
    -79.999893, -124.262131, 166.641046, 144.879217, -68.558870,
    -64.695810, -70.045530, 144.879217, -122.491419,
    -124.262131, -71.590683, -75.029840
  )
)
combined_data3 <- combined_data2 %>%
  left_join(Coords, by = c("Location of MSR" = "Location"))

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Research Cruises Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Filters for Research Topic
      selectizeInput("research_topic", "Select Research Topic:", 
                     choices = c("Select All Topics" = "All", 
                                 unique(combined_data2$`Category of Research`)), 
                     selected = NULL, multiple = TRUE),
      
      # Filters for Region
      selectizeInput("region", "Select Region:", 
                     choices = c("Select All Regions" = "All", 
                                 unique(combined_data2$`Location of MSR`)), 
                     selected = NULL, multiple = TRUE),
      
      # Filters for Active Cruises (based on the ActiveCruise column)
      checkboxInput("active_cruise", "Show Active Cruises Only", value = FALSE),
      
      # Apply filters button
      actionButton("apply_filters", "Apply Filters")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("cruise_table")),
        tabPanel("Map", leafletOutput("cruise_map"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to filter the data based on user inputs
  filtered_data <- reactive({
    data <- combined_data3  # Use the joined data with coordinates
    
    # Create a new research_topic value based on user input
    research_topic_selected <- if ("All" %in% input$research_topic) {
      unique(data$`Category of Research`)  # Set to all categories if "All" is selected
    } else {
      input$research_topic  # Keep the selected topics
    }
    
    # Filter by research topic
    if (!is.null(research_topic_selected) && length(research_topic_selected) > 0) {
      data <- data %>% filter(`Category of Research` %in% research_topic_selected)
    }
    
    # If 'Select All' is chosen for region, use all regions
    region_selected <- if ("All" %in% input$region) {
      unique(data$`Location of MSR`)  # Set to all regions if "All" is selected
    } else {
      input$region  # Keep the selected regions
    }
    
    # Filter by region
    if (!is.null(region_selected) && length(region_selected) > 0) {
      data <- data %>% filter(`Location of MSR` %in% region_selected)
    }
    
    # Filter by active cruises
    if (input$active_cruise) {
      today <- Sys.Date()
      data <- data %>% filter(today >= as.Date(`Start Date`) & today <= as.Date(`End Date`))
    }
    
    return(data)
  })
  
  # Apply filters when the button is pressed
  observeEvent(input$apply_filters, {
    # Render the data table with filtered data
    output$cruise_table <- renderDT({
      req(filtered_data())  # Re-render table after filtering
      datatable(filtered_data())
    })
    
    # Render the map with the same filtered data
    output$cruise_map <- renderLeaflet({
      req(filtered_data())  # Re-render map after filtering
      cruise_data <- filtered_data()
      
      # Initialize the leaflet map with filtered data directly
      leaflet(data = cruise_data) %>%
        addTiles() %>%
        addMarkers(
          lng = ~lon, lat = ~lat, 
          popup = ~paste0("<b>", `Location of MSR`, "</b>")
        )
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

