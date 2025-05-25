library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(shinyWidgets) # For pickerInput or selectizeInput enhancements if needed
library(scales)

# Load data
df <- readRDS("transparentcalifornia-outreach.rds")

# Prepare df_filtered and choices for dropdowns
df_filtered <- df %>%
  select(Agency, Year, `Employee Name`, `Job Title`, `Base Pay`, full_time, email_final) %>% # Using email_final
  rename(Name = `Employee Name`, Title = `Job Title`, BasePay = `Base Pay`, FullTime = full_time, Email = email_final) %>%
  mutate(
    Year = as.character(Year), # Ensure Year is character for reliable filtering
    Agency = as.character(Agency) # Ensure Agency is character
  )

# Convert BasePay to integer after ensuring it's suitable (e.g. NAs handled)
# The scraper already converts pay columns to numeric, handling parsing errors.
# Here, we just ensure it's integer for display or specific operations if needed.
df_filtered$BasePay <- suppressWarnings(as.integer(df_filtered$BasePay))

# Get unique choices for filters
unique_agencies <- sort(unique(df_filtered$Agency))
unique_years <- sort(unique(df_filtered$Year), decreasing = TRUE) # Show recent years first

# Shiny UI
ui <-  fluidPage(
  align="center",
  theme = shinytheme("spacelab"),
  br(),
  titlePanel("Transparent California Outreach Tool"),
  br(),
  fluidRow(
    column(3,
           searchInput(inputId = "Title",
                       label = "Search Job Title:",
                       btnSearch = icon("search"),
                       btnReset = icon("remove"),
                       width = "100%" 
           ),
           br(), # Add some space
           selectizeInput(inputId = "Agency",
                          label = "Filter by Agency:",
                          choices = unique_agencies,
                          multiple = TRUE,
                          options = list(placeholder = 'Select agency/agencies...')
           ),
           br(),
           selectizeInput(inputId = "Year",
                          label = "Filter by Year:",
                          choices = unique_years,
                          multiple = TRUE,
                          options = list(placeholder = 'Select year(s)...')
           ),
           br(),
           downloadButton("download", "Download Filtered Data (.csv)")
    ),
    column(9, # Increased width for the table
           DT::dataTableOutput("df_filtered_table") # Changed ID for clarity
    )
  )
)

# Shiny Server backend
server <- function(input, output, session) {
  
  datasetInput1 <- reactive({
    reactivedata <- df_filtered
    
    # Filter by Job Title
    if (is.character(input$Title) && nchar(input$Title) > 0) {
      # Using str_to_lower for case-insensitive search on both data and input
      search_term_lower <- str_to_lower(input$Title)
      reactivedata <- reactivedata %>%
        filter(str_detect(str_to_lower(Title), fixed(search_term_lower)))
    }
    
    # Filter by Agency
    if (!is.null(input$Agency) && length(input$Agency) > 0) {
      reactivedata <- reactivedata %>%
        filter(Agency %in% input$Agency)
    }
    
    # Filter by Year
    if (!is.null(input$Year) && length(input$Year) > 0) {
      reactivedata <- reactivedata %>%
        filter(Year %in% input$Year)
    }
    
    return(reactivedata)
  })
  
  output$df_filtered_table <- renderDT({ # Changed ID here as well
    datatable(datasetInput1(), 
              rownames = TRUE, 
              options = list(
                dom = 'itp', # Shows table, info, pagination
                pageLength = 10, # Default number of rows to display
                lengthMenu = c(10, 25, 50, 100) # Options for rows per page
              ),
              filter = 'top', # Add individual column filters
              class = 'display compact table-striped hover' # Styling
             )
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("transparentcalifornia_report_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(datasetInput1(), file, row.names = FALSE)
    }
  )
}

# How to Test:
# 1. Launch the Shiny app.
# 2. The data table should load with all data initially.
# 3. Use the "Filter by Agency" dropdown:
#    - Select one agency: The table should update to show only records from that agency.
#    - Select multiple agencies: The table should show records from all selected agencies.
#    - Deselect all agencies: The table should show records from all agencies (or respect other active filters).
# 4. Use the "Filter by Year" dropdown:
#    - Select one year: The table should update to show only records from that year.
#    - Select multiple years: The table should show records from all selected years.
#    - Deselect all years: The table should show records from all years (or respect other active filters).
# 5. Use the "Search Job Title" input:
#    - Type a job title (e.g., "Manager"): The table should filter by job title.
# 6. Combine Filters:
#    - Select an agency, a year, and type a job title. The table should update to show records matching all three criteria.
#    - Test various combinations.
# 7. Download Data:
#    - After applying filters, click the "Download Filtered Data (.csv)" button.
#    - Open the CSV and verify it contains the same data as displayed in the filtered table.
# 8. Reset/Clear Filters:
#    - Clear the Job Title search (click the 'x' if `searchInput` provides it, or manually delete text).
#    - Deselect items from the Agency and Year dropdowns. The table should update accordingly.

# Run Shiny server
shinyApp(ui = ui, server = server)
