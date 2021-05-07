library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(scales)


df <- read_csv("C:/Users/Matt/Documents/transparentcalifornia-outreach.csv")

org_stats <- df %>%
    group_by(Agency) %>%
    summarise(year = median(Year), employees = n(), mean_pay = mean(`Total Pay`), mean_benefit = mean(Benefits), median_pay = median(`Total Pay`[`Total Pay`>0]), median_benefit = median(Benefits[Benefits>0]), benefit_rate = median(Benefits[Benefits>0]) / median(`Total Pay`[`Total Pay`>0])) %>%
    mutate(across(c(mean_pay, mean_benefit, median_pay, median_benefit), dollar))

org_stats$year <- as.integer(org_stats$year)


top_positions <- df %>% group_by(Agency, `Job Title`) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    slice_max(order_by = count, n = 10) %>%
    ungroup()

top_salaries <- df %>%
    group_by(Agency) %>%
    slice_max(order_by = `Total Pay`, n = 10) %>%
    select(Agency, `Employee Name`, `Job Title`, `Total Pay`, Benefits, `Total Pay & Benefits`) %>%
    filter(`Total Pay & Benefits` > 0) %>%
    ungroup()

top_salaries <- top_salaries %>% mutate(across(c(`Total Pay`, Benefits, `Total Pay & Benefits`), dollar))

police <- df %>%
    filter(str_detect(`Job Title`, "police|Police|sheriff|Sheriff")) %>%
    group_by(Agency) %>%
    summarise(police_staff = n(), mean_pay = mean(`Total Pay`), mean_benefit = mean(Benefits), mean_overtime = mean(`Overtime Pay`), median_pay = median(`Total Pay`[`Total Pay`>0]), median_benefit = median(Benefits[Benefits>0]), median_overtime = median(`Overtime Pay`), benefit_rate = median(Benefits[Benefits>0]) / median(`Total Pay`[`Total Pay`>0])) %>%
    mutate(across(c(mean_pay, mean_benefit, mean_overtime, median_pay, median_benefit, median_overtime), dollar))


police_top_positions <- df %>%
    filter(str_detect(`Job Title`, "police|Police|sheriff|Sheriff")) %>%
    group_by(Agency) %>%
    slice_max(order_by = `Total Pay`, n = 5) %>%
    select(Agency, `Employee Name`, `Job Title`, `Overtime Pay`, `Total Pay`, Benefits, `Total Pay & Benefits`) %>%
    filter(`Total Pay & Benefits` > 0)

ui <-  fluidPage(
    theme = bslib::bs_theme(bootswatch = "sandstone"),
    titlePanel("Transparent California"),
    fluidRow(
        selectInput("Organization", "Choose an organization", unique(org_stats$Agency)),
    ),
    fluidRow(
        h3(textOutput("header1"))
    ),
    fluidRow(
        tableOutput("org_stats")
    ),
    fluidRow(
        column(3,
               tableOutput("top_positions")
               ),
        column(9,
            tableOutput("top_salaries")
        )
    ),
    fluidRow(
        h3("Policing Information"),
        tableOutput("police")
    )
)

server <- function(input, output) {
    datasetInput1 <- reactive({
        org_stats %>% filter(Agency == input$Organization) %>% select(-Agency)
    })
    datasetInput2 <- reactive({
        top_positions %>% filter(Agency == input$Organization) %>% select(-Agency)
    })
    datasetInput3 <- reactive({
        top_salaries %>% filter(Agency == input$Organization) %>% select(-Agency)
    })
    datasetInput4 <- reactive({
        police %>% filter(Agency == input$Organization) %>% select(-Agency)
    })
    output$org_stats <- renderTable({
        dataset <- datasetInput1()
    })
    output$top_positions <- renderTable({
        dataset <- datasetInput2()
    })
    output$top_salaries <- renderTable({
        dataset <- datasetInput3()
    })
    output$police <- renderTable({
        dataset <- datasetInput4()
    })
    output$header1 <- renderText({
        paste0(input$Organization)
    })
}

shinyApp(ui = ui, server = server)
