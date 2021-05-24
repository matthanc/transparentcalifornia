library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(scales)


#Load data
df <- read_csv("transparentcalifornia-outreach.csv")


df_filtered <- df %>%
  select(Agency, Year, `Employee Name`, `Job Title`, `Base Pay`, full_time, email) %>%
  rename(Name = `Employee Name`, Title = `Job Title`, BasePay = `Base Pay`, FullTime = full_time, Email = email)

df_filtered$BasePay <- as.integer(df_filtered$BasePay)

#shiny server ui
ui <-  fluidPage(
  align="center",
  # theme = shinytheme("spacelab"),
  br(),
  titlePanel("Transparent California Outreach Tool"),
  br(),
  fluidRow(
    column(3,
           searchInput(inputId = "Title",
                       label = "Search Job Title",
                       btnSearch = icon("search"),
                       btnReset = icon("remove"),
                       width = "75%"
           ),
           downloadButton("download", "Download .csv")
    ),
    column(6,
           DT::dataTableOutput("df_filtered")
    ),
    column(3)
  )
)

#shiny server back end
server <- function(input, output, session) {
  
  datasetInput1 <- reactive({
    df_filtered$Title <- str_to_title(df_filtered$Title)
    reactivedata <- df_filtered
    if (is.character(input$Title)) {
      reactivedata <- reactivedata %>%
        #filter(str_detect(Title, fixed(input$Title, ignore_case=TRUE)))
        filter(str_detect(Title, str_to_title(input$Title)))
    }
    reactivedata <- reactivedata
    return(reactivedata)
  })
  
  output$df_filtered <- renderDT({
    datatable(datasetInput1(), rownames = TRUE, options = list(dom = 'itp'))
  })
  
  output$download <- downloadHandler(
    filename = "report.csv",
    content = function(file) {
      write.csv(datasetInput1(), file)
    }
  )
}

#run shiny server
shinyApp(ui = ui, server = server)
