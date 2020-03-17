library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(lubridate)
library(DT)

sales <- read.csv('C:\\Karthik_Anumalasetty\\Personal\\My resume\\Value validation projects\\R_shiny_drilldown_visualization\\Sales.csv')
sales <- sales %>% mutate(order_date2 = parse_date_time(order_date,orders = c("%m.%Y")))


ui <- fluidPage(
    plotlyOutput("country", height = 200),
    plotlyOutput("city", height = 200),
    plotlyOutput("sales", height = 300),
    dataTableOutput("mytable")
)

# avoid repeating this code
axis_titles <- . %>%
    layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Sales")
    )

server <- function(input, output, session) {
    
    # for maintaining the state of drill-down variables
    country <- reactiveVal()
    city <- reactiveVal()
    order_date2 <- reactiveVal()

    # when clicking on a country,
    observeEvent(event_data("plotly_click", source = "country"), {
        country(event_data("plotly_click", source = "country")$x)
        city(NULL)
        order_date2(NULL)
    })

    observeEvent(event_data("plotly_click", source = "city"), {
        city(
            event_data("plotly_click", source = "city")$x
        )
        order_date2(NULL)
    })

    observeEvent(event_data("plotly_click", source = "order_date"), {
        order_date2(event_data("plotly_click", source = "order_date")$x)
    })
    
    output$country <- renderPlotly({
        sales %>%
            group_by(country) %>% 
            summarise(TotalUnits = sum(Sales.units)) %>%
            plot_ly(x = ~country, y = ~TotalUnits, source = "country") %>%
            axis_titles() %>% 
            layout(title = "Sales by country")
    })
    
    output$city <- renderPlotly({
        if (is.null(country())) return(NULL)

        sales %>%
            filter(country %in% country()) %>%
            group_by(city) %>% 
            summarise(TotalUnits = sum(Sales.units)) %>%
            plot_ly(x = ~city, y = ~TotalUnits, source = "city") %>%
            axis_titles() %>%
            layout(title = paste(country()," sales by city"))
    })

    output$sales <- renderPlotly({
        if (is.null(city())) return(NULL)

        sales %>%
            filter(city %in% city()) %>%
            group_by(order_date2) %>% 
            summarise(TotalUnits = sum(Sales.units)) %>%
            plot_ly(x = ~order_date2, y = ~TotalUnits, source = "order_date") %>%
            add_lines() %>%
            axis_titles() %>%
            layout(title = paste(city(), "sales over time"))
    })

    output$mytable <- DT::renderDataTable({
        if (is.null(order_date2())) return(NULL)
        sales %>%
            filter(
                city == city() & 
                    order_date2 == as.Date(order_date2()))
    })
    
}

shinyApp(ui, server)
