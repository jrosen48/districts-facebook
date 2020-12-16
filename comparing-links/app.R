#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

dd <- read_csv("data-for-shiny.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Comparing Links Shared by U.S. School Districts' Facebook Pages"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
                     p("The measure pertains to how links are compared:"),
                     tags$div(class="header", checked=NA,
                              list(
                                  tags$ul("Raw: Counts domains per year"),
                                  tags$ul("Prop: Proportion of domains (of all links) per year"),
                                  tags$ul("Raw District: Number of districts posting a domain per year")
                              )
                     ),
                     #p("Prop District: Proportion of districts  posting a domain per year"),
                     selectInput("measure",
                                 "Measure:",
                                 choices = c("raw", "prop", "raw_district"),
                                 selected = "raw"),
                     p("Year pertains to what year is the focus of the plot"),
                     selectInput("year",
                                 "Selected Year:",
                                 choices = unique(dd$year) %>% sort(),
                                 selected = 2020),
                     # checkboxInput("smooth_line",
                     #                "All fitted line")
                     # checkboxInput("all_years",
                     #                "All Years")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(width = 6,
            tabsetPanel(
                tabPanel("Time Series Plot",
                         shiny::plotOutput("ts_plot")
                ),
                tabPanel("Table of Plotted Data",
                         shiny::dataTableOutput("to_plot")
                ),
                tabPanel("Table with All Data Selected Year",
                         shiny::dataTableOutput("dt_table")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$dt_table <- renderDataTable({
        
        if (input$measure == "raw") {
            # raw counts
            dd %>% group_by(year) %>% count(domain) %>% arrange(year, desc(n)) %>% filter(year == input$year)
        } else if (input$measure == "prop") {
            # props of all links
            dd %>% group_by(year) %>% count(domain) %>% mutate(prop = n/sum(n)) %>% arrange(year, desc(prop)) %>% filter(year == input$year)
        } else if (input$measure == "raw_district") {
            # present or not in district
            dd %>% group_by(year) %>% count(nces_id, domain) %>% count(domain) %>% arrange(year, desc(n)) %>% filter(year == input$year)
        } else if (input$measure == "prop_district") {
            # props of all districts with any links
            dd %>% group_by(year, nces_id) %>% count(domain) %>% group_by(year) %>% mutate(prop = n / n()) %>% arrange(year, desc(n)) %>% filter(year == input$year)
        }
        
    })
    
    to_plot <- reactive({
        
        # prep
        if (input$measure == "raw") {
            # raw counts
            t10 <- dd %>% group_by(year) %>% count(domain) %>% arrange(year, desc(n)) %>% filter(year == input$year) %>% slice(1:10)
        } else if (input$measure == "prop") {
            # props of all links
            t10 <- dd %>% group_by(year) %>% count(domain) %>% mutate(prop = n/sum(n)) %>% arrange(year, desc(prop)) %>% filter(year == input$year) %>% slice(1:10)
        } else if (input$measure == "raw_district") {
            # present or not in district
            t10 <- dd %>% group_by(year) %>% count(nces_id, domain) %>% count(domain) %>% arrange(year, desc(n)) %>% filter(year == input$year) %>% slice(1:10)
        } else if (input$measure == "prop_district") {
            # props of all districts with any links
            t10 <- dd %>% group_by(year, nces_id) %>% count(domain) %>% group_by(year) %>% mutate(prop = n / n()) %>% arrange(year, desc(n)) %>% filter(year == input$year) %>% slice(1:10)
        }
        
        # plot
        if (input$measure == "raw") {
            # raw counts
            dtp <- dd %>% group_by(year) %>% count(domain) %>% arrange(year, desc(n)) %>% filter(domain %in% t10$domain)
        } else if (input$measure == "prop") {
            # props of all links
            dtp <- dd %>% group_by(year) %>% count(domain) %>% mutate(prop = n/sum(n)) %>% arrange(year, desc(prop)) %>% filter(domain %in% t10$domain)
        } else if (input$measure == "raw_district") {
            # present or not in district
            dtp <- dd %>% group_by(year) %>% count(nces_id, domain) %>% count(domain) %>% arrange(year, desc(n))  %>% filter(domain %in% t10$domain)
        } else if (input$measure == "prop_district") {
            # props of all districts with any links
            dtp <- dd %>% group_by(year, nces_id) %>% count(domain) %>% group_by(year) %>% mutate(prop = n / n()) %>% arrange(year, desc(n)) %>% filter(domain %in% t10$domain)
        }
        
        dtp
    })
    
    output$to_plot <- renderDataTable(
        to_plot()
    )
    
    output$ts_plot <- renderPlot({
        
        if (input$measure %in% c("prop")) {
            to_plot() %>% 
                ggplot(aes(x = year, y = prop, group = domain, color = domain)) +
                geom_point() +
                geom_line() +
                hrbrthemes::theme_ipsum(base_size = 16) +
                scale_x_continuous(breaks = unique(dd$year) %>% sort())
        } else {
            to_plot() %>% 
                ggplot(aes(x = year, y = n, group = domain, color = domain)) +
                geom_point() +
                geom_line() +
                hrbrthemes::theme_ipsum(base_size = 16) +
                scale_x_continuous(breaks = unique(dd$year) %>% sort())
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
