library(shiny)
library(tidyverse)
library(forcats)
library(plotly)
library(vistime)
library(reactable)
library(htmltools)
library(scales)
library(wesanderson)
library(bslib)

theme_set(theme_minimal())
pallette <- c(
  wes_palette("Royal2")[5],
  wes_palette("Rushmore1")[4],
  wes_palette("Royal1")[1], 
  wes_palette("Royal2")[3],
  wes_palette("Rushmore1")[3],
  wes_palette("BottleRocket2")[1], 
  wes_palette("Royal2")[1],
  wes_palette("BottleRocket2")[4:5], 
  wes_palette("Royal1")[4], 
  sample(wes_palette("Darjeeling1"), 5))



#### load data ####
## all_issues summary
issues <- read.csv("data/issues_summary.csv") %>% select(-X)

#### helpers #### 
# newspaper types choices
np_types <- issues %>% 
  filter(DocumentType == "NEWSPAPER") %>% 
  select(section) %>% 
  distinct %>% pull()

## timeline
# set names to keyids to display titles in selectInput
title_id <- setNames(issues$keyid, issues$title)

## download csv 
csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}

###################
####### app #######
###################
#### ui ####
ui <- fluidPage(
  
  theme = bs_theme(version = 4, bootswatch = "sandstone"),
  
  titlePanel(h2("Explore DIGAR collection", align = "center")),

  #### sidebar inputs ####
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 helpText("Choosen sidebar options applied to all tabs except for the Timeline"),
                 p(""),
      sliderInput("year", "Year of issue", 
                             min = 1800, max = 2020, 
                             value = c(1918,1940),
                             sep = ""), 
      
      selectInput("select_lang", "Language", choices = unique(issues$language), multiple = TRUE,
                selected = unique(issues$language)),
      
      selectInput("select_countries", "Country of issue", 
                  choices = unique(issues$country),
                  selected = unique(issues$country),
                  multiple = TRUE),
      
      selectInput("select_np_type", "Newspaper type",
                  choices = np_types,
                  selected = np_types, 
                  multiple = TRUE)
    ),
    
    mainPanel(width = 8,
      tabsetPanel(
    #### home ####
    tabPanel("Home",
             fluidRow(h5("Hello!"),
                      p("This app is a prototype for the app for DIGAR collection data exploration. At this point the app allows to look into the metadata on the level of", em("issues"), "and filter them according to the user's interests."),
                      p("The", code("sidebar"), "on the left allows to select a range of ", strong("years of publishing"), " as well as to choose ", strong("language"), " ", strong("country"), " and ", strong("type"), " of a newspaper. These options works in every tab except for the", code("Timeline"), ", since the latter uses different types of data and goes beyond simple selection and search. All the tabs present summarised info for all newspapers available in the DIGAR metadata for the sidebar selection, more precise search of a particular newspaper titles is possible using ", code("Timeline"), " and ", code("Newspaper search"), ". Happy exploration!"),
                      p(""),
                      p("Description of tabs:"),
                      p(code("Metadata"), " - a table showing basic metadata for all the newspapers finded according to the filters in the sidebar;"),
                      p(code("Newspaper search"), " - search by title which will return a visualisation of the newspaper's issues distribution over time and a table with newspaper's metadata;"),
                      p(code("Timeline"), " - comparison of the newspapers' lifespan: the visualisation allows to search by a newspaper's title and compare its years of publishing with other newspapers;"),
                      p(code("Languages", "Countries", "Types"), " - visualisation of the languages used in the newspapers, countries of publising or types of newspapers distributed over time."))
             ),
    
    #### metadata ####
    tabPanel("Metadata",
             fluidRow(
               h3("Explore collection"),
               p("The table provides with basic metadata according to the current sidebar selection. All columns can be filtered and sorted, resulting table can be downloaded using the button below."),
               reactableOutput("search_table")
             ),
             fluidRow(
               csvDownloadButton("search_table", filename = "search_output.csv")
             )
             
          ),
    #### np search ####
    
    tabPanel("Newspaper search",
             fluidRow(
               h3("Search by title"),
               p("Here one or multiple newspapers can be searched by title and filtered using sidebar. The output displays the issues distribution on a timeline and newspaper's metadata in the table"),
               p(strong("All titles are included in the search, but if there is no output in the plot and the table, the sidebar options such as year range and language should be loosened."), "The table below show summarised information for each newspaper.")
               ),
             fluidRow(
               selectizeInput(
                 "search_title", "Choose newspaper(s):",
                 multiple = TRUE,
                 choices = NULL
               )
             ),
             fluidRow(
               plotlyOutput("search_title_plot")
             ),
             fluidRow(
               reactableOutput("search_title_table")
             )
    ),
    
    #### timeline ####
    tabPanel("Timeline",
             fluidRow(h3("Compare periods of newspaper publishing on a timeline")),
             fluidRow(
               p("This plot allows to compare for how long newspapers were issued."), 
               p("The newspaper can be selected from the list or searched by typing the beginning of the title (click at the empty space inside the box and select or start typing)."), 
               p("Selected newspapers", strong("can be"), "filtered by languages of issue (select", code("langauges"), "at the sidebar on the left)."),
               p("However,", code("Year of issue", "country"), "and", code("type"), 
                 "selection options", strong("are not applicable"), "for this plot"),
               p("By default 5 the longest running newspapers are displayed."),
               
               selectizeInput(
                 "newspaper", "Select the title", multiple = TRUE,
                 choices = NULL
               ), 
               helpText("Additional information (country and language of issue as well as exact dates of issuing) are showed when pointing cursore at the beginning of each horisontal bar.")
             ),
             fluidRow(
               plotlyOutput("timeline")
             )
    ),
    
    #### languages ####
    tabPanel("Languages",
  
      fluidRow(h3("Language distribution in the newspapers"),
               p("This page allows to select languages and a time period to see total number of issues on a timescale together with the data on countries of publishing and calculation of total number of issues (see tables below).")),
      
      fluidRow(
        column(12,
               plotlyOutput("plot_languages"))
      ),
      fluidRow(),
      fluidRow(
        column(5, offset = 1,
               p("Total number of issues in selected languages"),
               reactableOutput("lang_total")),
        column(6, 
               p("Top 10 newspapers with most issues in the period"), 
               reactableOutput("title"))
      )
    ),
    
    #### countries ####
    tabPanel("Countries",
             
             fluidRow(h3("Countries of publishing"),
                      p("The plot shows number of issues published in Estonia and abroad on a timescale.")
              ),
            
             fluidRow(
               plotlyOutput("plot_countries")
             ),
             fluidRow(),
             fluidRow(
               column(6,
                      p("Total number of issues per country during the period"),
                      tableOutput("table_countries"))
          )
        ),
    #### np types ####
    tabPanel("Types",
             
             fluidRow(h3("Types of newspaper"),
                      p("The metadata provides several categories for the newspapers issued before 2016 categorising the regularity of a newspaper issuing (daily / weekly newspapers) or category (exile, institutional, local, etc.).")
                      ),
             
             fluidRow(
               plotlyOutput("plot_types")
             ),
             fluidRow(),
             fluidRow(
               column(6,
                      tableOutput("table_types")
                      ),
               column(6,
                      p("Countries of issue of", strong("exile"), "newspapers"),
                      tableOutput("exile_countries")
                      )
               )
        )
      )         
    )
  )
)  

##################
##################
##### server #####
server <- function(input, output, session) {
  
  # issues data to reactive 
  selected_r <- reactive({
    issues %>% 
      filter(year > input$year[1], year < input$year[2]) %>% 
      filter(language %in% c(input$select_lang)) %>% 
      filter(country %in% c(input$select_countries) & country != "") %>% 
      filter(section %in% input$select_np_type)
  })
  
  #### metadata search table ####
  
  output$search_table <- renderReactable({
    t <- selected_r() %>% 
      select(title, language, country, place, publisher, start, end, 
             pages_exist, sections_exist, permalink, ester_id)
      
    reactable(t,
              searchable = FALSE,
              paginationType = "simple",
              defaultPageSize = 5, 
              defaultSorted = "title",
              sortable = TRUE, showSortable = TRUE,
              fullWidth = FALSE, 
              columns = list(
                title = colDef(
                  sticky = "left",
                  style = list(backgroundColor = "#f7f7f7"),
                  filterable = TRUE
                ),
                place = colDef(
                  filterable = TRUE
                ),
                publisher = colDef(
                  filterable = TRUE
                ),
                permalink = colDef(cell = function(value) {
                  htmltools::tags$a(href = value, target = "_blank", value)
                }),
                ester_id = colDef(cell = function(value) {
                  htmltools::tags$a(href = value, target = "_blank", value)
                })
                )
    )
  })
  
  #### languages ####
  
  output$lang_total <- renderReactable({
    t <- selected_r() %>% 
      group_by(language) %>% 
      summarise(Newspapers = n(),
                Issues = sum(n)) %>% 
      arrange(desc(Issues))
    
    reactable(t,
              sortable = FALSE, 
              fullWidth = TRUE)
  })
  
  output$title <- renderReactable({
    t <- selected_r() %>% 
      group_by(keyid) %>% 
      summarise(years_published = n(),
                total_issues = sum(n)) %>% 
      ungroup() %>% 
      arrange(desc(total_issues)) %>% 
      group_by(keyid = ifelse(row_number() < 11, keyid, "Other")) %>% 
      summarise(across(everything(), sum)) %>% 
      mutate(years_published = ifelse(keyid == "Other", "NA", years_published)) %>% 
      arrange(keyid == "Other", -total_issues) %>% 
      rename(`Shortened title` = keyid,
             `Years published` = years_published,
             `Issues total` = total_issues)
    
    reactable(t,
              sortable = TRUE, showSortable = TRUE,
              fullWidth = TRUE)
    
  })
  
  output$plot_languages <- renderPlotly({
    
    if (nrow(selected_r()) < 1 ) {
      
      p <- selected_r() %>% 
        ggplot(aes(x = as.factor(year), y = n)) + 
        labs(x = "No values available for this selection",
             y = "") + 
        theme(axis.text = element_text(color = "red"))
      
    } else { 
    
    p <- selected_r() %>% 
      group_by(year) %>% 
      ggplot(aes(x = year, 
                 y = n, 
                 text = paste0("Title: ", keyid), 
                 fill = language)) + 
      geom_col() + 
      labs(y = "Number of issues", 
           x = "Year",
           fill = "Language") + 
      scale_fill_manual(values = pallette) + 
      scale_y_continuous(labels = label_number(accuracy = 1))
    
    ggplotly(p)
      
    }
    })
  
  #### countries ####
  
  output$plot_countries <- renderPlotly({
    
    if (nrow(selected_r()) < 1 ) {
      p <- selected_r() %>% 
        ggplot(aes(x = year, y = n)) + 
        labs(x = "No values available for this selection",
             y = "") + 
        theme(axis.text = element_text(color = "red"))

    } else {

      p <- selected_r() %>% 
        group_by(year) %>% 
        ggplot(aes(x = year, 
                   y = n, 
                   text = paste0("Title: ", keyid), 
                   fill = country)) + 
        geom_col() + 
        labs(y = "Number of issues", 
             x = "Year",
             fill = "Country") + 
        scale_fill_manual(values = pallette)
      
      ggplotly(p)
     }
  })
  
  output$table_countries <- renderTable(
    selected_r() %>% 
      count(country, sort = T) %>% 
      rename(Country = country,
             Issues = n),
    width = "100%"
  )
  
  #### types ####
  
  output$plot_types <- renderPlotly({
    
    if (nrow(selected_r()) < 1 ) {
      p <- selected_r() %>% 
        ggplot(aes(x = as.factor(year), y = n)) + 
        labs(x = "No values available for this selection",
             y = "") + 
        theme(axis.text = element_text(color = "red"))
    } else { 
      
      p <- selected_r() %>% 
        filter(DocumentType == "NEWSPAPER") %>% 
        group_by(year) %>% 
        ggplot(aes(x = year, y = n, 
                   fill = section, text = paste0("Title: ", keyid))) + 
        geom_col() + 
        scale_fill_manual(values = pallette) + 
        labs(x = "Year", y = "Number of issues", fill = "Type")
      
      ggplotly(p)
    }
  })
  
  output$table_types <- renderTable(
    selected_r() %>% 
      count(section, sort = T) %>% 
      rename(`Type of newspaper` = section,
             `Number of issues` = n),
    width = "100%"
  )
  
  output$exile_countries <- renderTable(
    selected_r() %>% 
      filter(section == "Exile newspaper") %>% 
      count(country, sort = T) %>% 
      rename(Country = country,
             `Number of issues` = n),
    width = "100%"
  )
  
  #### select np ####
  updateSelectizeInput(session, "search_title", choices = title_id, server = TRUE,
                       selected = c("paevalehtew", "postimeesew"))
  
  selected_t <- reactive(
    selected_r() %>% 
      filter(keyid %in% c(input$search_title))
  )
  
  output$search_title_plot <- renderPlotly({
    p <- selected_t() %>% 
      group_by(year) %>% 
      ggplot(aes(x = year, y = n, fill = keyid)) + 
      geom_col() + 
      labs(x = "Year", y = "Number of issues") + 
      theme(legend.position = "none") + 
      scale_fill_manual(values = pallette)
    
    ggplotly(p)
  })
  
  output$search_title_table <- renderReactable({
    t <- selected_t() %>% 
      select(title, language, country, place, publisher, 
             section, access, pages_exist, sections_exist, permalink, ester_id) %>% 
      distinct() %>% 
      rename(type = section)
    
    reactable(t, 
              searchable = TRUE,  
              defaultSorted = "title",
              sortable = TRUE, showSortable = TRUE,
              paginationType = "simple",
              defaultPageSize = 5,
              fullWidth = FALSE,
              columns = list(
                title = colDef(
                  sticky = "left",
                  style = list(backgroundColor = "#f7f7f7")
                ),
                permalink = colDef(cell = function(value) {
                  htmltools::tags$a(href = value, target = "_blank", value)
                }),
                ester_id = colDef(cell = function(value) {
                  htmltools::tags$a(href = value, target = "_blank", value)
                })
              ))
    
  })
  
  #### timeline ####
  updateSelectizeInput(session, "newspaper", choices = title_id, server = TRUE, 
                       selected = c("estpostimjututuba", "sakalaew", "tallorahvak", 
                                    "ekmteataja", "JVeestirootsiselts"))
  
  
  d_time <- reactive({
    
    issues %>% 
      select(keyid, title, start, end, language, country, l_color, tooltip) %>% 
      distinct() %>% 
      filter(keyid %in% c(input$newspaper)) %>% 
      filter(language %in% c(input$select_lang))
      
  })
  
  
  output$timeline <- renderPlotly({
    
    if (nrow(d_time()) < 1) {
      
      d_empty <- tibble(keyid = "no_value",
                        title = "Please choose the newspaper name <b>and</b> at at least one language",
                        start = "1900-01-01", 
                        end = "2000-01-01", 
                        tooltip = "Please choose the newspaper name", 
                        l_color = "#FDD262") 
      
      vistime(
        d_empty,
        col.event = "title", 
        col.group = "keyid", 
        col.color = "l_color",
        col.tooltip = "tooltip"
      )
      
    } else {
      vistime(
        d_time(), 
        col.event = "title", 
        col.group = "keyid", 
        col.color = "l_color",
        col.tooltip = "tooltip"
      )
    }
    
  })

}


shinyApp(ui, server)

