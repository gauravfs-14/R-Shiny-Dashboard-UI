library(shiny)
library(leaflet)
library(highcharter)
library(reactable)

ui <- fluidPage(
  # Global CSS for consistent styling
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
        background-color: #1e1e2f;
        color: white;
        margin: 0;
        padding: 0;
      }
      .navbar {
        background-color: #1e1e2f !important;
        border-bottom: 1px solid #444;
        padding: 10px 20px;
        display: flex;
        align-items: center;
        font-size: 1em;
      }
      .navbar-brand {
        color: white !important;
        font-size: 1.25em;
        font-weight: bold;
      }
      .navbar-nav {
        display: flex;
        align-items: center;
        list-style: none;
        padding: 0;
        margin-left: 20px;
      }
      .navbar-nav .nav-item {
        margin-right: 20px;
      }
      .navbar-nav .nav-link {
        color: white !important;
        text-decoration: none;
      }
      .navbar-nav .nav-link:hover {
        color: #00bcd4 !important;
      }
      .content-row {
        display: flex;
        height: calc(100vh - 100px); /* Full height minus navbar */
      }
      .left-panel, .right-panel {
        flex: 1;
        display: flex;
        flex-direction: column;
        height: 100%;
        overflow: hidden;
      }
      .middle-panel {
        flex: 2;
        display: flex;
        flex-direction: column;
        height: 100%;
        overflow: hidden;
      }
      .card {
        background-color: #1e1e2f;
        border: 1px solid #444;
        border-radius: 5px;
        padding: 10px;
        margin-bottom: 10px;
        flex: 1;
        overflow: hidden;
        margin-left: 10px;
        margin-right: 10px;
        position: relative;
      }
      #map {
        height: 100% !important;
      }
      .select-city {
        position: absolute;
        top: 15px;
        right: 15px;
        z-index: 1000;
        background-color: #1e1e2f;
        color: white;
        border: 1px solid #444;
        border-radius: 5px;
        padding: 5px 10px;
        font-family: 'Arial', sans-serif;
      }
      .table-container {
        display: flex;
        flex-direction: column;
        height: 100%;
      }
      .table-content {
        flex: 1;
        overflow-y: auto;
        overflow-x: auto;
        scrollbar-width: thin;
        scrollbar-color: #888 #1e1e2f; /* Custom scrollbar for Firefox */
      }
      .table-content::-webkit-scrollbar {
        width: 8px;
        height: 8px;
      }
      .table-content::-webkit-scrollbar-thumb {
        background: #888;
        border-radius: 4px;
      }
      .table-content::-webkit-scrollbar-thumb:hover {
        background: #555;
      }
      .leaflet-container {
        background-color: #1e1e2f !important;
      }
      .select-city .form-control {
        background-color: #1e1e2f !important;
        color: white !important;
        border-radius: 5px !important;
      }
      .select-city .form-control:focus {
        border-color: #00bcd4 !important;
        box-shadow: 0 0 5px #00bcd4 !important;
      }
      .select-city .form-control option {
        background-color: #1e1e2f !important;
        color: white !important;
      }

")),
    tags$script(HTML("
  $(document).ready(function() {
    // Apply custom styles to the select input
    $('.select-city select').addClass('form-control');
    $('.select-city select').css({
      'background-color': '#1e1e2f',
      'color': 'white',
      'border': '1px solid #444',
      'border-radius': '5px'
    });
  });
"))
    
  ),
  
  # Navbar
  fluidRow(
    column(12, 
           tags$nav(
             class = "navbar",
             tags$a(class = "navbar-brand", href = "#", "Dashboard"),
             tags$ul(
               class = "navbar-nav",
               tags$li(class = "nav-item", 
                       tags$a(class = "nav-link", href = "#", "Home")),
               tags$li(class = "nav-item", 
                       tags$a(class = "nav-link", href = "#", "Explorer")),
               tags$li(class = "nav-item", 
                       tags$a(class = "nav-link", href = "#", "Project"))
             )
           )
    )
  ),
  
  # Main content layout
  div(class = "content-row",
      # Left-side table
      div(class = "left-panel",
          div(class = "card table-container",
              div(class = "table-header",
                  tags$h5("All Departures", style = "color: white; font-size: 1.25em;"),
              ),
              div(class = "table-content",
                  reactableOutput("data_table")
              )
          )
      ),
      
      # Map in the center
      div(class = "middle-panel",
          div(class = "card map-container",
              div(class = "select-city", 
                  selectInput("city", "Select City:", choices = c("Auckland", "Wellington", "Christchurch"), selected = "Auckland")
              ),
              leafletOutput("map")
          )
      ),
      
      # Right-side bar graphs
      div(class = "right-panel",
          div(class = "card chart-container",
              tags$h5("Mode Of Travel", style = "color: white; font-weight: bold;"),
              highchartOutput("bar_chart_1", height = "20%"),
              tags$p("Work at Home", style = "color: white; font-size: 12px;"),
              highchartOutput("bar_chart_2", height = "20%"),
              tags$p("Commute by Car", style = "color: white; font-size: 12px;"),
              highchartOutput("bar_chart_3", height = "20%"),
              tags$p("Use Public Transport", style = "color: white; font-size: 12px;"),
              highchartOutput("bar_chart_4", height = "20%")
          )
      )
  )
)

server <- function(input, output, session) {
  # Meaningful Dataset
  data <- data.frame(
    Suburb = c(
      "Eden Terrace", "Freemans Bay", "Ormiston South", "Morningside (Auckland)", 
      "Birkdale South", "Parnell West", "One Tree Hill", "Papatoetoe Central",
      "Mount Wellington Central", "Birkenhead West", "Point Chevalier East",
      "Greenlane North", "Avondale North (Auckland)", "Grey Lynn Central",
      "Kingsland", "Western Heights (Auckland)", "Mount Roskill White Swan"
    ),
    WorkAtHome = sample(150:400, 17),
    CommuteByCar = sample(800:2000, 17),
    PublicTransport = sample(300:900, 17),
    WalkOrJog = sample(50:300, 17),
    Bike = sample(10:50, 17),
    Total = sample(2000:5000, 17)
  )
  
  cities <- data.frame(
    City = c("Auckland", "Wellington", "Christchurch"),
    Latitude = c(-36.8485, -41.2865, -43.5321),
    Longitude = c(174.7633, 174.7762, 172.6362)
  )
  
  bar_data <- data.frame(
    City = rep(c("Auckland", "Wellington", "Christchurch"), each = 5),
    Category = rep(c("Work At Home", "Commute by Car", "Public Transport", "Walk or Jog", "Bike"), times = 3),
    Values = c(
      # Auckland
      30, 60, 40, 15, 10,
      # Wellington
      25, 50, 35, 20, 15,
      # Christchurch
      20, 70, 30, 10, 20
    )
  )
  
  # React to City Selection
  selected_city <- reactive({
    cities[cities$City == input$city, ]
  })
  
  # Render Table
  output$data_table <- renderReactable({
    reactable(
      data,
      searchable = TRUE,
      pagination = TRUE,
      defaultPageSize = 20,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE,
      resizable = TRUE,
      showPageSizeOptions = TRUE,
      theme = reactableTheme(
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)"),
        color = "white",
        backgroundColor = "#2b2b3d",
        borderColor = "#444",
        headerStyle = list(
          backgroundColor = "#1e1e2f",
          color = "white",
          fontWeight = "bold"
        ),
        highlightColor = "#444",
        searchInputStyle = list(backgroundColor = "#1e1e2f", color = "white", margin = "10px", width = "calc(100% - 20px)")
      )
    )
  })
  
  # Render Leaflet Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter, options = providerTileOptions(opacity = 0.8)) %>%
      setView(lng = selected_city()$Longitude, lat = selected_city()$Latitude, zoom = 11) %>%
      addMarkers(lng = selected_city()$Longitude, lat = selected_city()$Latitude, popup = selected_city()$City)
  })
  
  # Render Highcharts
  output$bar_chart_1 <- renderHighchart({
    hchart(bar_data[bar_data$Category == "Work At Home", ], "column", hcaes(x = City, y = Values), color = "lightblue") %>%
      hc_chart(backgroundColor = "#1e1e2f") %>%
      hc_title(text = NULL) %>%
      hc_xAxis(title = list(text = NULL), labels = list(style = list(color = "white"))) %>%
      hc_yAxis(title = list(text = NULL), labels = list(style = list(color = "white"), gridLineColor = "#444444"))
  })
  
  output$bar_chart_2 <- renderHighchart({
    hchart(bar_data[bar_data$Category == "Commute by Car", ], "column", hcaes(x = City, y = Values), color = "lightgreen") %>%
      hc_chart(backgroundColor = "#1e1e2f") %>%
      hc_title(text = NULL) %>%
      hc_xAxis(title = list(text = NULL), labels = list(style = list(color = "white"))) %>%
      hc_yAxis(title = list(text = NULL), labels = list(style = list(color = "white"), gridLineColor = "#444444"))
  })
  
  output$bar_chart_3 <- renderHighchart({
    hchart(bar_data[bar_data$Category == "Public Transport", ], "column", hcaes(x = City, y = Values), color = "lightyellow") %>%
      hc_chart(backgroundColor = "#1e1e2f") %>%
      hc_title(text = NULL) %>%
      hc_xAxis(title = list(text = NULL), labels = list(style = list(color = "white"))) %>%
      hc_yAxis(title = list(text = NULL), labels = list(style = list(color = "white"), gridLineColor = "#444444"))
  })
  
  output$bar_chart_4 <- renderHighchart({
    hchart(bar_data[bar_data$Category == "Bike", ], "column", hcaes(x = City, y = Values), color = "lightcoral") %>%
      hc_chart(backgroundColor = "#1e1e2f") %>%
      hc_title(text = NULL) %>%
      hc_xAxis(title = list(text = NULL), labels = list(style = list(color = "white"))) %>%
      hc_yAxis(title = list(text = NULL), labels = list(style = list(color = "white"), gridLineColor = "#444444"))
  })
}

shinyApp(ui, server)
