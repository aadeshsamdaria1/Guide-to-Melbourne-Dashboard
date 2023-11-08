
############################################################################################
#This code is prepared for Assignment 03 of GEOM90007 Information Visualisation by group4. #
############################################################################################

library(sf)
library(dplyr)
library(stringr)
library(leaflet)
library(purrr)
library(shiny)
library(ggplot2)
library(ggiraph)
library(shinyjs)

##################################################
#Input datasets, images, sources into 
source('tableau-in-shiny-v1.0.R')
data_og = read.csv("MelbAirbnb.csv",header = TRUE)
icon_path = "285659_marker_map_icon.png"
banner = "banner-melbourne.jpg"

##################################################
#Let the window be presented on browser
options(shiny.launch.browser = TRUE)

##################################################
#Clean dataset of Melbourne Airbnb

data = dplyr::filter(data_og, neighbourhood_cleansed == "Melbourne")
data = data[, c(2, 12, 6, 16, 28, 29, 31, 32, 33, 34, 35, 38, 41)]
data <- data %>%
  mutate(
    Airbnb_Type = str_extract(name, "^[^·]*"),  
    Rating = paste0("★", str_extract(name, "(?<=★)[\\d.]+")),  
    Response_Message = paste("The host will respond ", host_response_time, sep = "", "."),
    neighbourhood = str_extract(neighbourhood, "^[^,]*"),
    neighbourhood = ifelse(neighbourhood %in% c("Southbank", "Southbank Melbourne"), "Southbank", neighbourhood),
    Airbnb_Type = str_replace(Airbnb_Type, "\\sin.*", ""),
    Airbnb_Name = paste0(host_name, "'s ", Airbnb_Type),  
  ) %>%
  select(-name)  %>%
  mutate_all(~replace(., . %in% c("N/A", "★NA"), NA)) %>%
  na.omit()

data$price <- as.numeric(gsub("[$,]", "", data$price))
data <- data %>% filter(price != max(price, na.rm = TRUE))

neighbourhood_choices <- trimws(data$neighbourhood)  
neighbourhood_choices <- unique(neighbourhood_choices)  
neighbourhood_choices <- sort(neighbourhood_choices)    
neighbourhood_choices <- neighbourhood_choices[neighbourhood_choices != ""]  

column_layout = function(choices){
  len = length(choices)
  n = ceiling(len / 3) 
  list(choices[1:n], choices[(n+1):(2*n)], choices[(2*n+1):len])
}

columns = column_layout(neighbourhood_choices)

##################################################
#Set up the colour for the banner of the interface
navbarCSS <- "
  .navbar {   
  background-color: #000000 !important; 
}
  
.navbar-default .navbar-nav > li > a {
  color: #c7c9cd !important;
}

.navbar-default .navbar-brand {
  color: #c7c9cd !important;
}

.navbar-default .navbar-nav > li > a:hover {
  color: white !important;
}

"

homeTabCSS <- "
  #homeDivId {
    background-color: #000000;
    position: absolute;
    top: 0;
    bottom: 0;
    left: 0;
    right: 0;
    overflow-y: auto;
  }
"

##################################################
## Set up UI components and server logic for individual sections

############
####Home####
############
homeUI <- function(id) {
  div(
    id = id,
    tags$div(
      style = "text-align: center;",
      img(src = 'PHOTO-2023-10-20-13-28-57.jpg', height = 400, width = 1200),
      tags$p(
        style = "font-size: 24px; padding: 20px; color: white; align: justify;",
        HTML("<div style='text-align: center; color: white; font-size: 20px;'>
  Welcome to Guide to Melbourne!<br/><br/>
  Explore the beautiful city of Melbourne with our guide. Use the tabs above to find information on:<br/>
  <ul style='list-style-type: disc; text-align: left; padding-left: 20px; display: inline-block;'>
    <li>Accommodation - Customise your accommodation options using this tab to find the perfect place to stay in Melbourne.</li>
    <li>Transport - Discover how you can navigate Melbourne by Bus/Tram/Car with ease!</li>
    <li>Restaurants - Locate your must-visit restaurants in this tab!</li>
    <li>Activities - Discover a variety of activities that match your interests.</li>
  </ul><br/>
  Start your Melbourne adventure now!
</div>")
      ),
      tags$style(type = 'text/css', homeTabCSS)
    )
  )
}

############
##Landmark##
############
landmarksUI <- function(id) {
  div(id = id)
}
landmarksServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    shinyjs::runjs("
      var placeholderDiv = document.getElementById('landmarksViz');
      placeholderDiv.style.width = '1200px';  // Set the desired width here
      placeholderDiv.style.height = '800px'; // Set the desired height here
      var url = 'https://public.tableau.com/views/LandmarksFinal1/editMelbourneLandmarks2?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link';
      var options = { 
        hideTabs: true, 
        width: '1500px',     // Set the desired width here
        height: '800px'     // Set the desired height here
      };
      new tableau.Viz(placeholderDiv, url, options);
    ")
  })
}

###################
##Outdoor Artwork##
###################
landmarksUI <- 
outdoorArtworksUI <- function(id) {
  div(id = id)
}
outdoorArtworksServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    shinyjs::runjs("
      var placeholderDiv = document.getElementById('outdoorArtworksViz');
      placeholderDiv.style.width = '1200px';  
      placeholderDiv.style.height = '800px'; 
      var url = 'https://public.tableau.com/views/OutdoorArtworkFinal/editMelbourneOutdoorArtworks2?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link';
      var options = { 
        hideTabs: true, 
        width: '1500px',     
        height: '800px'     
      };
      new tableau.Viz(placeholderDiv, url, options);
    ")
  })
}

####################
##Self-guided Walk##
####################
SelfGuidedWalksUI <- function(id) {
  div(id = id)
}
SelfGuidedWalksServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    shinyjs::runjs("
      var placeholderDiv = document.getElementById('SelfGuidedWalksViz');
      placeholderDiv.style.width = '1200px';  // Set the desired width here
      placeholderDiv.style.height = '800px'; // Set the desired height here
      var url = 'https://public.tableau.com/views/Self-GuidedWalksFinal/editMelbourneSelfGuidedWalks2?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link';
      var options = { 
        hideTabs: true, 
        width: '1500px',    
        height: '800px'   
      };
      new tableau.Viz(placeholderDiv, url, options);
    ")
  })
}

##################
####Restaurant####
##################
restaurantsUI <- function(id) {
  div(id = id)
}
restaurantsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    shinyjs::runjs("
      var placeholderDiv = document.getElementById('restaurantsViz');      
      placeholderDiv.style.width = '1200px';  
      placeholderDiv.style.height = '800px'; 
      var url = 'https://public.tableau.com/views/RestaurantsBars_edit/Dashboard12?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link';
      var options = { 
        hideTabs: true, 
        width: '1500px',     
        height: '800px'   
      };
      new tableau.Viz(placeholderDiv, url, options);
    ")
  })
}

##############
##Transport##
##############
transportUI <- function(id) {
  div(id = id)
}
transportServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    shinyjs::runjs("
  var placeholderDiv = document.getElementById('transportViz');
      placeholderDiv.style.width = '1200px';  // Set the desired width here
      placeholderDiv.style.height = '800px'; // Set the desired height here
  var url = 'https://public.tableau.com/views/TransportationFinal/TransportDashboard2?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link';
      var options = { 
        hideTabs: true, 
        width: '1500px',     
        height: '800px'   
      };
      new tableau.Viz(placeholderDiv, url, options);
    ")
  })
}

##################
##Accommodations##
##################
accommodationUI <- fluidPage(
  tags$image(src="PHOTO-2023-10-20-19-04-25.jpg", width="1450px", height="130px"),
  tags$style(navbarCSS),
  tags$style(type = 'text/css', "
  .shiny-options-group {
    width: 100%;
    text-align: left;
  }
  .shiny-options-group > label {
    width: 30%;
    display: inline-block;
    vertical-align: top;
  }
  .shiny-options-group > label:hover {
    color: red; /* Change the color to your desired color */
  }
")
  ,
  sidebarLayout(
    sidebarPanel(style= "background-color: #dfdfe8;",
      numericInput("guests",
                   label = "Number of Guests:",
                   value = 0,
                   min = 0,
                   max = 16),
      sliderInput(inputId = "price_range",
                  label = "Price Range:",
                  min = 0,
                  max = 2000,
                  value = c(0, 2000),
                  ticks = TRUE,
                  step = 50,
                  sep = ""),
      h3("City:", style = "font-size: 15px; font-weight: bold;"),
      div(
        style = "display: inline-block; width: 32%; vertical-align: top;",
        checkboxGroupInput("neighbourhoods1", NULL, choices = columns[[1]], selected = columns[[1]])
      ),
      div(
        style = "display: inline-block; width: 32%; vertical-align: top;",
        checkboxGroupInput("neighbourhoods2", NULL, choices = columns[[2]], selected = columns[[2]])
      ),
      div(
        style = "display: inline-block; width: 32%; vertical-align: top;",
        checkboxGroupInput("neighbourhoods3", NULL, choices = columns[[3]], selected = columns[[3]])
      ),
      selectInput(inputId = "min_rating",
                  label = "Minimum Rating:",
                  choices = 1:5,
                  selected = 1),  
      actionButton("done", "Done")
    ),
    mainPanel(
      leafletOutput("map", height = "890px")
    )
  )
)

##################################################
#Define the mainNavPanel
mainNavPanel <- navbarMenu(
  title = 'Activities',
  tabPanel(title='Landmarks', landmarksUI("landmarksViz")),
  tabPanel(title='Outdoor Artworks', outdoorArtworksUI("outdoorArtworksViz")),
  tabPanel(title='Self Guided Walks', SelfGuidedWalksUI("SelfGuidedWalksViz"))
)

##################################################
#Define the navbarPage
ui <- navbarPage(
  header = tagList(
    tags$script(src = "https://public.tableau.com/javascripts/api/tableau-2.min.js"),
    useShinyjs(),
    setUpTableauInShiny()
  ),
  title="Guide to Melbourne",
  id = "tabs",
  tags$style(HTML(".navbar .navbar-brand { font-weight: bold; }")),
  tabPanel(
    title='Home',
    homeUI("homeDivId"),
  ),
  tabPanel("Accommodation", accommodationUI),
  tabPanel(title='Transport', transportUI('transportViz')),
  tabPanel(title='Restaurants', restaurantsUI("restaurantsViz")),
  mainNavPanel
)

##################################################
# Set up server functions for the Shiny application
server <- function(input, output, session) {
  data$numeric_rating <- as.numeric(gsub("★", "", data$Rating))
  my_icon <- makeIcon(
    iconUrl = icon_path,
    iconWidth = 16, 
    iconHeight = 16,
    iconAnchorX = 4,
    iconAnchorY = 4
  )
  output$map <- renderLeaflet({
    five_star_data <- data %>% filter(Rating == "★5.0")
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.VoyagerLabelsUnder) %>%
      addMarkers(
        ~longitude, 
        ~latitude, 
        popup = ~paste(
          "<span style='font-size: 20px;'><strong>", Airbnb_Name, "</strong></span><br>",
          "Website ", 
          sprintf("<a href='%s' target='_blank'><strong>%s</strong></a>", listing_url, listing_url), "<br>",
          "Rating <strong>", Rating, "</strong><br>",
          "Price(AU$) <strong>", price, "</strong><br>",
          "Guests <strong>", accommodates, "</strong><br>",
          "Rooms <strong>", bedrooms, "</strong><br>",
          "Response <strong>", Response_Message, "</strong>"
        ),
        icon = my_icon,
        options = markerOptions(opacity = 0.8)
      ) %>%
      setView(lat = -37.81224, lng = 144.96013, zoom = 14)
  })
  observeEvent(input$done, {
    selected_neighbourhoods <- c(input$neighbourhoods1, input$neighbourhoods2, input$neighbourhoods3)
    filtered_data <- data %>%
      filter(accommodates == input$guests &
               price >= input$price_range[1] & price <= input$price_range[2] &
               neighbourhood %in% selected_neighbourhoods &
               numeric_rating >= as.numeric(input$min_rating))
    cat("Selected Neighbourhoods:\n")
    print(selected_neighbourhoods)
    cat("Number of records after filtering:", nrow(filtered_data), "\n")
    leafletProxy("map", data = filtered_data) %>%
      clearMarkers() %>%
      addMarkers(
        ~longitude, 
        ~latitude, 
        popup = ~paste(
          "<span style='font-size: 20px;'><strong>", Airbnb_Name, "</strong></span><br>",
          "Website ", 
          sprintf("<a href='%s' target='_blank'><strong>%s</strong></a>", listing_url, listing_url), "<br>",
          "Rating <strong>", Rating, "</strong><br>",
          "Price(AU$) <strong>", price, "</strong><br>",
          "Guests <strong>", accommodates, "</strong><br>",
          "Rooms <strong>", bedrooms, "</strong><br>",
          "Response <strong>", Response_Message, "</strong>"
        ),
        icon = my_icon
      )
  })
  observe({
    if(input$tabs == "Landmarks") {
      landmarksServer("landmarksViz")
    }
    if(input$tabs == "Outdoor Artworks") {
      outdoorArtworksServer("outdoorArtworksViz")
    }
    if(input$tabs == "Self Guided Walks") {
      SelfGuidedWalksServer("SelfGuidedWalksViz")
    }
    if(input$tabs == "Restaurants") {
      restaurantsServer("restaurantsViz")
    }
    if(input$tabs == "Transport") {
      transportServer("transportViz")
    }
  })
}

shinyApp(ui = ui, server = server)
