#### Header ####
## What: Data Visualization and Analytics Assessment (C7083)
## Who: Jaben Peter Darinyan Bako - 21322100
## last edited: 28-02-23


# CONTENTS ####
## Setup
## Scatter plot of the relationship between Abnormal drought and Abnormal wet using Base R
## Bar Chart of drought severity by state using ggplot 
## Drought Severity levels over time from 2000 to 2022 signig plotly
## Combination of a line chart and a scatter plot with jittered points of Abnormal drought conditions over time (ggplot)
## Interactive map of US showing the total drought sum


# Setup ####
# Load needed libraries
library(ggplot2)
library(tidyverse)
library(ggiraph)
library(viridisLite)
library(leaflet)
library(maps)
library(ggthemes)
library(sf)
library(plotly)
library(shiny)
library(knitr)
library(RColorBrewer)

# load the drought data from tidytuesday
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')

# Summary of the drought data
Variable <- c("0", "DATE", "D0", "D1", "D2", "D3", "D4", "-9", "W0", "W1", "W2", "W3", "W4", "state")
Class <- c("numeric", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character")
Description <- c("Percentage area with no drought",
                 "Date",
                 "Percentage area with Abnormal Drought",
                 "Percentage area with Moderately Drought ",
                 "Percentage area with severe drought",
                 "Percentage area with Extreme Drought",
                 "Percentage area with Exceptional Drought",
                 "Percentage area with missing data",
                 "Percentage area with Abnormal Wet conditions",
                 "Percentage area with Moderate Wet conditions",
                 "Percentage area with Severe Wet conditions",
                 "Percentage area with Extreme Wet conditions",
                 "Percentage area with Exceptional Wet conditions",
                 "State")
drought_data_frame <- data.frame(Variable, Class, Description)

knitr::kable(drought_data_frame, align = "l")


## Scatter plot of the relationship between Abnormal drought and Abnormal wet using Base R ####
# Calculate correlation coefficient of Abnormal drought and Abnormal wet
corr <- cor(drought$W0, drought$D0)

# Create plot
plot(drought$W0, drought$D0, 
     xlab = "Abnormal wet", ylab = "Abnormal drought", 
     main = "Relationship between Abnormal drought and Abnormal wet",
     sub = "Data from tidytuesday Github repo",
     col = ifelse(drought$D0 > drought$W0, "goldenrod", "black"))
abline(a = 1, b = 0, lty = 2, col = "magenta")

# Add correlation coefficient to plot
text(x = max(drought$W0), y = max(drought$D0), 
     labels = paste("r =", round(corr, 2)), pos = 2)


## Bar Chart of drought severity by state using ggplot ####

# Create the bar chart using the ggplot function
ggplot(drought, aes(x = state)) + 
  geom_bar(aes(y = D0, fill = "Abnormal drought"), stat = "identity") + # Add the bars
  geom_bar(aes(y = D1, fill = "Moderately Drought"), stat = "identity") +
  geom_bar(aes(y = D2, fill = "Severe Drought"), stat = "identity") +
  geom_bar(aes(y = D3, fill = "Extreme Drought"), stat = "identity") +
  geom_bar(aes(y = D4, fill = "Exceptional Drought"), stat = "identity") +
  labs(x = "State", y = "Drought Severity") +  # Add axis labels
  scale_fill_viridis_d(option = "G") + # Set color palette
  theme_minimal() +
  coord_flip() + # Flip the chart horizontally
  labs(title = "Bar Chart of Drought Severity by State",  # Add chart title and subtitle
       subtitle = "Data from tidytuesday Github repo"
  )


## Drought Severity levels over time from 2000 to 2022 usignig plotly####

# Convert DATE variable to date format
drought$DATE <- as.Date(substr(drought$DATE, 3, 10), format = "%Y%m%d")

# Subset data to include only observations from 2000 to 2022
drought_subset <- drought[drought$DATE >= as.Date("2000-01-01") & drought$DATE <= as.Date("2022-12-31"), ]

# Gather drought severity data into a long format for better plotting and filtering
drought_gathered <- drought_subset %>% 
  select(DATE, D0:D4) %>% 
  tidyr::gather(key = "severity", value = "value", D0:D4)

# Create a Plotly object with a dropdown menu to select drought severity levels
plot <- plot_ly(data = drought_gathered, x = ~DATE, y = ~value, color = ~severity, 
             colors = c("orange", "skyblue1", "yellow3", "purple3", "grey50"),
             type = "scatter", mode = "lines") %>% 
  layout(title = "Drought Severity Levels over Time from 2000 to 2022",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Drought Severity Level"),
         updatemenus = list(
           list(
             type = "buttons",
             direction = "left",
             buttons = list(
               list(method = "restyle",
                    args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE)),
                    label = "Abnormal Drought"),
               list(method = "restyle",
                    args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE)),
                    label = "Moderate Drought"),
               list(method = "restyle",
                    args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE)),
                    label = "Severe Drought"),
               list(method = "restyle",
                    args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE)),
                    label = "Extreme Drought"),
               list(method = "restyle",
                    args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE)),
                    label = "Exceptional Drought")))))

# Show the interactive plot
plot


## Combination of a line chart and a scatter plot with jittered points of Abnormal drought conditions over time (ggplot)####

# Filter the data
drought_filtered <- drought %>% 
  filter(DATE >= as.Date("2020-01-01") & DATE <= as.Date("2022-12-31"))

# Calculate percentages for each condition category
drought_filtered <- drought_filtered %>% 
  mutate(D0_m = D0 / sum(D0))

# Find the highest value and its corresponding date
max_value <- max(drought_filtered$D0_m) # Find the maximum value in the Abnormal drought column
max_value_rounded <- round(max_value, 4) # Round the maximum value to 4 decimal places
max_date <- drought_filtered$DATE[drought_filtered$D0_m == max_value][1] # Find the date where the maximum value occurred

# Create the plot using ggplot
ggplot(data = drought_filtered, aes(x = DATE, y = D0_m)) + # Set the data and the x- and y-axes
  geom_jitter(aes(color = "Observation"), size = 3, alpha = 0.7) + # Add jittered points to the plot with color
  geom_line(aes(group = 1, color = "Line"), size = 1) + # Add a line to the plot with color
  geom_point(aes(x = max_date, y = max_value, color = "Max"), size = 3) + # Add a point at the maximum value with color
  geom_text(aes(x = max_date, y = max_value, label = max_value_rounded),
            vjust = -1.5, size = 3, color = "red") + # Add text at the maximum value with label, position, size, and color
  labs(x = "Year", y = "Percentage of Observations (%)", # Add axis labels and title
       title = "Abnormal Drought Category from 2020-2022") +
  theme_classic() + # Set the theme to "classic"
  scale_color_manual(values = c("Observation" = "goldenrod", "Line" = "black", "Max" = "blue")) + # Set the colors for each element
  scale_y_continuous(limits = c(0, max(drought_filtered$D0_m) * 1.1)) # Set the y-axis limits

## Time series plot of drought and wet conditions over time (ggplot) ####

# Define state choices
state_choices <- c("All", unique(drought$state))

# Define state names and abbreviations
state_names <- state.name
state_abbreviations <- state.abb

#  Create UI components for the Shiny app
ui <- fluidPage(
  
  # Set the app title
  titlePanel("Drought Data Visualization"),
  
  # Create a sidebar layout with input controls
  sidebarLayout(
    sidebarPanel(
      
      # Create a drop-down menu for selecting a state
      selectInput("state", "Select a state:",
                  choices = state_choices, selected = "All"),
      
      # Create a slider for selecting the number of bins in the histogram
      sliderInput("bins", "Number of bins:",
                  min = 1, max = 50, value = 10)
    ),
    
    # Create the main panel that displays the histogram
    mainPanel(
      plotOutput("histogram")
    )
  )
)

#Define the server-side logic for the Shiny app
server <- function(input, output) {
  
  # Filter data based on selected state
  selected_data <- reactive({
    if (input$state == "All") {
      drought
    } else {
      drought %>% filter(state == input$state)
    }
  })
  
  # Calculate percentages for each condition category
  data_p <- reactive({
    selected_data() %>%
      mutate(D0_p = D0 / sum(D0),
             D1_p = D1 / sum(D1),
             D2_p = D2 / sum(D2),
             D3_p = D3 / sum(D3),
             D4_p = D4 / sum(D4),
             W0_p = W0 / sum(W0),
             W1_p = W1 / sum(W1),
             W2_p = W2 / sum(W2),
             W3_p = W3 / sum(W3),
             W4_p = W4 / sum(W4))
  })
  
  # Create histogram plot using ggplot
  output$histogram <- renderPlot({
    data <- data_p() # filter based on selected states
    ggplot(data = data_p(), aes(x = DATE, fill = state)) +
      geom_histogram(aes(weight = D0_p), bins = input$bins, alpha = 0.5) +
      geom_histogram(aes(weight = D1_p), bins = input$bins, alpha = 0.5) +
      geom_histogram(aes(weight = D2_p), bins = input$bins, alpha = 0.5) +
      geom_histogram(aes(weight = D3_p), bins = input$bins, alpha = 0.5) +
      geom_histogram(aes(weight = D4_p), bins = input$bins, alpha = 0.5) +
      geom_histogram(aes(weight = W0_p), bins = input$bins, alpha = 0.5) +
      geom_histogram(aes(weight = W1_p), bins = input$bins, alpha = 0.5) +
      geom_histogram(aes(weight = W2_p), bins = input$bins, alpha = 0.5) +
      geom_histogram(aes(weight = W3_p), bins = input$bins, alpha = 0.5) +
      geom_histogram(aes(weight = W4_p), bins = input$bins, alpha = 0.5) +
      
      # Set the fill color of the histogram bars
      scale_fill_viridis_d() +
                                              
      
      # Set x and y axis labels and fill legend title
      labs(x = "Year", y = "Number of Observations", fill = "Condition Category") +
      theme_classic()
  })
}

# View on shiny
shinyApp(ui, server)
     

## Interactive map of US showing the total drought sum ####

# Load US state data from maps package
states <- map_data("state")

# Aggregate drought data by state
state_drought <- drought %>%
  group_by(state) %>%
  summarise(total_drought = sum(D0, D1, D2, D3, D4))

# Join state_drought with states by state name
state_map_data <- left_join(states, state_drought, by = c("region" = "state"))

# Convert to spatial object and set projection
state_map_data_sf <- st_as_sf(state_map_data, coords = c("long", "lat"), crs = 4326)
state_map_data_sf <- st_transform(state_map_data_sf, 2163)

# Create a color palette
pal <- colorNumeric(palette = "YlGnBu", domain = state_map_data$total_drought)

# Plot using ggplot2
p <- ggplot() +
  geom_sf(data = state_map_data_sf, aes(fill = total_drought, tooltip = region)) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu"), na.value = "grey", name = "Total Drought Sum") +
  ggtitle("US Drought Data by State") +
  labs(subtitle = "Total drought severity by state") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        plot.margin = margin(0, 0, 0, 0, unit = "cm")) + # Remove plot margins
  geom_text(x = -85, y = 50, label = "Note pole", hjust = 1, vjust = 1, size = 5) + # Add note pole
  xlab("Longitude") + # Add x-axis label
  ylab("Latitude") + # Add y-axis label
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) # Adjust axis title font size

# Convert ggplot to plotly object for interactivity
ggplotly(p, tooltip = c("region", "total_drought")) %>%
  add_annotations(
    x = -100,
    y = 40,
    text = "Highest drought area",
    showarrow = TRUE,
    font = list(color = "white", size = 5),
    align = "center",
    bgcolor = "goldenrod",
    opacity = 0.4,
    xref = "x",
    yref = "y"
  )