library(shiny)
library(bslib)
library(fontawesome)
library(curl)
library(shinyjs)

# Define the MHCLG color palette
mhclg_colors <- list(
  teal = "#00625E",
  black = "#000000",
  white = "#FFFFFF",
  pink = "#932A72",
  red = "#85292A",
  orange = "#BF4A1D",
  green = "#40611f",
  blue = "#205083",
  indigo = "#333366",
  grey = "#535453"
)

# Create a custom theme
mhclg_theme <- bs_theme(
  version = 5,
  primary = mhclg_colors$teal,
  secondary = mhclg_colors$indigo,
  success = mhclg_colors$green,
  info = mhclg_colors$blue,
  warning = mhclg_colors$orange,
  danger = mhclg_colors$red,
  base_font = font_google("Open Sans"),  # Using Open Sans as a web-safe alternative to Arial
  font_scale = 0.9
)

# Custom CSS for hover effects and styling
custom_css <- "
  .nav-link:hover {
    font-weight: bold;
    color: #FFFFFF !important;
  }
  
  .sidebar-title {
    color: #FFFFFF !important;
    font-weight: bold;
  }
  
  .navbar-brand {
    font-weight: bold;
    color: #FFFFFF !important;
    display: flex;
    align-items: center;
  }
  
  .navbar {
    background-color: #00625E !important;
    border-bottom: 2px solid #00625E;
    height: 56px; /* Set fixed navbar height */
  }
  
  /* Logo styling - scaled down to be appropriate for the navbar */
  .app-logo {
    height: 70px; 
    margin-right: 15px;
    width: auto; /* Maintain aspect ratio */
  }

  /* Sidebar styling */
  .sidebar {
    background-color: #00625E !important;
    color: #FFFFFF !important;
  }

  /* Make accordion headers teal */
  .accordion-button {
    background-color: #00625E !important;
    color: #FFFFFF !important;
  }

  .accordion-button:not(.collapsed) {
    background-color: #00625E !important;
    color: #FFFFFF !important;
  }

  .accordion-button::after {
    background-color: #FFFFFF !important;
    border-radius: 50%;
  }

  /* Fix for accordion body background color */
  .accordion-body {
    background-color: #00625E !important;
    color: #FFFFFF !important;
    padding: 0 !important;
  }

  /* Style sidebar links */
  .sidebar .nav-link {
    color: #FFFFFF !important;
    padding: 8px 16px !important;
  }

  /* No tab highlighting in the navbar */
  .nav-item.active {
    display: none;
  }

  /* Content cards */
  .content-card {
    display: none;
    margin-bottom: 20px;
  }

  /* Show the introduction card by default */
  #introduction {
    display: block;
  }

  /* Custom link styling */
  .custom-nav-link {
    display: block;
    color: #FFFFFF !important;
    padding: 8px 16px;
    text-decoration: none;
    transition: all 0.3s ease;
  }

  .custom-nav-link:hover {
    background-color: rgba(255, 255, 255, 0.2);
    font-weight: bold;
  }

  /* Fix for accordion collapsed state */
  .accordion-collapse {
    background-color: #00625E !important;
  }

  /* Accordion padding fixes */
  .accordion-item {
    background-color: #00625E !important;
    border: none !important;
  }
  
  /* Fix margin around accordion items */
  .accordion {
    --bs-accordion-border-color: transparent !important;
    --bs-accordion-bg: #00625E !important;
    --bs-accordion-active-bg: #00625E !important;
    --bs-accordion-active-color: #FFFFFF !important;
    --bs-accordion-btn-color: #FFFFFF !important;
    --bs-accordion-btn-bg: #00625E !important;
    --bs-accordion-btn-focus-border-color: transparent !important;
    --bs-accordion-btn-focus-box-shadow: none !important;
  }

  /* Standalone nav item styling */
  .standalone-nav-item {
    padding: 10px 15px;
    border-bottom: 1px solid rgba(255, 255, 255, 0.1);
  }
"

ui <- page_fillable(
  theme = mhclg_theme,
  useShinyjs(),  # Add shinyjs for showing/hiding panels
  
  # Custom navbar without navigation tabs
  tags$div(
    class = "navbar navbar-expand-lg navbar-dark",
    style = "background-color: #00625E !important;",
    tags$div(
      class = "container-fluid",
      tags$span(
        class = "navbar-brand",
        # Add the actual logo image - replace the URL with your actual logo URL
        tags$img(
          src = "images/logo3.png",
          class = "app-logo",
          alt = "MHCLG Logo"
        ),
        "LGF Dashboard"
      )
    )
  ),
  
  # Layout with sidebar and main content
  layout_sidebar(
    sidebar = sidebar(
      width = 250,
      class = "sidebar",
      bg = "#00625E",
      
      div(class = "sidebar-title p-3", "Dashboard Navigation"),
      
      # Standalone navigation items for Introduction and Map Explorer
      div(
        class = "standalone-nav-item",
        actionLink("nav_introduction", "Introduction", class = "custom-nav-link")
      ),
      
      div(
        class = "standalone-nav-item",
        actionLink("nav_map_explorer", "Map Explorer", class = "custom-nav-link")
      ),
      
      # Main navigation items using accordion with improved styling for items with sub-pages
      accordion(
        id = "sidebarAccordion",
        accordion_panel(
          "LGF Overview",
          actionLink("nav_lgf_overview", "Summary", class = "custom-nav-link"),
          style = "padding: 0;"
        ),
        accordion_panel(
          "ASC",
          tags$div(
            style = "background-color: #00625E;",
            actionLink("nav_asc_spend", "Spend", class = "custom-nav-link"),
            actionLink("nav_asc_funding", "Funding", class = "custom-nav-link"),
            actionLink("nav_asc_detailed_spend", "Detailed Spend", class = "custom-nav-link"),
            actionLink("nav_asc_context", "ASC in Context", class = "custom-nav-link")
          ),
          style = "padding: 0;"
        )
      ),
      
      # Footer of sidebar
      div(
        class = "mt-auto p-3 small",
        style = "color: #FFFFFF;",
        "© 2025 Analysis - Local Government"
      )
    ),
    
    # Main content area with cards - replacing card_stack with a div container
    div(
      id = "main_content",
      style = "padding: 15px; height: 100%; overflow-y: auto;",
      
      # Introduction panel
      card(
        id = "introduction",
        class = "content-card",
        full_screen = TRUE,
        card_header("Welcome to the LGF Dashboard"),
        card_body(
          "This dashboard provides insights into Local Government Finance data. Use the navigation menu on the left to explore different sections."
        )
      ),
      
      # Map Explorer panel
      card(
        id = "map_explorer",
        class = "content-card",
        full_screen = TRUE,
        card_header("Map Explorer"),
        card_body(
          "Interactive map visualization will be displayed here."
        )
      ),
      
      # LGF Overview panel
      card(
        id = "lgf_overview",
        class = "content-card",
        full_screen = TRUE,
        card_header("LGF Overview Summary"),
        card_body(
          "Overview of Local Government Finance data and trends."
        )
      ),
      
      # ASC Spend panel
      card(
        id = "asc_spend",
        class = "content-card",
        full_screen = TRUE,
        card_header("Adult Social Care Spend"),
        card_body(
          "Analysis of Adult Social Care spending."
        )
      ),
      
      # ASC Funding panel
      card(
        id = "asc_funding",
        class = "content-card",
        full_screen = TRUE,
        card_header("Adult Social Care Funding"),
        card_body(
          "Overview of funding sources for Adult Social Care."
        )
      ),
      
      # ASC Detailed Spend panel
      card(
        id = "asc_detailed_spend",
        class = "content-card",
        full_screen = TRUE,
        card_header("Detailed Adult Social Care Spend Analysis"),
        card_body(
          "Detailed breakdown of Adult Social Care spending."
        )
      ),
      
      # ASC in Context panel
      card(
        id = "asc_context",
        class = "content-card",
        full_screen = TRUE,
        card_header("Adult Social Care in Context"),
        card_body(
          "Adult Social Care spend in the broader context of local government spend"
        )
      )
    )
  ),
  
  # Apply custom CSS
  tags$head(tags$style(HTML(custom_css)))
)

# Define server logic
server <- function(input, output, session) {
  # Create a list of navigation links and their corresponding content panels
  nav_links <- list(
    "nav_introduction" = "introduction",
    "nav_map_explorer" = "map_explorer",
    "nav_lgf_overview" = "lgf_overview",
    "nav_asc_spend" = "asc_spend",
    "nav_asc_funding" = "asc_funding",
    "nav_asc_detailed_spend" = "asc_detailed_spend",
    "nav_asc_context" = "asc_context"
  )
  
  # Set up click observers for each navigation link
  lapply(names(nav_links), function(link_id) {
    observeEvent(input[[link_id]], {
      # Hide all panels
      lapply(nav_links, function(panel_id) {
        shinyjs::hide(panel_id)
      })
      
      # Show the selected panel
      shinyjs::show(nav_links[[link_id]])
      
      # Remove active class from all nav links
      shinyjs::runjs("$('.custom-nav-link').removeClass('active');")
      
      # Add active class to the clicked link
      shinyjs::runjs(sprintf("$('#%s').addClass('active');", link_id))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)