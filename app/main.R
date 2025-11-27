<<<<<<< HEAD
# app/main.R
# WBES Business Environment Benchmarking Dashboard
# Main Application Entry Point

box::use(
  shiny[
    shinyApp, moduleServer, NS, reactive, reactiveVal, req, observe, observeEvent,
    tags, div, icon, HTML, fluidPage, navbarPage, tabPanel, fluidRow, column,
    selectInput, actionButton, renderUI, uiOutput
  ],
  bslib[
    bs_theme, page_navbar, nav_panel, nav_spacer, nav_item,
    card, card_header, card_body
  ],
  waiter[useWaiter, waiter_show, waiter_hide, spin_fading_circles]
)

box::use(
  app/logic/wbes_data[load_wbes_data, load_sample_data],
  app/view/overview,
  app/view/country_profile,
  app/view/benchmark,
  app/view/infrastructure,
  app/view/finance_access,
  app/view/data_quality,
  app/view/about
)

# Kwiz Research Theme
kwiz_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1B6B5F",
  secondary = "#F49B7A",
  success = "#2E7D32",
  warning = "#F4A460",
  danger = "#dc3545",
  info = "#17a2b8",
  base_font = "Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif",
  heading_font = "Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif"
)

#' @export
ui <- page_navbar(
  id = "main_nav",
  title = tags$span(
    tags$img(src = "static/favicon.ico", height = "28px", class = "me-2"),
    "WBES Dashboard"
  ),
  theme = kwiz_theme,
  fillable = TRUE,
  
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles/main.css"),
    tags$script(src = "js/index.js"),
    useWaiter()
  ),
  
  nav_panel(
    title = "Overview",
    value = "overview",
    icon = icon("globe"),
    overview$ui("overview")
  ),
  
  nav_panel(
    title = "Country Profile",
    value = "country",
    icon = icon("flag"),
    country_profile$ui("country_profile")
  ),
  
  nav_panel(
    title = "Benchmark",
    value = "benchmark",
    icon = icon("chart-bar"),
    benchmark$ui("benchmark")
  ),
  
  nav_panel(
    title = "Infrastructure",
    value = "infrastructure",
    icon = icon("bolt"),
    infrastructure$ui("infrastructure")
  ),
  
  nav_panel(
    title = "Finance",
    value = "finance",
    icon = icon("university"),
    finance_access$ui("finance_access")
  ),
  
  nav_panel(
    title = "Data Quality",
    value = "quality",
    icon = icon("clipboard-check"),
    data_quality$ui("data_quality")
  ),
  
  nav_spacer(),
  
  nav_panel(
    title = "About",
    value = "about",
    icon = icon("info-circle"),
    about$ui("about")
  ),
  
  nav_item(
    tags$a(
      href = "https://kwizresearch.com",
      target = "_blank",
      class = "nav-link",
      tags$span(class = "text-secondary-coral", "Kwiz Research")
    )
  ),
  
  footer = tags$footer(
    class = "bg-primary-teal text-white py-3 mt-4",
    fluidRow(
      column(6,
        tags$p(
          class = "mb-0",
          icon("database"), " Data: ",
          tags$a(
            href = "https://www.enterprisesurveys.org",
            target = "_blank",
            class = "text-white",
            "World Bank Enterprise Surveys"
          )
        )
      ),
      column(6,
        tags$p(
          class = "mb-0 text-end",
          "Developed by ",
          tags$a(
            href = "https://kwizresearch.com",
            target = "_blank",
            class = "text-secondary-coral",
            "Kwiz Computing Technologies"
=======
# app.R - Main Application Entry Point
# World Bank Enterprise Surveys - Business Environment Benchmarking Dashboard
# Rhino Framework Application

box::use(
  shiny[bootstrapPage, moduleServer, NS, tags, icon, HTML],
  bslib[
    bs_theme, bs_add_rules, nav_panel, nav_spacer, 
    nav_item, page_navbar, card, card_header, card_body
  ],
  waiter[useWaiter, waiterPreloader, spin_fading_circles]
)

box::use(
  app/view/mod_overview,
  app/view/mod_country_profile,
  app/view/mod_benchmark,
  app/view/mod_infrastructure,
  app/view/mod_finance_access,
  app/view/mod_data_quality,
  app/view/mod_about,
  app/logic/data_loader
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  # Kwiz Research Theme
  kwiz_theme <- bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B6B5F",
    secondary = "#F49B7A",
    success = "#2E7D32",
    info = "#17a2b8",
    warning = "#F4A460",
    danger = "#dc3545",
    bg = "#FFFFFF",
    fg = "#333333",
    base_font = "Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif",
    heading_font = "Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif"
  ) |>
    bs_add_rules(sass::sass_file("app/styles/main.scss"))
  
  page_navbar(
    id = ns("main_navbar"),
    title = tags$span(
      tags$img(
        src = "static/images/logo.svg",
        height = "35px",
        style = "margin-right: 10px; vertical-align: middle;"
      ),
      "Business Environment Benchmarking"
    ),
    theme = kwiz_theme,
    fillable = TRUE,
    bg = "#1B6B5F",
    inverse = TRUE,
    
    # Header with branding
    header = tags$head(
      tags$link(rel = "icon", type = "image/svg+xml", href = "static/images/favicon.svg"),
      tags$meta(name = "description", content = "World Bank Enterprise Surveys Dashboard"),
      tags$meta(name = "author", content = "Kwiz Computing Technologies"),
      useWaiter()
    ),
    
    # Navigation Panels
    nav_panel(
      title = "Overview",
      value = "overview",
      icon = icon("globe"),
      mod_overview$ui(ns("overview"))
    ),
    
    nav_panel(
      title = "Country Profile",
      value = "country_profile",
      icon = icon("flag"),
      mod_country_profile$ui(ns("country_profile"))
    ),
    
    nav_panel(
      title = "Cross-Country Benchmark",
      value = "benchmark",
      icon = icon("chart-bar"),
      mod_benchmark$ui(ns("benchmark"))
    ),
    
    nav_panel(
      title = "Infrastructure",
      value = "infrastructure",
      icon = icon("bolt"),
      mod_infrastructure$ui(ns("infrastructure"))
    ),
    
    nav_panel(
      title = "Access to Finance",
      value = "finance",
      icon = icon("university"),
      mod_finance_access$ui(ns("finance"))
    ),
    
    nav_spacer(),
    
    nav_panel(
      title = "Data Quality",
      value = "data_quality",
      icon = icon("clipboard-check"),
      mod_data_quality$ui(ns("data_quality"))
    ),
    
    nav_panel(
      title = "About",
      value = "about",
      icon = icon("info-circle"),
      mod_about$ui(ns("about"))
    ),
    
    nav_item(
      tags$a(
        href = "https://kwizresearch.com",
        target = "_blank",
        icon("external-link-alt"),
        " Kwiz Research",
        class = "nav-link",
        style = "color: #F49B7A !important;"
      )
    ),
    
    # Footer
    footer = tags$footer(
      class = "dashboard-footer",
      tags$div(
        class = "container-fluid",
        tags$div(
          class = "row",
          tags$div(
            class = "col-md-6",
            tags$span(
              icon("copyright"),
              " 2025 ",
              tags$a(
                href = "https://kwizresearch.com",
                target = "_blank",
                "Kwiz Computing Technologies"
              )
            )
          ),
          tags$div(
            class = "col-md-6 text-end",
            tags$span(
              "Data: ",
              tags$a(
                href = "https://www.enterprisesurveys.org",
                target = "_blank",
                "World Bank Enterprise Surveys"
              )
            )
>>>>>>> origin/main
          )
        )
      )
    )
  )
<<<<<<< HEAD
)

#' @export
server <- function(input, output, session) {
  
  # Show loading spinner
  waiter_show(
    html = tags$div(
      spin_fading_circles(color = "#1B6B5F"),
      tags$h4("Loading WBES Data...", class = "mt-3 text-primary-teal")
    ),
    color = "#ffffff"
  )
  
  # Load data reactively
  wbes_data <- reactiveVal(NULL)
  
  observe({
    data <- tryCatch({
      load_wbes_data()
    }, error = function(e) {
      message("Error loading data: ", e$message)
      load_sample_data()
    })
    
    wbes_data(data)
    waiter_hide()
  })
  
  # Initialize modules
  overview$server("overview", wbes_data)
  country_profile$server("country_profile", wbes_data)
  benchmark$server("benchmark", wbes_data)
  infrastructure$server("infrastructure", wbes_data)
  finance_access$server("finance_access", wbes_data)
  data_quality$server("data_quality", wbes_data)
  about$server("about")
=======
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Show loading screen
    waiter::waiter_show(
      html = waiterPreloader(
        spin_fading_circles(),
        color = "#1B6B5F"
      ),
      color = "#FFFFFF"
    )
    
    # Load data reactively (shared across modules)
    wbes_data <- shiny::reactiveVal(NULL)
    
    # Initialize data loading
    shiny::observe({
      # Load and cache data
      data <- data_loader$load_wbes_data()
      wbes_data(data)
      waiter::waiter_hide()
    })
    
    # Module servers
    mod_overview$server("overview", wbes_data)
    mod_country_profile$server("country_profile", wbes_data)
    mod_benchmark$server("benchmark", wbes_data)
    mod_infrastructure$server("infrastructure", wbes_data)
    mod_finance_access$server("finance", wbes_data)
    mod_data_quality$server("data_quality", wbes_data)
    mod_about$server("about")
  })
>>>>>>> origin/main
}
