shinyUI(fluidPage(
  tags$style(type = "text/css", "
      .irs-grid-text {font-family: 'arial'; font-size: 11pt; z-index: 2;}
      .irs-single{font-size: 11pt;} 
      .irs-max {display: none;}
      .irs-min {display: none;}
        "),
  titlePanel(h4("Display options")),
  sidebarLayout(
    sidebarPanel(width = 12,
                 fluidRow(
                   column(3,
                          selectInput("data2plot", label="Choose data to plot:",
                                      choices=c("Number of deaths in the army in the east" = "death_data_melt",
                                                "Annual mortality rate " = "annual_mortality_rate_melt")),
                          checkboxGroupInput(inputId = "show_cases",
                                             label = "Display:",
                                             choices = c("Zymotic diseases" = "Zymotic_diseases", "Wounds and injuries" = "Wounds_and_injuries", "All other causes" = "All_other_causes"),
                                             selected = c("Zymotic_diseases", "Wounds_and_injuries", "All_other_causes"))
                   ), column(1), 
                   column(7,
                          sliderInput("Date",
                                      label = "Dates to display:",
                                      min = as.Date("1854-04-01","%Y-%m-%d"),
                                      max = as.Date("1856-03-01","%Y-%m-%d"), 
                                      value = as.Date("1854-04-01"),
                                      timeFormat = "%Y-%b", 
                                      step = 30)
                   ), column(1)
                 )),
    mainPanel(plotOutput("plot", width = "100%"), width = 12)
  )
))