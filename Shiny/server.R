library(tidyverse)
library(ggpubr)
library(lubridate)
library(patchwork)
library(extrafont)

# Read data ----

death_data <- readRDS("data/death_data.rds")
death_data_melt <- death_data %>%
  reshape2::melt(id.vars = c("Month", "Average_size_of_army"), value.name = "Deaths", variable.name = "Death_cause")

annual_mortality_rate <- readRDS("data/annual_mortality_rate.rds")
annual_mortality_rate_melt <- annual_mortality_rate %>%
  reshape2::melt(id.vars = c("Month", "Average_size_of_army"), value.name = "Deaths", variable.name = "Death_cause")

# Shiny server code ----

shinyServer(function(input, output) {
  
  output$plot <- renderPlot({ 
    
    label_data <- eval(parse(text = input$data2plot)) %>%
      group_by(Month) %>%
      summarise(y = max(Deaths)) %>%
      mutate(y = ifelse(Month < as.Date("1854-07-01"), 225, y)) %>% # Change label pos for some months in first plot
      mutate(y = ifelse(Month > as.Date("1855-09-01"), 275, y)) %>% # Change label pos for some months in second plot
      mutate(label = toupper(month(Month, label = TRUE, abbr = FALSE))) %>%
      mutate(label = ifelse(Month == as.Date("1854-04-01"), "APRIL\n1854", ifelse(Month == as.Date("1855-01-01"), "JANUARY 1855", ifelse(Month == as.Date("1855-03-01"), "MARCH 1855.", label)))) %>%
      mutate(label = ifelse(Month == as.Date("1855-04-01"), "APRIL 1855", ifelse(Month == as.Date("1856-01-01"), "JANUARY\n1856", label))) 
    
    eval(parse(text = input$data2plot)) %>% 
      filter(Month <= as.Date("1855-03-01")) %>% 
      mutate(Month2 = month(Month, label = TRUE)) %>% 
      filter(Death_cause == "Zymotic_diseases") %>% 
      select(Month2) -> months_limits
    
    eval(parse(text = input$data2plot)) %>%
      filter(Month <= as.Date("1855-03-01") & Month <= as.Date(input$Date)) %>%
      mutate(Death_cause = factor(Death_cause, levels = c("Zymotic_diseases", "All_other_causes", "Wounds_and_injuries"))) %>% # need to reorder to set overlay order in plot
      arrange(Month, Death_cause) %>% 
      filter(Death_cause %in% input$show_cases) %>% 
      ggplot(aes(x = month(Month, label = TRUE))) +
      geom_bar(aes(y = Deaths, fill = Death_cause), stat = "identity", position = "identity", width = 1, color = "black", size = 0.05) +
      geom_text(data = filter(label_data, Month <= as.Date("1855-03-01") & Month <= as.Date(input$Date)),
                aes(x = month(Month, label = TRUE), y = y, label = label),
                vjust = -1,
                angle = c(seq(165, 15, length.out = 6), seq(-15, -165, length.out = 6))[1:dim(filter(label_data, Month <= as.Date("1855-03-01")& Month <= as.Date(input$Date)))[1]] - 90, # need two sequences to account for clockwise and counterclockwise turns. Then consider the 90º turn we finally make to put july on top
                size = 3,
                family = "Verdana") +
      scale_fill_manual(name = "Death cause:",
                        values = c("Zymotic_diseases" = "#7ac1e4", "All_other_causes" = "#3a3634","Wounds_and_injuries" = "#db251c"),
                        labels = c("Zymotic diseases", "All other causes", "Wounds and injuries")) +
      scale_x_discrete(limits = as.character(matrix(as.matrix(months_limits), nrow = 1)))+
      scale_y_sqrt(limits = c(0, max(eval(parse(text = input$data2plot))$Deaths))) + # for making the surface area of the section proportional to the number of deaths (sector area formula)
      labs(title = "1.\nAPRIL 1854 TO MARCH 1855") +
      theme_void() +
      theme(text = element_text(size = 16),
            legend.text=element_text(size = 16),
            plot.background = element_rect(fill = "#fff9fd", 
                                           colour = "#fff9fd"),
            panel.background = element_rect(fill = "#fff9fd",
                                            colour = "#fff9fd"),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, vjust = -15, family = "Fondamento")) +
      coord_polar(start = 3*pi/2) -> p1
    
    eval(parse(text=input$data2plot)) %>%
      filter(Month > as.Date("1855-03-01") & Month <= as.Date(input$Date)) %>%
      mutate(Death_cause = factor(Death_cause, levels = c("Zymotic_diseases", "Wounds_and_injuries", "All_other_causes"))) %>% # need to reorder to set overlay order in plot
      arrange(Month, Death_cause) %>% 
      filter(Death_cause %in% input$show_cases) %>% 
      ggplot(aes(x = month(Month, label = TRUE), y = Deaths)) +
      geom_bar(aes(fill = Death_cause), stat = "identity", position = "identity", width = 1, color = "black", size = 0.05) +
      scale_fill_manual(name="Death cause",
                        values = c("Zymotic_diseases" = "#7ac1e4", "All_other_causes" = "#3a3634", "Wounds_and_injuries" = "#db251c")) +
      scale_y_sqrt(limits = c(0, max(eval(parse(text = input$data2plot))$Deaths))) +
      labs(title = "2.\nAPRIL 1855 TO MARCH 1856") +
      theme_void() +
      theme(text = element_text(size = 16),
            plot.background = element_rect(fill = "#fff9fd", 
                                           colour = "#fff9fd"),
            panel.background = element_rect(fill = "#fff9fd",
                                            colour = "#fff9fd"),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, vjust = -15, family = "Fondamento")) +
      coord_polar(start = 3*pi/2) -> p2
    
    if(as.Date(input$Date) >= as.Date("1855-04-01")) {
      p2=p2 + scale_x_discrete(limits = as.character(matrix(as.matrix(months_limits), nrow = 1))) +
        geom_text(data = filter(label_data, Month >= as.Date("1855-04-01") & Month <= as.Date(input$Date)),
                  aes(x = month(Month, label = TRUE), y = y, label = label),
                  vjust = -1,
                  angle = c(seq(165, 15, length.out = 6), seq(-15, -165, length.out = 6))[1:dim(filter(label_data, Month > as.Date("1855-03-01") & Month <= as.Date(input$Date)))[1]] - 90, # need two sequences to account for clockwise and counterclockwise turns. Then consider the 90º turn we finally make to put july on top
                  size = 3)
    }
    
    p2 + p1 +
      plot_annotation(
        title = 'DIAGRAM OF THE CAUSES OF MORTALITY',
        subtitle = 'IN THE ARMY IN THE EAST',
        theme = theme(
          text = element_text(size = 18),
          plot.title = element_text(family = 'Harry Piel', hjust = 0.5),
          plot.subtitle = element_text(family = 'Verdana', hjust = 0.5, vjust = -2),
          plot.background = element_rect(fill = "#fff9fd"),
          panel.background = element_rect(fill = "#fff9fd",
                                          colour = "#fff9fd")))
    
  }, height = 900, width = 1600
  )
}
)