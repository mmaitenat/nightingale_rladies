library(tidyverse)
library(readxl)
library(lubridate)
library(patchwork)
library(extrafont)

# Load data ----

data <- read_excel("datos_florence.xlsx", skip = 1) %>%
  setNames(., str_replace_all(colnames(.), c(" " = "_", "&" = "and"))) %>%
  mutate(Month = parse_date_time(Month, orders = c("mY")))

# Separate data into 2 datasets
death_data <- data %>%
  select(1:5) %>%
  setNames(., gsub("\\..*", "", colnames(.)))
dir.create(file.path("Shiny", "data"))
saveRDS(death_data, file = file.path("Shiny", "data", "death_data.rds"))

annual_mortality_rate_data <- data %>%
  select(c(1:2, 6:ncol(data))) %>%
  setNames(., gsub("\\..*", "", colnames(.)))
# mortality rate is per 1000
saveRDS(annual_mortality_rate_data, file = file.path("Shiny", "data", "annual_mortality_rate.rds"))

# Plots ----
# Polar chart
death_data_melt <- death_data %>%
  reshape2::melt(id.vars = c("Month", "Average_size_of_army"), value.name = "Deaths", variable.name = "Death_cause")

label_data <- death_data_melt %>%
  group_by(Month) %>%
  summarise(y = max(Deaths)) %>%
  mutate(y = ifelse(Month < as.Date("1854-07-01"), 225, y)) %>% # Change label pos for some months in first plot
  mutate(y = ifelse(Month > as.Date("1855-09-01"), 275, y)) %>% # Change label pos for some months in second plot
  mutate(label = toupper(month(Month, label = TRUE, abbr = FALSE))) %>%
  mutate(label = ifelse(Month == as.Date("1854-04-01"), "APRIL\n1854", ifelse(Month == as.Date("1855-01-01"), "JANUARY 1855", ifelse(Month == as.Date("1855-03-01"), "MARCH 1855.", label)))) %>%
  mutate(label = ifelse(Month == as.Date("1855-04-01"), "APRIL 1855", ifelse(Month == as.Date("1856-01-01"), "JANUARY\n1856", label))) 

death_data_melt %>%
  filter(Month <= as.Date("1855-03-01")) %>%
  mutate(Death_cause = factor(Death_cause, levels = c("Zymotic_diseases", "All_other_causes", "Wounds_and_injuries"))) %>% # need to reorder to set overlay order in plot
  arrange(Month, Death_cause) %>%
  ggplot() +
  geom_bar(aes(x = month(Month, label = TRUE), y = Deaths, fill = Death_cause), stat = "identity", position = "identity", width = 1, color = "black", size = 0.05) +
  geom_bar(data = filter(death_data_melt, Month == as.Date("1854-11-01") & Death_cause == "All_other_causes"), aes(x = month(Month, label = TRUE), y = Deaths), color = "black", stat = "identity", width = 1, alpha = 0, size = 0.1) + # In november 1854 there is a line for the sector showing deaths from all other causes
  geom_text(data = filter(label_data, Month <= as.Date("1855-03-01")),
            aes(x = month(Month, label = TRUE), y = y, label = label),
            vjust = -1,
            angle = c(seq(165, 15, length.out = 6), seq(-15, -165, length.out = 6)) - 90, # need two sequences to account for clockwise and counterclockwise turns. Then consider the 90º turn we finally make to put july on top
            size = 3,
            family = "Verdana") +
  annotate("text", 
           x = month(as.Date("1854-07-01")), 
           y = 120, 
           family = "Verdana", 
           size = 3,
           angle = 90, 
           vjust = -3, 
           fontface = 'italic', 
           label = "BULGARIA") +
  annotate("text", 
           x = month(as.Date("1854-10-01")), 
           y = 750, 
           family = "Verdana", 
           size = 3, 
           vjust = -5.5, 
           fontface = 'italic', 
           label = "CRIMEA") +
  scale_fill_manual(values = c("#7ac1e4", "#3a3634", "#db251c")) +
  scale_y_sqrt() + #for making the surface area of the section proportional to the number of deaths (sector area formula)
  labs(title = "1.\nAPRIL 1854 TO MARCH 1855") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#fff9fd", 
                                       colour = "#fff9fd"),
        panel.background = element_rect(fill = "#fff9fd",
                                        colour = "#fff9fd"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, family = "Fondamento")) +
  coord_polar(start = pi) -> p1

death_data_melt %>%
  filter(Month > as.Date("1855-03-01")) %>%
  mutate(Death_cause = factor(Death_cause, levels = c("Zymotic_diseases", "Wounds_and_injuries", "All_other_causes"))) %>% # need to reorder to set overlay order in plot
  arrange(Month, Death_cause) %>%
  ggplot(aes(x = month(Month, label = TRUE), y = Deaths)) +
  geom_bar(aes(fill = Death_cause), stat = "identity", position = "identity", width = 1) +
  geom_text(data = filter(label_data, Month >= as.Date("1855-04-01")),
            aes(x = month(Month, label = TRUE), y = y, label = label),
            vjust = -1,
            angle = c(seq(165, 15, length.out = 6), seq(-15, -165, length.out = 6)) - 90, # need two sequences to account for clockwise and counterclockwise turns. Then consider the 90º turn we finally make to put july on top
            size = 3,
            family = "Verdana") +
  scale_fill_manual(values = c("#7ac1e4", "#db251c", "#3a3634")) +
  scale_y_sqrt() + #for making the surface area of the section proportional to the number of deaths (sector area formula)
  labs(title = "2.\nAPRIL 1855 TO MARCH 1856") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#fff9fd", 
                                       colour = "#fff9fd"),
        panel.background = element_rect(fill = "#fff9fd",
                                        colour = "#fff9fd"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, family = "Fondamento")) +
  coord_polar(start = pi) -> p2

description_text <- c("The Areas of the blue, red, & black wedges are each measured from \nthe centre as the common vertex. \nThe blue wedges measured from the centre of the circle represent area \nfor area the deaths from Preventible or Mitigable Zymotic diseases; the \nred wedges measured from the centre the deaths from wounds; & the \nblack wedges measured from the centre the deaths from all other causes. \nThe black line across the red triangle in Nov. 1854 marks the boundary \nof the deaths from all other causes during the month.\nIn October 2854, & April 1855, the black area coincides with the red; \nin January & February 1856, the blue coincides with the black.\nThe entire areas may be compared by following the blue, the red & the \nblack lines enclosing them.")

ggplot() +
  annotate("text", x = 0, y = 0, size = 3.7, label = description_text, family = 'Fondamento', fontface = 'italic') +
  theme_void() +
  theme(plot.background = element_rect(fill = "#fff9fd", 
                                       colour = "#fff9fd"),
        panel.background = element_rect(fill = "#fff9fd",
                                        colour = "#fff9fd")) -> p3

(p2 / p3 | p1) +
  plot_annotation(
    title = 'DIAGRAM OF THE CAUSES OF MORTALITY',
    subtitle = 'IN THE ARMY IN THE EAST',
    theme = theme(
      plot.title = element_text(family = 'Harry Piel', hjust = 0.5),
      plot.subtitle = element_text(family = 'Verdana', hjust = 0.5, vjust = -2),
      plot.background = element_rect(fill = "#fff9fd"),
      panel.background = element_rect(fill = "#fff9fd",
                                      colour = "#fff9fd"))) -> pfinal

png("coxcomb.png", height = 900, width = 1600)
print(pfinal)
dev.off()