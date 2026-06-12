#### Packages ####
library(googlesheets4)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(plotly)

#### Access information from Google sheets ####

gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1iWwUMKtqyEcwFUpJwv5PO1NDM4QgJX9_snzBHYOZpYs/edit?gid=0#gid=0")

articles <- read_sheet(sheet, "electrive")
articles_nv_raw <- read_sheet(sheet, "Northvolt")
annual_reports <- read_sheet(sheet,"Årsredovisningar")

#### Filtering the articles ####

articles_nv <- articles_nv_raw %>% 
  filter(!(comment %in% c("dubblett","out of scope","not relevant","no event","speculation")))  %>%  
  select(ID, date, Event, Category, comment,`Teoretical category`,`Group 1`, `Group 2`)  %>% 
  arrange(date)  %>% 
  mutate(date = as.Date(date),ypos = rep(c(0.3, -0.3), length.out = n()))


categories <- unique(sort(articles_nv$Category))


#### Plotting timeline using "Category" ####

#sorts the categories in alphabetic order for the plot
articles_nv$Category <- factor(articles_nv$Category,
  levels = rev(sort(unique(articles_nv$Category))))

ggplot(articles_nv, aes(x = date, y = Category, color = Category)) +
  geom_point(size = 3) +
  labs(title = "Northvolt Timeline",
    x = "Date",
    y = "Category"  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
#ggsave("northvolt_timeline.png", width = 8, height = 5, dpi = 300)

#### Plotting timeline using "Group 1" as categories ####

#plotting all events on the x-axis
ggplot(articles_nv, aes(x = date, y = 0, color = `Group 1`)) +
  geom_point(size = 3) +
  labs(title = "Northvolt Timeline",
    x = "Date",
    y = NULL,
    color = NULL) +
  scale_color_brewer(palette = "Dark2")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 14) +
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank())

#sorting the events in 2 groups, 2018-2021 & 2022-2026
articles_nv <- articles_nv  %>% 
  mutate(period = ifelse(year(date) <= 2021, "2018–2021", "2022–2026"))

ggplot(articles_nv, aes(x = date, y = 0, color = `Group 1`)) +
  geom_point(size = 3) +
  facet_wrap(~ period, ncol = 1, scales = "free_x") +
  labs(title = "Northvolt Timeline",
    x = "Date",
    y = NULL,
    color = NULL) +
  scale_color_brewer(palette = "Dark2")+
  theme_minimal(base_size = 14) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())+
  theme(panel.spacing = unit(1, "lines"))
#ggsave("northvolt_timeline_group1.png", width = 8, height = 5, dpi = 300)

#### Plotting timeline using "Group 2" as categories ####
articles_nv$`Group 2` <- factor(articles_nv$`Group 2`,
  levels = c("Rise", "Neutral", "Fall"))


ggplot() +
  geom_jitter(data = subset(articles_nv, `Group 2` != "Rise"),
    aes(x = date, y = 0, color = `Group 2`),
    height = 0, size = 4, alpha = 0.7  ) +
  geom_jitter(data = subset(articles_nv, `Group 2` == "Rise"),
    aes(x = date, y = 0, color = `Group 2`),
    height = 0, size = 4, alpha = 0.7  ) +
  labs(title = "Northvolt Timeline",
    x = "Date",
    y = NULL,
    color = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c(
    "Rise"    = "#1b9e77",
    "Neutral" = "#999999",  
    "Fall"    = "#d95f02")) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "right",
    panel.grid.minor = element_blank())
#ggsave("northvolt_timeline_rise_fall.png", width = 8, height = 5, dpi = 300)

#### Interactive plots ####
pl_gr1 <- articles_nv %>% 
  mutate(`Teoretical category` = fct_rev(fct_reorder(`Teoretical category`, date, .fun = min)))%>%
  ggplot(aes(date, `Teoretical category`, color = `Teoretical category`, 
             text= str_wrap(Event, 20))) + 
  geom_point(show.legend = F)+
  scale_color_brewer(palette = "Dark2")+
  labs(x = "Date", y = "") + 
  theme_bw() + 
  theme(legend.position = "none")

plotly_pl_gr1 <- ggplotly(pl_gr1, tooltip = "text") %>%
  style(hoverlabel = list(bgcolor = "white"), traces = NULL) %>%
  layout(hoverlabel = list(align = "left"))
plotly_pl_gr1
#saveWidget(plotly_pl_gr1, "northvolt_interactive.html") 

#### Data from annual reports ####
pl_ar <- annual_reports %>% 
  mutate(date = as.Date(Date)) %>%
  arrange(date)  %>%
  ggplot(aes(date, `Teoretical category`, color = `Teoretical category`, 
             text= str_wrap(Event, 20))) + 
  geom_jitter(height = 0.2, width = 0, show.legend = FALSE)+
  scale_color_brewer(palette = "Dark2")+
  labs(x = "Date", y = "") + 
  theme_bw() + 
  theme(legend.position = "none")

plotly_pl_ar <- ggplotly(pl_ar, tooltip = "text") %>%
  style(hoverlabel = list(bgcolor = "white"), traces = NULL) %>%
  layout(hoverlabel = list(align = "left"))
plotly_pl_ar





