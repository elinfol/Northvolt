#### Packages ####
library(googlesheets4)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)


#### Access information from Google sheets ####

gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"))

sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1iWwUMKtqyEcwFUpJwv5PO1NDM4QgJX9_snzBHYOZpYs/edit?gid=0#gid=0")

articles <- read_sheet(sheet, "electrive")
articles_nv_raw <- read_sheet(sheet, "Northvolt")

#sample_100 <- slice_sample(articles,n=100)
#write_sheet(sample_100, ss = sheet, sheet = "elin_analys")


#### Filtering the articles ####

articles_nv <- articles_nv_raw |> 
  filter(!(comment %in% c("dubblett","out of scope","not relevant","no event","speculation"))) |> 
  select(ID, date, Event, Category, comment,`Group 1`, `Group 2`) |> 
  arrange(date) |> 
  mutate(
    date = as.Date(date),
    ypos = rep(c(0.3, -0.3), length.out = n())
  )


categories <- unique(sort(articles_nv$Category))


#### Plotting timeline ####
articles_nv$Category <- factor(
  articles_nv$Category,
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



tol_bright <- c(
  "#4477AA",
  "#66CCEE",
  "#228833",
  "#CCBB44",
  "#EE6677",
  "#AA3377",
  "#BBBBBB")

ggplot(articles_nv, aes(x = date, y = 0, color = `Group 1`)) +
  geom_point(size = 3) +
  labs(
    title = "Northvolt Timeline",
    x = "Date",
    y = NULL,
    color = NULL
  ) +
  scale_color_manual(values = tol_bright) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )



#### Order Group 2 explicitly ####
articles_nv$`Group 2` <- factor(articles_nv$`Group 2`,
  levels = c("Rise", "Neutral", "Fall"))


ggplot() +
  geom_jitter(
    data = subset(articles_nv, `Group 2` != "Rise"),
    aes(x = date, y = 0, color = `Group 2`),
    height = 0, size = 4, alpha = 0.7  ) +
  geom_jitter(
    data = subset(articles_nv, `Group 2` == "Rise"),
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


####Test
articles_nv <- articles_nv |>
  mutate(
    period = ifelse(year(date) <= 2021, "2018–2021", "2022–2026")
  )

ggplot(articles_nv, aes(x = date, y = 0, color = `Group 1`)) +
  geom_point(size = 3) +
  facet_wrap(~ period, ncol = 1, scales = "free_x") +
  labs(
    title = "Northvolt Timeline",
    x = "Date",
    y = NULL,
    color = NULL
  ) +
  scale_color_manual(values = tol_bright) +
  theme_minimal(base_size = 14) +
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank())+
  theme(panel.spacing = unit(1, "lines"))
#ggsave("northvolt_timeline_group1.png", width = 8, height = 5, dpi = 300)






