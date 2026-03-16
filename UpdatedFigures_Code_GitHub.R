# Analysis script: barriers and benefits to cohort–RCT coordination
# Reads survey data from an Excel file and generates two stacked bar plots:
# 1. Reported barriers to coordination
# 2. Reported benefits of increased coordination

library(tidyverse)
library(readxl)

# Read the sheet
df <- read_excel("/Users/priya/Documents/Work /Cohort and RCT coordination/Updated_Dataset.xlsx")

# Shared status levels and color palette
all_status <- c("CCB", "TCB", "Both", "None")

status_palette <- c(
  "CCB"  = "#4682B4",
  "TCB"  = "#06C1C8",
  "Both" = "#56B4E9",
  "None" = "#D3D3D3"
)

# Barriers figure 

# Renaming columns for the graph
bk_bp_cols <- c(
  "What were the key barriers to improving coordination between cohort studies and RCTs in COVID-19 and Mpox response?Check all that apply  (choice=Legal barriers (e.g., GDPR compliance))",
  "What were the key barriers to improving coordination between cohort studies and RCTs in COVID-19 and Mpox response?Check all that apply  (choice=Ethical barriers (e.g., lack of consent to reuse data))",
  "What were the key barriers to improving coordination between cohort studies and RCTs in COVID-19 and Mpox response?Check all that apply  (choice=Data harmonization)",
  "What were the key barriers to improving coordination between cohort studies and RCTs in COVID-19 and Mpox response?Check all that apply  (choice=Lack of infrastructure)",
  "What were the key barriers to improving coordination between cohort studies and RCTs in COVID-19 and Mpox response?Check all that apply  (choice=Funding limitations)",
  "What were the key barriers to improving coordination between cohort studies and RCTs in COVID-19 and Mpox response?Check all that apply  (choice=Other)"
)

bar_names <- c(
  "Legal barriers",
  "Ethical barriers",
  "Data harmonization",
  "Lack of infrastructure",
  "Funding limitations",
  "Other"
)

plot_data <- df %>%
  select(Status, all_of(bk_bp_cols)) %>%
  pivot_longer(
    cols = all_of(bk_bp_cols),
    names_to = "Barrier",
    values_to = "Checked"
  ) %>%
  mutate(
    Barrier = recode(Barrier, !!!setNames(bar_names, bk_bp_cols))
  ) %>%
  group_by(Barrier, Status) %>%
  summarise(n = sum(Checked == "Checked", na.rm = TRUE), .groups = "drop") %>%
  complete(Barrier = bar_names, Status = all_status, fill = list(n = 0)) %>%
  mutate(
    Barrier = factor(Barrier, levels = bar_names),
    Status = factor(Status, levels = all_status)
  )

barriers_plot <- ggplot(plot_data, aes(x = Barrier, y = n, fill = Status)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(
    aes(label = ifelse(n == 0, "", n)),
    position = position_stack(vjust = 0.5),
    color = "white", size = 5, fontface = "bold"
  ) +
  scale_fill_manual(values = status_palette, name = "Status") +
  labs(
    x = "Barrier",
    y = "Count",
    title = "Reported Barriers to Cohort–RCT Coordination (Stacked by Status)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, angle = 30, hjust = 1),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.line = element_line(color = "black")
  )

print(barriers_plot)

# Benefits figure 

# Renaming columns for the graph
bs_bx_cols <- c(
  "In which of the following areas would you see the most benefit from increased coordination between cohorts and trials in epidemic response? Check all that apply (choice=Data collection)",
  "In which of the following areas would you see the most benefit from increased coordination between cohorts and trials in epidemic response? Check all that apply (choice=Participant recruitment)",
  "In which of the following areas would you see the most benefit from increased coordination between cohorts and trials in epidemic response? Check all that apply (choice=Outcome measurement)",
  "In which of the following areas would you see the most benefit from increased coordination between cohorts and trials in epidemic response? Check all that apply (choice=Long-term follow-up)",
  "In which of the following areas would you see the most benefit from increased coordination between cohorts and trials in epidemic response? Check all that apply (choice=Data sharing and harmonization)",
  "In which of the following areas would you see the most benefit from increased coordination between cohorts and trials in epidemic response? Check all that apply (choice=Other)"
)

bar_names2 <- c(
  "Data collection",
  "Participant recruitment",
  "Outcome measurement",
  "Long-term follow-up",
  "Data sharing and harmonization",
  "Other"
)

plot_data2 <- df %>%
  select(Status, all_of(bs_bx_cols)) %>%
  pivot_longer(
    cols = all_of(bs_bx_cols),
    names_to = "Area",
    values_to = "Checked"
  ) %>%
  mutate(
    Area = recode(Area, !!!setNames(bar_names2, bs_bx_cols))
  ) %>%
  group_by(Area, Status) %>%
  summarise(n = sum(Checked == "Checked", na.rm = TRUE), .groups = "drop") %>%
  complete(Area = bar_names2, Status = all_status, fill = list(n = 0)) %>%
  mutate(
    Area = factor(Area, levels = bar_names2),
    Status = factor(Status, levels = all_status)
  )

benefits_plot <- ggplot(plot_data2, aes(x = Area, y = n, fill = Status)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(
    aes(label = ifelse(n == 0, "", n)),
    position = position_stack(vjust = 0.5),
    color = "white", size = 5, fontface = "bold"
  ) +
  scale_fill_manual(values = status_palette, name = "Status") +
  labs(
    x = "Area of Benefit",
    y = "Count",
    title = "Areas of Benefit from Cohort–Trial Coordination (Stacked by Status)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14, angle = 30, hjust = 1),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.line = element_line(color = "black")
  )

print(benefits_plot)

# Save plots 
dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/barriers_plot.png", barriers_plot, width = 10, height = 6, dpi = 300)
ggsave("outputs/benefits_plot.png", benefits_plot, width = 10, height = 6, dpi = 300)