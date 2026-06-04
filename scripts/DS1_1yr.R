# ==============================================================================
# Story 1: Labor Supply — Who is still participating in NYC's economy?
# Data: ACS PUMS 1-Year, 2014-2024
# ==============================================================================

library(tidyverse)
library(srvyr)
library(scales)

source("graph_elements.R")

# NYC employment ---------------------------------------------------------------

nyc_laborforce <- readxl::read_xlsx("datasets/nyclfsa_0.xlsx")

employment_ces <- nyc_laborforce %>%
  mutate(YEAR = as.Date(YEAR)) %>%
  filter(year(YEAR) >= 2014, year(YEAR) <= 2024)

ggplot(employment_ces, aes(x = YEAR, y = Employment)) +
  geom_line(aes(color = "Monthly Employment", linetype = "Monthly Employment"),
            linewidth = 1) +
  geom_smooth(aes(color = "Trend Line", linetype = "Trend Line"),
              se = FALSE, linewidth = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma,
                     breaks = seq(0, max(employment_ces$Employment, na.rm = TRUE),
                                  by = 100)) +
  scale_color_manual(name   = "Series",
                     values = c("Monthly Employment" = BLUE,
                                "Trend Line"         = ORANGE)) +
  scale_linetype_manual(name   = "Series",
                        values = c("Monthly Employment" = "solid",
                                   "Trend Line"         = "dashed")) +
  guides(color    = guide_legend(override.aes = list(linewidth = c(1, 0.8))),
         linetype = guide_legend()) +
  labs(title   = "New York City's Employment Levels, 2014–2024",
       x       = NULL,
       y       = "Number of Employed (Thousands)",
       caption = "Source: NYC Current Employment Statistics (CES)") +
  theme_all() +
  theme(plot.caption = element_text(hjust = 1))

ggsave("output/plots/story1_ces_employment.png",
       width = SAVE_W, height = SAVE_H, dpi = 300, bg = BG_Color)

# Prep ACS file ----------------------------------------------------------------

acs_nyc_1yr <- readRDS("datasets/ACS_1yr_2014-2024.rds")

acs_nyc_1yr <- acs_nyc_1yr %>%
  mutate(
    education_3groups = case_when(
      education == "BA+"                        ~ "BA+",
      education %in% c("HS/GED", "Some College") ~ "HS/Some College",
      education == "Less than HS"               ~ "Less than HS"
    )
  )

acs_survey_1yr <- acs_nyc_1yr %>%
  as_survey_design(weights = PWGTP)

# Chart 1 — Employment rates by education (prime-age) -------------------------

nilf_by_edu <- acs_survey_1yr %>%
  filter(AGEP >= 25, AGEP <= 54,
         !is.na(education_3groups),
         emp_status %in% c("Employed", "Unemployed", "NILF")) %>%
  group_by(year, education_3groups, emp_status) %>%
  summarise(n = survey_total(), .groups = "drop") %>%
  group_by(year, education_3groups) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

emp_primeage <- nilf_by_edu %>%
  filter(year %in% c(2014, 2019, 2021, 2024), emp_status == "Employed") %>%
  mutate(education_3groups = factor(education_3groups,
                                    levels = c("BA+", "HS/Some College", "Less than HS")))

ggplot(emp_primeage,
       aes(x = factor(year), y = share,
           group = education_3groups, color = education_3groups)) +
  geom_line(linewidth = 1.5, alpha = 0.8) +
  geom_label(aes(label = percent(share, accuracy = 0.1)),
             fill = BG_Color, fontface = "bold", size = 3.5,
             label.size = 0.5, show.legend = FALSE) +
  scale_color_manual(values = COLS_EDU3) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = 0.1)) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  labs(title    = "Employment Rates by Educational Attainment",
       subtitle = "Prime-age New Yorkers (25–54)",
       x        = NULL,
       y        = "Employment Rate",
       caption  = "Source: ACS PUMS 1-Year") +
  theme_all() +
  theme(plot.caption = element_text(hjust = 1))

ggsave("output/plots/story1_emp_rate_by_edu.png",
       width = SAVE_W, height = SAVE_H, dpi = 300, bg = BG_Color)

# Chart 2 — Labor force share by education: line chart ------------------------

composition_data <- acs_survey_1yr %>%
  filter(AGEP >= 25, AGEP <= 54, in_lf == "1", !is.na(education_3groups)) %>%
  group_by(year, education_3groups) %>%
  summarise(n = survey_total(vartype = NULL), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = n / sum(n),
         education_3groups = factor(education_3groups,
                                    levels = rev(c("BA+", "HS/Some College", "Less than HS"))))

edu_labels_2024 <- composition_data %>% filter(year == 2024)

ggplot(composition_data,
       aes(x = year, y = share,
           color = education_3groups, group = education_3groups)) +
  geom_line(linewidth = 1.5) +
  geom_point(data = composition_data %>% filter(year %in% c(2014, 2024)),
             size = 3.5) +
  geom_text(data = edu_labels_2024,
            aes(label = paste0(education_3groups, " (",
                               percent(share, accuracy = 0.1), ")")),
            hjust = -0.08, size = 3.5) +
  scale_color_manual(values = COLS_EDU3_AREA) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0.05, 0.60),
                     breaks = seq(0.1, 0.6, by = 0.1),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = c(2014:2019, 2021:2024),
                     limits = c(2014, 2027)) +
  labs(title   = "Share of NYC Labor Force by Education Level, 2014–2024",
       x       = NULL,
       y       = "Share of Labor Force",
       caption = "Source: ACS PUMS 1-Year") +
  theme_all() +
  theme(legend.position    = "none",
        panel.grid.major.x = element_blank(),
        plot.caption       = element_text(hjust = 1))

ggsave("output/plots/story1_lf_share_line.png",
       width = SAVE_W, height = SAVE_H, dpi = 300, bg = BG_Color)

# Chart 3 — 65+ labor force vs. population indexed growth ---------------------

age_indexed_raw <- bind_rows(
  acs_survey_1yr %>%
    filter(age_group == "65+") %>%
    group_by(year, age_group) %>%
    summarise(total_pop = survey_total(vartype = NULL), .groups = "drop") %>%
    mutate(data_type = "Total Population"),
  acs_survey_1yr %>%
    filter(age_group == "65+", in_lf == 1) %>%
    group_by(year, age_group) %>%
    summarise(total_pop = survey_total(vartype = NULL), .groups = "drop") %>%
    mutate(data_type = "Labor Force")
)

age_indexed_1yr <- age_indexed_raw %>%
  group_by(age_group, data_type) %>%
  mutate(index_val = (total_pop / total_pop[year == 2014]) * 100) %>%
  ungroup()

ggplot(age_indexed_1yr,
       aes(x = year, y = index_val,
           color = data_type, linetype = data_type,
           group = interaction(age_group, data_type))) +
  geom_hline(yintercept = 100, color = "#888888", linewidth = 0.6) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  geom_text(data = filter(age_indexed_1yr, year == 2024),
            aes(label = data_type),
            hjust = -0.1, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = COLS_2) +
  scale_linetype_manual(values = c("Labor Force"      = "solid",
                                   "Total Population" = "dashed")) +
  scale_x_continuous(breaks = seq(2014, 2024, 2),
                     limits = c(2014, 2026)) +
  scale_y_continuous(breaks = seq(80, 170, 10)) +
  labs(title    = "Workers Aged 65+ Labor Force vs. Population Growth, Indexed to 2014",
       subtitle = "Solid = Labor Force  |  Dashed = Population",
       x        = NULL,
       y        = "Index Value (2014 = 100)",
       caption  = "Source: ACS PUMS 1-Year") +
  theme_all() +
  theme(legend.position = "none",
        plot.caption    = element_text(hjust = 1))

ggsave("output/plots/story1_65plus_indexed.png",
       width = SAVE_W, height = SAVE_H, dpi = 300, bg = BG_Color)

# Chart 4 — 65+ LFPR by education ---------------------------------------------

lfpr_65_by_edu <- acs_survey_1yr %>%
  filter(AGEP >= 65,
         !is.na(education_3groups),
         year %in% c(2014, 2024)) %>%
  group_by(year, education_3groups) %>%
  summarise(
    lfpr = survey_mean(in_lf, vartype = "se", na.rm = TRUE),
    n    = survey_total(vartype = NULL),
    .groups = "drop"
  ) %>%
  mutate(education_3groups = factor(education_3groups,
                                    levels = c("BA+", "HS/Some College", "Less than HS")))

ggplot(lfpr_65_by_edu %>% mutate(year = factor(year)),
       aes(x = education_3groups, y = lfpr, fill = year)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = percent(lfpr, accuracy = 0.1)),
            position = position_dodge(width = 0.75),
            vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("2014" = ORANGE, "2024" = BLUE)) +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "Labor Force Participation Rate by Educational Attainment",
       subtitle = "Older Adults 65+",
       x        = NULL,
       y        = "Participation Rate",
       fill     = "Year",
       caption  = "Source: ACS PUMS 1-Year") +
  theme_all() +
  theme(panel.grid.major.x = element_blank(),
        plot.caption       = element_text(hjust = 1))

ggsave("output/plots/story1_65plus_edu_lfpr.png",
       width = SAVE_W, height = SAVE_H, dpi = 300, bg = BG_Color)

# Table 1 — LFPR by race and education ----------------------------------------

race_edu_table_data <- acs_survey_1yr %>%
  filter(AGEP >= 25, AGEP <= 54,
         !is.na(education_3groups),
         race_eth != "Other",
         year %in% c(2019, 2024)) %>%
  group_by(year, race_eth, education_3groups) %>%
  summarise(
    lfpr = survey_mean(in_lf, vartype = "cv", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    education_3groups = factor(education_3groups,
                               levels = c("BA+", "HS/Some College", "Less than HS")),
    race_eth = factor(race_eth,
                      levels = c("Hispanic", "Black", "Asian", "White")),
    reliable = lfpr_cv <= 0.2,
    label    = paste0(percent(lfpr, accuracy = 0.1),
                      ifelse(reliable, "", "*"))
  )

pp_data <- race_edu_table_data %>%
  dplyr::select(race_eth, education_3groups, year, lfpr, lfpr_cv) %>%
  pivot_wider(names_from = year, values_from = c(lfpr, lfpr_cv)) %>%
  dplyr::mutate(
    pp_change     = (lfpr_2024 - lfpr_2019) * 100,
    both_reliable = lfpr_cv_2019 <= 0.2 & lfpr_cv_2024 <= 0.2,
    pp_label      = paste0(sprintf("%+.1f pp", pp_change),
                           ifelse(both_reliable, "", "*"))
  )

final_table <- data.frame(
  Education = levels(race_edu_table_data$education_3groups)
)

for (race in c("Hispanic", "Black", "Asian", "White")) {
  sub <- race_edu_table_data %>%
    dplyr::filter(race_eth == race) %>%
    dplyr::select(education_3groups, year, label) %>%
    pivot_wider(names_from = year, values_from = label) %>%
    dplyr::rename(!!paste0(race, "_2019") := `2019`,
                  !!paste0(race, "_2024") := `2024`)
  
  pp <- pp_data %>%
    dplyr::filter(race_eth == race) %>%
    dplyr::select(education_3groups, pp_change, pp_label) %>%
    dplyr::rename(!!paste0(race, "_pp_num") := pp_change,
                  !!paste0(race, "_pp")     := pp_label)
  
  final_table <- final_table %>%
    left_join(sub, by = c("Education" = "education_3groups")) %>%
    left_join(pp,  by = c("Education" = "education_3groups"))
}

final_table %>%
  gt() %>%
  tab_header(
    title    = md("**Labor Force Participation Rate by Race and Education**"),
    subtitle = "Prime-age New Yorkers (25–54) | * = estimate unreliable (CV > 20%)"
  ) %>%
  tab_spanner(label = "Hispanic", columns = starts_with("Hispanic")) %>%
  tab_spanner(label = "Black",    columns = starts_with("Black"))    %>%
  tab_spanner(label = "Asian",    columns = starts_with("Asian"))    %>%
  tab_spanner(label = "White",    columns = starts_with("White"))    %>%
  cols_hide(columns = ends_with("_pp_num")) %>%
  cols_label(
    Education     = "",
    Hispanic_2019 = "2019", Hispanic_2024 = "2024", Hispanic_pp = "Change",
    Black_2019    = "2019", Black_2024    = "2024", Black_pp    = "Change",
    Asian_2019    = "2019", Asian_2024    = "2024", Asian_pp    = "Change",
    White_2019    = "2019", White_2024    = "2024", White_pp    = "Change"
  ) %>%
  cols_align(align = "center", columns = -Education) %>%
  cols_align(align = "left",   columns =  Education) %>%
  tab_style(style     = cell_text(color = ORANGE, weight = "bold"),
            locations = cells_body(columns = "Hispanic_pp",
                                   rows    = Hispanic_pp_num < 0)) %>%
  tab_style(style     = cell_text(color = "#27ae60", weight = "bold"),
            locations = cells_body(columns = "Hispanic_pp",
                                   rows    = Hispanic_pp_num > 0)) %>%
  tab_style(style     = cell_text(color = ORANGE, weight = "bold"),
            locations = cells_body(columns = "Black_pp",
                                   rows    = Black_pp_num < 0)) %>%
  tab_style(style     = cell_text(color = "#27ae60", weight = "bold"),
            locations = cells_body(columns = "Black_pp",
                                   rows    = Black_pp_num > 0)) %>%
  tab_style(style     = cell_text(color = ORANGE, weight = "bold"),
            locations = cells_body(columns = "Asian_pp",
                                   rows    = Asian_pp_num < 0)) %>%
  tab_style(style     = cell_text(color = "#27ae60", weight = "bold"),
            locations = cells_body(columns = "Asian_pp",
                                   rows    = Asian_pp_num > 0)) %>%
  tab_style(style     = cell_text(color = ORANGE, weight = "bold"),
            locations = cells_body(columns = "White_pp",
                                   rows    = White_pp_num < 0)) %>%
  tab_style(style     = cell_text(color = "#27ae60", weight = "bold"),
            locations = cells_body(columns = "White_pp",
                                   rows    = White_pp_num > 0)) %>%
  tab_style(style     = cell_fill(color = "#EDE8DC"),
            locations = cells_body(rows = Education == "HS/Some College")) %>%
  tab_source_note("Source: ACS PUMS 1-Year") %>%
  tab_options(
    table.background.color         = "#F4F0E8",
    column_labels.background.color = "#E8E3D8",
    column_labels.font.weight      = "bold",
    heading.align                  = "left",
    table.border.top.color         = BLUE,
    table.border.top.width         = px(3),
    table.font.names               = "Georgia",
    data_row.padding               = px(6)
  ) %>%
  gtsave("output/plots/story1_race_edu_table.png", expand = 20)