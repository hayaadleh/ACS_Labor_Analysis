# ==============================================================================
# Graph Elements
# ==============================================================================

BG_Color <- "#F4F0E8"
BLUE <- "#003585"
ORANGE <- "#FF6600"

COLS_EDU3 <- c(
  "BA+"  = BLUE,
  "HS/Some College" = "#265799",
  "Less than HS" = ORANGE
)

COLS_EDU3_AREA <- c(
  "BA+" = BLUE,
  "HS/Some College" = "#7AA3CC",
  "Less than HS" = ORANGE
)

COLS_RACE <- c(
  "White" = BLUE,
  "Black"    = "#27ae60",
  "Hispanic" = ORANGE,
  "Asian"    = "#8E44AD"
)

COLS_2 <- c("Labor Force"      = BLUE,
            "Total Population" = ORANGE)

COLS_LF_POP <- c("Share of Labor Force" = BLUE,
                 "Share of Population"  = ORANGE)


theme_all <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background   = element_rect(fill = BG_Color, color = NA),
      panel.background  = element_rect(fill = BG_Color, color = NA),
      legend.background = element_rect(fill = BG_Color, color = NA),
      legend.key        = element_rect(fill = BG_Color, color = NA),
      
      plot.title    = element_text(
        face = "bold",
        color = BLUE,
        size = base_size + 2
      ),
      plot.subtitle = element_text(
        color = "#555555",
        size = base_size - 1,
        margin = margin(t = 2, b = 8)
      ),
      plot.caption  = element_text(
        color = "#888888",
        size = base_size - 3,
        hjust = 0
      ),
      
      axis.title = element_text(color = "#333333", size = base_size - 1),
      axis.text  = element_text(color = "#333333", size = base_size - 2),
      axis.line  = element_blank(),
      
      panel.grid.major = element_line(color = "#D8D2C4", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      
      strip.text      = element_text(face = "bold", size = base_size),
      strip.background = element_rect(fill = BG_Color, color = NA),
      
      legend.title    = element_blank(),
      legend.text     = element_text(color = "#333333", size = base_size - 2),
      legend.position = "bottom",
      
      plot.margin = margin(12, 16, 10, 12)
    )
}

SAVE_W <- 12
SAVE_H <- 6