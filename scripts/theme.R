# Add Google Font "Lato"
font_add_google(name = "Lato", family = "Lato")
showtext_auto()  # Automatically use 'showtext' for future plots

# Define a single font variable for the custom font
font_family <- "Lato"

# Define the custom color palette
custom_colors1 <- c(
  primary = "#2A9D8F",  # Soft blue
  accent1 = "#E76F51",  # Coral
  accent2 = "#F4A261",  # Light orange
  accent3 = "#A68A98"   # Soft purple
)

# Fresh Green & Blue Palette
custom_colors2 <- c(
  primary = "#6C7A89",  # Soft Green
  accent1 = "#26A65B",  # Lime Green
  accent2 = "#3A539B",  # Aqua Blue
  accent3 = "#F5AB35"   # Soft Orange
)

# Cool Blues & Purples Palette
custom_colors3 <- c(
  primary = "#4A90E2",  # Cool Blue
  accent1 = "#9013FE",  # Dark Purple
  accent2 = "#BD10E0",  # Soft Purple
  accent3 = "#50E3C2"   # Light Cyan
)

# Warm & Neutral Earth Tones
custom_colors4 <- c(
  primary = "#D6A85F",  # Warm Beige
  accent1 = "#D35400",  # Burnt Orange
  accent2 = "#8E44AD",  # Warm Brown
  accent3 = "#AAB7B8"   # Olive Green
)

# Bold & Modern Bright Palette
custom_colors5 <- c(
  primary = "#F4D03F",  # Bright Yellow
  accent1 = "#EB5757",  # Bold Pink
  accent2 = "#BB6BD9",  # Bright Purple
  accent3 = "#27AE60"   # Cyan
)

# Elegant Grayscale Palette
custom_colors6 <- c(
  primary = "#4B4B4B",  # Dark Gray
  accent1 = "#7F8C8D",  # Medium Gray
  accent2 = "#BDC3C7",  # Soft Gray
  accent3 = "#ECF0F1"   # Light Gray
)

# Pastel & Soft Palette
custom_colors7 <- c(
  primary = "#F1948A",  # Soft Coral
  accent1 = "#C39BD3",  # Soft Lavender
  accent2 = "#85C1E9",  # Light Blue
  accent3 = "#F7DC6F"   # Soft Yellow
)

# Set the active color palette
custom_colors <- custom_colors1

# Define a custom theme function for ggplot2
custom_theme <- function() {
  theme_minimal(base_size = 14) +  # Start with a minimal theme and adjust font size
    theme(
      plot.title = element_text(size = 18, face = "bold", color = custom_colors["primary"], family = font_family),
      plot.subtitle = element_text(face = "italic", size = 12, margin = margin(b = 10), family = font_family),  # Style subtitle
      axis.title = element_text(size = 16, face = "bold", color = custom_colors["primary"], family = font_family),
      axis.text = element_text(size = 14, color = "black", family = font_family),  # Customize axis text
      panel.grid.major = element_line(size = 0.2, color = "gray85"),  # Light grid lines
      panel.grid.minor = element_blank(),
      legend.position = "top",  # Customize legend position and appearance
      legend.title = element_text(face = "bold", color = custom_colors["primary"], family = font_family),
      legend.text = element_text(size = 12, family = font_family),
      panel.background = element_rect(fill = "white", color = NA)  # Keep the background white
    )
}
