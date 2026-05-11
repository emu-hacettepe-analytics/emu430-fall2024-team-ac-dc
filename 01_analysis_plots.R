# ============================================================
# EMU-430 Project — EV Charging Infrastructure, Turkey
# Script 01: Brand & City Analysis + ggplot2 Visualizations
# Author  : Azra Azizoğlu
# Source  : EPDK Şarj İstasyonları Lisans Kayıt Verileri
#           (sarj_istasyonlari_birlesik.xlsx, accessed April 2026)
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(scales)

# ── Colour palette ───────────────────────────────────────────
AC_COL  <- "#4A90D9"
DC_COL  <- "#E84855"
BRAND_COLS <- c(
  "ZES"     = "#E74C3C",
  "Trugo"   = "#3498DB",
  "Voltrun" = "#2ECC71",
  "eŞarj"   = "#F39C12",
  "Astor"   = "#9B59B6"
)

# ── 1. Load & clean ──────────────────────────────────────────
df_raw <- read_excel("sarj_istasyonlari_birlesik.xlsx")

df <- df_raw %>%
  # Remove duplicate sockets (same socket appearing in multiple monthly files)
  distinct(`Soket No`, .keep_all = TRUE) %>%
  # Extract city from address (format: "... / CİTY")
  mutate(City = str_extract(Adres, "[^/]+$") %>%
                str_trim() %>%
                str_to_upper()) %>%
  # Standardise brand labels for top 5
  mutate(Brand = case_when(
    Marka == "zes"     ~ "ZES",
    Marka == "Trugo"   ~ "Trugo",
    Marka == "VOLTRUN" ~ "Voltrun",
    Marka == "eşarj"   ~ "eŞarj",
    Marka == "ASTOR"   ~ "Astor",
    TRUE               ~ Marka
  ))

cat(sprintf("Raw rows : %s\n", format(nrow(df_raw), big.mark=",")))
cat(sprintf("Clean rows: %s  (removed %s duplicate sockets)\n",
    format(nrow(df), big.mark=","),
    format(nrow(df_raw) - nrow(df), big.mark=",")))

TOP5 <- c("ZES", "Trugo", "Voltrun", "eŞarj", "Astor")
df5  <- df %>% filter(Brand %in% TOP5) %>%
               mutate(Brand = factor(Brand, levels = TOP5))

# ── 2. Summary table ─────────────────────────────────────────
summary_tbl <- df5 %>%
  group_by(Brand) %>%
  summarise(
    Total  = n(),
    AC     = sum(`Soket Tipi` == "AC"),
    DC     = sum(`Soket Tipi` == "DC"),
    DC_pct = round(DC / Total * 100, 1),
    Cities = n_distinct(City, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- TOP 5 BRAND SUMMARY (clean data) ---\n")
print(summary_tbl)

# ╔══════════════════════════════════════════════════════════╗
# ║  PLOT 1 — Total Sockets by Brand                        ║
# ╚══════════════════════════════════════════════════════════╝
p1 <- summary_tbl %>%
  mutate(Brand = fct_reorder(Brand, Total)) %>%
  ggplot(aes(x = Brand, y = Total, fill = Brand)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = comma(Total)),
            vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = BRAND_COLS) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Total Number of Sockets by Brand (Top 5)",
    subtitle = "Source: EPDK Charging Station Registry, April 2026",
    x = NULL, y = "Number of Sockets"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave("plot1_total_sockets_top5.png", p1, width = 9, height = 6, dpi = 150)

# ╔══════════════════════════════════════════════════════════╗
# ║  PLOT 2 — AC vs DC Grouped Bar                          ║
# ╚══════════════════════════════════════════════════════════╝
p2 <- df5 %>%
  count(Brand, `Soket Tipi`) %>%
  ggplot(aes(x = Brand, y = n, fill = `Soket Tipi`)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(aes(label = comma(n)),
            position = position_dodge(width = 0.7),
            vjust = -0.4, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("AC" = AC_COL, "DC" = DC_COL),
                    name = "Socket Type") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "AC vs DC Socket Distribution (Top 5 Brands)",
    subtitle = "Source: EPDK Charging Station Registry, April 2026",
    x = NULL, y = "Number of Sockets"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave("plot2_ac_dc_comparison_top5.png", p2, width = 10, height = 6, dpi = 150)

# ╔══════════════════════════════════════════════════════════╗
# ║  PLOT 3 — DC Ratio / Charging Strategy                  ║
# ╚══════════════════════════════════════════════════════════╝
p3 <- summary_tbl %>%
  mutate(
    Brand    = fct_reorder(Brand, DC_pct),
    Strategy = ifelse(DC_pct >= 50, "DC-dominant (fast charging)", "AC-dominant")
  ) %>%
  ggplot(aes(x = Brand, y = DC_pct, fill = Strategy)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(DC_pct, "%")),
            vjust = -0.5, fontface = "bold", size = 4) +
  geom_hline(yintercept = 50, linetype = "dashed", colour = "#555555") +
  annotate("text", x = 5.4, y = 52.5, label = "50% threshold",
           size = 3.5, colour = "#555555", hjust = 1) +
  scale_fill_manual(values = c("DC-dominant (fast charging)" = DC_COL,
                               "AC-dominant"                 = AC_COL),
                    name = "Strategy") +
  scale_y_continuous(limits = c(0, 110), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "DC Socket Ratio by Brand — Charging Strategy",
    subtitle = "Source: EPDK Charging Station Registry, April 2026",
    x = NULL, y = "DC Socket Ratio (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave("plot3_dc_strategy_top5.png", p3, width = 9, height = 6, dpi = 150)

# ╔══════════════════════════════════════════════════════════╗
# ║  PLOT 4 — Top 8 Cities per Brand (faceted)              ║
# ╚══════════════════════════════════════════════════════════╝
city_data <- df5 %>%
  count(Brand, City) %>%
  group_by(Brand) %>%
  slice_max(n, n = 8) %>%
  mutate(City = fct_reorder(City, n))

p4 <- ggplot(city_data, aes(x = n, y = City, fill = Brand)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  scale_fill_manual(values = BRAND_COLS) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  facet_wrap(~Brand, scales = "free", nrow = 1) +
  labs(
    title    = "Top 5 Brands — Socket Distribution by City (Top 8 Cities Each)",
    subtitle = "Source: EPDK Charging Station Registry, April 2026",
    x = "Number of Sockets", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title  = element_text(face = "bold"),
    strip.text  = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_blank()
  )

ggsave("plot4_city_distribution_top5.png", p4, width = 20, height = 6, dpi = 150)

# ╔══════════════════════════════════════════════════════════╗
# ║  PLOT 5 — Geographic Coverage (number of cities)        ║
# ╚══════════════════════════════════════════════════════════╝
p5 <- summary_tbl %>%
  mutate(Brand = fct_reorder(Brand, Cities)) %>%
  ggplot(aes(x = Brand, y = Cities, fill = Brand)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste(Cities, "cities")),
            vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = BRAND_COLS) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Geographic Coverage — Number of Cities per Brand (Top 5)",
    subtitle = "Source: EPDK Charging Station Registry, April 2026",
    x = NULL, y = "Number of Cities"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave("plot5_geographic_coverage_top5.png", p5, width = 9, height = 6, dpi = 150)

# ╔══════════════════════════════════════════════════════════╗
# ║  PLOT 6 — Overall AC/DC Distribution (Pie)              ║
# ╚══════════════════════════════════════════════════════════╝
overall <- df %>%
  count(`Soket Tipi`) %>%
  mutate(
    pct   = n / sum(n) * 100,
    label = paste0(`Soket Tipi`, "\n", round(pct, 1), "%\n(", comma(n), ")")
  )

p6 <- ggplot(overall, aes(x = "", y = n, fill = `Soket Tipi`)) +
  geom_col(width = 1, colour = "white", linewidth = 1.5) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            colour = "white", fontface = "bold", size = 5) +
  coord_polar("y") +
  scale_fill_manual(values = c("AC" = AC_COL, "DC" = DC_COL)) +
  labs(
    title    = "Overall AC / DC Socket Distribution in Turkey",
    subtitle = paste0("Total: ", comma(sum(overall$n)), " sockets | Source: EPDK, April 2026"),
    fill = "Socket Type"
  ) +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("plot6_overall_ac_dc_pie.png", p6, width = 7, height = 6, dpi = 150)

# ╔══════════════════════════════════════════════════════════╗
# ║  PLOT 7 — Socket Density by City (Top 20)               ║
# ╚══════════════════════════════════════════════════════════╝
city_top20 <- df %>%
  filter(!is.na(City)) %>%
  count(City, name = "Sockets") %>%
  slice_max(Sockets, n = 20) %>%
  mutate(City = fct_reorder(City, Sockets))

p7 <- ggplot(city_top20, aes(x = Sockets, y = City)) +
  geom_col(fill = "#1565C0", width = 0.7) +
  geom_text(aes(label = comma(Sockets)), hjust = -0.15, size = 3.5, fontface = "bold") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Socket Density by City — Top 20",
    subtitle = "All brands and socket types | Source: EPDK, April 2026",
    x = "Number of Sockets", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

ggsave("plot7_top20_cities.png", p7, width = 10, height = 8, dpi = 150)

cat("\nAll 7 plots saved.\n")
