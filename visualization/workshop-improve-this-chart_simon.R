# Workshop I — solution: progressive build of one journalist's chart.
# Each step adds one editorial decision. Same plot area across all five PNGs,
# so flipping through them shows the layers being added — not the panel moving.

library(tidyverse)
library(scales)
library(showtext)              # install.packages("showtext")  if needed
library(shadowtext)            # install.packages("shadowtext") if needed — text halos

# ---- Fonts ------------------------------------------------------------------
# Pull Fira Sans from Google Fonts on first use (cached locally afterwards).
font_add_google("Fira Sans", "Fira Sans")
showtext_auto()
showtext_opts(dpi = 300)       # keep font sizes in sync with ggsave dpi


# ---- Data + roles -----------------------------------------------------------
inflation <- read_csv("inflation.csv", show_col_types = FALSE) |>
  mutate(role = factor(if_else(country == "Germany", "news", "context"),
                       levels = c("context", "news")))

pal        <- c(news = "#C2001A", context = "grey75")
labels_end <- inflation |> group_by(country) |> filter(date == max(date)) |> ungroup()


# ---- Things that lock the plot area across versions ------------------------
# Identical scales + theme + figure dimensions => the panel sits in the same
# pixels every time. Annotations get added in front; the canvas does not move.

scales_lock <- list(
  scale_x_date(
    breaks = seq(as.Date("2019-01-01"), as.Date("2025-01-01"), by = "1 year"),
    labels = label_date_short(),
    expand = expansion(mult = c(0.02, 0.10))           # room on the right for end labels
  ),
  scale_y_continuous(
    breaks = seq(0, 14, 2),
    labels = label_number(suffix = "%"),
    limits = c(-2.5, 14)                               # headroom for peak callout
  ),
  coord_cartesian(clip = "off")                        # don't crop annotations at edges
)

theme_dj <- function(base_size = 12) {
  theme_minimal(base_family = "Fira Sans", base_size = base_size) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(family = "Fira Sans", face = "bold",
                                   size = 14, lineheight = 1.1,
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(family = "Fira Sans", colour = "grey30",
                                   size = 11, margin = margin(b = 18)),
      plot.caption  = element_text(family = "Fira Sans", colour = "grey45",
                                   size = 8, hjust = 0,
                                   margin = margin(t = 14)),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey92"),
      axis.text          = element_text(family = "Fira Sans", colour = "grey30"),
      plot.margin        = margin(14, 36, 10, 14)      # extra right margin for end labels
    )
}

# Halo text helper — shadowtext-backed annotation that reads cleanly even when
# it overlaps a line. Wraps geom_shadowtext() in an annotate-style call.
halo <- function(label, x, y,
                 colour = "grey30", size = 3, hjust = 0, vjust = 0,
                 fontface = "plain", lineheight = 0.95,
                 bg.colour = "white", bg.r = 0.18) {
  geom_shadowtext(
    data = data.frame(x = x, y = y, label = label),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    family = "Fira Sans", colour = colour, size = size,
    hjust = hjust, vjust = vjust, fontface = fontface, lineheight = lineheight,
    bg.colour = bg.colour, bg.r = bg.r
  )
}

# Constant title block — same wording across versions to keep the title
# height locked. Captions vary slightly so the reader sees the data lineage
# get richer in the banger version.
TITLE    <- "Germany's inflation peaked at 11.6% in October 2022"
SUBTITLE <- "Year-on-year change in HICP, monthly, %"
CAPTION  <- "Source: Eurostat (prc_hicp_minr)"

# Save helper — every version goes out at the same dimensions.
save_v <- function(p, name, w = 9, h = 5) {
  ggsave(paste0("solution-", name, ".png"), p,
         width = w, height = h, dpi = 300, bg = "white")
}


# =============================================================================
# v1 — Baseline journalism: declarative title, units in subtitle, source line.
# =============================================================================

v1 <- ggplot(inflation, aes(date, hicp_yoy, group = country)) +
  geom_line(linewidth = 0.9, colour = "grey25") +
  scales_lock +
  theme_dj() +
  labs(title = TITLE, subtitle = SUBTITLE, caption = CAPTION,
       x = NULL, y = NULL)

save_v(v1, "v1-baseline")


# =============================================================================
# v2 — Encoding hierarchy: Germany foregrounded, others greyed.
#      Direct line-end labels replace the legend.
# =============================================================================

v2 <- ggplot(inflation, aes(date, hicp_yoy, group = country, colour = role)) +
  geom_line(linewidth = 0.9) +
  geom_shadowtext(data = labels_end, aes(label = country, colour = role),
                  hjust = -0.15, vjust = 0.5, size = 3.4,
                  family = "Fira Sans", fontface = "bold",
                  bg.colour = "white", bg.r = 0.18, show.legend = FALSE) +
  scale_colour_manual(values = pal, guide = "none") +
  scales_lock +
  theme_dj() +
  labs(title = TITLE, subtitle = SUBTITLE, caption = CAPTION,
       x = NULL, y = NULL)

save_v(v2, "v2-color-hierarchy")


# =============================================================================
# v3 — Free context: ECB target reference line + first event annotation.
# =============================================================================

v3 <- ggplot(inflation, aes(date, hicp_yoy, group = country, colour = role)) +
  geom_hline(yintercept = 2, linetype = "dashed",
             colour = "grey55", linewidth = 0.4) +
  halo("ECB target: 2%", x = as.Date("2019-02-01"), y = 2.6,
       colour = "grey40", fontface = "italic", size = 3) +
  annotate("segment",
           x = as.Date("2022-02-24"), xend = as.Date("2022-02-24"),
           y = 0, yend = 9, colour = "grey45", linetype = "dotted") +
  halo("Russia invades\nUkraine",
       x = as.Date("2022-03-15"), y = 0.6,
       colour = "grey25", size = 3) +
  geom_line(linewidth = 0.9) +
  geom_shadowtext(data = labels_end, aes(label = country, colour = role),
                  hjust = -0.15, vjust = 0.5, size = 3.4,
                  family = "Fira Sans", fontface = "bold",
                  bg.colour = "white", bg.r = 0.18, show.legend = FALSE) +
  scale_colour_manual(values = pal, guide = "none") +
  scales_lock +
  theme_dj() +
  labs(title = TITLE, subtitle = SUBTITLE, caption = CAPTION,
       x = NULL, y = NULL)

save_v(v3, "v3-context")


# =============================================================================
# v4 — Headline finding made explicit: peak point + label callout.
# =============================================================================

v4 <- ggplot(inflation, aes(date, hicp_yoy, group = country, colour = role)) +
  geom_hline(yintercept = 2, linetype = "dashed",
             colour = "grey55", linewidth = 0.4) +
  halo("ECB target: 2%", x = as.Date("2019-02-01"), y = 2.6,
       colour = "grey40", fontface = "italic", size = 3) +
  annotate("segment",
           x = as.Date("2022-02-24"), xend = as.Date("2022-02-24"),
           y = 0, yend = 9, colour = "grey45", linetype = "dotted") +
  halo("Russia invades\nUkraine",
       x = as.Date("2022-03-15"), y = 0.6,
       colour = "grey25", size = 3) +
  geom_line(linewidth = 0.9) +
  geom_shadowtext(data = labels_end, aes(label = country, colour = role),
                  hjust = -0.15, vjust = 0.5, size = 3.4,
                  family = "Fira Sans", fontface = "bold",
                  bg.colour = "white", bg.r = 0.18, show.legend = FALSE) +
  annotate("point", x = as.Date("2022-10-01"), y = 11.6,
           colour = pal["news"], size = 2.6) +
  halo("Peak: 11.6%\nOct 2022",
       x = as.Date("2022-10-01"), y = 12.2,
       colour = pal["news"], hjust = 0.5, size = 3.1, fontface = "bold") +
  scale_colour_manual(values = pal, guide = "none") +
  scales_lock +
  theme_dj() +
  labs(title = TITLE, subtitle = SUBTITLE, caption = CAPTION,
       x = NULL, y = NULL)

save_v(v4, "v4-peak-callout")


# =============================================================================
# v5 — The banger: above-target shading, second event (ECB pivot), italicised
#      Italy-peaked-higher footnote, polished caption with full lineage.
# =============================================================================

above_target <- tibble(
  xmin = as.Date("2021-07-01"),
  xmax = as.Date("2024-04-01"),
  ymin = -Inf, ymax = Inf
)

v5 <- ggplot(inflation, aes(date, hicp_yoy, group = country, colour = role)) +
  geom_rect(data = above_target,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "#F8E8EA") +
  geom_hline(yintercept = 2, linetype = "dashed",
             colour = "grey55", linewidth = 0.4) +
  halo("ECB target: 2%", x = as.Date("2019-02-01"), y = 2.6,
       colour = "grey40", fontface = "italic", size = 3) +
  annotate("segment",
           x = as.Date("2022-02-24"), xend = as.Date("2022-02-24"),
           y = 0, yend = 9, colour = "grey45", linetype = "dotted") +
  halo("Russia invades\nUkraine",
       x = as.Date("2022-03-15"), y = 0.6,
       colour = "grey25", size = 3) +
  annotate("segment",
           x = as.Date("2022-07-21"), xend = as.Date("2022-07-21"),
           y = -2.2, yend = 7, colour = "grey45", linetype = "dotted") +
  halo("ECB starts\nraising rates",
       x = as.Date("2022-08-05"), y = -2.2,
       colour = "grey25", size = 3) +
  geom_line(linewidth = 1) +
  geom_shadowtext(data = labels_end, aes(label = country, colour = role),
                  hjust = -0.15, vjust = 0.5, size = 3.4,
                  family = "Fira Sans", fontface = "bold",
                  bg.colour = "white", bg.r = 0.18, show.legend = FALSE) +
  annotate("point", x = as.Date("2022-10-01"), y = 11.6,
           colour = pal["news"], size = 2.8) +
  halo("Peak: 11.6%\nOct 2022",
       x = as.Date("2022-10-01"), y = 12.2,
       colour = pal["news"], hjust = 0.5, size = 3.2, fontface = "bold") +
  halo("(Italy peaked higher, at 12.6%)",
       x = as.Date("2022-10-01"), y = 13.5,
       colour = "grey45", hjust = 0.5, size = 2.8, fontface = "italic") +
  scale_colour_manual(values = pal, guide = "none") +
  scales_lock +
  theme_dj() +
  labs(
    title    = TITLE,
    subtitle = paste0(SUBTITLE, "  ·  Shaded period: months above the 2% target"),
    caption  = "Source: Eurostat (prc_hicp_minr) · Chart: E1493 Data Journalism, Hertie School",
    x = NULL, y = NULL
  )

save_v(v5, "v5-banger")
