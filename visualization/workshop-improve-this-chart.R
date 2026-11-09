# Workshop I — Fix this chart (15 min, solo)
# Data: HICP inflation, year-on-year %, monthly, DE/FR/IT, 2019-01 to 2024-12.
# Source: Eurostat, table prc_hicp_minr (unit = RCH_A, coicop18 = TOTAL)
#   https://ec.europa.eu/eurostat/databrowser/view/prc_hicp_minr/default/table?lang=en
# (prc_hicp_minr replaced the discontinued prc_hicp_manr in 2026.)

library(tidyverse)


# ---- Data -------------------------------------------------------------------

inflation <- read_csv("inflation.csv", show_col_types = FALSE)

glimpse(inflation)
range(inflation$date)
unique(inflation$country)


# ---- The bad chart ----------------------------------------------------------

bad_chart <-
  ggplot(inflation, aes(x = date, y = hicp_yoy, color = country)) +
  geom_line() +
  labs(title = "Figure 1", x = "date", y = "hicp_yoy")

bad_chart


# ---- Your turn --------------------------------------------------------------
# Turn the analyst's chart above into a journalist's chart.
# Pick one headline finding, then apply the annotation checklist from the lecture.
# Save your finished chart as `good_chart`.

good_chart <- bad_chart

# ... your edits here ...

good_chart


# ---- Save at mobile size ----------------------------------------------------

ggsave("good_chart.png", good_chart,
       width = 6, height = 4, dpi = 300, bg = "white")
