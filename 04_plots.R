# ==========================================================
# 04_plots.R  --  the payoff
#   fig1: every artist's decay curve, aligned at its own peak
#   fig2: does your loyalty deepen over time?
#   fig3: your favourites ranked by stickiness
# ==========================================================

source("00_config.R")
library(ggplot2)

bundle       <- readRDS(MONTHLY_FILE)
artist_month <- bundle$artist_month
hl           <- readRDS(file.path(RESULTS_DIR, "half_lives.rds"))

roll_mean <- function(x, w = SMOOTH_WIDTH) {
  n <- length(x); half <- (w - 1) %/% 2
  vapply(seq_len(n), function(i) mean(x[max(1, i - half):min(n, i + half)]), numeric(1))
}

usable <- hl[hl$status %in% c("decayed", "still burning"), ]

# ---- build peak-aligned, peak-normalised curves -------------
series <- split(artist_month, artist_month$artist)
curves <- lapply(usable$artist, function(a) {
  d  <- series[[a]][order(series[[a]]$month), ]
  sm <- roll_mean(d$plays)
  pk <- which.max(sm)
  idx <- pk:min(length(sm), pk + PLOT_HORIZON)
  data.frame(artist = a, months_since_peak = idx - pk,
             rel = sm[idx] / sm[pk], stringsAsFactors = FALSE)
})
curves <- do.call(rbind, curves)

if (is.null(HIGHLIGHT_ARTISTS)) {
  HIGHLIGHT_ARTISTS <- head(usable$artist[order(-usable$total_plays)], 6)
}
hi <- curves[curves$artist %in% HIGHLIGHT_ARTISTS, ]
med_hl <- median(hl$half_life_months[hl$status == "decayed"], na.rm = TRUE)

# ---- FIGURE 1: the spaghetti of decline ---------------------
p1 <- ggplot(curves, aes(months_since_peak, rel, group = artist)) +
  geom_line(colour = "grey75", linewidth = 0.3, alpha = 0.45) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey30") +
  geom_line(data = hi, aes(colour = artist), linewidth = 1.1) +
  annotate("text", x = PLOT_HORIZON, y = 0.53, label = "half of peak",
           hjust = 1, size = 3, colour = "grey30") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_colour_brewer(palette = "Dark2", name = NULL) +
  labs(
    title    = "How fast the fire goes out",
    subtitle = paste0("Every artist aligned at their own peak month. Median half-life: ",
                      med_hl, " months."),
    x = "Months since peak", y = "Listening, relative to peak"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave(file.path(FIG_DIR, "fig1_decay_curves.png"), p1,
       width = 9, height = 6, dpi = 200)

# ---- FIGURE 2: loyalty vs. when you found them --------------
sc <- hl[hl$status == "decayed", ]
yr <- aggregate(list(median_hl = sc$half_life_months),
                by = list(discovery_year = sc$discovery_year), FUN = median)
yr_n <- aggregate(list(n = sc$half_life_months),
                  by = list(discovery_year = sc$discovery_year), FUN = length)
yr <- merge(yr, yr_n)
yr <- yr[yr$n >= 3, ]

p2 <- ggplot(sc, aes(discovery_year, half_life_months)) +
  geom_jitter(width = 0.18, height = 0, alpha = 0.35, colour = "grey45", size = 1.6) +
  geom_line(data = yr, aes(discovery_year, median_hl),
            colour = "#C0392B", linewidth = 1.2) +
  geom_point(data = yr, aes(discovery_year, median_hl),
             colour = "#C0392B", size = 3) +
  scale_x_continuous(breaks = seq(min(sc$discovery_year), max(sc$discovery_year), 1)) +
  labs(
    title    = "Is your taste settling down, or speeding up?",
    subtitle = "Each dot is an artist. Red line is the median half-life of artists discovered that year.",
    x = "Year you first heard them", y = "Half-life (months)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave(file.path(FIG_DIR, "fig2_loyalty_by_year.png"), p2,
       width = 9, height = 5.5, dpi = 200)

# ---- FIGURE 3: your top artists, ranked by stickiness -------
top <- head(usable[order(-usable$total_plays), ], 25)
top$label_hl <- ifelse(top$status == "still burning",
                       top$months_after_peak, top$half_life_months)
top$artist <- factor(top$artist, levels = top$artist[order(top$label_hl)])

p3 <- ggplot(top, aes(label_hl, artist, colour = character)) +
  geom_segment(aes(x = 0, xend = label_hl, yend = artist),
               colour = "grey85", linewidth = 0.8) +
  geom_point(size = 3.4) +
  scale_colour_manual(values = c("Supernova"  = "#E74C3C",
                                 "Steady"     = "#E39A2B",
                                 "Slow burn"  = "#2E86C1",
                                 "Evergreen"  = "#1E8449"), name = NULL) +
  labs(
    title    = "Your 25 most-played artists, ranked by stickiness",
    subtitle = "Green bars are evergreens - the number is months since peak, still going.",
    x = "Months", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"))

ggsave(file.path(FIG_DIR, "fig3_top_artists.png"), p3,
       width = 9, height = 8, dpi = 200)

print(p1); print(p2); print(p3)
message("Three figures written to ", FIG_DIR, "/")

