# ==========================================================
# 06_interactive.R  --  hover-and-poke versions of the charts
# Writes three standalone .html files into figures/.
# Open them in any browser; no R needed once they exist.
# ==========================================================

source("00_config.R")
library(plotly)
library(htmlwidgets)

bundle       <- readRDS(MONTHLY_FILE)
artist_month <- bundle$artist_month
hl           <- readRDS(file.path(RESULTS_DIR, "half_lives.rds"))

roll_mean <- function(x, w = SMOOTH_WIDTH) {
  n <- length(x); half <- (w - 1) %/% 2
  vapply(seq_len(n), function(i) mean(x[max(1, i - half):min(n, i + half)]), numeric(1))
}

save_widget <- function(widget, file) {
  path <- normalizePath(file.path(FIG_DIR, file), mustWork = FALSE)
  ok <- try(saveWidget(widget, path, selfcontained = TRUE), silent = TRUE)
  if (inherits(ok, "try-error")) {
    saveWidget(widget, path, selfcontained = FALSE)   # needs the _files/ folder
    message("  (saved non-selfcontained - keep the accompanying _files folder)")
  }
  message("  -> ", path)
}

usable <- hl[hl$status %in% c("decayed", "still burning"), ]
series  <- split(artist_month, artist_month$artist)

# ==========================================================
# 1. DECAY CURVES - now you can hover to see whose line is whose
# ==========================================================
curves <- lapply(usable$artist, function(a) {
  d  <- series[[a]][order(series[[a]]$month), ]
  sm <- roll_mean(d$plays)
  pk <- which.max(sm)
  idx <- pk:min(length(sm), pk + PLOT_HORIZON)
  data.frame(artist = a, months_since_peak = idx - pk,
             rel = sm[idx] / sm[pk], month = d$month[idx],
             stringsAsFactors = FALSE)
})
curves <- do.call(rbind, curves)
curves <- merge(curves, hl[, c("artist", "half_life_months", "character", "total_plays")],
                by = "artist")
curves$hover <- with(curves, paste0(
  "<b>", artist, "</b><br>",
  format(month, "%b %Y"), "  (month ", months_since_peak, " after peak)<br>",
  round(rel * 100), "% of peak<br>",
  character, " | half-life: ",
  ifelse(is.na(half_life_months), "still burning", paste(half_life_months, "mo")), "<br>",
  total_plays, " plays all-time"))

if (is.null(HIGHLIGHT_ARTISTS)) {
  HIGHLIGHT_ARTISTS <- head(usable$artist[order(-usable$total_plays)], 6)
}
bg <- curves[!curves$artist %in% HIGHLIGHT_ARTISTS, ]
fg <- curves[ curves$artist %in% HIGHLIGHT_ARTISTS, ]

p1 <- plot_ly() |>
  add_lines(data = bg, x = ~months_since_peak, y = ~rel, split = ~artist,
            line = list(color = "rgba(140,140,140,0.30)", width = 1),
            hoverinfo = "text", text = ~hover, showlegend = FALSE) |>
  add_lines(data = fg, x = ~months_since_peak, y = ~rel, color = ~artist,
            line = list(width = 3), hoverinfo = "text", text = ~hover) |>
  layout(
    title  = list(text = paste0("How fast the fire goes out<br>",
                                "<sup>hover any grey line to name it</sup>")),
    xaxis  = list(title = "Months since peak"),
    yaxis  = list(title = "Share of peak listening", tickformat = ".0%",
                  range = c(0, 1.02)),
    hovermode = "closest",
    shapes = list(list(type = "line", x0 = 0, x1 = PLOT_HORIZON, y0 = 0.5, y1 = 0.5,
                       line = list(dash = "dash", color = "grey", width = 1))),
    legend = list(orientation = "h", y = -0.15))

message("Writing decay curves...")
save_widget(p1, "interactive_decay_curves.html")

# ==========================================================
# 2. THE MAP - how hot they burned vs. how long they lasted
# ==========================================================
sc <- hl[hl$status == "decayed", ]
sc$hover <- with(sc, paste0(
  "<b>", artist, "</b><br>",
  "peaked ", format(peak_month, "%b %Y"), " at ", peak_intensity, " plays/mo<br>",
  "half-life: ", half_life_months, " months<br>",
  total_plays, " plays | found ", discovery_year))

pal <- c("Supernova" = "#E74C3C", "Steady" = "#E39A2B", "Slow burn" = "#2E86C1")

p2 <- plot_ly(sc, x = ~peak_intensity, y = ~half_life_months,
              size = ~total_plays, color = ~character, colors = pal,
              type = "scatter", mode = "markers",
              sizes = c(60, 1200),
              marker = list(opacity = 0.7, line = list(width = 0.5, color = "white")),
              hoverinfo = "text", text = ~hover) |>
  layout(
    title = list(text = paste0("Burn brightly, or burn long?<br>",
                               "<sup>bubble size = lifetime plays. ",
                               "top-right = obsessions that stuck</sup>")),
    xaxis = list(title = "Peak intensity (plays per month)", type = "log"),
    yaxis = list(title = "Half-life (months)"),
    hovermode = "closest")

message("Writing burn map...")
save_widget(p2, "interactive_burn_map.html")

# ==========================================================
# 3. THE TIMELINE - when your eras happened
# ==========================================================
tl <- hl[hl$status %in% c("decayed", "still burning"), ]
tl$plot_hl <- ifelse(is.na(tl$half_life_months), tl$months_after_peak, tl$half_life_months)
tl$hover <- with(tl, paste0(
  "<b>", artist, "</b><br>",
  "peaked ", format(peak_month, "%b %Y"), "<br>",
  character, ": ", plot_hl, " months<br>",
  total_plays, " plays"))

pal2 <- c(pal, "Evergreen" = "#1E8449")

p3 <- plot_ly(tl, x = ~peak_month, y = ~plot_hl,
              size = ~total_plays, color = ~character, colors = pal2,
              type = "scatter", mode = "markers", sizes = c(60, 1200),
              marker = list(opacity = 0.7, line = list(width = 0.5, color = "white")),
              hoverinfo = "text", text = ~hover) |>
  layout(
    title = list(text = paste0("Your eras<br><sup>drag to zoom a period. ",
                               "vertical clusters are months when everything peaked at once</sup>")),
    xaxis = list(title = "Month of peak"),
    yaxis = list(title = "Months held above half"),
    hovermode = "closest")

message("Writing era timeline...")
save_widget(p3, "interactive_eras.html")

message("\nDone. Open the .html files in figures/ - they work offline and are shareable.")

