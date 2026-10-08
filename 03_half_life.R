# ==========================================================
# 03_half_life.R  --  the actual analysis
# For each artist: smooth, find the peak, measure how many
# months until listening falls below 50% and STAYS there.
# ==========================================================

source("00_config.R")

bundle       <- readRDS(MONTHLY_FILE)
artist_month <- bundle$artist_month
artist_meta  <- bundle$artist_meta

# ---- centred rolling mean, partial windows at the edges ----
roll_mean <- function(x, w = SMOOTH_WIDTH) {
  n <- length(x); half <- (w - 1) %/% 2
  vapply(seq_len(n), function(i) mean(x[max(1, i - half):min(n, i + half)]), numeric(1))
}

# ---- half-life for one artist's smoothed series -------------
compute_hl <- function(sm) {
  n        <- length(sm)
  peak_idx <- which.max(sm)
  peak_val <- sm[peak_idx]
  med      <- median(sm[sm > 0])
  months_after <- n - peak_idx
  
  out <- list(peak_idx = peak_idx, peak_val = peak_val, median_month = med,
              months_after_peak = months_after,
              half_life = NA_real_, status = NA_character_)
  
  # Guard rails: is this a real peak, with room to observe a decline?
  if (is.na(med) || med == 0 || peak_val < MIN_PEAK_RATIO * med) {
    out$status <- "no clear peak"; return(out)
  }
  if (months_after < MIN_MONTHS_AFTER) {
    out$status <- "peak too recent"; return(out)
  }
  
  threshold <- 0.5 * peak_val
  for (i in (peak_idx + 1):n) {
    window <- sm[i:min(n, i + TAIL_CONFIRM)]
    if (all(window <= threshold)) {
      out$half_life <- i - peak_idx
      out$status    <- "decayed"
      return(out)
    }
  }
  out$status <- "still burning"   # right-censored: never dropped and stayed
  out
}

# ---- run over every artist ---------------------------------
series <- split(artist_month, artist_month$artist)

res <- lapply(names(series), function(a) {
  d  <- series[[a]][order(series[[a]]$month), ]
  sm <- roll_mean(d$plays)
  r  <- compute_hl(sm)
  data.frame(
    artist            = a,
    n_months          = nrow(d),
    peak_month        = d$month[r$peak_idx],
    peak_intensity    = round(r$peak_val, 2),
    median_month      = round(r$median_month, 2),
    months_after_peak = r$months_after_peak,
    half_life_months  = r$half_life,
    status            = r$status,
    stringsAsFactors  = FALSE
  )
})
hl <- do.call(rbind, res)

hl <- merge(hl, artist_meta[, c("artist", "total_plays", "first_play", "discovery_year")],
            by = "artist")

# ---- character classification ------------------------------
hl$character <- with(hl, ifelse(
  status == "still burning", "Evergreen", ifelse(
    status != "decayed",       "Unclassified", ifelse(
      half_life_months <= 3,     "Supernova", ifelse(
        half_life_months <= 11,    "Steady", "Slow burn")))))

hl <- hl[order(-hl$total_plays), ]
write.csv(hl, HL_FILE, row.names = FALSE)
saveRDS(hl, file.path(RESULTS_DIR, "half_lives.rds"))

# ---- console summary ---------------------------------------
scored <- hl[hl$status == "decayed", ]

cat("\n=====================================================\n")
cat("  LISTENING HALF-LIVES for", USERNAME, "\n")
cat("=====================================================\n\n")
cat("Artists analysed:        ", nrow(hl), "\n")
cat("Measurable half-lives:   ", nrow(scored), "\n")
cat("Still burning (evergreen):", sum(hl$character == "Evergreen"), "\n")
cat("Excluded (no clear peak / too recent):",
    sum(hl$character == "Unclassified"), "\n\n")

cat("*** YOUR MEDIAN HALF-LIFE:", median(scored$half_life_months), "months ***\n\n")

cat("Breakdown:\n")
print(table(hl$character))

cat("\n--- Fastest burnouts (Supernovas) ---\n")
sn <- scored[order(scored$half_life_months, -scored$peak_intensity), ]
print(head(sn[, c("artist", "total_plays", "peak_month", "half_life_months")], 10),
      row.names = FALSE)

cat("\n--- Longest loyalty (Slow burns) ---\n")
sb <- scored[order(-scored$half_life_months), ]
print(head(sb[, c("artist", "total_plays", "peak_month", "half_life_months")], 10),
      row.names = FALSE)

cat("\n--- Evergreens: never fell below half ---\n")
eg <- hl[hl$character == "Evergreen", ]
eg <- eg[order(-eg$total_plays), ]
print(head(eg[, c("artist", "total_plays", "peak_month", "months_after_peak")], 10),
      row.names = FALSE)

cat("\nFull table written to", HL_FILE, "\n")

