# ==========================================================
# 02_build_monthly.R  --  turn raw scrobbles into an
# artist x month grid, zero-filled from each artist's first
# play through the end of your history.
# ==========================================================

source("00_config.R")

scr <- readRDS(RAW_FILE)

scr$time  <- as.POSIXct(scr$uts, origin = "1970-01-01", tz = "UTC")
scr$month <- as.Date(format(scr$time, "%Y-%m-01"))

last_month <- max(scr$month)

# Total plays per artist, and the discovery date (first ever scrobble)
totals <- aggregate(list(total_plays = scr$uts), by = list(artist = scr$artist), FUN = length)
firsts <- aggregate(list(first_play  = scr$time), by = list(artist = scr$artist), FUN = min)
artist_meta <- merge(totals, firsts, by = "artist")
artist_meta$discovery_year <- as.integer(format(artist_meta$first_play, "%Y"))
artist_meta <- artist_meta[order(-artist_meta$total_plays), ]

keep <- artist_meta$artist[artist_meta$total_plays >= MIN_PLAYS]
message(length(keep), " artists have >= ", MIN_PLAYS, " plays (of ",
        nrow(artist_meta), " total).")

counts <- as.data.frame(table(artist = scr$artist[scr$artist %in% keep],
                              month  = scr$month[scr$artist %in% keep]),
                        stringsAsFactors = FALSE)
counts <- counts[counts$Freq > 0, ]
names(counts)[names(counts) == "Freq"] <- "plays"
counts$month  <- as.Date(counts$month)
counts$artist <- as.character(counts$artist)

# Zero-fill each artist's own timeline
filled <- lapply(split(counts, counts$artist), function(d) {
  ms <- seq(min(d$month), last_month, by = "month")
  data.frame(
    artist = d$artist[1],
    month  = ms,
    plays  = ifelse(is.na(match(ms, d$month)), 0, d$plays[match(ms, d$month)]),
    stringsAsFactors = FALSE
  )
})
artist_month <- do.call(rbind, filled)
rownames(artist_month) <- NULL

saveRDS(list(artist_month = artist_month, artist_meta = artist_meta),
        MONTHLY_FILE)

message("Built monthly grid: ", nrow(artist_month), " artist-month rows.")
message("Saved to ", MONTHLY_FILE)

