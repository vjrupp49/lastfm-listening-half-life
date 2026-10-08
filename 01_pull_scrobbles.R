# ==========================================================
# 01_pull_scrobbles.R  --  download your full scrobble history
# Run ONCE. It caches to data/raw_scrobbles.rds and will not
# re-download unless you set FORCE_REFRESH <- TRUE.
# ==========================================================

source("00_config.R")
library(httr)
library(jsonlite)

FORCE_REFRESH <- FALSE

if (file.exists(RAW_FILE) && !FORCE_REFRESH) {
  message("Cache found at ", RAW_FILE, " - skipping download.")
  message("Set FORCE_REFRESH <- TRUE at the top of this script to re-pull.")
} else {
  
  # Pin the end timestamp ONCE so pages don't shift if you scrobble mid-pull.
  TO_TS <- as.integer(Sys.time())
  
  fetch_page <- function(page, to_ts, tries = 4) {
    url <- paste0(
      "http://ws.audioscrobbler.com/2.0/",
      "?method=user.getrecenttracks",
      "&user=",    utils::URLencode(USERNAME, reserved = TRUE),
      "&api_key=", API_KEY,
      "&format=json",
      "&limit=200",
      "&to=",      to_ts,
      "&page=",    page
    )
    for (attempt in seq_len(tries)) {
      resp <- try(GET(url, user_agent(USER_AGENT_STRING), timeout(30)), silent = TRUE)
      if (!inherits(resp, "try-error") && status_code(resp) == 200) {
        txt <- content(resp, as = "text", encoding = "UTF-8")
        dat <- fromJSON(txt, simplifyVector = FALSE)
        if (!is.null(dat$error)) {
          stop("Last.fm error ", dat$error, ": ", dat$message)
        }
        return(dat)
      }
      message("  page ", page, " attempt ", attempt, " failed; retrying...")
      Sys.sleep(2 * attempt)
    }
    stop("Gave up on page ", page)
  }
  
  parse_tracks <- function(tracks) {
    rows <- lapply(tracks, function(tr) {
      # The "now playing" track has no date node - drop it.
      if (is.null(tr$date)) return(NULL)
      data.frame(
        artist = tr$artist[["#text"]] %||% NA_character_,
        track  = tr$name             %||% NA_character_,
        album  = tr$album[["#text"]] %||% NA_character_,
        uts    = as.numeric(tr$date$uts),
        stringsAsFactors = FALSE
      )
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) return(NULL)
    do.call(rbind, rows)
  }
  
  message("Contacting Last.fm...")
  first <- fetch_page(1, TO_TS)
  total_pages <- as.integer(first$recenttracks[["@attr"]][["totalPages"]])
  total_plays <- as.integer(first$recenttracks[["@attr"]][["total"]])
  message("Found ", format(total_plays, big.mark = ","), " scrobbles across ",
          total_pages, " pages. Estimated time: ~",
          round(total_pages * 0.6 / 60, 1), " minutes.")
  
  all_pages <- vector("list", total_pages)
  all_pages[[1]] <- parse_tracks(first$recenttracks$track)
  
  if (total_pages > 1) {
    pb <- txtProgressBar(min = 1, max = total_pages, style = 3)
    for (p in 2:total_pages) {
      Sys.sleep(0.25)                       # be polite; avoid rate limiting
      dat <- fetch_page(p, TO_TS)
      all_pages[[p]] <- parse_tracks(dat$recenttracks$track)
      setTxtProgressBar(pb, p)
      if (p %% 25 == 0) {                   # partial save, in case of crash
        saveRDS(do.call(rbind, all_pages[1:p]),
                file.path(DATA_DIR, "raw_scrobbles_partial.rds"))
      }
    }
    close(pb)
  }
  
  scrobbles <- do.call(rbind, all_pages)
  scrobbles <- scrobbles[!is.na(scrobbles$uts) & !is.na(scrobbles$artist), ]
  scrobbles <- scrobbles[!duplicated(scrobbles[c("artist", "track", "uts")]), ]
  scrobbles <- scrobbles[order(scrobbles$uts), ]
  
  saveRDS(scrobbles, RAW_FILE)
  unlink(file.path(DATA_DIR, "raw_scrobbles_partial.rds"))
  
  message("\nSaved ", nrow(scrobbles), " scrobbles to ", RAW_FILE)
  message("Date range: ",
          format(as.POSIXct(min(scrobbles$uts), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"),
          " to ",
          format(as.POSIXct(max(scrobbles$uts), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"))
  message("Distinct artists: ", length(unique(scrobbles$artist)))
}

