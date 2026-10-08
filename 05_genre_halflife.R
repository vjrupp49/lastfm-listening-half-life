# ==========================================================
# 05_genre_halflife.R  --  OPTIONAL extension
# Tags each artist with its top Last.fm genre tag, then asks:
# which sounds are structurally sticky for you?
# ==========================================================

source("00_config.R")
library(httr); library(jsonlite); library(ggplot2)

hl <- readRDS(file.path(RESULTS_DIR, "half_lives.rds"))
targets <- head(hl$artist[order(-hl$total_plays)], 200)

# ---- fetch tags (cached) ------------------------------------
tags <- if (file.exists(TAGS_FILE)) readRDS(TAGS_FILE) else list()
todo <- setdiff(targets, names(tags))

if (length(todo) > 0) {
  message("Fetching tags for ", length(todo), " artists...")
  pb <- txtProgressBar(min = 0, max = length(todo), style = 3)
  for (i in seq_along(todo)) {
    a <- todo[i]
    url <- paste0("http://ws.audioscrobbler.com/2.0/",
                  "?method=artist.gettoptags",
                  "&artist=",  utils::URLencode(a, reserved = TRUE),
                  "&api_key=", API_KEY, "&format=json&autocorrect=1")
    r <- try(GET(url, user_agent(USER_AGENT_STRING), timeout(20)), silent = TRUE)
    top <- NA_character_
    if (!inherits(r, "try-error") && status_code(r) == 200) {
      d <- try(fromJSON(content(r, "text", encoding = "UTF-8"),
                        simplifyVector = FALSE), silent = TRUE)
      if (!inherits(d, "try-error") && length(d$toptags$tag) > 0) {
        top <- tolower(d$toptags$tag[[1]]$name)
      }
    }
    tags[[a]] <- top
    setTxtProgressBar(pb, i)
    Sys.sleep(0.25)
    if (i %% 25 == 0) saveRDS(tags, TAGS_FILE)
  }
  close(pb)
  saveRDS(tags, TAGS_FILE)
}

hl$tag <- unlist(tags)[hl$artist]

# ---- median half-life by genre ------------------------------
sc <- hl[hl$status == "decayed" & !is.na(hl$tag), ]
g  <- aggregate(list(median_hl = sc$half_life_months),
                by = list(tag = sc$tag), FUN = median)
gn <- aggregate(list(n = sc$half_life_months),
                by = list(tag = sc$tag), FUN = length)
g  <- merge(g, gn)
g  <- g[g$n >= 4, ]
g  <- g[order(-g$median_hl), ]

write.csv(g, file.path(RESULTS_DIR, "genre_half_lives.csv"), row.names = FALSE)

cat("\n--- Median half-life by genre (>= 4 artists) ---\n")
print(g, row.names = FALSE)

g$tag <- factor(g$tag, levels = g$tag[order(g$median_hl)])
p <- ggplot(g, aes(median_hl, tag)) +
  geom_segment(aes(x = 0, xend = median_hl, yend = tag),
               colour = "grey85", linewidth = 0.8) +
  geom_point(aes(size = n), colour = "#6C3483") +
  scale_size_continuous(name = "artists", range = c(2, 6)) +
  labs(title = "Which genres hold you longest?",
       subtitle = "Median half-life of artists whose top Last.fm tag is...",
       x = "Median half-life (months)", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave(file.path(FIG_DIR, "fig4_genre_half_lives.png"), p,
       width = 8, height = 7, dpi = 200)
print(p)

