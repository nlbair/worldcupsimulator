library(dplyr)
library(stringr)

DOWNLOADS <- r"(C:\Users\noahl\Downloads)"

dk_raw <- read.csv(file.path(DOWNLOADS, "wc2026_odds_final.csv"), stringsAsFactors = FALSE)
sim    <- read.csv(file.path(DOWNLOADS, "wc2026_sim_probs.csv"),  stringsAsFactors = FALSE)

# ==================== NAME MAPPING ====================
name_map <- c(
  "USA"            = "United States",
  "Czech Republic" = "Czechia",
  "Bosnia"         = "Bosnia and Herzegovina",
  "DR Congo"       = "DR Congo",
  "Curacao"        = "Curaçao"
)

dk <- dk_raw %>%
  mutate(team_mapped = ifelse(team %in% names(name_map), name_map[team], team)) %>%
  filter(!team_mapped %in% c("Italy", "Poland", "Denmark", "Kosovo", "Jamaica", "Bolivia"))

# ==================== CONVERSION ====================
dk <- dk %>%
  mutate(
    raw_prob = 1 / decimal_odds,
    payout   = decimal_odds - 1,
    dk_prob  = raw_prob
  )

# ==================== MARKET -> SIM COLUMN MAPPING ====================
market_map <- list(
  "Winner"                     = "Champion",
  "To Reach The Semi Final"    = "SF",
  "To Reach The Quarter Final" = "QF",
  "To Qualify From Group"      = "Qualify",
  "Group Winner"               = "GroupWin"
)

# ==================== CALCULATE EV ====================
results <- lapply(names(market_map), function(mkt) {
  sim_col <- market_map[[mkt]]
  
  subset <- dk %>% filter(market == mkt)
  if (nrow(subset) == 0) return(NULL)
  
  subset %>%
    left_join(
      sim %>% select(Team, sim_col_val = all_of(sim_col)),
      by = c("team_mapped" = "Team")
    ) %>%
    filter(!is.na(sim_col_val), !is.na(dk_prob), !is.na(payout)) %>%
    mutate(
      sim_prob    = sim_col_val / 100,
      ev          = (sim_prob * payout) - (1 - sim_prob),
      edge        = sim_prob - dk_prob,
      market_name = mkt
    ) %>%
    select(
      market  = market_name,
      team    = team_mapped,
      event,
      dk_odds = american_odds,
      dk_prob,
      sim_prob,
      edge,
      ev,
      payout
    )
}) %>%
  bind_rows() %>%
  mutate(
    dk_prob_pct  = round(dk_prob  * 100, 1),
    sim_prob_pct = round(sim_prob * 100, 1),
    edge_pct     = round(edge     * 100, 1),
    ev_pct       = round(ev       * 100, 1)
  )


write.csv(results, file.path(DOWNLOADS, "wc2026_ev_results.csv"), row.names = FALSE)