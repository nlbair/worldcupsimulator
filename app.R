library(shiny)
library(dplyr)
library(stringr)
library(DT)
library(ggplot2)
library(tidyr)

raw_text <- paste(readLines("newelotext.txt", encoding = "UTF-8"), collapse = "")
raw_text <- gsub("âˆ'", "-", raw_text)
sim_probs <- read.csv("wc2026_sim_probs.csv", stringsAsFactors = FALSE)
ev_results <- read.csv("wc2026_ev_results.csv", stringsAsFactors = FALSE)

ev_results <- ev_results %>%
  mutate(
    dk_prob  = round(dk_prob  * 100, 1),
    sim_prob = round(sim_prob * 100, 1),
    edge     = round(edge     * 100, 1),
    ev      = round(ev       * 100, 1)
  )

team_names <- c(
  "Spain", "Argentina", "France", "Portugal", "England", "Brazil", "Colombia", 
  "Netherlands", "Ecuador", "Croatia", "Uruguay", "Denmark", "Italy", "Germany",
  "Norway", "Switzerland", "Mexico", "Japan", "Austria", "Belgium", "Paraguay",
  "Turkey", "Morocco", "Serbia", "Senegal", "Russia", "Canada", "Australia",
  "Ukraine", "Greece", "Scotland", "Czechia", "Iran", "South Korea", "Panama",
  "Slovenia", "Sweden", "Wales", "Slovakia", "Uzbekistan", "Venezuela", "Poland",
  "Peru", "Georgia", "United States", "Bolivia", "Algeria", "Hungary", "Chile",
  "Costa Rica", "Egypt", "Romania", "Israel", "Tunisia", "Kosovo", "Albania",
  "North Macedonia", "Jordan", "Mali", "Ireland", "Saudi Arabia", 
  "DR Congo", "Ivory Coast", "Nigeria", "Cameroon",
  "Northern Ireland", "Honduras", "New Zealand", "Jamaica", "Iraq", "Iceland",
  "Angola", "United Arab Emirates", "Finland", "Cape Verde", "Burkina Faso",
  "Oman", "Haiti", "South Africa", "Bosnia and Herzegovina", "Guatemala", "Ghana",
  "Guinea", "Gabon", "Qatar", "Belarus", "Benin", "Equatorial Guinea",
  "Northern Cyprus", "Palestine", "Bahrain", "Montenegro", "Bulgaria", "Uganda",
  "Syria", "Libya", "Suriname", "Gambia", "Kurdistan", "Luxembourg", "Armenia",
  "Curaçao", "Zambia", "China", "Comoros", "Mozambique", "Kazakhstan",
  "Trinidad and Tobago", "Niger", "North Korea", "Sudan", "Martinique",
  "El Salvador", "Zimbabwe", "Azerbaijan", "Kenya", "Madagascar", "Estonia",
  "Thailand", "Indonesia", "Togo", "Lebanon", "Réunion", "Mauritania",
  "Sierra Leone", "Guadeloupe", "Tanzania", "Faroe Islands", "Namibia",
  "Zanzibar", "Vietnam", "Rwanda", "Cyprus", "Kyrgyzstan", "Botswana", "Malaysia",
  "Nicaragua", "Moldova", "New Caledonia", "Liberia", "Latvia", "Kuwait",
  "Dominican Republic", "Tajikistan", "Lithuania", "Malawi", "Ethiopia", "Guyana",
  "Malta", "Burundi", "Guinea-Bissau", "French Guiana", "Central African Republic",
  "Eswatini", "Mayotte", "Congo", "Turkmenistan", "Cuba", "Lesotho", "Hong Kong",
  "India", "Tahiti", "Fiji", "St Vincent & Grenadines", "Bermuda", "Eritrea",
  "Chad", "South Sudan", "Philippines", "Puerto Rico", "Solomon Islands", "Yemen",
  "Grenada", "Papua New Guinea", "Mauritius", "Andorra", "Singapore", "Belize",
  "Saint Kitts and Nevis", "Vanuatu", "Afghanistan", "Saint Lucia",
  "São Tomé and Príncipe", "Gibraltar", "Saint Martin", "Somaliland",
  "Western Sahara", "Montserrat", "Dominica", "Myanmar", "Greenland", "Barbados",
  "Sint Maarten", "Djibouti", "Aruba", "Nepal", "Antigua and Barbuda", "Somalia",
  "Monaco", "Liechtenstein", "Bangladesh", "Taiwan", "Maldives", "Seychelles",
  "San Marino", "Bonaire", "Cambodia", "Pakistan", "Cayman Islands",
  "Chagos Islands", "Sri Lanka", "Bahamas", "Tuvalu", "Sint Eustatius", "Samoa",
  "Mongolia", "Saint Barthélemy", "Guam", "Laos", "Wallis and Futuna", "Vatican",
  "Saba", "East Timor", "Saint Pierre and Miquelon", "Brunei",
  "Turks and Caicos Islands", "Tibet", "British Virgin Islands", "Cook Islands",
  "Bhutan", "US Virgin Islands", "Anguilla", "Macao", "Christmas Island",
  "Falkland Islands", "Federated States of Micronesia", "Marshall Islands",
  "Kiribati", "Tonga", "Niue", "Northern Mariana Islands", "Cocos Islands",
  "Palau", "Eastern Samoa"
)

teams_data <- list()
for (i in 1:length(team_names)) {
  team <- team_names[i]
  rank <- i
  pos <- str_locate(raw_text, fixed(team))[1]
  if (is.na(pos)) next
  after_team <- substr(raw_text, pos + nchar(team), nchar(raw_text))
  elo <- if (rank >= 187) substr(after_team, 1, 3) else substr(after_team, 1, 4)
  teams_data[[i]] <- data.frame(Rank = rank, Team = team, Rating = as.numeric(elo), 
                                stringsAsFactors = FALSE)
}
df <- bind_rows(teams_data)

# Check all WC2026 teams have valid ELO ratings
wc_teams <- c("Mexico", "South Korea", "South Africa", "Czechia",
              "Canada", "Switzerland", "Qatar", "Bosnia and Herzegovina",
              "Brazil", "Morocco", "Scotland", "Haiti",
              "United States", "Paraguay", "Australia", "Turkey",
              "Germany", "Ecuador", "Ivory Coast", "Curaçao",
              "Netherlands", "Japan", "Tunisia", "Sweden",
              "Belgium", "Iran", "Egypt", "New Zealand",
              "Spain", "Uruguay", "Saudi Arabia", "Cape Verde",
              "France", "Senegal", "Norway", "Iraq",
              "Argentina", "Austria", "Algeria", "Jordan",
              "Portugal", "Colombia", "Uzbekistan", "DR Congo",
              "England", "Croatia", "Panama", "Ghana")

for (team in wc_teams) {
  elo <- df$Rating[df$Team == team]
  if (length(elo) == 0 || is.na(elo)) {
    cat("MISSING ELO:", team, "\n")
  } else {
    cat("OK:", team, "-", elo, "\n")
  }
}

# Complete country codes for all teams
country_codes <- c(
  "Spain" = "es", "Argentina" = "ar", "France" = "fr", "Portugal" = "pt", 
  "England" = "gb-eng", "Brazil" = "br", "Colombia" = "co", "Netherlands" = "nl",
  "Ecuador" = "ec", "Croatia" = "hr", "Uruguay" = "uy", "Denmark" = "dk",
  "Italy" = "it", "Germany" = "de", "Norway" = "no", "Switzerland" = "ch",
  "Mexico" = "mx", "Japan" = "jp", "Austria" = "at", "Belgium" = "be",
  "Paraguay" = "py", "Turkey" = "tr", "Morocco" = "ma", "Serbia" = "rs",
  "Senegal" = "sn", "Russia" = "ru", "Canada" = "ca", "Australia" = "au",
  "Ukraine" = "ua", "Greece" = "gr", "Scotland" = "gb-sct", "Czechia" = "cz",
  "Iran" = "ir", "South Korea" = "kr", "Panama" = "pa", "Slovenia" = "si",
  "Sweden" = "se", "Wales" = "gb-wls", "Slovakia" = "sk", "Uzbekistan" = "uz",
  "Venezuela" = "ve", "Poland" = "pl", "Peru" = "pe", "Georgia" = "ge",
  "United States" = "us", "Bolivia" = "bo", "Algeria" = "dz", "Hungary" = "hu",
  "Chile" = "cl", "Costa Rica" = "cr", "Egypt" = "eg", "Romania" = "ro",
  "Israel" = "il", "Tunisia" = "tn", "Kosovo" = "xk", "Albania" = "al",
  "North Macedonia" = "mk", "Jordan" = "jo", "Mali" = "ml", "Ireland" = "ie",
  "Saudi Arabia" = "sa", "Ivory Coast" = "ci", "Nigeria" = "ng", "Cameroon" = "cm",
  "Honduras" = "hn", "New Zealand" = "nz", "Jamaica" = "jm", "Ghana" = "gh",
  "Qatar" = "qa", "South Africa" = "za", "Cape Verde" = "cv", "Iraq" = "iq",
  "Iceland" = "is", "Angola" = "ao", "United Arab Emirates" = "ae", "Finland" = "fi",
  "Burkina Faso" = "bf", "Oman" = "om", "Haiti" = "ht", "Bosnia and Herzegovina" = "ba",
  "DR Congo" = "cd",
  "Guatemala" = "gt", "Guinea" = "gn", "Gabon" = "ga", "Belarus" = "by",
  "Benin" = "bj", "Equatorial Guinea" = "gq", "Palestine" = "ps", "Bahrain" = "bh",
  "Montenegro" = "me", "Bulgaria" = "bg", "Uganda" = "ug", "Syria" = "sy",
  "Libya" = "ly", "Suriname" = "sr", "Gambia" = "gm", "Luxembourg" = "lu",
  "Armenia" = "am", "Curaçao" = "cw", "Zambia" = "zm", "China" = "cn",
  "Comoros" = "km", "Mozambique" = "mz", "Kazakhstan" = "kz", "Trinidad and Tobago" = "tt",
  "Niger" = "ne", "North Korea" = "kp", "Sudan" = "sd", "El Salvador" = "sv",
  "Zimbabwe" = "zw", "Azerbaijan" = "az", "Kenya" = "ke", "Madagascar" = "mg",
  "Estonia" = "ee", "Thailand" = "th", "Indonesia" = "id", "Togo" = "tg",
  "Lebanon" = "lb", "Mauritania" = "mr", "Sierra Leone" = "sl", "Tanzania" = "tz",
  "Faroe Islands" = "fo", "Namibia" = "na", "Vietnam" = "vn", "Rwanda" = "rw",
  "Cyprus" = "cy", "Kyrgyzstan" = "kg", "Botswana" = "bw", "Malaysia" = "my",
  "Nicaragua" = "ni", "Moldova" = "md", "Liberia" = "lr", "Latvia" = "lv",
  "Kuwait" = "kw", "Dominican Republic" = "do", "Tajikistan" = "tj", "Lithuania" = "lt",
  "Malawi" = "mw", "Ethiopia" = "et", "Guyana" = "gy", "Malta" = "mt",
  "Burundi" = "bi", "Guinea-Bissau" = "gw", "Central African Republic" = "cf",
  "Eswatini" = "sz", "Congo" = "cg", "Turkmenistan" = "tm", "Cuba" = "cu",
  "Lesotho" = "ls", "Hong Kong" = "hk", "India" = "in", "Fiji" = "fj",
  "Bermuda" = "bm", "Eritrea" = "er", "Chad" = "td", "South Sudan" = "ss",
  "Philippines" = "ph", "Puerto Rico" = "pr", "Solomon Islands" = "sb", "Yemen" = "ye",
  "Grenada" = "gd", "Papua New Guinea" = "pg", "Mauritius" = "mu", "Andorra" = "ad",
  "Singapore" = "sg", "Belize" = "bz", "Saint Kitts and Nevis" = "kn", "Vanuatu" = "vu",
  "Afghanistan" = "af", "Saint Lucia" = "lc", "São Tomé and Príncipe" = "st",
  "Gibraltar" = "gi", "Barbados" = "bb", "Sint Maarten" = "sx", "Djibouti" = "dj",
  "Aruba" = "aw", "Nepal" = "np", "Antigua and Barbuda" = "ag", "Somalia" = "so",
  "Monaco" = "mc", "Liechtenstein" = "li", "Bangladesh" = "bd", "Taiwan" = "tw",
  "Maldives" = "mv", "Seychelles" = "sc", "San Marino" = "sm", "Cambodia" = "kh",
  "Pakistan" = "pk", "Cayman Islands" = "ky", "Sri Lanka" = "lk", "Bahamas" = "bs",
  "Tuvalu" = "tv", "Samoa" = "ws", "Mongolia" = "mn", "Guam" = "gu",
  "Laos" = "la", "Brunei" = "bn", "Turks and Caicos Islands" = "tc",
  "British Virgin Islands" = "vg", "Cook Islands" = "ck", "Bhutan" = "bt",
  "US Virgin Islands" = "vi", "Anguilla" = "ai", "Macao" = "mo",
  "Falkland Islands" = "fk", "Federated States of Micronesia" = "fm",
  "Marshall Islands" = "mh", "Kiribati" = "ki", "Tonga" = "to", "Niue" = "nu",
  "Northern Mariana Islands" = "mp", "Palau" = "pw"
)

get_flag <- function(team_name) {
  # Special-case Northern Ireland with local PNG
  if (team_name == "Northern Ireland") {
    return('<img src="northernireland.png" style="margin-right: 5px; vertical-align: middle;">')
  }
  
  # Everything else uses flagcdn + country_codes
  code <- country_codes[[team_name]]
  if (!is.null(code)) {
    return(sprintf(
      '<img src="https://flagcdn.com/24x18/%s.png" style="margin-right: 5px; vertical-align: middle;">',
      code
    ))
  }
  
  # Fallback: no flag
  return("")
}



simulate_draw <- function() {
  groups <- list(
    A = c("Mexico", "South Korea", "South Africa", "Czechia"),
    B = c("Canada", "Switzerland", "Qatar", "Bosnia and Herzegovina"),
    C = c("Brazil", "Morocco", "Scotland", "Haiti"),
    D = c("United States", "Paraguay", "Australia", "Turkey"),
    E = c("Germany", "Ecuador", "Ivory Coast", "Curaçao"),
    F = c("Netherlands", "Japan", "Tunisia", "Sweden"),
    G = c("Belgium", "Iran", "Egypt", "New Zealand"),
    H = c("Spain", "Uruguay", "Saudi Arabia", "Cape Verde"),
    I = c("France", "Senegal", "Norway", "Iraq"),
    J = c("Argentina", "Austria", "Algeria", "Jordan"),
    K = c("Portugal", "Colombia", "Uzbekistan", "DR Congo"),
    L = c("England", "Croatia", "Panama", "Ghana")
  )
  return(groups)
}

simulate_match_with_goals <- function(team1, team2) {
  team1_elo <- df$Rating[df$Team == team1]
  team2_elo <- df$Rating[df$Team == team2]
  if (length(team1_elo) == 0 || is.na(team1_elo)) return(NULL)
  if (length(team2_elo) == 0 || is.na(team2_elo)) return(NULL)
  base_goals <- 1.3
  elo_diff <- team1_elo - team2_elo
  team1_multiplier <- 1.25^(elo_diff / 100)
  team2_multiplier <- 1.25^(-elo_diff / 100)
  team1_expected_goals <- max(0.1, base_goals * team1_multiplier)
  team2_expected_goals <- max(0.1, base_goals * team2_multiplier)
  team1_goals <- rpois(1, lambda = team1_expected_goals)
  team2_goals <- rpois(1, lambda = team2_expected_goals)
  
  generate_goal_minutes <- function(n_goals, max_min = 90) {
    if (n_goals == 0) return(character(0))
    sort(sample(1:max_min, n_goals, replace = TRUE))
  }
  
  goal_minutes_team1 <- generate_goal_minutes(team1_goals)
  goal_minutes_team2 <- generate_goal_minutes(team2_goals)
  
  if (team1_goals > team2_goals) {
    winner <- team1; team1_pts <- 3; team2_pts <- 0
  } else if (team2_goals > team1_goals) {
    winner <- team2; team1_pts <- 0; team2_pts <- 3
  } else {
    winner <- "Draw"; team1_pts <- 1; team2_pts <- 1
  }
  
  list(team1_goals = team1_goals, team2_goals = team2_goals,
       team1_pts = team1_pts, team2_pts = team2_pts, winner = winner,
       team1_elo = team1_elo, team2_elo = team2_elo,
       goal_minutes_team1 = goal_minutes_team1,
       goal_minutes_team2 = goal_minutes_team2)
}

simulate_group_stage <- function(group_teams) {
  standings <- data.frame(Team = group_teams, Points = 0, Wins = 0, Draws = 0, 
                          Losses = 0, GF = 0, GA = 0, GD = 0, stringsAsFactors = FALSE)
  matches <- list()
  for (i in 1:(length(group_teams) - 1)) {
    for (j in (i + 1):length(group_teams)) {
      team1 <- group_teams[i]; team2 <- group_teams[j]
      result <- simulate_match_with_goals(team1, team2)
      matches[[length(matches) + 1]] <- list(team1 = team1, team2 = team2, result = result)
      standings$Points[standings$Team == team1] <- standings$Points[standings$Team == team1] + result$team1_pts
      standings$GF[standings$Team == team1] <- standings$GF[standings$Team == team1] + result$team1_goals
      standings$GA[standings$Team == team1] <- standings$GA[standings$Team == team1] + result$team2_goals
      standings$Points[standings$Team == team2] <- standings$Points[standings$Team == team2] + result$team2_pts
      standings$GF[standings$Team == team2] <- standings$GF[standings$Team == team2] + result$team2_goals
      standings$GA[standings$Team == team2] <- standings$GA[standings$Team == team2] + result$team1_goals
      if (result$winner == team1) {
        standings$Wins[standings$Team == team1] <- standings$Wins[standings$Team == team1] + 1
        standings$Losses[standings$Team == team2] <- standings$Losses[standings$Team == team2] + 1
      } else if (result$winner == team2) {
        standings$Wins[standings$Team == team2] <- standings$Wins[standings$Team == team2] + 1
        standings$Losses[standings$Team == team1] <- standings$Losses[standings$Team == team1] + 1
      } else {
        standings$Draws[standings$Team == team1] <- standings$Draws[standings$Team == team1] + 1
        standings$Draws[standings$Team == team2] <- standings$Draws[standings$Team == team2] + 1
      }
    }
  }
  standings$GD <- standings$GF - standings$GA
  standings <- standings %>% arrange(desc(Points), desc(GD), desc(GF))
  list(standings = standings, matches = matches)
}

simulate_penalties <- function(team1, team2) {
  team1_elo <- df$Rating[df$Team == team1]
  team2_elo <- df$Rating[df$Team == team2]
  base_rate <- 0.75
  team1_rate <- max(0.60, min(0.90, base_rate + (team1_elo - team2_elo) / 200 * 0.05))
  team2_rate <- max(0.60, min(0.90, base_rate + (team2_elo - team1_elo) / 200 * 0.05))
  team1_score <- sum(runif(5) < team1_rate)
  team2_score <- sum(runif(5) < team2_rate)
  round <- 6
  while (team1_score == team2_score && round <= 10) {
    team1_score <- team1_score + (runif(1) < team1_rate)
    team2_score <- team2_score + (runif(1) < team2_rate)
    round <- round + 1
  }
  if (team1_score == team2_score) {
    winner <- ifelse(team1_elo > team2_elo, team1, team2)
  } else {
    winner <- ifelse(team1_score > team2_score, team1, team2)
  }
  list(winner = winner, score = sprintf("%d-%d", team1_score, team2_score))
}

simulate_knockout_match_full <- function(team1, team2, noise_sd = 75) {
  e1 <- df$Rating[df$Team == team1] + rnorm(1, 0, noise_sd)
  e2 <- df$Rating[df$Team == team2] + rnorm(1, 0, noise_sd)
  base_goals <- 1.2
  elo_diff <- e1 - e2
  t1g <- rpois(1, max(0.1, base_goals * 1.15^(elo_diff / 100)))
  t2g <- rpois(1, max(0.1, base_goals * 1.15^(-elo_diff / 100)))
  
  generate_goal_minutes <- function(n_goals, max_min = 90) {
    if (n_goals == 0) return(character(0))
    sort(sample(1:max_min, n_goals, replace = TRUE))
  }
  
  score_text <- sprintf("%d-%d", t1g, t2g)
  method <- "In Normal Time"
  all_minutes_team1 <- generate_goal_minutes(t1g)
  all_minutes_team2 <- generate_goal_minutes(t2g)
  
  if (t1g == t2g) {
    base <- 1.1 / 3
    et1 <- rpois(1, max(0.05, base * 1.15^(elo_diff / 100)))
    et2 <- rpois(1, max(0.05, base * 1.15^(-elo_diff / 100)))
    if (et1 > 0) all_minutes_team1 <- c(all_minutes_team1, sort(sample(91:120, et1, replace = TRUE)))
    if (et2 > 0) all_minutes_team2 <- c(all_minutes_team2, sort(sample(91:120, et2, replace = TRUE)))
    total1 <- t1g + et1; total2 <- t2g + et2
    score_text <- sprintf("%d-%d", total1, total2)
    method <- "AET"
    if (total1 == total2) {
      pen_result <- simulate_penalties(team1, team2)
      score_text <- sprintf("%d-%d (%s)", total1, total2, pen_result$score)
      winner <- pen_result$winner
      method <- "Pens"
    } else {
      winner <- ifelse(total1 > total2, team1, team2)
    }
    t1g <- total1; t2g <- total2
  } else {
    winner <- ifelse(t1g > t2g, team1, team2)
  }
  
  list(winner = winner, score = score_text, method = method, team1 = team1, team2 = team2,
       team1_goals = t1g, team2_goals = t2g,
       team1_elo = df$Rating[df$Team == team1],
       team2_elo = df$Rating[df$Team == team2],
       goal_minutes_team1 = all_minutes_team1,
       goal_minutes_team2 = all_minutes_team2)
}

assign_third_place_teams <- function(third_place_df, group_results) {
  best_third <- third_place_df$Team[1:8]
  third_groups <- third_place_df$Group[1:8]
  third_place_matches <- list(
    list(74, "E", c("A", "B", "C", "D", "F")), list(77, "I", c("C", "D", "F", "G", "H")),
    list(79, "A", c("C", "E", "F", "H", "I")), list(80, "L", c("E", "H", "I", "J", "K")),
    list(81, "D", c("B", "E", "F", "I", "J")), list(82, "G", c("A", "E", "H", "I", "J")),
    list(85, "B", c("E", "F", "G", "I", "J")), list(87, "K", c("D", "E", "I", "J", "L"))
  )
  assignments <- list(); used_thirds <- c()
  find_assignment <- function(match_idx) {
    if (match_idx > length(third_place_matches)) return(TRUE)
    match_info <- third_place_matches[[match_idx]]
    match_num <- match_info[[1]]; opponent_group <- match_info[[2]]; allowed_groups <- match_info[[3]]
    for (i in 1:length(best_third)) {
      team <- best_third[i]; team_group <- third_groups[i]
      if (team_group %in% allowed_groups && team_group != opponent_group && !(team %in% used_thirds)) {
        assignments[[as.character(match_num)]] <<- team; used_thirds <<- c(used_thirds, team)
        if (find_assignment(match_idx + 1)) return(TRUE)
        assignments[[as.character(match_num)]] <<- NULL; used_thirds <<- setdiff(used_thirds, team)
      }
    }
    return(FALSE)
  }
  if (!find_assignment(1)) {
    for (match_info in third_place_matches) {
      match_num <- match_info[[1]]
      if (is.null(assignments[[as.character(match_num)]])) {
        for (i in 1:length(best_third)) {
          team <- best_third[i]
          if (!(team %in% used_thirds)) {
            assignments[[as.character(match_num)]] <- team; used_thirds <- c(used_thirds, team); break
          }
        }
      }
    }
  }
  return(assignments)
}

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
      .modal-open { overflow: auto !important; }
      .main-title {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white; padding: 30px; border-radius: 10px; text-align: center;
        margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .champion-box {
        background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
        color: white; padding: 30px; border-radius: 15px; text-align: center;
        margin: 20px 0; box-shadow: 0 10px 25px rgba(0,0,0,0.3);
        animation: pulse 2s infinite;
      }
      @keyframes pulse { 0%, 100% { transform: scale(1); } 50% { transform: scale(1.05); } }
      .status-box {
        background: #f8f9fa; border-left: 4px solid #4CAF50;
        padding: 15px; margin: 15px 0; border-radius: 5px;
      }
      .match-card {
        background: white; border: 2px solid #e0e0e0; border-radius: 10px;
        padding: 15px; margin: 10px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        transition: all 0.3s; cursor: pointer;
      }
      .match-card:hover {
        box-shadow: 0 4px 8px rgba(0,0,0,0.2); transform: translateY(-2px);
      }
      .winner-highlight { font-weight: bold; color: #4CAF50; }
      .group-card {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        border-radius: 10px; padding: 15px; margin: 15px 0;
      }
      .team-clickable { cursor: pointer; color: #007bff; text-decoration: underline; }
      .team-clickable:hover { color: #0056b3; }
      .progress-bar-custom {
        width: 100%; height: 30px; background: #e0e0e0;
        border-radius: 15px; overflow: hidden; margin: 10px 0;
      }
      .progress-fill {
        height: 100%; background: linear-gradient(90deg, #4CAF50, #45a049);
        transition: width 0.5s ease; display: flex; align-items: center;
        justify-content: center; color: white; font-weight: bold;
      }
      .stat-card {
        background: white; border-radius: 10px; padding: 20px;
        margin: 10px 0; box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
      .stat-title { font-size: 14px; color: #666; margin-bottom: 5px; }
      .stat-value { font-size: 24px; font-weight: bold; color: #333; }
      .bracket-tree {
        display: flex;
        gap: 40px;
        padding: 20px;
        min-width: fit-content;
      }
      .bracket-column {
        display: flex;
        flex-direction: column;
        justify-content: space-around;
        min-width: 250px;
      }
      .bracket-match-box {
        background: white;
        border: 2px solid #ddd;
        border-radius: 8px;
        padding: 12px;
        margin: 8px 0;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .bracket-team {
        padding: 8px;
        border-bottom: 1px solid #eee;
      }
      .bracket-team:last-child {
        border-bottom: none;
      }
      .bracket-winner {
        background: #d4edda;
        font-weight: bold;
      }
      .trophy-animation {
        font-size: 80px;
        animation: spinBounce 2s ease-in-out infinite;
        display: inline-block;
      }
      #user_bracket_ui input[type='number'] {
        font-size: 11px;
        padding: 2px 4px;
      }
      @keyframes spinBounce {
        0%, 100% { transform: translateY(0) rotate(0deg); }
        25% { transform: translateY(-15px) rotate(10deg); }
        50% { transform: translateY(0) rotate(0deg); }
        75% { transform: translateY(-10px) rotate(-10deg); }
      }
    "))
  ),
  
  div(class = "main-title",
      h1("🏆 FIFA World Cup 2026 Simulator"),
      p("Simulate the entire tournament with realistic ELO-based predictions")
  ),
  
  fluidRow(
    column(3,
           wellPanel(
             actionButton("simulate", "⚽ Simulate Tournament", 
                          class = "btn-primary btn-lg", 
                          style = "width: 100%; font-size: 18px; padding: 15px;"),
             br(), br(),
             uiOutput("simulation_status"),
             br(),
             uiOutput("champion_display")
           )
    ),
    
    column(9,
           tabsetPanel(id = "tabs", type = "pills",
                       tabPanel("📊 Group Stage", br(), uiOutput("group_tables")),
                       tabPanel("🏆 Knockouts", br(),
                                h4("Round of 32"), uiOutput("r32_display"), hr(),
                                h4("Round of 16"), uiOutput("r16_display"), hr(),
                                h4("Quarter-Finals"), uiOutput("qf_display"), hr(),
                                h4("Semi-Finals"), uiOutput("sf_display"), hr(),
                                h4("Third Place & Final"), uiOutput("final_display")
                       ),
                       tabPanel("🌳 Bracket View", br(),
                                div(style = "overflow-x: auto;", uiOutput("bracket_tree"))
                       ),
                       tabPanel("📈 Statistics", br(), uiOutput("stats_display")),
                       tabPanel("🎯 Win Probabilities", br(), uiOutput("prob_table_display")),
                       tabPanel("⚡ EV Calculator", br(), uiOutput("ev_display")),
                       tabPanel("📉 Team Funnel", br(),
                                uiOutput("funnel_controls"),
                                br(),
                                uiOutput("funnel_display")
                       ),
                       tabPanel("🎖️ Final Standings", br(),
                                uiOutput("share_button_ui"),
                                uiOutput("final_standings")),
                       tabPanel("🔮 Your Bracket", br(),
                                uiOutput("user_bracket_ui")
                       ),
                       tabPanel("⚔️ Head to Head", br(),
                                uiOutput("h2h_ui")
                       )
           )
    )
  )
)

server <- function(input, output, session) {
  
  sim_data <- reactiveValues(
    groups = NULL, group_results = NULL, knockout_results = NULL,
    champion = NULL, runner_up = NULL, third = NULL, fourth = NULL,
    all_matches = list(), team_journeys = list()
  )
  
  observeEvent(input$simulate, {
    progress <- reactiveVal(0)
    output$simulation_status <- renderUI({
      div(class = "status-box",
          h4("⏳ Simulating Tournament..."),
          div(class = "progress-bar-custom",
              div(class = "progress-fill", 
                  style = sprintf("width: %d%%;", progress()),
                  sprintf("%d%%", progress()))
          )
      )
    })
    
    progress(10)
    groups <- simulate_draw()
    group_results_full <- list()
    all_group_matches <- list()
    
    for (g in names(groups)) {
      result <- simulate_group_stage(groups[[g]])
      group_results_full[[g]] <- result$standings
      all_group_matches[[g]] <- result$matches
    }
    
    progress(30)
    sim_data$groups <- groups
    sim_data$group_results <- group_results_full
    
    group_winners <- sapply(group_results_full, function(x) x$Team[1])
    group_runners_up <- sapply(group_results_full, function(x) x$Team[2])
    third_place_teams <- lapply(group_results_full, function(x) x[3, ])
    third_place_df <- bind_rows(third_place_teams, .id = "Group")
    third_place_df <- third_place_df %>% arrange(desc(Points), desc(GD), desc(GF))
    third_assignments <- assign_third_place_teams(third_place_df, group_results_full)
    
    progress(50)
    
    knockout <- list()
    knockout$r32 <- list()
    knockout$r32$m73 <- simulate_knockout_match_full(as.character(group_runners_up["A"]), as.character(group_runners_up["B"]))
    knockout$r32$m74 <- simulate_knockout_match_full(as.character(group_winners["E"]), as.character(third_assignments[["74"]]))
    knockout$r32$m75 <- simulate_knockout_match_full(as.character(group_winners["F"]), as.character(group_runners_up["C"]))
    knockout$r32$m76 <- simulate_knockout_match_full(as.character(group_winners["C"]), as.character(group_runners_up["F"]))
    knockout$r32$m77 <- simulate_knockout_match_full(as.character(group_winners["I"]), as.character(third_assignments[["77"]]))
    knockout$r32$m78 <- simulate_knockout_match_full(as.character(group_runners_up["E"]), as.character(group_runners_up["I"]))
    knockout$r32$m79 <- simulate_knockout_match_full(as.character(group_winners["A"]), as.character(third_assignments[["79"]]))
    knockout$r32$m80 <- simulate_knockout_match_full(as.character(group_winners["L"]), as.character(third_assignments[["80"]]))
    knockout$r32$m81 <- simulate_knockout_match_full(as.character(group_winners["D"]), as.character(third_assignments[["81"]]))
    knockout$r32$m82 <- simulate_knockout_match_full(as.character(group_winners["G"]), as.character(third_assignments[["82"]]))
    knockout$r32$m83 <- simulate_knockout_match_full(as.character(group_runners_up["K"]), as.character(group_runners_up["L"]))
    knockout$r32$m84 <- simulate_knockout_match_full(as.character(group_winners["H"]), as.character(group_runners_up["J"]))
    knockout$r32$m85 <- simulate_knockout_match_full(as.character(group_winners["B"]), as.character(third_assignments[["85"]]))
    knockout$r32$m86 <- simulate_knockout_match_full(as.character(group_winners["J"]), as.character(group_runners_up["H"]))
    knockout$r32$m87 <- simulate_knockout_match_full(as.character(group_winners["K"]), as.character(third_assignments[["87"]]))
    knockout$r32$m88 <- simulate_knockout_match_full(as.character(group_runners_up["D"]), as.character(group_runners_up["G"]))
    
    progress(60)
    
    knockout$r16 <- list()
    knockout$r16$m89 <- simulate_knockout_match_full(knockout$r32$m74$winner, knockout$r32$m77$winner)
    knockout$r16$m90 <- simulate_knockout_match_full(knockout$r32$m73$winner, knockout$r32$m75$winner)
    knockout$r16$m91 <- simulate_knockout_match_full(knockout$r32$m76$winner, knockout$r32$m78$winner)
    knockout$r16$m92 <- simulate_knockout_match_full(knockout$r32$m79$winner, knockout$r32$m80$winner)
    knockout$r16$m93 <- simulate_knockout_match_full(knockout$r32$m83$winner, knockout$r32$m84$winner)
    knockout$r16$m94 <- simulate_knockout_match_full(knockout$r32$m81$winner, knockout$r32$m82$winner)
    knockout$r16$m95 <- simulate_knockout_match_full(knockout$r32$m86$winner, knockout$r32$m88$winner)
    knockout$r16$m96 <- simulate_knockout_match_full(knockout$r32$m85$winner, knockout$r32$m87$winner)
    
    progress(75)
    
    knockout$qf <- list()
    knockout$qf$m97 <- simulate_knockout_match_full(knockout$r16$m89$winner, knockout$r16$m90$winner)
    knockout$qf$m98 <- simulate_knockout_match_full(knockout$r16$m93$winner, knockout$r16$m94$winner)
    knockout$qf$m99 <- simulate_knockout_match_full(knockout$r16$m91$winner, knockout$r16$m92$winner)
    knockout$qf$m100 <- simulate_knockout_match_full(knockout$r16$m95$winner, knockout$r16$m96$winner)
    
    progress(85)
    
    knockout$sf <- list()
    knockout$sf$m101 <- simulate_knockout_match_full(knockout$qf$m97$winner, knockout$qf$m98$winner)
    knockout$sf$m102 <- simulate_knockout_match_full(knockout$qf$m99$winner, knockout$qf$m100$winner)
    
    third_1 <- setdiff(c(knockout$qf$m97$winner, knockout$qf$m98$winner), knockout$sf$m101$winner)
    third_2 <- setdiff(c(knockout$qf$m99$winner, knockout$qf$m100$winner), knockout$sf$m102$winner)
    knockout$third <- simulate_knockout_match_full(third_1, third_2)
    knockout$final <- simulate_knockout_match_full(knockout$sf$m101$winner, knockout$sf$m102$winner)
    
    progress(100)
    
    sim_data$knockout_results <- knockout
    sim_data$champion <- knockout$final$winner
    sim_data$runner_up <- setdiff(c(knockout$sf$m101$winner, knockout$sf$m102$winner), knockout$final$winner)
    sim_data$third <- knockout$third$winner
    sim_data$fourth <- setdiff(c(third_1, third_2), knockout$third$winner)
    sim_data$all_matches <- c(all_group_matches, knockout)
    
    team_journeys <- list()
    all_teams <- unique(unlist(groups))
    for (team in all_teams) {
      journey <- list()
      for (g in names(groups)) {
        if (team %in% groups[[g]]) {
          journey$group <- g
          journey$group_matches <- Filter(function(m) m$team1 == team || m$team2 == team, all_group_matches[[g]])
          standings <- group_results_full[[g]]
          journey$group_position <- which(standings$Team == team)
          break
        }
      }
      journey$knockout <- list()
      for (round_name in c("r32", "r16", "qf", "sf", "third", "final")) {
        if (round_name %in% names(knockout)) {
          round_matches <- knockout[[round_name]]
          if (!is.list(round_matches[[1]])) round_matches <- list(round_matches)
          for (match in round_matches) {
            if (match$team1 == team || match$team2 == team) {
              journey$knockout[[round_name]] <- match
              if (match$winner != team) {
                journey$eliminated_in <- round_name
                break
              }
            }
          }
          if (!is.null(journey$eliminated_in)) break
        }
      }
      if (team == sim_data$champion) journey$final_result <- "Champion"
      else if (team == sim_data$runner_up) journey$final_result <- "Runner-up"
      else if (team == sim_data$third) journey$final_result <- "Third Place"
      else if (team == sim_data$fourth) journey$final_result <- "Fourth Place"
      else if (!is.null(journey$eliminated_in)) {
        round_names <- c(r32 = "Round of 32", r16 = "Round of 16", qf = "Quarter-Finals", 
                         sf = "Semi-Finals", third = "Third Place Match")
        journey$final_result <- paste("Eliminated in", round_names[[journey$eliminated_in]])
      } else {
        journey$final_result <- "Group Stage"
      }
      team_journeys[[team]] <- journey
    }
    sim_data$team_journeys <- team_journeys
    
    output$simulation_status <- renderUI({
      div(class = "status-box",
          h4("✅ Simulation Complete!", style = "color: #4CAF50;"),
          p("🎉 Click on any team to see their tournament journey!")
      )
    })
  })
  
  output$champion_display <- renderUI({
    req(sim_data$champion)
    div(class = "champion-box",
        div(class = "trophy-animation", "🏆"),
        h2(HTML(paste0(get_flag(sim_data$champion), sim_data$champion))),
        h4("World Cup Champion"),
        p(style = "font-size: 36px;", "🎉 🎊 ✨")
    )
  })
  
  output$group_tables <- renderUI({
    req(sim_data$group_results)
    
    third_place_teams <- lapply(sim_data$group_results, function(x) x[3, ])
    third_place_df <- bind_rows(third_place_teams, .id = "Group")
    third_place_df <- third_place_df %>% arrange(desc(Points), desc(GD), desc(GF))
    advancing_third_place <- third_place_df$Team[1:8]
    
    lapply(names(sim_data$group_results), function(g) {
      standings <- sim_data$group_results[[g]]
      standings_display <- standings
      
      standings_display$Team <- sapply(1:nrow(standings), function(i) {
        
        team <- standings$Team[i]
        
        # Print each team being handled
        message("🟦 Rendering team row: ", team)
        
        tryCatch({
          
          is_qualified <- if (i <= 2) TRUE else if (i == 3) team %in% advancing_third_place else FALSE
          
          row_style <- if (is_qualified)
            'style="background-color: #d4edda; font-weight: bold;"'
          else
            'style="background-color: #f8d7da;"'
          
          # TEAM HTML
          html_out <- HTML(paste0(
            '<div ', row_style, '>',
            get_flag(team),
            '<span class="team-clickable" onclick="Shiny.setInputValue(\'team_click\', \'',
            team, '\', {priority: \'event\'})">', 
            team, '</span></div>'
          ))
          
          return(html_out)
          
        }, error = function(e) {
          
          # Debug output → prints to console so you see the actual team causing failure
          message("❌ ERROR while rendering team: ", team)
          message("❌ ERROR MESSAGE: ", e$message)
          
          # Return fallback so UI doesn't break
          return(HTML(paste0('<div style="background-color:#ffcccc;">ERROR TEAM: ', team, '</div>')))
        })
      })
      
      
      div(class = "group-card",
          h4(paste("Group", g), style = "color: #333;"),
          p(style = "font-size: 12px; color: #666;", "✅ Green = Qualified | ❌ Red = Eliminated"),
          renderDT({
            datatable(standings_display[, c("Team", "Points", "Wins", "Draws", "Losses", "GF", "GA", "GD")],
                      options = list(dom = 't', pageLength = 4, ordering = FALSE),
                      rownames = FALSE, escape = FALSE, class = 'cell-border stripe')
          })
      )
    }) %>% tagList()
  })
  
  output$stats_display <- renderUI({
    req(sim_data$knockout_results)
    
    all_knockout <- list()
    for (round_name in names(sim_data$knockout_results)) {
      round_data <- sim_data$knockout_results[[round_name]]
      if (is.list(round_data) && !is.null(round_data$team1)) {
        all_knockout <- c(all_knockout, list(round_data))
      } else if (is.list(round_data)) {
        all_knockout <- c(all_knockout, round_data)
      }
    }
    
    team_goals <- list()
    for (match in all_knockout) {
      if (!is.null(match$team1)) {
        team_goals[[match$team1]] <- (team_goals[[match$team1]] %||% 0) + match$team1_goals
        team_goals[[match$team2]] <- (team_goals[[match$team2]] %||% 0) + match$team2_goals
      }
    }
    
    if (length(team_goals) > 0) {
      top_scorer <- names(which.max(unlist(team_goals)))
      top_scorer_goals <- team_goals[[top_scorer]]
    } else {
      top_scorer <- "N/A"; top_scorer_goals <- 0
    }
    
    match_totals <- sapply(all_knockout, function(m) if (!is.null(m$team1_goals)) m$team1_goals + m$team2_goals else 0)
    highest_idx <- which.max(match_totals)
    highest_match <- all_knockout[[highest_idx]]
    
    blowouts <- sapply(all_knockout, function(m) if (!is.null(m$team1_goals)) abs(m$team1_goals - m$team2_goals) else 0)
    blowout_idx <- which.max(blowouts)
    blowout_match <- all_knockout[[blowout_idx]]
    
    upsets <- sapply(all_knockout, function(m) {
      if (!is.null(m$team1_elo) && !is.null(m$team2_elo)) {
        if ((m$winner == m$team1 && m$team1_elo < m$team2_elo) ||
            (m$winner == m$team2 && m$team2_elo < m$team1_elo)) {
          abs(m$team1_elo - m$team2_elo)
        } else 0
      } else 0
    })
    upset_idx <- which.max(upsets)
    upset_match <- all_knockout[[upset_idx]]
    
    div(
      h3("Tournament Statistics", style = "text-align: center; margin-bottom: 30px;"),
      fluidRow(
        column(3, div(class = "stat-card",
                      div(class = "stat-title", "Top Scoring Team"),
                      div(class = "stat-value", HTML(paste0(get_flag(top_scorer), top_scorer))),
                      p(paste(top_scorer_goals, "goals in the tournament"))
        )),
        column(3, div(class = "stat-card",
                      div(class = "stat-title", "Highest Scoring Match"),
                      div(class = "stat-value", match_totals[highest_idx], "goals"),
                      p(HTML(paste0(get_flag(highest_match$team1), highest_match$team1, " ", 
                                    highest_match$score, " ", get_flag(highest_match$team2), highest_match$team2)))
        )),
        column(3, div(class = "stat-card",
                      div(class = "stat-title", "Biggest Blowout"),
                      div(class = "stat-value", blowouts[blowout_idx], "goal margin"),
                      p(HTML(paste0(get_flag(blowout_match$team1), blowout_match$team1, " ", 
                                    blowout_match$score, " ", get_flag(blowout_match$team2), blowout_match$team2)))
        )),
        column(3, div(class = "stat-card",
                      div(class = "stat-title", "Biggest Upset"),
                      div(class = "stat-value", round(upsets[upset_idx]), "ELO diff"),
                      p(HTML(paste0(get_flag(upset_match$winner), upset_match$winner, " beat ",
                                    get_flag(setdiff(c(upset_match$team1, upset_match$team2), upset_match$winner)),
                                    setdiff(c(upset_match$team1, upset_match$team2), upset_match$winner))))
        ))
      )
    )
  })
  
  observeEvent(input$team_click, {
    team <- input$team_click
    journey <- sim_data$team_journeys[[team]]
    req(journey)
    
    group_matches_ui <- lapply(journey$group_matches, function(m) {
      opponent <- if (m$team1 == team) m$team2 else m$team1
      score_display <- if (m$team1 == team) {
        sprintf("%s %d-%d %s", team, m$result$team1_goals, m$result$team2_goals, opponent)
      } else {
        sprintf("%s %d-%d %s", opponent, m$result$team1_goals, m$result$team2_goals, team)
      }
      result_color <- if (m$result$winner == team) "green" else if (m$result$winner == "Draw") "orange" else "red"
      p(span(style = paste0("color:", result_color, "; font-weight:bold;"), score_display))
    })
    
    knockout_ui <- if (length(journey$knockout) > 0) {
      lapply(names(journey$knockout), function(round) {
        m <- journey$knockout[[round]]
        round_names <- c(r32 = "Round of 32", r16 = "Round of 16", qf = "Quarter-Finals", 
                         sf = "Semi-Finals", third = "Third Place", final = "Final")
        div(
          h5(round_names[[round]]),
          p(HTML(paste0(get_flag(m$team1), m$team1, " ", m$score, " ", get_flag(m$team2), m$team2))),
          p(style = "color: #666;", paste("Winner:", m$winner, "-", m$method))
        )
      })
    } else {
      p("Did not qualify for knockout stage")
    }
    
    showModal(modalDialog(
      title = HTML(paste0(get_flag(team), " ", team, " - Tournament Journey")),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l",
      div(
        h4(paste("Group", journey$group, "- Finished", 
                 if (journey$group_position == 1) "1st" else if (journey$group_position == 2) "2nd"
                 else if (journey$group_position == 3) "3rd" else "4th")),
        h5("Group Stage Matches:"),
        group_matches_ui,
        hr(),
        h4("Knockout Stage:"),
        knockout_ui,
        hr(),
        h3(journey$final_result, 
           style = if (journey$final_result == "Champion") "color: gold; text-align: center;" 
           else "text-align: center;")
      )
    ))
  })
  
  observeEvent(input$match_click_id, {
    match_id <- input$match_click_id
    req(match_id)
    
    match <- NULL
    for (round_name in names(sim_data$knockout_results)) {
      round_data <- sim_data$knockout_results[[round_name]]
      if (is.list(round_data) && !is.null(round_data$team1)) {
        if (round_name == match_id) match <- round_data
      } else if (is.list(round_data)) {
        if (match_id %in% names(round_data)) match <- round_data[[match_id]]
      }
    }
    
    req(match)
    
    format_goals <- function(minutes) {
      if (length(minutes) == 0) return("None")
      paste0(minutes, "'", collapse = ", ")
    }
    
    showModal(modalDialog(
      title = HTML(paste0(get_flag(match$team1), " ", match$team1, " vs ", 
                          get_flag(match$team2), " ", match$team2)),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "m",
      div(
        h3(sprintf("%s %s %s", match$team1, match$score, match$team2),
           style = "text-align: center; margin: 20px 0;"),
        hr(),
        div(style = "padding: 15px;",
            h5(HTML(paste0(get_flag(match$team1), " ", match$team1, " Goals:"))),
            p(format_goals(match$goal_minutes_team1), style = "margin-left: 20px; font-size: 16px;"),
            br(),
            h5(HTML(paste0(get_flag(match$team2), " ", match$team2, " Goals:"))),
            p(format_goals(match$goal_minutes_team2), style = "margin-left: 20px; font-size: 16px;")
        ),
        hr(),
        div(style = "text-align: center;",
            h4(style = "color: #4CAF50;", HTML(paste0("Winner: ", get_flag(match$winner), match$winner))),
            p(style = "color: #666; font-size: 14px;", paste("Result decided in:", match$method))
        )
      )
    ))
  })
  
  create_match_card <- function(match, id) {
    winner_class1 <- if (match$winner == match$team1) "winner-highlight" else ""
    winner_class2 <- if (match$winner == match$team2) "winner-highlight" else ""
    
    div(class = "match-card",
        onclick = sprintf("Shiny.setInputValue('match_click_id', '%s', {priority: 'event'})", id),
        div(HTML(paste0(get_flag(match$team1), '<span class="', winner_class1, '">', match$team1, '</span>')),
            span(style = "float: right;", match$score)),
        div(HTML(paste0(get_flag(match$team2), '<span class="', winner_class2, '">', match$team2, '</span>'))),
        tags$small(style = "color: #888;", paste("Winner:", match$winner, "-", match$method))
    )
  }
  
  output$r32_display <- renderUI({
    req(sim_data$knockout_results)
    ko <- sim_data$knockout_results$r32
    fluidRow(
      column(3, mapply(create_match_card, ko[1:4], names(ko[1:4]), SIMPLIFY = FALSE)),
      column(3, mapply(create_match_card, ko[5:8], names(ko[5:8]), SIMPLIFY = FALSE)),
      column(3, mapply(create_match_card, ko[9:12], names(ko[9:12]), SIMPLIFY = FALSE)),
      column(3, mapply(create_match_card, ko[13:16], names(ko[13:16]), SIMPLIFY = FALSE))
    )
  })
  
  output$r16_display <- renderUI({
    req(sim_data$knockout_results)
    ko <- sim_data$knockout_results$r16
    fluidRow(
      column(3, mapply(create_match_card, ko[1:2], names(ko[1:2]), SIMPLIFY = FALSE)),
      column(3, mapply(create_match_card, ko[3:4], names(ko[3:4]), SIMPLIFY = FALSE)),
      column(3, mapply(create_match_card, ko[5:6], names(ko[5:6]), SIMPLIFY = FALSE)),
      column(3, mapply(create_match_card, ko[7:8], names(ko[7:8]), SIMPLIFY = FALSE))
    )
  })
  
  output$qf_display <- renderUI({
    req(sim_data$knockout_results)
    ko <- sim_data$knockout_results$qf
    fluidRow(
      column(3, create_match_card(ko$m97, "m97")),
      column(3, create_match_card(ko$m98, "m98")),
      column(3, create_match_card(ko$m99, "m99")),
      column(3, create_match_card(ko$m100, "m100"))
    )
  })
  
  output$sf_display <- renderUI({
    req(sim_data$knockout_results)
    ko <- sim_data$knockout_results$sf
    fluidRow(
      column(6, create_match_card(ko$m101, "m101")),
      column(6, create_match_card(ko$m102, "m102"))
    )
  })
  
  output$final_display <- renderUI({
    req(sim_data$knockout_results)
    ko <- sim_data$knockout_results
    div(
      h5("Third Place Match"),
      create_match_card(ko$third, "third"),
      br(),
      h4("🏆 FINAL"),
      create_match_card(ko$final, "final")
    )
  })
  
  output$final_standings <- renderUI({
    req(sim_data$champion)
    div(style = "text-align: center; padding: 50px;",
        h2("Final Standings", style = "margin-bottom: 40px;"),
        div(style = "font-size: 24px; margin: 20px;",
            div(style = "padding: 20px; background: gold; border-radius: 10px; margin: 10px;
                     box-shadow: 0 4px 8px rgba(0,0,0,0.2);",
                "🥇 ", strong(HTML(paste0(get_flag(sim_data$champion), sim_data$champion)))
            ),
            div(style = "padding: 20px; background: silver; border-radius: 10px; margin: 10px;
                     box-shadow: 0 4px 8px rgba(0,0,0,0.2);",
                "🥈 ", strong(HTML(paste0(get_flag(sim_data$runner_up), sim_data$runner_up)))
            ),
            div(style = "padding: 20px; background: #CD7F32; border-radius: 10px; margin: 10px;
                     box-shadow: 0 4px 8px rgba(0,0,0,0.2);",
                "🥉 ", strong(HTML(paste0(get_flag(sim_data$third), sim_data$third)))
            ),
            div(style = "padding: 20px; background: #e0e0e0; border-radius: 10px; margin: 10px;",
                "4th: ", HTML(paste0(get_flag(sim_data$fourth), sim_data$fourth))
            )
        ),
        div(style = "font-size: 100px; margin-top: 30px; animation: spinBounce 2s ease-in-out infinite;", "🏆"),
        div(style = "font-size: 60px; margin-top: 20px;", "✨ 🎊 ⚽ 🎊 ✨")
    )
  })
  
  output$share_button_ui <- renderUI({
    req(sim_data$champion)
    div(style = "text-align: center; margin: 20px;",
        actionButton("share_results", "📤 Share Results", 
                     class = "btn-success btn-lg",
                     style = "font-size: 16px; padding: 12px 30px;")
    )
  })
  
  observeEvent(input$share_results, {
    final_match <- sim_data$knockout_results$final
    
    all_knockout <- list()
    for (round_name in names(sim_data$knockout_results)) {
      round_data <- sim_data$knockout_results[[round_name]]
      if (is.list(round_data) && !is.null(round_data$team1)) {
        all_knockout <- c(all_knockout, list(round_data))
      } else if (is.list(round_data)) {
        all_knockout <- c(all_knockout, round_data)
      }
    }
    
    upsets <- sapply(all_knockout, function(m) {
      if (!is.null(m$team1_elo) && !is.null(m$team2_elo)) {
        if ((m$winner == m$team1 && m$team1_elo < m$team2_elo) ||
            (m$winner == m$team2 && m$team2_elo < m$team1_elo)) {
          abs(m$team1_elo - m$team2_elo)
        } else 0
      } else 0
    })
    biggest_upset_idx <- which.max(upsets)
    biggest_upset <- all_knockout[[biggest_upset_idx]]
    
    share_text <- sprintf(
      "%s just won the 2026 FIFA World Cup in my simulation! They beat %s %s in the final. %sSimulator made by @NoahBairCuse! Check it out at https://noahbair.shinyapps.io/worldcupsimulator/",
      sim_data$champion,
      sim_data$runner_up,
      final_match$score,
      if (upsets[biggest_upset_idx] > 50) {
        sprintf("Biggest upset: %s beat %s! 🤯 ", biggest_upset$winner,
                setdiff(c(biggest_upset$team1, biggest_upset$team2), biggest_upset$winner))
      } else ""
    )
    
    showModal(modalDialog(
      title = "📤 Share Your Tournament Results",
      easyClose = TRUE,
      size = "l",
      div(
        h4("Copy this text to share on social media:"),
        br(),
        div(style = "background: #f8f9fa; padding: 20px; border-radius: 8px; 
             border: 1px solid #ddd; font-family: Arial; line-height: 1.6;",
            share_text
        ),
        br(),
        actionButton("copy_share", "📋 Copy to Clipboard", 
                     class = "btn-primary btn-lg",
                     style = "width: 100%; font-size: 16px;",
                     onclick = sprintf(
                       "navigator.clipboard.writeText(`%s`).then(() => { 
                         alert('✅ Copied to clipboard! Ready to share!'); 
                       });",
                       share_text
                     ))
      ),
      footer = modalButton("Close")
    ))
  })
  
  output$bracket_tree <- renderUI({
    req(sim_data$knockout_results)
    ko <- sim_data$knockout_results
    
    create_bracket_match <- function(match) {
      if (is.null(match)) return(div())
      team1_class <- if (match$winner == match$team1) "bracket-team bracket-winner" else "bracket-team"
      team2_class <- if (match$winner == match$team2) "bracket-team bracket-winner" else "bracket-team"
      
      div(class = "bracket-match-box",
          div(class = team1_class,
              HTML(paste0(get_flag(match$team1), match$team1)),
              span(style = "float: right;", match$team1_goals)
          ),
          div(class = team2_class,
              HTML(paste0(get_flag(match$team2), match$team2)),
              span(style = "float: right;", match$team2_goals)
          ),
          if (match$method != "In Normal Time") {
            tags$small(style = "color: #888; text-align: center; display: block; margin-top: 5px;",
                       match$method)
          } else NULL
      )
    }
    
    div(class = "bracket-tree",
        div(class = "bracket-column",
            h4("Round of 32", style = "text-align: center; color: #333; margin-bottom: 20px;"),
            lapply(ko$r32, create_bracket_match)
        ),
        div(class = "bracket-column",
            h4("Round of 16", style = "text-align: center; color: #333; margin-bottom: 20px;"),
            create_bracket_match(ko$r16$m89), create_bracket_match(ko$r16$m90),
            create_bracket_match(ko$r16$m91), create_bracket_match(ko$r16$m92),
            create_bracket_match(ko$r16$m93), create_bracket_match(ko$r16$m94),
            create_bracket_match(ko$r16$m95), create_bracket_match(ko$r16$m96)
        ),
        div(class = "bracket-column",
            h4("Quarter-Finals", style = "text-align: center; color: #333; margin-bottom: 20px;"),
            div(style = "margin-top: 50px;", create_bracket_match(ko$qf$m97)),
            div(style = "margin-top: 50px;", create_bracket_match(ko$qf$m98)),
            div(style = "margin-top: 50px;", create_bracket_match(ko$qf$m99)),
            div(style = "margin-top: 50px;", create_bracket_match(ko$qf$m100))
        ),
        div(class = "bracket-column",
            h4("Semi-Finals", style = "text-align: center; color: #333; margin-bottom: 20px;"),
            div(style = "margin-top: 150px;", create_bracket_match(ko$sf$m101)),
            div(style = "margin-top: 150px;", create_bracket_match(ko$sf$m102))
        ),
        div(class = "bracket-column",
            h4("Final", style = "text-align: center; color: #333; margin-bottom: 20px;"),
            div(style = "margin-top: 300px;",
                create_bracket_match(ko$final),
                br(), br(),
                div(style = "text-align: center; padding: 25px; background: linear-gradient(135deg, #FFD700, #FFA500); 
                     border-radius: 15px; box-shadow: 0 6px 12px rgba(0,0,0,0.3);",
                    div(class = "trophy-animation", "🏆"),
                    h3(style = "margin: 10px 0; color: white; text-shadow: 2px 2px 4px rgba(0,0,0,0.3);", 
                       HTML(paste0(get_flag(sim_data$champion), sim_data$champion))),
                    p(style = "margin: 0; color: white; font-weight: bold;", "WORLD CHAMPIONS")
                )
            )
        )
    )
  })
    # ==================== WIN PROBABILITY TABLE ====================
    output$prob_table_display <- renderUI({
      req(sim_probs)
      
      prob_display <- sim_probs %>%
        mutate(
          Flag = sapply(Team, function(t) get_flag(t)),
          TeamHTML = paste0(Flag, Team)
        ) %>%
        select(TeamHTML, Group, ELO, GroupWin, Qualify, R16, QF, SF, Final, Champion) %>%
        rename(
          Team = TeamHTML,
          `Group Win%` = GroupWin,
          `Qualify%` = Qualify,
          `R16%` = R16,
          `QF%` = QF,
          `SF%` = SF,
          `Final%` = Final,
          `Champion%` = Champion
        )
      
      div(
        h3("10,000-Simulation Win Probabilities", style = "text-align: center; margin-bottom: 20px;"),
        p(style = "text-align: center; color: #666;", 
          "Based on 10,000 Monte Carlo simulations using World Football ELO ratings"),
        renderDT({
          datatable(
            prob_display,
            options = list(
              dom = 'ftp',
              pageLength = 48,
              ordering = TRUE,
              order = list(list(9, 'desc')),
              columnDefs = list(
                list(className = 'dt-center', targets = 1:9)
              )
            ),
            rownames = FALSE,
            escape = FALSE,
            class = 'cell-border stripe'
          ) %>%
            formatStyle("Champion%",
                        background = styleColorBar(c(0, max(sim_probs$Champion)), "#667eea"),
                        backgroundSize = '98% 88%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center'
            ) %>%
            formatStyle("Champion%",
                        color = styleInterval(c(5, 15, 25), c('#333', '#1a6b2e', '#0d4a1f', '#ffffff')),
                        fontWeight = styleInterval(5, c('normal', 'bold'))
            )
        })
      )
    })
    
    # ==================== EV CALCULATOR ====================
    output$ev_display <- renderUI({
      req(ev_results)
      
      pos_ev <- ev_results %>%
        filter(ev_pct > 0) %>%
        arrange(desc(edge)) %>%
        mutate(
          Flag = sapply(team, function(t) get_flag(t)),
          TeamHTML = paste0(Flag, team),
          ev_color = ifelse(ev_pct > 50, "#1a6b2e", ifelse(ev_pct > 20, "#2d8a4e", "#4CAF50"))
        )
      
      market_choices <- c("All Markets", unique(ev_results$market))
      
      div(
        h3("EV Calculator vs DraftKings", style = "text-align: center; margin-bottom: 5px;"),
        p(style = "text-align: center; color: #666; margin-bottom: 20px;",
          "Sim probability vs DraftKings implied probability (vig removed). Positive edge = undervalued by DK."),
        fluidRow(
          column(4,
                 selectInput("ev_market_filter", "Filter by Market:",
                             choices = market_choices, selected = "All Markets")
          ),
          column(4,
                 selectInput("ev_sort", "Sort by:",
                             choices = c("Edge" = "edge", "EV%" = "ev_pct", "Sim Prob%" = "sim_prob"),
                             selected = "edge")
          ),
          column(4,
                 checkboxInput("ev_pos_only", "Positive EV only", value = TRUE)
          )
        ),
        uiOutput("ev_table_output")
      )
    })
    
    output$ev_table_output <- renderUI({
      req(ev_results)
      
      filtered <- ev_results
      
      if (input$ev_market_filter != "All Markets") {
        filtered <- filtered %>% filter(market == input$ev_market_filter)
      }
      if (input$ev_pos_only) {
        filtered <- filtered %>% filter(ev_pct > 0)
      }
      filtered <- filtered %>% arrange(desc(.data[[input$ev_sort]]))
      
      display <- filtered %>%
        mutate(
          Flag = sapply(team, function(t) get_flag(t)),
          TeamHTML = paste0(Flag, team)
        ) %>%
        select(Market = market, Team = TeamHTML, `DK Odds` = dk_odds,
               `DK Prob%` = dk_prob, `Sim Prob%` = sim_prob,
               `Edge%` = edge, `EV%` = ev_pct)
      
      renderDT({
        datatable(
          display,
          options = list(
            dom = 'ftp',
            pageLength = 25,
            ordering = TRUE,
            columnDefs = list(
              list(className = 'dt-center', targets = 2:6)
            )
          ),
          rownames = FALSE,
          escape = FALSE,
          class = 'cell-border stripe'
        ) %>%
          formatStyle("EV%",
                      color = styleInterval(c(0, 25, 75), c('#c0392b', '#333', '#1a6b2e', '#0d4a1f')),
                      fontWeight = styleInterval(0, c('normal', 'bold'))
          ) %>%
          formatStyle("Edge%",
                      color = styleInterval(0, c('#c0392b', '#1a6b2e')),
                      fontWeight = 'bold'
          )
      })
    })
    
    
    # ==================== TEAM FUNNEL ====================
    output$funnel_display <- renderUI({
      req(input$funnel_team, sim_probs)
      
      team <- input$funnel_team
      row <- sim_probs %>% filter(Team == team)
      if (nrow(row) == 0) return(p("Team not found."))
      
      stages <- c("Qualify", "R16", "QF", "SF", "Final", "Champion")
      stage_labels <- c("Qualify", "Round of 16", "Quarter-Final", "Semi-Final", "Final", "Champion")
      probs <- as.numeric(row[1, stages])
      
      funnel_data <- data.frame(
        Stage = factor(stage_labels, levels = stage_labels),
        Prob  = probs
      )
      
      div(
        h3(HTML(paste0(get_flag(team), " ", team, " — Tournament Probability Funnel")),
           style = "text-align: center; margin-bottom: 5px;"),
        p(style = "text-align: center; color: #666;",
          paste0("ELO: ", row$ELO, " | Group ", row$Group)),
        renderPlot({
          ggplot(funnel_data, aes(x = Stage, y = Prob, fill = Prob)) +
            geom_col(width = 0.6, show.legend = FALSE) +
            geom_text(aes(label = paste0(Prob, "%")), 
                      vjust = -0.5, fontface = "bold", size = 5) +
            scale_fill_gradient(low = "#a8d5b5", high = "#1a6b2e") +
            scale_y_continuous(limits = c(0, 110), labels = function(x) paste0(x, "%")) +
            labs(x = NULL, y = "Probability") +
            theme_minimal(base_size = 14) +
            theme(
              plot.background  = element_rect(fill = "white", color = NA),
              panel.grid.major.x = element_blank(),
              panel.grid.minor  = element_blank(),
              axis.text.x = element_text(face = "bold", size = 12),
              plot.margin = margin(20, 20, 20, 20)
            )
        }, height = 400)
      )
    })
    output$funnel_controls <- renderUI({
      selectInput("funnel_team", "Select a team:", 
                  choices = sort(sim_probs$Team), selected = "Spain")
    })
    
    # ==================== YOUR BRACKET ====================
    
    user_bracket <- reactiveValues(
      group_scores = list(),
      group_standings = list(),
      advancing_third = NULL,
      r32 = list(), r16 = list(), qf = list(), sf = list(),
      third_match = NULL, final_match = NULL,
      stage = "groups"  # groups, r32, r16, qf, sf, final
    )
    
    generate_scoreline <- function(team1, team2, result) {
      e1 <- df$Rating[df$Team == team1]
      e2 <- df$Rating[df$Team == team2]
      base_goals <- 1.3
      elo_diff <- e1 - e2
      lambda1 <- max(0.1, base_goals * 1.25^(elo_diff / 100))
      lambda2 <- max(0.1, base_goals * 1.25^(-elo_diff / 100))
      
      for (attempt in 1:50) {
        g1 <- rpois(1, lambda1)
        g2 <- rpois(1, lambda2)
        valid <- (result == "team1" && g1 > g2) ||
          (result == "team2" && g2 > g1) ||
          (result == "draw"  && g1 == g2)
        if (valid) return(c(g1, g2))
      }
      # fallback
      if (result == "team1") return(c(1, 0))
      if (result == "team2") return(c(0, 1))
      return(c(1, 1))
    }
    
    calc_group_standings <- function(group_name) {
      teams <- switch(group_name,
                      A = c("Mexico", "South Korea", "South Africa", "Czechia"),
                      B = c("Canada", "Switzerland", "Qatar", "Bosnia and Herzegovina"),
                      C = c("Brazil", "Morocco", "Scotland", "Haiti"),
                      D = c("United States", "Paraguay", "Australia", "Turkey"),
                      E = c("Germany", "Ecuador", "Ivory Coast", "Curaçao"),
                      F = c("Netherlands", "Japan", "Tunisia", "Sweden"),
                      G = c("Belgium", "Iran", "Egypt", "New Zealand"),
                      H = c("Spain", "Uruguay", "Saudi Arabia", "Cape Verde"),
                      I = c("France", "Senegal", "Norway", "Iraq"),
                      J = c("Argentina", "Austria", "Algeria", "Jordan"),
                      K = c("Portugal", "Colombia", "Uzbekistan", "DR Congo"),
                      L = c("England", "Croatia", "Panama", "Ghana")
      )
      
      standings <- data.frame(
        Team = teams, Pts = 0, W = 0, D = 0, L = 0,
        GF = 0, GA = 0, GD = 0, stringsAsFactors = FALSE
      )
      
      scores <- user_bracket$group_scores[[group_name]]
      if (is.null(scores)) return(standings)
      
      pairs <- combn(teams, 2, simplify = FALSE)
      for (pair in pairs) {
        key <- paste(pair[1], pair[2], sep = "__")
        s <- scores[[key]]
        if (is.null(s)) next
        g1 <- s[1]; g2 <- s[2]
        t1 <- pair[1]; t2 <- pair[2]
        standings$GF[standings$Team == t1] <- standings$GF[standings$Team == t1] + g1
        standings$GA[standings$Team == t1] <- standings$GA[standings$Team == t1] + g2
        standings$GF[standings$Team == t2] <- standings$GF[standings$Team == t2] + g2
        standings$GA[standings$Team == t2] <- standings$GA[standings$Team == t2] + g1
        if (g1 > g2) {
          standings$Pts[standings$Team == t1] <- standings$Pts[standings$Team == t1] + 3
          standings$W[standings$Team == t1] <- standings$W[standings$Team == t1] + 1
          standings$L[standings$Team == t2] <- standings$L[standings$Team == t2] + 1
        } else if (g2 > g1) {
          standings$Pts[standings$Team == t2] <- standings$Pts[standings$Team == t2] + 3
          standings$W[standings$Team == t2] <- standings$W[standings$Team == t2] + 1
          standings$L[standings$Team == t1] <- standings$L[standings$Team == t1] + 1
        } else {
          standings$Pts[standings$Team == t1] <- standings$Pts[standings$Team == t1] + 1
          standings$Pts[standings$Team == t2] <- standings$Pts[standings$Team == t2] + 1
          standings$D[standings$Team == t1] <- standings$D[standings$Team == t1] + 1
          standings$D[standings$Team == t2] <- standings$D[standings$Team == t2] + 1
        }
      }
      standings$GD <- standings$GF - standings$GA
      standings %>% arrange(desc(Pts), desc(GD), desc(GF))
    }
    
    get_group_teams_bracket <- function(g) {
      switch(g,
             A = c("Mexico", "South Korea", "South Africa", "Czechia"),
             B = c("Canada", "Switzerland", "Qatar", "Bosnia and Herzegovina"),
             C = c("Brazil", "Morocco", "Scotland", "Haiti"),
             D = c("United States", "Paraguay", "Australia", "Turkey"),
             E = c("Germany", "Ecuador", "Ivory Coast", "Curaçao"),
             F = c("Netherlands", "Japan", "Tunisia", "Sweden"),
             G = c("Belgium", "Iran", "Egypt", "New Zealand"),
             H = c("Spain", "Uruguay", "Saudi Arabia", "Cape Verde"),
             I = c("France", "Senegal", "Norway", "Iraq"),
             J = c("Argentina", "Austria", "Algeria", "Jordan"),
             K = c("Portugal", "Colombia", "Uzbekistan", "DR Congo"),
             L = c("England", "Croatia", "Panama", "Ghana")
      )
    }
    
    output$user_bracket_ui <- renderUI({
      stage <- user_bracket$stage
      
      if (stage == "groups") {
        groups_list_b <- c("A","B","C","D","E","F","G","H","I","J","K","L")
        
        all_scores <- reactiveValuesToList(user_bracket)$group_scores
        
        group_cards <- lapply(groups_list_b, function(g) {
          teams <- get_group_teams_bracket(g)
          pairs <- combn(teams, 2, simplify = FALSE)
          scores <- all_scores[[g]]
          
          match_inputs <- lapply(pairs, function(pair) {
            key <- paste(pair[1], pair[2], sep = "__")
            existing <- scores[[key]]
            p1_safe <- gsub(" ", "_", pair[1])
            p2_safe <- gsub(" ", "_", pair[2])
            
            fluidRow(style = "margin-bottom: 8px; align-items: center;",
                     column(3, style = "text-align: right; padding-right: 5px;",
                            tags$img(
                              src = sprintf("https://flagcdn.com/24x18/%s.png", country_codes[[pair[1]]]),
                              style = "cursor: pointer; margin-right: 4px; vertical-align: middle;",
                              onclick = sprintf("Shiny.setInputValue('bracket_pick', {g: '%s', t1: '%s', t2: '%s', winner: '1', nonce: Math.random()}, {priority: 'event'})", g, pair[1], pair[2])
                            ),
                            span(style = "font-weight: bold;", pair[1])
                     ),
                     column(1, style = "padding: 2px;",
                            numericInput(paste0("score_", g, "_", p1_safe, "_", p2_safe, "_g1"),
                                         NULL, value = if (!is.null(existing)) existing[1] else NA,
                                         min = 0, max = 20, step = 1, width = "55px")
                     ),
                     column(2, style = "text-align: center; padding-top: 6px;",
                            actionButton(paste0("draw_", g, "_", p1_safe, "_", p2_safe),
                                         "Draw", class = "btn-xs btn-default",
                                         style = "font-size: 10px; padding: 2px 5px;")
                     ),
                     column(1, style = "padding: 2px;",
                            numericInput(paste0("score_", g, "_", p1_safe, "_", p2_safe, "_g2"),
                                         NULL, value = if (!is.null(existing)) existing[2] else NA,
                                         min = 0, max = 20, step = 1, width = "55px")
                     ),
                     column(3, style = "font-weight: bold; padding-left: 5px;",
                            span(pair[2]),
                            tags$img(
                              src = sprintf("https://flagcdn.com/24x18/%s.png", country_codes[[pair[2]]]),
                              style = "cursor: pointer; margin-left: 4px; vertical-align: middle;",
                              onclick = sprintf("Shiny.setInputValue('bracket_pick', {g: '%s', t1: '%s', t2: '%s', winner: '2', nonce: Math.random()}, {priority: 'event'})", g, pair[1], pair[2])
                            )
                     ),
                     column(2, style = "text-align: center; padding-top: 4px;",
                            actionButton(paste0("confirm_", g, "_", p1_safe, "_", p2_safe),
                                         if (!is.null(existing)) "✓" else "Confirm",
                                         class = if (!is.null(existing)) "btn-xs btn-success" else "btn-xs btn-outline-secondary",
                                         style = "font-size: 10px; padding: 2px 6px;")
                     )
            )
          })
          
          # standings preview
          standings <- calc_group_standings(g)
          n_entered <- sum(sapply(pairs, function(p) {
            key <- paste(p[1], p[2], sep = "__")
            !is.null(scores[[key]])
          }))
          
          div(class = "stat-card", style = "margin-bottom: 15px;",
              h4(paste("Group", g), style = "color: #333; margin-bottom: 10px;"),
              match_inputs,
              if (n_entered > 0) {
                div(style = "margin-top: 10px; border-top: 1px solid #eee; padding-top: 10px;",
                    tags$small(style = "color: #666;", "Current standings:"),
                    tags$table(style = "width: 100%; font-size: 12px; margin-top: 5px;",
                               tags$tr(tags$th("Team"), tags$th("Pts"), tags$th("GD")),
                               lapply(1:nrow(standings), function(i) {
                                 row <- standings[i,]
                                 bg <- if (i <= 2) "#d4edda" else if (i == 3) "#fff3cd" else "#f8d7da"
                                 tags$tr(style = paste0("background:", bg,";"),
                                         tags$td(HTML(paste0(get_flag(row$Team), row$Team))),
                                         tags$td(row$Pts),
                                         tags$td(row$GD)
                                 )
                               })
                    )
                )
              }
          )
        })
        # Check all matches entered
        all_entered <- all(sapply(c("A","B","C","D","E","F","G","H","I","J","K","L"), function(g) {
          teams <- get_group_teams_bracket(g)
          pairs <- combn(teams, 2, simplify = FALSE)
          scores <- all_scores[[g]]
          all(sapply(pairs, function(p) {
            key <- paste(p[1], p[2], sep = "__")
            !is.null(scores[[key]])
          }))
        }))
        
        div(
          h3("🔮 Your Bracket — Group Stage", style = "text-align: center; margin-bottom: 20px;"),
          p(style = "text-align: center; color: #666; margin-bottom: 20px;",
            "Click a flag to pick that team as winner, click Draw for a draw. Green = qualified, yellow = potential best 3rd, red = eliminated."),
          fluidRow(
            column(6, group_cards[1:6]),
            column(6, group_cards[7:12])
          ),
          div(style = "text-align: center; margin: 15px 0;",
              actionButton("sim_remaining_groups", "🎲 Sim Remaining Games in This Round",
                           class = "btn-warning btn-md",
                           style = "font-size: 14px; padding: 8px 20px;")
          ),
          if (all_entered) {
            div(style = "text-align: center; margin-top: 10px;",
                actionButton("advance_to_knockouts", "⚽ Advance to Knockouts →",
                             class = "btn-success btn-lg",
                             style = "font-size: 18px; padding: 15px 30px;")
            )
          }
        )
        
      } else if (stage %in% c("r32", "r16", "qf", "sf", "final")) {
        
        stage_labels <- c(r32 = "Round of 32", r16 = "Round of 16", 
                          qf = "Quarter-Finals", sf = "Semi-Finals", final = "Final")
        stage_order <- c("r32", "r16", "qf", "sf", "final")
        current_idx <- which(stage_order == stage)
        
        matches <- user_bracket[[stage]]
        if (is.null(matches) || length(matches) == 0) {
          return(div(p("Loading knockout stage...")))
        }
        
        match_cards <- lapply(names(matches), function(mid) {
          m <- matches[[mid]]
          if (is.null(m)) return(NULL)
          
          team1 <- m$team1; team2 <- m$team2
          winner <- m$winner
          
          if (!is.null(winner)) {
            # Already picked - show result cleanly
            div(class = "match-card", style = "border-color: #4CAF50;",
                fluidRow(
                  column(5, HTML(paste0(
                    '<span style="', if(winner == team1) "font-weight:bold; color:#4CAF50;" else "color:#999;", '">',
                    get_flag(team1), team1, '</span>'
                  ))),
                  column(2, style = "text-align: center; font-weight: bold;", m$score),
                  column(5, HTML(paste0(
                    '<span style="', if(winner == team2) "font-weight:bold; color:#4CAF50;" else "color:#999;", '">',
                    get_flag(team2), team2, '</span>'
                  )))
                ),
                tags$small(style = "color: #4CAF50;", paste("✓ Winner:", winner))
            )
          } else {
            # Need to pick - flag click UI
            div(class = "match-card", style = "border-color: #667eea;",
                fluidRow(
                  column(4, style = "text-align: center; padding-top: 5px;",
                         tags$img(
                           src = sprintf("https://flagcdn.com/24x18/%s.png", country_codes[[team1]]),
                           style = "cursor: pointer; width: 36px; height: 27px;",
                           onclick = sprintf("Shiny.setInputValue('ko_bracket_pick', {stage: '%s', mid: '%s', side: '1', nonce: Math.random()}, {priority: 'event'})", stage, mid)
                         ),
                         br(),
                         tags$small(team1, style = "font-weight: bold;")
                  ),
                  column(4, style = "text-align: center; padding-top: 8px;",
                         tags$small("click flag to pick winner", style = "color: #999; font-size: 10px;")
                  ),
                  column(4, style = "text-align: center; padding-top: 5px;",
                         tags$img(
                           src = sprintf("https://flagcdn.com/24x18/%s.png", country_codes[[team2]]),
                           style = "cursor: pointer; width: 36px; height: 27px;",
                           onclick = sprintf("Shiny.setInputValue('ko_bracket_pick', {stage: '%s', mid: '%s', side: '2', nonce: Math.random()}, {priority: 'event'})", stage, mid)
                         ),
                         br(),
                         tags$small(team2, style = "font-weight: bold;")
                  )
                )
            )
          }
        })
        
        all_picked <- all(sapply(matches, function(m) !is.null(m$winner)))
        
        next_label <- if (stage == "sf") "🏆 Go to Final" else if (stage == "final") NULL else paste("→", stage_labels[stage_order[current_idx + 1]])
        
        div(
          h3(paste("🔮 Your Bracket —", stage_labels[stage]), 
             style = "text-align: center; margin-bottom: 20px;"),
          if (stage == "final" && all_picked) {
            winner_team <- matches[[1]]$winner
            div(class = "champion-box",
                div(class = "trophy-animation", "🏆"),
                h2(HTML(paste0(get_flag(winner_team), " ", winner_team))),
                h4("Your World Cup Champion!"),
                br(),
                # Show sim probability
                if (!is.null(sim_probs)) {
                  prob <- sim_probs$Champion[sim_probs$Team == winner_team]
                  if (length(prob) > 0) {
                    p(style = "font-size: 18px;", paste0("The model gives ", winner_team, " a ", prob, "% chance of winning."))
                  }
                }
            )
          },
          fluidRow(
            lapply(seq_along(match_cards), function(i) {
              column(if (length(match_cards) <= 2) 6 else if (length(match_cards) <= 4) 3 else 3,
                     match_cards[[i]])
            })
          ),
          if (!all_picked) {
            div(style = "text-align: center; margin: 15px 0;",
                actionButton(paste0("sim_remaining_", stage), "🎲 Sim Remaining Games in This Round",
                             class = "btn-warning btn-md",
                             style = "font-size: 14px; padding: 8px 20px;")
            )
          },
          if (all_picked && !is.null(next_label)) {
            div(style = "text-align: center; margin-top: 20px;",
                actionButton(paste0("advance_", stage), next_label,
                             class = "btn-success btn-lg",
                             style = "font-size: 16px; padding: 12px 25px;")
            )
          },
          br(),
          actionButton("reset_bracket", "🔄 Reset Bracket", 
                       class = "btn-danger btn-sm",
                       style = "margin-top: 10px;")
        )
      }
    })
    
    # Handle score pick buttons for group stage
    observe({
      groups_list_b <- c("A","B","C","D","E","F","G","H","I","J","K","L")
      for (g in groups_list_b) {
        teams <- get_group_teams_bracket(g)
        pairs <- combn(teams, 2, simplify = FALSE)
        for (pair in pairs) {
          local({
            lg <- g; lp <- pair
            btn_id <- paste0("pick_", lg, "_", gsub(" ", "_", lp[1]), "_", gsub(" ", "_", lp[2]))
            observeEvent(input[[btn_id]], {
              g1_id <- paste0("score_", lg, "_", gsub(" ", "_", lp[1]), "_", gsub(" ", "_", lp[2]), "_g1")
              g2_id <- paste0("score_", lg, "_", gsub(" ", "_", lp[1]), "_", gsub(" ", "_", lp[2]), "_g2")
              
              g1_val <- input[[g1_id]]; g2_val <- input[[g2_id]]
              
              if (!is.na(g1_val) && !is.na(g2_val)) {
                score <- c(g1_val, g2_val)
              } else {
                e1 <- df$Rating[df$Team == lp[1]]; e2 <- df$Rating[df$Team == lp[2]]
                we <- 1 / (10^(-(e1 - e2) / 400) + 1)
                r <- runif(1)
                result <- if (r < we * 0.7) "team1" else if (r > 1 - (1 - we) * 0.7) "team2" else "draw"
                score <- generate_scoreline(lp[1], lp[2], result)
              }
              
              key <- paste(lp[1], lp[2], sep = "__")
              current <- user_bracket$group_scores[[lg]]
              if (is.null(current)) current <- list()
              current[[key]] <- score
              user_bracket$group_scores[[lg]] <- current
            }, ignoreInit = TRUE)
          })
        }
      }
    })
    # Logo click → auto-pick winner
    observeEvent(input$bracket_pick, {
      bp <- input$bracket_pick
      req(bp)
      lg <- bp$g; t1 <- bp$t1; t2 <- bp$t2; side <- bp$winner
      result <- if (side == "1") "team1" else "team2"
      score <- generate_scoreline(t1, t2, result)
      key <- paste(t1, t2, sep = "__")
      current <- user_bracket$group_scores[[lg]]
      if (is.null(current)) current <- list()
      current[[key]] <- score
      user_bracket$group_scores[[lg]] <- current
      updateNumericInput(session, paste0("score_", lg, "_", gsub(" ", "_", t1), "_", gsub(" ", "_", t2), "_g1"), value = score[1])
      updateNumericInput(session, paste0("score_", lg, "_", gsub(" ", "_", t1), "_", gsub(" ", "_", t2), "_g2"), value = score[2])
    })
    # Knockout flag click → pick winner
    observeEvent(input$ko_bracket_pick, {
      kp <- input$ko_bracket_pick
      req(kp)
      lst <- kp$stage; lmid <- kp$mid; lside <- kp$side
      m <- user_bracket[[lst]][[lmid]]
      if (is.null(m)) return()
      winner <- if (lside == "1") m$team1 else m$team2
      result <- if (lside == "1") "team1" else "team2"
      score <- generate_scoreline(m$team1, m$team2, result)
      score_text <- paste(score[1], score[2], sep = "-")
      updated <- user_bracket[[lst]]
      updated[[lmid]]$winner <- winner
      updated[[lmid]]$score  <- score_text
      user_bracket[[lst]] <- updated
    })
    
    # Sim remaining knockout games per stage
    for (st in c("r32", "r16", "qf", "sf", "final")) {
      local({
        lst <- st
        observeEvent(input[[paste0("sim_remaining_", lst)]], {
          matches <- user_bracket[[lst]]
          if (is.null(matches)) return()
          updated <- matches
          for (mid in names(matches)) {
            m <- matches[[mid]]
            if (!is.null(m) && is.null(m$winner)) {
              e1 <- df$Rating[df$Team == m$team1]
              e2 <- df$Rating[df$Team == m$team2]
              we <- 1 / (10^(-(e1 - e2) / 400) + 1)
              winner <- if (runif(1) < we) m$team1 else m$team2
              result <- if (winner == m$team1) "team1" else "team2"
              score <- generate_scoreline(m$team1, m$team2, result)
              updated[[mid]]$winner <- winner
              updated[[mid]]$score  <- paste(score[1], score[2], sep = "-")
            }
          }
          user_bracket[[lst]] <- updated
        }, ignoreInit = TRUE)
      })
    }
    
    # Draw buttons for group stage
    observe({
      groups_list_b <- c("A","B","C","D","E","F","G","H","I","J","K","L")
      for (g in groups_list_b) {
        teams <- get_group_teams_bracket(g)
        pairs <- combn(teams, 2, simplify = FALSE)
        for (pair in pairs) {
          local({
            lg <- g; lp <- pair
            btn_id <- paste0("draw_", lg, "_", gsub(" ", "_", lp[1]), "_", gsub(" ", "_", lp[2]))
            observeEvent(input[[btn_id]], {
              score <- generate_scoreline(lp[1], lp[2], "draw")
              key <- paste(lp[1], lp[2], sep = "__")
              current <- user_bracket$group_scores[[lg]]
              if (is.null(current)) current <- list()
              current[[key]] <- score
              user_bracket$group_scores[[lg]] <- current
              updateNumericInput(session, paste0("score_", lg, "_", gsub(" ", "_", lp[1]), "_", gsub(" ", "_", lp[2]), "_g1"), value = score[1])
              updateNumericInput(session, paste0("score_", lg, "_", gsub(" ", "_", lp[1]), "_", gsub(" ", "_", lp[2]), "_g2"), value = score[2])
            }, ignoreInit = TRUE)
          })
        }
      }
    })
    
    # Confirm button — locks in whatever is in the score boxes
    observe({
      groups_list_b <- c("A","B","C","D","E","F","G","H","I","J","K","L")
      for (g in groups_list_b) {
        teams <- get_group_teams_bracket(g)
        pairs <- combn(teams, 2, simplify = FALSE)
        for (pair in pairs) {
          local({
            lg <- g; lp <- pair
            p1_safe <- gsub(" ", "_", lp[1])
            p2_safe <- gsub(" ", "_", lp[2])
            btn_id <- paste0("confirm_", lg, "_", p1_safe, "_", p2_safe)
            observeEvent(input[[btn_id]], {
              g1_val <- input[[paste0("score_", lg, "_", p1_safe, "_", p2_safe, "_g1")]]
              g2_val <- input[[paste0("score_", lg, "_", p1_safe, "_", p2_safe, "_g2")]]
              if (!is.na(g1_val) && !is.na(g2_val)) {
                key <- paste(lp[1], lp[2], sep = "__")
                current <- user_bracket$group_scores[[lg]]
                if (is.null(current)) current <- list()
                current[[key]] <- c(g1_val, g2_val)
                user_bracket$group_scores[[lg]] <- current
              }
            }, ignoreInit = TRUE)
          })
        }
      }
    })
    observeEvent(input$sim_remaining_groups, {
      groups_list_b <- c("A","B","C","D","E","F","G","H","I","J","K","L")
      for (g in groups_list_b) {
        teams <- get_group_teams_bracket(g)
        pairs <- combn(teams, 2, simplify = FALSE)
        current <- user_bracket$group_scores[[g]]
        if (is.null(current)) current <- list()
        for (pair in pairs) {
          key <- paste(pair[1], pair[2], sep = "__")
          if (is.null(current[[key]])) {
            e1 <- df$Rating[df$Team == pair[1]]
            e2 <- df$Rating[df$Team == pair[2]]
            we <- 1 / (10^(-(e1 - e2) / 400) + 1)
            r <- runif(1)
            result <- if (r < we * 0.7) "team1" else if (r > 1 - (1 - we) * 0.7) "team2" else "draw"
            score <- generate_scoreline(pair[1], pair[2], result)
            current[[key]] <- score
            updateNumericInput(session, paste0("score_", g, "_", gsub(" ", "_", pair[1]), "_", gsub(" ", "_", pair[2]), "_g1"), value = score[1])
            updateNumericInput(session, paste0("score_", g, "_", gsub(" ", "_", pair[1]), "_", gsub(" ", "_", pair[2]), "_g2"), value = score[2])
          }
        }
        user_bracket$group_scores[[g]] <- current
      }
    })
    
    # Advance to knockouts
    observeEvent(input$advance_to_knockouts, {
      # Calculate all group standings
      all_standings <- lapply(c("A","B","C","D","E","F","G","H","I","J","K","L"), function(g) {
        calc_group_standings(g)
      })
      names(all_standings) <- c("A","B","C","D","E","F","G","H","I","J","K","L")
      
      winners  <- sapply(all_standings, function(s) s$Team[1])
      runners  <- sapply(all_standings, function(s) s$Team[2])
      
      # Best 8 third place
      thirds <- bind_rows(lapply(names(all_standings), function(g) {
        s <- all_standings[[g]]
        row <- s[3, ]
        row$Group <- g
        row
      })) %>% arrange(desc(Pts), desc(GD), desc(GF))
      
      user_bracket$advancing_third <- thirds$Team[1:8]
      ta <- assign_third_place_teams(thirds, all_standings)
      
      make_ko_match <- function(t1, t2) list(team1 = t1, team2 = t2, winner = NULL, score = NULL)
      
      user_bracket$r32 <- list(
        m73 = make_ko_match(runners["A"], runners["B"]),
        m74 = make_ko_match(winners["E"], ta[["74"]]),
        m75 = make_ko_match(winners["F"], runners["C"]),
        m76 = make_ko_match(winners["C"], runners["F"]),
        m77 = make_ko_match(winners["I"], ta[["77"]]),
        m78 = make_ko_match(runners["E"], runners["I"]),
        m79 = make_ko_match(winners["A"], ta[["79"]]),
        m80 = make_ko_match(winners["L"], ta[["80"]]),
        m81 = make_ko_match(winners["D"], ta[["81"]]),
        m82 = make_ko_match(winners["G"], ta[["82"]]),
        m83 = make_ko_match(runners["K"], runners["L"]),
        m84 = make_ko_match(winners["H"], runners["J"]),
        m85 = make_ko_match(winners["B"], ta[["85"]]),
        m86 = make_ko_match(winners["J"], runners["H"]),
        m87 = make_ko_match(winners["K"], ta[["87"]]),
        m88 = make_ko_match(runners["D"], runners["G"])
      )
      
      user_bracket$stage <- "r32"
    })
    
    # Handle knockout pick buttons
    observe({
      stages_ko <- c("r32", "r16", "qf", "sf", "final")
      match_ids <- list(
        r32 = paste0("m", 73:88),
        r16 = paste0("m", 89:96),
        qf  = paste0("m", 97:100),
        sf  = c("m101", "m102"),
        final = "mf"
      )
      
      for (st in stages_ko) {
        for (mid in match_ids[[st]]) {
          local({
            lst <- st; lmid <- mid
            for (side in c("1", "2")) {
              local({
                lside <- side
                btn_id <- paste0("ko_pick_", lmid, "_", lside)
                observeEvent(input[[btn_id]], {
                  m <- user_bracket[[lst]][[lmid]]
                  if (is.null(m)) return()
                  
                  winner <- if (lside == "1") m$team1 else m$team2
                  
                  g1_id <- paste0("ko_", lmid, "_g1")
                  g2_id <- paste0("ko_", lmid, "_g2")
                  g1_val <- input[[g1_id]]; g2_val <- input[[g2_id]]
                  
                  if (!is.na(g1_val) && !is.na(g2_val)) {
                    if ((lside == "1" && g1_val > g2_val) || (lside == "2" && g2_val > g1_val)) {
                      score <- c(g1_val, g2_val)
                    } else {
                      result <- if (lside == "1") "team1" else "team2"
                      score <- generate_scoreline(m$team1, m$team2, result)
                    }
                  } else {
                    result <- if (lside == "1") "team1" else "team2"
                    score <- generate_scoreline(m$team1, m$team2, result)
                  }
                  
                  score_text <- paste(score[1], score[2], sep = "-")
                  updated <- user_bracket[[lst]]
                  updated[[lmid]]$winner <- winner
                  updated[[lmid]]$score  <- score_text
                  user_bracket[[lst]] <- updated
                  
                }, ignoreInit = TRUE)
              })
            }
          })
        }
      }
    })
    
    # Advance knockout stages
    observeEvent(input$advance_r32, {
      r32 <- user_bracket$r32
      make_ko_match <- function(t1, t2) list(team1 = t1, team2 = t2, winner = NULL, score = NULL)
      user_bracket$r16 <- list(
        m89  = make_ko_match(r32$m74$winner, r32$m77$winner),
        m90  = make_ko_match(r32$m73$winner, r32$m75$winner),
        m91  = make_ko_match(r32$m76$winner, r32$m78$winner),
        m92  = make_ko_match(r32$m79$winner, r32$m80$winner),
        m93  = make_ko_match(r32$m83$winner, r32$m84$winner),
        m94  = make_ko_match(r32$m81$winner, r32$m82$winner),
        m95  = make_ko_match(r32$m86$winner, r32$m88$winner),
        m96  = make_ko_match(r32$m85$winner, r32$m87$winner)
      )
      user_bracket$stage <- "r16"
    })
    
    observeEvent(input$advance_r16, {
      r16 <- user_bracket$r16
      make_ko_match <- function(t1, t2) list(team1 = t1, team2 = t2, winner = NULL, score = NULL)
      user_bracket$qf <- list(
        m97  = make_ko_match(r16$m89$winner, r16$m90$winner),
        m98  = make_ko_match(r16$m93$winner, r16$m94$winner),
        m99  = make_ko_match(r16$m91$winner, r16$m92$winner),
        m100 = make_ko_match(r16$m95$winner, r16$m96$winner)
      )
      user_bracket$stage <- "qf"
    })
    
    observeEvent(input$advance_qf, {
      qf <- user_bracket$qf
      make_ko_match <- function(t1, t2) list(team1 = t1, team2 = t2, winner = NULL, score = NULL)
      user_bracket$sf <- list(
        m101 = make_ko_match(qf$m97$winner, qf$m98$winner),
        m102 = make_ko_match(qf$m99$winner, qf$m100$winner)
      )
      user_bracket$stage <- "sf"
    })
    
    observeEvent(input$advance_sf, {
      sf <- user_bracket$sf
      make_ko_match <- function(t1, t2) list(team1 = t1, team2 = t2, winner = NULL, score = NULL)
      user_bracket$final <- list(
        mf = make_ko_match(sf$m101$winner, sf$m102$winner)
      )
      user_bracket$stage <- "final"
    })
    
    observeEvent(input$reset_bracket, {
      user_bracket$group_scores <- list()
      user_bracket$group_standings <- list()
      user_bracket$advancing_third <- NULL
      user_bracket$r32 <- list()
      user_bracket$r16 <- list()
      user_bracket$qf  <- list()
      user_bracket$sf  <- list()
      user_bracket$third_match <- NULL
      user_bracket$final_match <- NULL
      user_bracket$stage <- "groups"
    })
    
    # ==================== HEAD TO HEAD ====================
    
    output$h2h_ui <- renderUI({
      all_wc_teams <- sort(c("Mexico", "South Korea", "South Africa", "Czechia",
                             "Canada", "Switzerland", "Qatar", "Bosnia and Herzegovina",
                             "Brazil", "Morocco", "Scotland", "Haiti",
                             "United States", "Paraguay", "Australia", "Turkey",
                             "Germany", "Ecuador", "Ivory Coast", "Curaçao",
                             "Netherlands", "Japan", "Tunisia", "Sweden",
                             "Belgium", "Iran", "Egypt", "New Zealand",
                             "Spain", "Uruguay", "Saudi Arabia", "Cape Verde",
                             "France", "Senegal", "Norway", "Iraq",
                             "Argentina", "Austria", "Algeria", "Jordan",
                             "Portugal", "Colombia", "Uzbekistan", "DR Congo",
                             "England", "Croatia", "Panama", "Ghana"))
      
      div(
        h3("⚔️ Head to Head Matchup Simulator", style = "text-align: center; margin-bottom: 5px;"),
        p(style = "text-align: center; color: #666; margin-bottom: 20px;",
          "Select two teams and simulate 1,000 group stage matchups"),
        fluidRow(
          column(4, selectInput("h2h_team1", "Team 1:", choices = all_wc_teams, selected = "Spain")),
          column(4, style = "text-align: center; padding-top: 30px;",
                 h3("vs", style = "color: #667eea;")),
          column(4, selectInput("h2h_team2", "Team 2:", choices = all_wc_teams, selected = "England"))
        ),
        fluidRow(
          column(12, style = "text-align: center;",
                 actionButton("run_h2h", "⚡ Run 1,000 Simulations",
                              class = "btn-primary btn-lg",
                              style = "font-size: 16px; padding: 12px 30px;")
          )
        ),
        br(),
        uiOutput("h2h_results")
      )
    })
    
    observeEvent(input$run_h2h, {
      output$h2h_results <- renderUI({
        team1 <- input$h2h_team1
        team2 <- input$h2h_team2
        
        if (team1 == team2) return(p("Please select two different teams.", style = "color: red; text-align: center;"))
        
        e1 <- df$Rating[df$Team == team1]
        e2 <- df$Rating[df$Team == team2]
        base_goals <- 1.3
        elo_diff <- e1 - e2
        lambda1 <- round(max(0.1, base_goals * 1.25^(elo_diff / 100)), 3)
        lambda2 <- round(max(0.1, base_goals * 1.25^(-elo_diff / 100)), 3)
        
        # Win expectancy from ELO formula
        we <- 1 / (10^(-elo_diff / 400) + 1)
        
        # Run 1000 sims
        results <- replicate(1000, {
          g1 <- rpois(1, lambda1); g2 <- rpois(1, lambda2)
          list(g1 = g1, g2 = g2,
               outcome = if (g1 > g2) "team1" else if (g2 > g1) "team2" else "draw")
        }, simplify = FALSE)
        
        outcomes <- sapply(results, function(r) r$outcome)
        w1 <- mean(outcomes == "team1")
        dr <- mean(outcomes == "draw")
        w2 <- mean(outcomes == "team2")
        
        scores_df <- data.frame(
          score = sapply(results, function(r) paste(r$g1, r$g2, sep = "-")),
          stringsAsFactors = FALSE
        ) %>%
          group_by(score) %>%
          summarise(n = n(), pct = round(n() / 10, 1), .groups = "drop") %>%
          arrange(desc(n)) %>%
          head(8)
        
        most_likely <- scores_df$score[1]
        
        div(
          hr(),
          # ELO and expected goals
          fluidRow(
            column(4, div(class = "stat-card", style = "text-align: center;",
                          div(class = "stat-title", "ELO Rating"),
                          div(class = "stat-value", e1),
                          p(HTML(paste0(get_flag(team1), " ", team1)))
            )),
            column(4, div(class = "stat-card", style = "text-align: center;",
                          div(class = "stat-title", "ELO Difference"),
                          div(class = "stat-value", style = "color: #667eea;", abs(e1 - e2)),
                          p(if (e1 > e2) paste(team1, "favoured") else paste(team2, "favoured"))
            )),
            column(4, div(class = "stat-card", style = "text-align: center;",
                          div(class = "stat-title", "ELO Rating"),
                          div(class = "stat-value", e2),
                          p(HTML(paste0(get_flag(team2), " ", team2)))
            ))
          ),
          br(),
          fluidRow(
            column(4, div(class = "stat-card", style = "text-align: center;",
                          div(class = "stat-title", "Expected Goals"),
                          div(class = "stat-value", style = "color: #4CAF50;", lambda1),
                          p(HTML(paste0(get_flag(team1), " xG")))
            )),
            column(4, div(class = "stat-card", style = "text-align: center;",
                          div(class = "stat-title", "Most Likely Score"),
                          div(class = "stat-value", most_likely),
                          p(paste0(scores_df$pct[1], "% of simulations"))
            )),
            column(4, div(class = "stat-card", style = "text-align: center;",
                          div(class = "stat-title", "Expected Goals"),
                          div(class = "stat-value", style = "color: #c0392b;", lambda2),
                          p(HTML(paste0(get_flag(team2), " xG")))
            ))
          ),
          br(),
          # Win/Draw/Loss bars
          h4("Match Outcome Probabilities (1,000 Sims)", style = "text-align: center;"),
          div(style = "margin: 15px 0;",
              # Team 1 win bar
              fluidRow(
                column(2, style = "text-align: right; padding-top: 5px; font-weight: bold;",
                       HTML(paste0(get_flag(team1), " Win"))),
                column(8,
                       div(style = "width: 100%; height: 28px; background: #e0e0e0; border-radius: 14px; overflow: hidden;",
                           div(style = sprintf("width: %.1f%%; height: 100%%; background: #4CAF50; border-radius: 14px; display: flex; align-items: center; justify-content: center; color: white; font-weight: bold;",
                                               w1 * 100),
                               sprintf("%.1f%%", w1 * 100))
                       )
                ),
                column(2, style = "padding-top: 5px; font-weight: bold;", sprintf("%.1f%%", w1 * 100))
              ),
              br(),
              fluidRow(
                column(2, style = "text-align: right; padding-top: 5px; font-weight: bold;", "Draw"),
                column(8,
                       div(style = "width: 100%; height: 28px; background: #e0e0e0; border-radius: 14px; overflow: hidden;",
                           div(style = sprintf("width: %.1f%%; height: 100%%; background: #f39c12; border-radius: 14px; display: flex; align-items: center; justify-content: center; color: white; font-weight: bold;",
                                               dr * 100),
                               sprintf("%.1f%%", dr * 100))
                       )
                ),
                column(2, style = "padding-top: 5px; font-weight: bold;", sprintf("%.1f%%", dr * 100))
              ),
              br(),
              fluidRow(
                column(2, style = "text-align: right; padding-top: 5px; font-weight: bold;",
                       HTML(paste0(get_flag(team2), " Win"))),
                column(8,
                       div(style = "width: 100%; height: 28px; background: #e0e0e0; border-radius: 14px; overflow: hidden;",
                           div(style = sprintf("width: %.1f%%; height: 100%%; background: #c0392b; border-radius: 14px; display: flex; align-items: center; justify-content: center; color: white; font-weight: bold;",
                                               w2 * 100),
                               sprintf("%.1f%%", w2 * 100))
                       )
                ),
                column(2, style = "padding-top: 5px; font-weight: bold;", sprintf("%.1f%%", w2 * 100))
              )
          ),
          br(),
          # Score distribution
          h4("Top 8 Most Common Scorelines", style = "text-align: center;"),
          p(style = "text-align: center; color: #666; font-size: 12px;",
            paste0("(", team1, " score listed first)")),
          renderPlot({
            scores_df$score <- factor(scores_df$score, levels = rev(scores_df$score))
            ggplot(scores_df, aes(x = score, y = pct, fill = pct)) +
              geom_col(width = 0.6, show.legend = FALSE) +
              geom_text(aes(label = paste0(pct, "%")), hjust = -0.2, fontface = "bold", size = 4.5) +
              scale_fill_gradient(low = "#a8c4e8", high = "#2c5f9e") +
              scale_y_continuous(limits = c(0, max(scores_df$pct) * 1.3),
                                 labels = function(x) paste0(x, "%")) +
              coord_flip() +
              labs(x = NULL, y = "% of Simulations") +
              theme_minimal(base_size = 13) +
              theme(
                plot.background = element_rect(fill = "white", color = NA),
                panel.grid.major.y = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.y = element_text(face = "bold", size = 12),
                plot.margin = margin(20, 40, 20, 20)
              )
          }, height = 350),
          br(),
          # Tournament sim probs comparison
          if (!is.null(sim_probs)) {
            p1 <- sim_probs[sim_probs$Team == team1, ]
            p2 <- sim_probs[sim_probs$Team == team2, ]
            if (nrow(p1) > 0 && nrow(p2) > 0) {
              div(
                h4("Tournament Probability Comparison", style = "text-align: center;"),
                tags$table(style = "width: 100%; border-collapse: collapse; font-size: 14px;",
                           tags$tr(style = "background: #667eea; color: white;",
                                   tags$th(style = "padding: 10px; text-align: left;", "Stage"),
                                   tags$th(style = "padding: 10px; text-align: center;",
                                           HTML(paste0(get_flag(team1), " ", team1))),
                                   tags$th(style = "padding: 10px; text-align: center;",
                                           HTML(paste0(get_flag(team2), " ", team2)))
                           ),
                           lapply(list(
                             list("Qualify%", "Qualify"),
                             list("R16%", "R16"),
                             list("QF%", "QF"),
                             list("SF%", "SF"),
                             list("Final%", "Final"),
                             list("Champion%", "Champion")
                           ), function(stage_info) {
                             label <- stage_info[[1]]
                             col   <- stage_info[[2]]
                             v1 <- p1[[col]]; v2 <- p2[[col]]
                             row_idx <- match(label, c("Qualify%", "R16%", "QF%", "SF%", "Final%", "Champion%"))
                             bg <- if (row_idx %% 2 == 0) "#f8f9fa" else "white"
                             tags$tr(style = paste0("background:", bg, ";"),
                                     tags$td(style = "padding: 8px 10px; font-weight: bold;", label),
                                     tags$td(style = paste0("padding: 8px 10px; text-align: center; color:",
                                                            if (v1 > v2) "#1a6b2e" else if (v1 < v2) "#c0392b" else "#333", 
                                                            "; font-weight: bold;"), paste0(v1, "%")),
                                     tags$td(style = paste0("padding: 8px 10px; text-align: center; color:",
                                                            if (v2 > v1) "#1a6b2e" else if (v2 < v1) "#c0392b" else "#333",
                                                            "; font-weight: bold;"), paste0(v2, "%"))
                             )
                           })
                )
              )
            }
          }
        )
      })
    })
  }

shinyApp(ui = ui, server = server)

