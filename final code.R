library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
set.seed(123)

data <- read.csv("nohead1.csv", header = TRUE)
colnames(data) <- c("Game.Time", "home.team", "score","diff", "away.team")

setwd("C:/Users/Niki/Desktop/училище/semester 6/Bachelor/file path")
data <- read.csv("bballfinal.csv", header = TRUE)
data <- data[,-1]

data$seconds <- sapply(strsplit(as.character(data$Game.Time), ":"), function(x) {
  as.numeric(x[1]) * 60 + as.numeric(x[2])
})

data$Game.Time <- NULL
data <- data[,c(5,1:4)]

data$diff <- as.numeric(data$diff)
data <- data[!is.na(data$diff), ]


data$score <- as.character(data$score)
data$seconds <- as.numeric(data$seconds)

potential_new_game <- data$score == "0 - 0" & data$seconds == 600
data$NewGame <- c(TRUE, diff(potential_new_game) == 1)
data$NewGame[1] <- potential_new_game[1]  

data$GameID <- cumsum(data$NewGame)
data$NewGame <- NULL  

data <- data %>%
  arrange(GameID, row_number()) %>%
  group_by(GameID) %>%
  mutate(
    new_quarter = seconds > lag(seconds, default = first(seconds)) + 500,
    quarter = cumsum(new_quarter) + 1
  ) %>%
  ungroup() %>%
  select(-new_quarter)



data$home.team <- gsub("^\\d+\\s+[a-z]+\\s+[a-z]+\\s+", "", data$home.team)
data$away.team <- gsub("^\\d+\\s+[a-z]+\\s+[a-z]+\\s+", "", data$away.team)

data$home.team <- gsub("^\\d+\\s+[A-Z][a-z]+\\s+[A-Z][a-z]+\\s+", "", data$home.team)
data$away.team <- gsub("^\\d+\\s+[A-Z][a-z]+\\s+[A-Z][a-z]+\\s+", "", data$away.team)

data$home.team <- gsub("^\\d+\\s+.*?\\s+[A-Z](\\s+[A-Z])?\\s+", "", data$home.team)
data$away.team <- gsub("^\\d+\\s+.*?\\s+[A-Z](\\s+[A-Z])?\\s+", "", data$away.team)

data$home.team <- gsub("\\s*\\(\\d+\\)$", "", data$home.team)
data$away.team <- gsub("\\s*\\(\\d+\\)$", "", data$away.team)

data$home.team <- gsub("^\\d+\\s+[A-Z][a-z]+\\s+[A-Z][a-z\\-]+\\s+", "", data$home.team)
data$away.team <- gsub("^\\d+\\s+[A-Z][a-z]+\\s+[A-Z][a-z\\-]+\\s+", "", data$away.team)

data$away.team <- gsub("^\\d+\\s+[A-Z][a-z]+\\s+[A-Z][a-z]+(-[A-Z][a-z]+)?\\s+", "", data$away.team)
data$away.team <- gsub("^\\d+\\s+[A-Z][a-z]+(-[A-Z][a-z]+)?\\s+[A-Z][a-z]+\\s+", "", data$away.team)

remove_keywords <- c(
  "steal",
  "substitution  out",
  "ft  missed",
  "jump ball  held ball won",
  "jump ball  held ball won made",
  "jump ball  lodged ball won",
  "jump ball  unclear possession won missed",
  "block",
  "substitution  in",
  "ft made",
  "jump ball  lost",
  "assist", 
  "jump ball won",
  "foul  drawn",
  "jump ball  contestedrebound won missed",  
  "jump ball  unclear possession won" 
)

pattern <- paste(remove_keywords, collapse = "|")

keep_rows <- !(
  (grepl(pattern, data$home.team, ignore.case = TRUE) & !is.na(data$home.team)) |
    (grepl(pattern, data$away.team, ignore.case = TRUE) & !is.na(data$away.team))
)

data <- data[keep_rows, ]

length(unique(data$home.team))

data$home.team <- gsub("foul  offensive", "turnover", data$home.team )
data$home.team <- gsub("foul  charge offensive", "turnover", data$home.team)
data$away.team <- gsub("foul  offensive", "turnover", data$away.team )
data$away.team <- gsub("foul  charge offensive", "turnover", data$away.team)


simple_event <- function(s) {
  
  if (strsplit(s, " ")[[1]][1] %in% c(2,3)) {
    
    words = strsplit(s, " ")[[1]]
    s = words[c(1,length(words))]
    
    if (s[2] != "missed") {
      s[2] = "made"
      
    }
    
    s = paste(s ,collapse = " ")
    
  }
  else if (startsWith(s, "free throw") ) {
    
    words = strsplit(s, " ")[[1]]
    if (!(tail(words,1) %in% c("missed", "made"))) {
      words = c(words, "made")
      
    }
    v = tail(words, 4)
    s = paste("free throw",paste(v, collapse = " "), collapse = " " )
  }
  else if (startsWith(s, "foul")) {
    s = "foul"
  }
  else if (startsWith(s, "in the paint")) {
    words = strsplit(s, " ")[[1]]
    if (words[length(words)] == "made") {
      s = "2 made"
    }
    else  {
      s = "2 missed"
    }
  }
  else if (startsWith(s, "rebound")) {
    
    words = strsplit(s, " ")[[1]]
    s = paste(words[c(1,length(words))], collapse = " ")
    
  }
  else if (startsWith(s, "timeOut")) {
    s = "timeOut"
    
  }
  else if (startsWith(s, "turnover")) {
    s = "turnover"
    
  }
  return(s)
  
}




data$home.team <- sapply(data$home.team, simple_event)
data$away.team <- sapply(data$away.team, simple_event)

data$away.team <- gsub("away made", "2 made", data$away.team)
data$away.team <- gsub("around jump shot missed", "2 missed", data$away.team)
data$away.team <- gsub("chance jump shot made", "2 made", data$away.team)
data$away.team <- gsub("chance jump shot missed", "2 missed", data$away.team)
data$away.team <- gsub("Gil 2 pts tip in", "2 made", data$away.team)
data$away.team <- gsub("Gil 3 pts jump shot", "3 made", data$away.team)
data$away.team <- gsub("jump shot made", "2 made", data$away.team)
data$away.team <- gsub("jump shot missed", "2 missed", data$away.team)
data$away.team <- gsub("shot made", "2 made", data$away.team)
data$away.team <- gsub("shot missed", "2 missed", data$away.team)
data$away.team <- gsub("up jump shot made", "2 made", data$away.team)
data$away.team <- gsub("up 2 made", "2 made", data$away.team)
data$away.team <- gsub("break 2 made" , "2 made", data$away.team)

remove_values <- c("jump ball  out of bounds won", "jump ball  contestedRebound won")
datatest <- data
data <- data[!data$away.team %in% remove_values, ]
data <- data[!(grepl("^made$", data$away.team)), ]



data$home.team <- gsub("away made", "2 made", data$home.team)
data$home.team <- gsub("away missed", "2 missed", data$home.team)
data$home.team <- gsub("back jump shot missed", "2 missed", data$home.team)
data$home.team <- gsub("break jump shot made", "2 made", data$home.team)
data$home.team <- gsub("chance floating jump shot missed", "2 missed", data$home.team)
data$home.team <- gsub("chance jump shot made", "2 made", data$home.team)
data$home.team <- gsub("chance jump shot missed", "2 missed", data$home.team)
data$home.team <- gsub("jump shot missed", "2 missed", data$home.team)
data$home.team <- gsub("shot made", "2 made", data$home.team)
data$home.team <- gsub("shot missed", "2 missed", data$home.team)
data$home.team <- gsub("up jump shot made", "2 made", data$home.team)
data$home.team <- gsub("up jump 2 made", "2 made", data$home.team)
data<-data[!(grepl("^missed$", data$home.team)), ]

event <- c()

for(i in 1:nrow(data)) {
  row <- data[i,]
  if (row$home.team == "") {
    x = paste("a", row$away.team, collapse = "")
  }
  else  {
    x = paste("h", row$home.team, collapse = "")
  }
  event[i] <- x
}

data$event <- event

data[,c(2,5)] <- NULL
data[,2] <- NULL


### Model Event Functions

# First Order Markov
from_event <- data$event[-nrow(data)]  
to_event   <- data$event[-1]           

transitions <- data.frame(from = from_event, to = to_event)
trans_counts <- table(transitions$from, transitions$to)

event_dist <- prop.table(trans_counts, 1)  
head(event_dist)

# Second Order Markov
triplets <- as.data.frame(embed(data$event, 3)[, 3:1])  

colnames(triplets) <- c("Prev1", "Prev2", "Next")

event_dist_2oM <- triplets %>%
  group_by(Prev1, Prev2, Next) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Prev1, Prev2) %>%
  mutate(probability = count / sum(count)) %>%
  ungroup()

event_dist_2oM <- event_dist_2oM %>% filter(probability > 0.02)





### Model Duration

from_seconds <- data$seconds[-nrow(data)]
to_seconds   <- data$seconds[-1]

from_quarter <- data$quarter[-nrow(data)]
to_quarter   <- data$quarter[-1]

same_quarter <- from_quarter == to_quarter
dur <- from_seconds[same_quarter] - to_seconds[same_quarter]

dur_data <- data.frame(
  from = from_event[same_quarter],
  to   = to_event[same_quarter],
  dur  = dur
)
dur_dist <- dur_data[dur_data$dur > 0 & dur_data$dur <= 24, ]




### Game Simulating Functions

# Help Functions

a_scoring_events <- c("h 2 made", "h 3 made", "a 2 made", "a 3 made","a free throw 1 of 1 made","a free throw 1 of 2 made", "a free throw 1 of 3 made", "a free throw 2 of 2 made", "a free throw 2 of 3 made", "a free throw 3 of 3 made")
h_scoring_events <-c("h 2 made", "h 3 made", "h free throw 1 of 1 made","h free throw 1 of 2 made", "h free throw 1 of 3 made", "h free throw 2 of 2 made", "h free throw 2 of 3 made", "h free throw 3 of 3 made")

sample_duration <- function(from_event, to_event) {
  row <- dur_dist %>%
    filter(from == from, to == to)
  
  if (nrow(row) > 0) {
    return(sample(row$dur[[1]], 1))
  } else {
    return(NA)  # ако няма наблюдавана такава двойка
  }
}
update_diff <- function(event, current_diff) {
  
  if (event %in% h_scoring_events) {
    if (grepl("free throw", event) && grepl("made$", event)) return(current_diff + 1)
    if (grepl("3 made", event)) return(current_diff + 3)
    if (grepl("2 made", event)) return(current_diff + 2)
    
  } else if (event %in% a_scoring_events) {
    if (grepl("free throw", event) && grepl("made$", event)) return(current_diff - 1)
    if (grepl("3 made", event)) return(current_diff - 3)
    if (grepl("2 made", event)) return(current_diff - 2)
    
  }
  return(current_diff)
}
simulate_next_row <- function(start_event, difference){
  
  timeLeft<- 600
  current_diff <- as.numeric(difference)
  current_event <- start_event
  start_event_dist <- as.data.frame(event_dist %>%
                                      filter(Var1 == start_event))
  simulated_couple <- as.data.frame(
    rbind(setNames(c(600,start_event, difference), c("timeLeft", "event", "diff"))),
    stringsAsFactors = FALSE)
  
  next_event <- as.character(sample(start_event_dist$Var2, 
                                    1, prob = start_event_dist$Freq))
  current_diff <- as.numeric(update_diff(next_event, current_diff))
  duration <- sample_duration(start_event,next_event)
  new_time <-  600 - as.numeric(duration)
  new_row <- c(new_time, next_event, current_diff)
  simulated_pair <- rbind(c(as.numeric(timeLeft), start_event, as.numeric(difference)), new_row)
  simulated_pair[,c(1,3)] <- as.numeric(simulated_pair[,c(1,3)])
  rownames(simulated_pair) <- NULL
  colnames(simulated_pair) <- c("timeLeft", "event", "diff")
  return(simulated_pair)
  
}

# Simulating function first/second order Markov

event_dist <- as.data.frame(event_dist)

simulate_quarter_1oM <- function(start_event, difference) {
  
  timeLeft<- 600
  current_diff <- difference
  current_event <- start_event
  
  start_event_dist <- as.data.frame(event_dist %>%
    filter(Var1 == start_event))
  
  simulated_quarter <- as.data.frame(
    rbind(setNames(c(600,start_event, difference), c("timeLeft", "event", "diff"))),
    stringsAsFactors = FALSE
  )
  while (timeLeft > 0) {
  
    next_event <- as.character(sample(start_event_dist$Var2, 
                                      1, prob = start_event_dist$Freq))
    print(next_event)
    
    duration <- sample_duration(start_event,next_event)
    
    timeLeft <- as.numeric(timeLeft)- duration
    
    current_diff <- as.numeric(update_diff(next_event, current_diff))
  
    new_row <- c(timeLeft, next_event, current_diff)
    simulated_quarter <- rbind(simulated_quarter, new_row)
  
    start_event <- next_event
    start_event_dist <- as.data.frame(event_dist %>%
                                        filter(Var1 == start_event))
    
  }
  simulated_quarter <- simulated_quarter[-nrow(simulated_quarter),]
  return(simulated_quarter)
}
simulate_quarter_2oM <- function(start_event, difference){
  
  simulated_quarter <- simulate_next_row(start_event, difference)

  current_diff <- as.numeric(simulated_quarter[2,3])
  current_events_dist <- event_dist_2oM %>% 
    filter(Prev1 == simulated_quarter[1,2] & Prev2 == simulated_quarter[2,2])
  
  timeLeft <- simulated_quarter[2,1]
  
  while(timeLeft > 0){
    
    next_event <- as.character(sample_n(current_events_dist, 1, prob =
                                      current_events_dist$probability)[1,3])
    
    dur <- sample_duration(simulated_quarter[nrow(simulated_quarter),2], next_event)
    
    current_diff <- as.numeric(update_diff(next_event, current_diff))
    
    timeLeft <- as.numeric(simulated_quarter[nrow(simulated_quarter),1]) - dur
    print(timeLeft)
    
    new_row <- c(timeLeft, next_event, current_diff)
    simulated_quarter <- rbind(simulated_quarter, new_row)
    
    current_events_dist <- event_dist_2oM %>% 
      filter(Prev1 == simulated_quarter[nrow(simulated_quarter)-1 ,2] & Prev2 == simulated_quarter[nrow(simulated_quarter),2])
  
    
    rownames(simulated_quarter) <- NULL
    
  }
  simulated_quarter <- as.data.frame(simulated_quarter, stringsAsFactors = FALSE)
  
  # Convert columns to appropriate types
  simulated_quarter$timeLeft <- as.numeric(simulated_quarter$timeLeft)
  simulated_quarter$diff <- as.numeric(simulated_quarter$diff)
  simulated_quarter <- simulated_quarter[-nrow(simulated_quarter),]
  
  
  return(simulated_quarter)
  
}


# Simulate Games

simulate_game <- function(n, simulate_quarter) {
  all_games <- data.frame()
  
  for (game in 1:n) {
    for (q in 1:4) {
      quarter_data <- simulate_quarter(start_event = "h jump ball  won", difference = 0)
      
      # Add game and quarter info
      quarter_data$GameID <- game
      quarter_data$quarter <- q
      
      # Combine with all_games
      all_games <- rbind(all_games, quarter_data)
    }
  }
  
  return(all_games)
}

simulated.data.1oM <- simulate_game(500, simulate_quarter_1oM)
simulated.data.2oM <- simulate_game(500, simulate_quarter_2oM)


#### Plot 1

scoring_patterns <- c(
  "2 made", "3 made",
  "free throw 1 of 1 made", "free throw 1 of 2 made", "free throw 1 of 3 made",
  "free throw 2 of 2 made", "free throw 2 of 3 made", "free throw 3 of 3 made"
)

scoring_summary <- data %>%
  mutate(
    diff_group = floor(diff / 3) * 3,
    made = ifelse(grepl(paste(scoring_patterns, collapse = "|"), event), 1, 0)
  ) %>%
  filter(grepl("2|3|free throw|turnover|foul", event)) %>%
  group_by(diff_group) %>%
  summarise(
    prob = mean(made),
    count = n()
  ) %>%
  filter(count > 5) %>%
  mutate(source = "Observed")


simulated_1oM_summary <- simulated.data.1oM %>%
  mutate(
    diff_group = floor(as.numeric(diff) / 3) * 3,
    made = ifelse(grepl(paste(scoring_patterns, collapse = "|"), event), 1, 0)
  ) %>%
  filter(grepl("2|3|free throw|turnover|foul", event)) %>%
  group_by(diff_group) %>%
  summarise(
    prob = mean(made),
    count = n()
  ) %>%
  filter(count > 5) %>%
  mutate(source = "Simulated First Order Markov Chain")




simulated_2oM_summary <- simulated.data.2oM %>%
  mutate(
    diff_group = floor(as.numeric(diff) / 3) * 3,
    made = ifelse(grepl(paste(scoring_patterns, collapse = "|"), event), 1, 0)
  ) %>%
  filter(grepl("2|3|free throw|turnover|foul", event)) %>%
  group_by(diff_group) %>%
  summarise(
    prob = mean(made),
    count = n()
  ) %>%
  filter(count > 5) %>%
  mutate(source = "Simulated Second Order Markov Chain")


simulated_2oM_summary
simulated_1oM_summary

plot_data <- bind_rows(scoring_summary, simulated_2oM_summary, simulated_1oM_summary)

ggplot(plot_data, aes(x = diff_group, y = prob, color = source, linetype = source)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    breaks = seq(-25, 25, by = 5),
    limits = c(-25, 25)
  ) 
  scale_color_manual(values = c("Observed" = "black", "Simulated" = "red")) +
  scale_linetype_manual(values = c("Observed" = "solid", "Simulated" = "dashed")) +
  labs(
    x = "Points difference",
    y = "Probability of scoring",
    title = "Scoring Probability by Point Difference",
    subtitle = "Observed vs Simulated (Only -25 to +25 Range)",
    color = "Data Source",
    linetype = "Data Source"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30")
  )
  
  
### Brier Score

  
# First Order Markov
  
classes <- colnames(evt_rel_freq)
brier_scores <- c()

for (i in 1:length(from_event)) {
  actual_class <- to_event[i]
  if (!(from_event[i] %in% rownames(evt_rel_freq))) next
    
    # Вземи вероятностите от EvtRelFreq модела
  probs <- evt_rel_freq[from_event[i], ]
  probs <- as.numeric(probs)
  names(probs) <- classes
    
    # Вектор с 1 за реалния клас, 0 за останалите
  actual <- as.numeric(classes == actual_class)
    
    # Ако няма такъв клас – прескачаме
  if (sum(actual) == 0) next
    
    # Brier score за наблюдението
  brier <- mean((probs - actual)^2)
  brier_scores <- c(brier_scores, brier)
}
  
  # Средна стойност
mean_brier <- mean(brier_scores) 
print(paste("Average Brier Score:", round(mean_brier, 4)))
sd_brier <- sd(brier_scores)  
  ## Plot
  
brier_df <- data.frame(
  from = from[1:length(brier_scores)],  # съответстващи събития
  brier = brier_scores
  )
  
brier_df <- brier_df %>%
  mutate(
    event_type = case_when(
      grepl("2 made|3 made", from) ~ "made shot",
      grepl("2 missed|3 missed", from) ~ "missed shot",
      grepl("turnover", from) ~ "turnover",
      grepl("foul", from) ~ "foul",
      grepl("inbound", from) ~ "inbound",
      grepl("rebound ", from) ~ "rebound offensive",
      TRUE ~ "other"
    )
  )
  
  
  
ggplot(brier_df, aes(x = brier, fill = event_type)) +
  geom_histogram(binwidth = 0.01, position = "stack", color = "black") +
    
  labs(
    title = "Stacked Histogram of Brier Scores ",
    x = "Brier Score",
    y = "Count",
    fill = "From Event Type"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette="Set3")

# 1. Group by 'from' and calculate mean Brier score per event
brier_by_event <- brier_df %>%
  group_by(from) %>%
  summarise(mean_brier = mean(brier), .groups = "drop") %>%
  arrange(mean_brier)

# 2. Top 10 best predicted events (lowest Brier scores)
best_predicted <- head(brier_by_event, 10)

# 3. Top 10 worst predicted events (highest Brier scores)
worst_predicted <- tail(brier_by_event, 10)

# 4. Display them
print("✅ Best predicted events:")
print(best_predicted)

print("❌ Worst predicted events:")
print(worst_predicted)

  
# Second Order Markov


brier_scores_2oM <- event_dist_2oM %>%
  group_by(Prev1, Prev2) %>%
  summarise(
    brier = {
      probs <- probability
      classes <- Next
      actual <- as.numeric(classes == classes[which.max(probability)])  # assume top event is the actual one
      mean((probs - actual)^2)
    },
    .groups = "drop"
  )

mean_brier_2oM <- mean(brier_scores_2oM$brier)
sd_brier_2oM <- var(brier_scores_2oM$brier)

print(paste("Mean Brier Score:", round(mean_brier_2oM, 4)))

brier_df_2oM <- brier_scores_2oM %>%
  mutate(event_type = case_when(
    grepl("2 made|3 made", Prev2) ~ "made shot",
    grepl("2 missed|3 missed", Prev2) ~ "missed shot",
    grepl("free throw.*made", Prev2) ~ "made FT",
    grepl("free throw.*missed", Prev2) ~ "missed FT",
    grepl("turnover", Prev2) ~ "turnover",
    grepl("foul", Prev2) ~ "foul",
    grepl("rebound", Prev2) ~ "rebound",
    grepl("inbound", Prev2) ~ "inbound",
    TRUE ~ "other"
  ))

ggplot(brier_df_2oM, aes(x = brier, fill = event_type)) +
  geom_histogram(binwidth = 0.01, position = "stack", color = "black") +
  labs(
    title = "Brier Scores by Event Type (based on Prev2)",
    x = "Brier Score",
    y = "Count",
    fill = "Event Type"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")




  
  
### Cross validation for Scoring Prediction


# Real data
scoring_summary <- data %>%
  mutate(
    diff_group = floor(as.numeric(diff) / 5) * 5,
    made = ifelse(grepl(paste(scoring_patterns, collapse = "|"), event), 1, 0)
  ) %>%
  filter(grepl("2|3|free throw|turnover|foul", event)) %>%
  group_by(diff_group) %>%
  summarise(
    prob_obs = mean(made),
    sd_obs = sd(made),
    count_obs = n(),
    .groups = "drop"
  ) %>%
  filter(count_obs > 5)

# Simulated data
simulated_1oM_summary <- simulated.data.1oM %>%
  mutate(
    diff_group = floor(as.numeric(diff) / 5) * 5,
    made = ifelse(grepl(paste(scoring_patterns, collapse = "|"), event), 1, 0)
  ) %>%
  filter(grepl("2|3|free throw|turnover|foul", event)) %>%
  group_by(diff_group) %>%
  summarise(
    prob_sim = mean(made),
    sd_sim = sd(made),
    count_sim = n(),
    .groups = "drop"
  ) %>%
  filter(count_sim > 5)


comparison_real_1oM <- inner_join(
  scoring_summary,
  simulated_1oM_summary,
  by = "diff_group",
  suffix = c("_obs", "_sim")
) %>%
  mutate(
    residual = prob_sim - prob_obs  # positive = overestimation by model
  )


ggplot(comparison_real_1oM, aes(x = diff_group, y = residual)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals: Observed - First Order Scoring Probability",
    x = "Points Difference Group",
    y = "Residual (Simulated - Observed)"
  ) +
  theme_minimal()

simulated_2oM_summary <- simulated.data.2oM %>%
  mutate(
    diff_group = floor(as.numeric(diff) / 5) * 5,
    made = ifelse(grepl(paste(scoring_patterns, collapse = "|"), event), 1, 0)
  ) %>%
  filter(grepl("2|3|free throw|turnover|foul", event)) %>%
  group_by(diff_group) %>%
  summarise(
    prob_sim1 = mean(made),
    sd_sim1 = sd(made),
    count_sim1 = n(),
    .groups = "drop"
  ) %>%
  filter(count_sim1 > 5)

comparison_real_2oM <- inner_join(
  scoring_summary,
  simulated_2oM_summary,
  by = "diff_group",
  suffix = c("_obs", "_sim")
) %>%
  mutate(
    residual = prob_sim - prob_obs  # positive = overestimation by model
  )


ggplot(comparison_real_2oM, aes(x = diff_group, y = residual)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals: Observed - Second Order Scoring Probability",
    x = "Points Difference Group",
    y = "Residual (Simulated - Observed)"
  ) +
  theme_minimal()

print(head(comparison, 10))


comparison_1oM_2oM <- inner_join(
  simulated_1oM_summary,
  simulated_2oM_summary,
  by = "diff_group",
  suffix = c("_sim", "_sim1")
) %>%
  mutate(
    residual = prob_sim - prob_sim1  # positive = overestimation by model
  )


ggplot(comparison_1oM_2oM, aes(x = diff_group, y = residual)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals: First Order - Second Order Scoring Probability",
    x = "Points Difference Group",
    y = "Residual (Simulated - Observed)"
  ) +
  theme_minimal()

### Cross Validation mean Brier score

# K folds 1 order Markov

set.seed(123)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(data)))
brier_scores <- c()

for (fold in 1:k) {
  train <- data[folds != fold, ]
  test  <- data[folds == fold, ]
  
  # Get from-to pairs
  from_train <- train$event[-nrow(train)]
  to_train   <- train$event[-1]
  
  from_test <- test$event[-nrow(test)]
  to_test   <- test$event[-1]
  
  # Build transition probabilities from training data
  freq <- table(from_train, to_train)
  probs <- prop.table(freq, 1)  # row-wise
  
  # All possible outcomes
  classes <- colnames(probs)
  
  # Compute Brier score on test pairs
  for (i in 1:length(from_test)) {
    if (!(from_test[i] %in% rownames(probs))) next
    pred <- as.numeric(probs[from_test[i], ])
    actual <- as.numeric(classes == to_test[i])
    if (sum(actual) == 0) next
    brier_scores <- c(brier_scores, mean((pred - actual)^2))
  }
}

# Average Brier score across folds
mean_brier <- mean(brier_scores)
cat("Average Brier Score:", round(mean_brier, 4), "\n")


