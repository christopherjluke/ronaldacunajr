library(baseballr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(boot)
library(ggthemes)
theme_set(theme_fivethirtyeight())

raID <- playerid_lookup(last_name = "Acuna", first_name = "Ronald")$mlbam_id

ra21 <- statcast_search(start_date = "2021-04-01",
                        end_date = "2021-07-31",
                        player_type = "batter",
                        playerid = raID)

ra22 <- statcast_search(start_date = "2022-04-07",
                       end_date = today(),
                       player_type = "batter",
                       playerid = raID)

av22 <- statcast_search(start_date = "2022-04-07",
                        end_date = today(),
                        player_type = "batter")

hitevents <- c("single", "double", "triple", "home_run")

out_events <- c("field_out", "fielders_choice_out", "force_out",
                "grounded_into_double_player", "double_player")

unique(ra22$events)

ra21 <- ra21 %>%
  mutate(
    paID = paste0(game_date,"_", at_bat_number),
    ball = ifelse(type == "B", 1, 0),
    strike = ifelse(type == "S", 1, 0),
    callstrike = ifelse(description == "called_strike", 1, 0),
    swingstrike = ifelse(description == "swinging_strike", 1, 0),
    BB = ifelse(events == "walk", 1, 0),
    SO = ifelse(events == "strikeout", 1, 0),
    BIP = ifelse(description == "hit_into_play", 1, 0),
    hit = ifelse(events %in% hitevents, 1, 0),
    single = ifelse(events == "single", 1, 0),
    double = ifelse(events == "double", 1, 0),
    triple = ifelse(events == "triple", 1, 0),
    HR = ifelse(events == "home_run", 1, 0)
  )

ra22 <- ra22 %>%
  mutate(
    paID = paste0(game_date,"_", at_bat_number),
    ball = ifelse(type == "B", 1, 0),
    strike = ifelse(type == "S", 1, 0),
    callstrike = ifelse(description == "called_strike", 1, 0),
    swingstrike = ifelse(description == "swinging_strike", 1, 0),
    BB = ifelse(events == "walk", 1, 0),
    SO = ifelse(events == "strikeout", 1, 0),
    BIP = ifelse(description == "hit_into_play", 1, 0),
    hit = ifelse(events %in% hitevents, 1, 0),
    single = ifelse(events == "single", 1, 0),
    double = ifelse(events == "double", 1, 0),
    triple = ifelse(events == "triple", 1, 0),
    HR = ifelse(events == "home_run", 1, 0)
  )

# Transpose data frames
t(
  ra21 %>%
    summarize(
      pitches = n(),
      PA = n_distinct(paID),
      balls = sum(ball, na.rm = T),
      strikes = sum(strike, na.rm = T),
      callstrikes = sum(callstrike, na.rm = T),
      swingstrikes = sum(swingstrike, na.rm = T),
      BB = sum(BB, na.rm = T),
      SO = sum(SO, na.rm = T),
      BIP = sum(BIP, na.rm = T),
      hits = sum(hit, na.rm = T),
      singles = sum(single, na.rm = T),
      doubles = sum(double, na.rm = T),
      triples = sum(triple, na.rm = T),
      HR = sum(HR, na.rm = T)
    ) %>%
    mutate(
      BABIP = hits/BIP,
      bbrate = BB/PA,
      sorate = SO/PA
    ))

t(
  ra22 %>%
    summarize(
      pitches = n(),
      PA = n_distinct(paID),
      balls = sum(ball, na.rm = T),
      strikes = sum(strike, na.rm = T),
      callstrikes = sum(callstrike, na.rm = T),
      swingstrikes = sum(swingstrike, na.rm = T),
      BB = sum(BB, na.rm = T),
      SO = sum(SO, na.rm = T),
      BIP = sum(BIP, na.rm = T),
      hits = sum(hit, na.rm = T),
      singles = sum(single, na.rm = T),
      doubles = sum(double, na.rm = T),
      triples = sum(triple, na.rm = T),
      HR = sum(HR, na.rm = T)
    ) %>%
    mutate(
      BABIP = hits/BIP,
      bbrate = BB/PA,
      sorate = SO/PA
    ))

ra1 <- ra22 %>% filter(description == "hit_into_play") %>%
  summarize(avgLaunchAngle = mean(launch_angle, na.rm = T),
            avLaunchSpeed = mean(launch_speed, na.rm = T)) %>%
  mutate(across(1:2, ~round(.x, digits = 1)))

ra2 <- ra21 %>% filter(description == "hit_into_play") %>%
  summarize(avgLaunchAngle = mean(launch_angle, na.rm = T),
            avLaunchSpeed = mean(launch_speed, na.rm = T)) %>%
  mutate(across(1:2, ~round(.x, digits = 1)))


av1 <- av22 %>% filter(description == "hit_into_play") %>%
  summarize(avgLaunchAngle = mean(launch_angle, na.rm = T),
            avgLaunchSpeed = mean(launch_speed, na.rm = T)) %>%
  mutate(across(1:2, ~round(.x, digits = 1)))

dist_exitvelo <- ggplot(av22) +
  geom_histogram(aes(launch_speed), color = "#CE1141", fill="#13274F", binwidth = 10) +
  scale_x_continuous("Launch Speed") +
  scale_y_continuous("Batted Ball Count") +
  ggtitle("Batted Ball Distribution") +
  geom_vline(xintercept = c(av1$avgLaunchSpeed, ra1$avLaunchSpeed),
             color = c("#CE1141", "#EAAA00"), linetype = "dashed", size =1)

dist_exitvelo + theme(axis.title = element_text())

tav <- av22 %>% select(launch_angle, launch_speed) %>%
  mutate(player = "MLB Average")
tra <- ra22 %>% select(launch_angle, launch_speed) %>%
  mutate(player = "Ronald Acuna Jr.")

mean(av22$launch_speed, na.rm = TRUE)
mean(ra22$launch_speed, na.rm = TRUE)

ggplot(tav) +
  geom_boxplot(aes(launch_speed, factor(player)), fill = "#CE1141")
ggplot(tra) +
  geom_boxplot(aes(launch_speed, factor(player)), fill = "#CE1141")

ra21 %>% filter(events %in% hitevents) %>%
  group_by(events) %>%
  summarize(avgLaunchAngle = mean(launch_angle, na.rm = TRUE),
            avgLaunchSpeed = mean(launch_speed, na.rm = TRUE))

ra22 %>% filter(events %in% hitevents) %>%
  group_by(events) %>%
  summarize(avgLaunchAngle = mean(launch_angle, na.rm = TRUE),
            avgLaunchSpeed = mean(launch_speed, na.rm = TRUE))

bygame21 <- ra21 %>%
  group_by(game_date) %>%
  summarize(
    pitches = n(),
    PA = n_distinct(paID),
    balls = sum(ball, na.rm = TRUE),
    strikes = sum(strike, na.rm = TRUE),
    callstrikes = sum(callstrike, na.rm = TRUE),
    swingstrikes = sum(swingstrike, na.rm = TRUE),
    BB = sum(BB, na.rm = TRUE),
    SO = sum(SO, na.rm = TRUE),
    BIP = sum(BIP, na.rm = TRUE),
    hits = sum(hit, na.rm = TRUE),
    singles = sum(single, na.rm = TRUE),
    doubles = sum(double, na.rm = TRUE),
    triples = sum(triple, na.rm = TRUE),
    HR = sum(HR, na.rm = TRUE)
  ) %>%
  mutate(
    game_seq = seq_along(unique(ra21$game_date)),
    cumpitches = cumsum(pitches),
    cumpa = cumsum(PA),
    cumbb = cumsum(BB),
    cumso = cumsum(SO),
    cumbip = cumsum(BIP),
    cumhit = cumsum(hits)
  )

bygame <- ra22 %>%
  group_by(game_date) %>%
  summarize(
    pitches = n(),
    PA = n_distinct(paID),
    balls = sum(ball, na.rm = TRUE),
    strikes = sum(strike, na.rm = TRUE),
    callstrikes = sum(callstrike, na.rm = TRUE),
    swingstrikes = sum(swingstrike, na.rm = TRUE),
    BB = sum(BB, na.rm = TRUE),
    SO = sum(SO, na.rm = TRUE),
    BIP = sum(BIP, na.rm = TRUE),
    hits = sum(hit, na.rm = TRUE),
    singles = sum(single, na.rm = TRUE),
    doubles = sum(double, na.rm = TRUE),
    triples = sum(triple, na.rm = TRUE),
    HR = sum(HR, na.rm = TRUE)
  ) %>%
  mutate(
    game_seq = seq_along(unique(ra22$game_date)),
    cumpitches = cumsum(pitches),
    cumpa = cumsum(PA),
    cumbb = cumsum(BB),
    cumso = cumsum(SO),
    cumbip = cumsum(BIP),
    cumhit = cumsum(hits)
  )

bygamebip21 <- ra21 %>%
  filter(type == "X") %>%
  mutate(
    bipover93 = ifelse(launch_speed > 93, 1, 0),
    bipover96 = ifelse(launch_speed > 96, 1, 0),
    bipover99 = ifelse(launch_speed > 99, 1, 0),
    gbover96 = ifelse(launch_speed > 96 & launch_angle < 0, 1, 0),
    gbover99 = ifelse(launch_speed > 99 & launch_angle < 0, 1, 0)) %>%
  group_by(game_date) %>%
  summarize(
    avgla = mean(launch_angle, na.rm = TRUE),
    minla = min(launch_angle, na.rm = TRUE),
    maxla = max(launch_angle, na.rm = TRUE),
    avgev = mean(launch_speed, na.rm = TRUE),
    minev = min(launch_speed, na.rm = TRUE),
    maxev = max(launch_speed, na.rm = TRUE),
    bipover93 = sum(bipover93, na.rm = TRUE),
    bipover96 = sum(bipover96, na.rm = TRUE),
    bipover99 = sum(bipover99, na.rm = TRUE),
    gbover96 = sum(gbover96, na.rm = TRUE),
    gbover99 = sum(gbover99, na.rm = TRUE)
  )

bygamebip <- ra22 %>%
  filter(type == "X") %>%
  mutate(
    bipover93 = ifelse(launch_speed > 93, 1, 0),
    bipover96 = ifelse(launch_speed > 96, 1, 0),
    bipover99 = ifelse(launch_speed > 99, 1, 0),
    gbover96 = ifelse(launch_speed > 96 & launch_angle < 0, 1, 0),
    gbover99 = ifelse(launch_speed > 99 & launch_angle < 0, 1, 0)) %>%
      group_by(game_date) %>%
      summarize(
        avgla = mean(launch_angle, na.rm = TRUE),
        minla = min(launch_angle, na.rm = TRUE),
        maxla = max(launch_angle, na.rm = TRUE),
        avgev = mean(launch_speed, na.rm = TRUE),
        minev = min(launch_speed, na.rm = TRUE),
        maxev = max(launch_speed, na.rm = TRUE),
        bipover93 = sum(bipover93, na.rm = TRUE),
        bipover96 = sum(bipover96, na.rm = TRUE),
        bipover99 = sum(bipover99, na.rm = TRUE),
        gbover96 = sum(gbover96, na.rm = TRUE),
        gbover99 = sum(gbover99, na.rm = TRUE)
      )

bygame21 <- bygame21 %>% left_join(bygamebip21, by = "game_date")
p2 <- ggplot(ra21 %>% filter(type == "X"))
cap2 <- "Source: Baseball Savant"
subt2 <- "Ronald Acuña Jr Balls in Play, 2021 Season"

bygame <- bygame %>% left_join(bygamebip, by = "game_date")
p1 <- ggplot(ra22 %>% filter(type == "X"))
  cap1 <- "Source: Baseball Savant"
  subt1 <- "Ronald Acuña Jr Balls in Play, 2022 Season"
  
v <- c("#13274F", "#CE1141")
names(v) <- c("0", "1")

v2 <- c("#CE1141", "#13274F")
names(v2) <- c("0", "1")
  
p1 + geom_point(aes(x = launch_angle, y = launch_speed, color = factor(hit))) +
  scale_color_manual(values = v) +
  labs(title = "Launch Speed by Launch Angle",
       subtitle = subt1, caption = cap1)

p1 + geom_boxplot(aes(x = launch_angle, y = factor(hit), color = factor(hit), fill=factor(hit)),
                  outlier.size = 3, size = 1) +
  scale_color_manual(values = v) +
  scale_fill_manual(values = v2) +
  labs(title = "Launch Angle", subtitle = subt1, caption = cap1)

p1 + geom_boxplot(aes(x = launch_speed, y=factor(hit), color = factor(hit), fill = factor(hit)),
                  outlier.size = 3, size = 1) +
  scale_color_manual(values = v) +
  scale_fill_manual(values = v2) +
  labs(title = "Launch Speed",
       subtitle = subt1, caption = cap1)

p1 + geom_histogram(aes(x=launch_angle), binwidth = 10,
                    color = "#CE1141", fill = "#13274F") +
  labs(title = "Launch Angle",
       subtitle = subt1, caption = cap1)

p2 + geom_histogram(aes(x=launch_angle), binwidth = 10,
                    color = "#CE1141", fill = "#13274F") +
  labs(title = "Launch Angle",
       subtitle = subt2, caption = cap2)

p1 + geom_histogram(aes(x=launch_speed), binwidth = 10,
                    fill = "#CE1141") +
  labs(title = "Launch Speed",
       subtitle = subt1, caption = cap1)

p3 <- ggplot(bygame)

p3 <- p3 + geom_line(aes(x = game_seq, y= cumpa), color = "#13274F") +
  geom_line(aes(x = game_seq, y = cumbip), color = "#CE1141") +
  labs(title = "Balls in Play by Game") +
  scale_x_continuous("Games") +
  scale_y_continuous("Plate Apperances")

p3 + theme(axis.title = element_text())

nGames = dim(bygame21)[1]
ra21 %>%
  summarize(
    PA = n_distinct(paID),
    BIP = sum(BIP, na.rm=TRUE),
    hits = sum(hit, na.rm=TRUE)
  ) %>%
  mutate(
    PApergame = PA/nGames,
    BIPpergame = BIP/nGames,
    Hpergame = hits/nGames,
    Pofbip = BIP/PA,
    Pofh = hits/PA
  )

nGames = dim(bygame)[1]
ra22 %>%
  summarize(
    PA = n_distinct(paID),
    BIP = sum(BIP, na.rm=TRUE),
    hits = sum(hit, na.rm=TRUE)
  ) %>%
  mutate(
    PApergame = PA/nGames,
    BIPpergame = BIP/nGames,
    Hpergame = hits/nGames,
    Pofbip = BIP/PA,
    Pofh = hits/PA
  )

bypitchtype21 <- ra21 %>% group_by(pitch_name) %>% summarize(totalSeen = n())

byptbip21 <- ra21 %>% filter (type == "X") %>%
  group_by(pitch_name) %>%
  summarize(totalBIP = n(),
            avgLaunchSpeed = mean(launch_speed, na.rm=TRUE),
            minLaunchSpeed = min(launch_speed, na.rm=TRUE),
            maxLaunchSpeed = max(launch_speed, na.rm =TRUE),
            avgLaunchAngle = mean(launch_angle, na.rm=TRUE),
            minLaunchAngle = min(launch_angle, na.rm=TRUE),
            maxLaunchAngle = max(launch_angle, na.rm=TRUE))

bypitchtype21 %>% left_join(byptbip21, by = "pitch_name")

bypitchtype <- ra22 %>% group_by(pitch_name) %>% summarize(totalSeen = n())

byptbip <- ra22 %>% filter (type == "X") %>%
  group_by(pitch_name) %>%
  summarize(totalBIP = n(),
            avgLaunchSpeed = mean(launch_speed, na.rm=TRUE),
            minLaunchSpeed = min(launch_speed, na.rm=TRUE),
            maxLaunchSpeed = max(launch_speed, na.rm =TRUE),
            avgLaunchAngle = mean(launch_angle, na.rm=TRUE),
            minLaunchAngle = min(launch_angle, na.rm=TRUE),
            maxLaunchAngle = max(launch_angle, na.rm=TRUE))

bypitchtype %>% left_join(byptbip, by = "pitch_name")

# Tally of hit events by pitch type 2021
ra21 %>% filter(events %in% hitevents) %>%
  group_by(pitch_name) %>% tally()

ra21 %>% filter(description == "called_strike")

ra21 <- ra21 %>% mutate(szRegion = case_when(
  zone %in% c(1:3) ~ "top",
  zone %in% c(4:6) ~ "mid",
  zone %in% c(7:9) ~ "bottom",
  zone %in% c(11:14) ~ "out",
  TRUE ~ "unk"
))

ra21 %>% select(szRegion)

byreg <- ra21 %>% group_by(szRegion) %>% summarize(totalSeen = n())

# Tally of hit events by pitch type
ra22 %>% filter(events %in% hitevents) %>%
  group_by(pitch_name) %>% tally()

ra22 %>% filter(description == "called_strike")

ra22 <- ra22 %>% mutate(szRegion = case_when(
  zone %in% c(1:3) ~ "top",
  zone %in% c(4:6) ~ "mid",
  zone %in% c(7:9) ~ "bottom",
  zone %in% c(11:14) ~ "out",
  TRUE ~ "unk"
))

ra22 %>% select(szRegion)

byreg <- ra22 %>% group_by(szRegion) %>% summarize(totalSeen = n())

ra21%>% filter(type == "X") %>% group_by(szRegion) %>%
  summarize(totalBIP = n(),
            avgLA = mean(launch_angle, na.rm=TRUE),
            avgEV = mean(launch_speed, na.rm=TRUE))

ra22 %>% filter(type == "X") %>% group_by(szRegion) %>%
  summarize(totalBIP = n(),
            avgLA = mean(launch_angle, na.rm=TRUE),
            avgEV = mean(launch_speed, na.rm=TRUE))

szt <- mean(ra22$sz_top, na.rm=TRUE)
szb <- mean(ra22$sz_bot, na.rm=TRUE)
szr <- 19.94/24
szl <- -19.94/24
home_plate <- data.frame("x" = c(-17/24, 17/24, 17/24, 0, -17/24),
                         "y" = c(0, 0, -0.2, -0.4, -0.2))
edge <- 1/12
ra22 <- ra22 %>%
  mutate(
    szoutside = ifelse(plate_x < (szl - edge), 1, 0),
    szinside = ifelse(plate_x > (szr + edge), 1, 0),
    szlow = ifelse(plate_z < (szb - edge), 1, 0),
    szhigh = ifelse(plate_z > (szt + edge), 1, 0)
  ) %>%
  mutate(
    szozone = ifelse(szoutside + szinside + szhigh + szlow > 0, 1, 0)
  ) %>%
  mutate(
    szbadcall = ifelse(szozone + callstrike > 1, 1, 0)
  )

cv1 <- c("#13274F", "#CE1141")
names(cv1) <- c("0", "1")

ggplot(data = ra22 %>% filter(description == "called_strike")) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-1, 7)) +
  geom_vline(xintercept = c(szl, szr), color = "#EAAA00", size=0.5, linetype = 2) +
  geom_hline(yintercept = c(szt, szb), color = "#EAAA00", size=0.5, linetype = 2) +
  annotate("rect", xmin = szl, xmax = szr, ymin = szb, ymax = szt, fill = "#13274F", alpha=0.3) +
  annotate("text", x = -3.9, y = -0.2, label = "Catcher's Perspective", color = "#13274F", hjust = 0) +
  annotate("rect", xmin = -5, xmax = 5, ymin = -2, ymax = 0, fill = "#CE1141", alpha=0.1) +
  annotate("polygon", x = home_plate$x, y = home_plate$y, fill = "#13274F", alpha=0.3) +
  labs(title = "Locations of Called Strikes",
       subtitle = "Ronald Acuna Jr., 2022 Season",
       x = "Horizontal Location (ft)",
       y = "Vertical Location (ft)") +
  geom_point(aes(x = plate_x, y = plate_z, color = factor(szbadcall)), shape = 16) +
  scale_color_manual(values = cv1)

badcallpaid <- ra22 %>% filter(szbadcall == 1) %>% pull(paID)
badcallpa <- ra22 %>% filter(paID %in% badcallpaid)
badcallpa %>% arrange(paID, pitch_number) %>%
  select(paID, pitch_number, description, szbadcall, szlow, szoutside, events) %>%
  print(n = 140)
