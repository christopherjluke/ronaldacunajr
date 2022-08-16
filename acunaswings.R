library(baseballr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(stringr)
library(lubridate)
theme_set(theme_fivethirtyeight())

raID <- playerid_lookup(last_name = "Acuna", first_name = "Ronald")$mlbam_id
raID <- 660670L

ra22 <- statcast_search(start_date = "2022-04-07",
                        end_date = today(),
                        player_type = "batter",
                        playerid = raID)

lg22 <- statcast_search(start_date = "2022-04-07",
                        end_date = today(),
                        player_type = "batter")

# Scrape Ronald Acuna Jr. data function (Statcast Era), excluding 2022 because end date is today()

SCseasons <- data.frame(
  season = 2018:2021,
  start = c("2018-03-29", "2019-03-30", "2020-07-23", "2021-04-01"),
  end = c("2018-10-01", "2019-10-03", "2020-09-27", "2021-10-03"))

getdata <- function(fourDigYr){
  d <- statcast_search(start_date = SCseasons$start[SCseasons$season == fourDigYr],
                       end_date = SCseasons$end[SCseasons$season == fourDigYr],
                       player_type = "batter",
                       playerid = raID)
  return(d)
}

ra21 <- getdata(2021)
ra20 <- getdata(2020)
ra19 <- getdata(2019)
ra18 <- getdata(2018)

acuna <- ra22 %>% union(ra21) %>%
  union(ra20) %>% union(ra19) %>%
  union(ra18) %>%
  mutate(season = as.numeric(str_sub(game_date, start = 1, end = 4)))

# Scrape league data, only doing June-August due to Payload issues with Statcast.
# 2020 can be full season

SC_lg_seasons <- data.frame(
  season = 2018:2021,
  start = c("2018-06-01", "2019-06-01","2020-06-01", "2021-06-01"),
  end = c("2018-08-31", "2019-08-31", "2020-09-27", "2021-08-31"))

getdata_lg <- function(fourDigYr){
  d <- statcast_search(start_date = SC_lg_seasons$start[SC_lg_seasons$season == fourDigYr],
                       end_date = SC_lg_seasons$end[SC_lg_seasons$season == fourDigYr],
                       player_type = "batter",)
  return(d)
}

lg21 <- getdata_lg(2021)
lg20 <- getdata_lg(2020)
lg19 <- getdata_lg(2019)
lg18 <- getdata_lg(2018)

avg <- lg22 %>% union(lg21) %>%
  union (lg20) %>% union(lg19) %>%
  union(lg18) %>%
  mutate(season = as.numeric(str_sub(game_date, start = 1, end = 4)))

swingevents <- c("hit_into_play", "swinging_strike", "foul",
                 "foul_tip", "swinging_strike_blocked")
missevents <- c("swinging_strike", "swinging_strike_blocked")
foulevents <- c("foul", "foul_tip")
hitevents <- c("single", "double", "triple", "home_run")
xbhevents <- c("double", "triple", "home_run")

freqdf <- data.frame("season" = 2022,
                     "pitches" = 99,
                     "swings" = 9,
                     "misses" = 9,
                     "fouls" = 9,
                     "bip" = 9,
                     "hit" = 9,
                     "xbh" = 9)

d2 <- avg %>%
  mutate(isswing = ifelse(description %in% swingevents, 1, 0),
         ismiss = ifelse(description %in% missevents, 1, 0),
         isfoul = ifelse(description %in% foulevents, 1, 0),
         isbip = ifelse(description == "hit_into_play", 1, 0),
         ishit = ifelse(events %in% hitevents, 1, 0),
         isxbh = ifelse(events %in% xbhevents, 1, 0)) %>%
  group_by(season) %>%
  summarize(
    pitches = n(),
    swings = sum(isswing, na.rm=TRUE),
    misses = sum(ismiss, na.rm=TRUE),
    fouls = sum(isfoul, na.rm=TRUE),
    bip = sum(isbip, na.rm=TRUE),
    hit = sum(ishit, na.rm=TRUE),
    xbh = sum(isxbh, na.rm=TRUE))

freqdf <- freqdf %>% union(d2)
freqdf <- freqdf[-1,]

sumdf <- freqdf %>%
  mutate(season = as.numeric(str_sub(season, start = 1, end = 4)),
         swingrate = swings/pitches,
         missrate = misses/swings,
         foulrate = fouls/swings,
         biprate = bip/swings,
         hitrate = hit/swings,
         xbhrate = xbh/swings) %>%
  summarize(pitches = sum(pitches, na.rm=TRUE),
            swingrate = mean(swingrate, na.rm=TRUE),
            missrate = mean(missrate, na.rm=TRUE),
            foulrate = mean(foulrate, na.rm=TRUE),
            biprate = mean(biprate, na.rm=TRUE),
            hitrate = mean(hitrate, na.rm=TRUE),
            xbhrate = mean(xbhrate, na.rm=TRUE))
sumdf
seasondf <- freqdf %>%
  mutate(season = as.numeric(str_sub(season, start = 1, end = 4)),
         swingrate = swings/pitches,
         missrate = misses/swings,
         foulrate = fouls/swings,
         biprate = bip/swings,
         hitrate = hit/swings,
         xbhrate = xbh/swings) %>%
  group_by(season) %>%
  summarize(pitches = sum(pitches, na.rm=TRUE),
            swingrate = mean(swingrate, na.rm=TRUE),
            missrate = mean(missrate, na.rm=TRUE),
            foulrate = mean(foulrate, na.rm=TRUE),
            biprate = mean(biprate, na.rm=TRUE),
            hitrate = mean(hitrate, na.rm=TRUE),
            xbhrate = mean(xbhrate, na.rm=TRUE)) %>%
  arrange(desc(season))
seasondf

sample <- sumdf %>% mutate(season = NA) %>%
  union(seasondf)
sample

# Writing csv for future users
write.csv(sample, "baseball/data/sampleswingrates.csv")

league <- sample

acuna <- acuna %>%
  mutate(isswing = ifelse(description %in% swingevents, 1, 0),
         ismiss = ifelse(description %in% missevents, 1, 0),
         isfoul = ifelse(description %in% foulevents, 1, 0),
         isbip = ifelse(description == "hit_into_play", 1, 0),
         ishit = ifelse(events %in% hitevents, 1, 0),
         isxbh = ifelse(events %in% xbhevents, 1, 0))

# Swing rate by season

byseason <- acuna %>% group_by(season) %>%
  summarize(pitches = n(),
            swings = sum(isswing, na.rm=TRUE),
            misses = sum(ismiss, na.rm=TRUE),
            fouls = sum(isfoul, na.rm=TRUE),
            bip = sum(isbip, na.rm=TRUE),
            hit = sum(ishit, na.rm=TRUE),
            xbh = sum(isxbh, na.rm=TRUE)) %>%
  mutate(swingrate = swings/pitches,
         missrate = misses/swings,
         foulrate = fouls/swings,
         biprate = bip/swings,
         hitrate = hit/swings,
         xbhrate = xbh/swings) %>%
  arrange(desc(season))

byseason

cap1 = "Date Source: Baseball Savant"
sub1 = "Ronald Acuña Jr., Regular Season Games"

ggplot(byseason) +
  labs(title = "Swing Rate on All Pitches",
       subtitle = sub1,
       caption = cap1,
       x = "Season", y="Swings/Pitches")+
  geom_line(aes(x = season, y = swingrate), color = "#13274F") +
  geom_point(aes(x = season, y = swingrate), color = "#13274F") +
  geom_point(data = league, aes(x = season, y = swingrate), color="#CE1141") +
  geom_line(data = league, aes(x = season, y = swingrate), color = "#CE1141") +
  annotate("text", x = c(2018:2018), y = c(0.472, 0.455), label = c("MLB Average", "Acuña Jr"), color = c("#CE1141", "#13274F"))

ggplot(byseason) +
  labs(title = "Whiff Rate on All Swings",
       subtitle = sub1,
       caption = cap1,
       x = "Season", y = "Swinging Strikes/Pitches") +
  geom_line(aes(x = season, y = missrate), color="#13274F") +
  geom_point(aes(x = season, y = missrate), color="#13274F") +
  geom_point(data = league, aes(x = season, y = missrate), color = "#CE1141") +
  geom_line(data = league, aes(x = season, y = missrate), color = "#CE1141") +
  annotate("text", x = c(2018:2018), y = c(0.255, 0.225), label = c("Acuña Jr", "MLB Average"), color = c("#13274F", "#CE1141"))

ggplot(byseason) +
  labs(title = "Foul Rate on All Swings",
       subtitle = sub1,
       caption = cap1,
       x = "Season", y = "Foul Rate/Pitches") +
  geom_line(aes(x = season, y = foulrate), color="#13274F") +
  geom_point(aes(x = season, y = foulrate), color="#13274F") +
  geom_point(data = league, aes(x = season, y = foulrate), color = "#CE1141") +
  geom_line(data = league, aes(x = season, y = foulrate), color = "#CE1141") +
  annotate("text", x = c(2018:2018), y = c(0.398, 0.389), label = c("Acuña Jr", "MLB Average"), color = c("#13274F", "#CE1141"))
  
ggplot(byseason) +
  labs(title = "Ball in Play Rate on All Swings",
       subtitle = sub1,
       caption = cap1,
       x = "Season", y = "BIP Rate/Swings") +
  geom_line(aes(x = season, y = biprate), color="#13274F") +
  geom_point(aes(x = season, y = biprate), color="#13274F") +
  geom_point(data = league, aes(x = season, y = biprate), color = "#CE1141") +
  geom_line(data = league, aes(x = season, y = biprate), color = "#CE1141") +
  annotate("text", x = c(2018:2018), y = c(0.375, 0.336), label = c("MLB Average", "Acuña Jr"), color = c("#CE1141", "#13274F"))

ggplot(byseason) +
  labs(title = "Hit Rate on All Swings",
       subtitle = sub1,
       caption = cap1,
       x = "Season", y = "Hit Rate/Pitches") +
  geom_line(aes(x = season, y = hitrate), color="#13274F") +
  geom_point(aes(x = season, y = hitrate), color="#13274F") +
  geom_point(data = league, aes(x = season, y = hitrate), color = "#CE1141") +
  geom_line(data = league, aes(x = season, y = hitrate), color = "#CE1141") +
  annotate("text", x = c(2018:2018), y = c(0.137, 0.123), label = c("Acuña Jr", "MLB Average"), color = c("#13274F", "#CE1141"))

ggplot(byseason) +
  labs(title = "Extra Base Hit Rate on All Swings",
       subtitle = sub1,
       caption = cap1,
       x = "Season", y = "Extra Base Hit Rate/Swings") +
  geom_line(aes(x = season, y = xbhrate), color="#13274F") +
  geom_point(aes(x = season, y = xbhrate), color="#13274F") +
  geom_point(data = league, aes(x = season, y = xbhrate), color = "#CE1141") +
  geom_line(data = league, aes(x = season, y = xbhrate), color = "#CE1141") +
  annotate("text", x = c(2018:2018), y = c(0.059, 0.041), label = c("Acuña Jr", "MLB Average"), color = c("#13274F", "#CE1141"))
  
# Logistic Regression

logistic <- function(L) {
  return((tanh(L / 2) + 1) / 2)
}

acuna_ind <- 1:dim(acuna)[1]
te_ind <- sample(acuna_ind, size = length(acuna_ind) * .2, replace = F)
ra.test1 <- acuna[te_ind,]
ra.train1 <- acuna[-te_ind,]
m1 <- glm(factor(isswing) ~ factor(pitch_type), data = ra.train1,
          family = "binomial")

l_swing <- predict(m1, newdata = ra.test1)
p_swing <- logistic(l_swing)


ggplot(ra.test1) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(aes(x = factor(pitch_type), y = p_swing), color = "#13274F") +
  labs(title = "Probability of Swing by Pitch Type",
       subtitle = "All Pitches, Ronald Acuna Jr., 2018-2022",
       caption = "Data Source: Baseball Savant") +
  geom_smooth(aes(x = factor(pitch_type), y = p_swing), color = "#13274F")

table(acuna$pitch_type)
keep <- c("CH", "CU", "FC", "FF", "FS", "KC", "SI", "SL")  
tra <- acuna %>% filter(pitch_type %in% keep)

acuna_ind <- 1:dim(tra)[1]
te_ind <- sample(acuna_ind, size = round(length(acuna_ind)*0.2), replace = F)
ra.te <- tra[te_ind,]
ra.tr <- tra[-te_ind,]
table(factor(ra.te$pitch_type))
table(factor(ra.tr$pitch_type))
mean(ra.tr$isswing[ra.tr$pitch_type == "CH"])

m1.2 <- glm(factor(isswing) ~ factor(pitch_type), data = ra.tr, family="binomial")
L_swing <- predict(m1.2, newdata = ra.te)
P_swing <- logistic(L_swing)
anova(m1.2, test = "Chisq")

P_pitchtype = logistic(m1.2$coefficients)
b = m1.2$coefficients[1]
P_pitchtype[2:8] = logistic(b + m1.2$coefficients[2:8])


p2 <- ggplot(ra.te) +
  coord_cartesian(ylim = c(0,1)) +
  geom_point(aes(x = factor(pitch_type), y = P_swing)) +
  labs(title = "Probability of Swing by Pitch Type",
       subtitle = "All Pitches, Ronald Acuna Jr., 2018-2022",
       caption = "Data Source: Baseball Savant")

p2 + geom_smooth(aes(x = factor(pitch_type), y=P_swing))


