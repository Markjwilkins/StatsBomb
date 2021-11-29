library(tidyverse)
library (StatsBombR)

# Save free StatsBomb 360 data as shown here:
# https://statsbomb.com/2021/11/statsbomb-announce-the-release-of-free-statsbomb-360-data-euro-2020-available-now/

## Import SB Data ## -------------

Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)
Matches = Matches %>% filter(competition.competition_name == "UEFA Euro")
data360 <- StatsBombFree360Events(MatchesDF = Matches, Parallel = T)

events <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)
events <- allclean(events)
events <- get.opposingteam(events)

data360 = data360 %>% rename(id = event_uuid)

events = events %>% left_join(data360, by = c("id" = "id"))

events = events %>% rename(match_id = match_id.x) %>% select(-match_id.y)

data <-
  events %>%
  select(
    id,
    match_id,
    team.name,
    type.name,
    OpposingTeam,
    player.name,
    type.name,
    minute,
    second,
    pass.outcome.name,
    pass.shot_assist,
    location.x,
    location.y,
    pass.end_location.x,
    pass.end_location.y,
    pass.type.name,
    pass.cross,
    freeze_frame
  )


data <-
  data %>% unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)),
         ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)),
         ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))

##save as .RDS file
saveRDS(data, "statsbomb_euros_2020_360.RDS")
