library(tidyverse)
library(lubridate) # time
library(sf) # maps
library(broom)

# Note: I didn't have to filter out Alaska since I'm working with state data.

# Getting/Cleaning Data -----------------
unemployment <- read_rds("data/unemployment_rate_by_county.rds")
pres_election <- read_rds("data/election_data_president_2012_2020.rds")

# This is a shapefile rds from lecture 22
shp <- read_rds("data/states.rds")

# This is a small file I made to convert census state codes into state abb.
states <- read_csv("data/state_codes.csv") 
week_17_data <- read_csv("data/pulse2020_puf_17.csv") # this may take a while.
week_16_data <- read_csv("data/pulse2020_puf_16.csv") # this may take a while.

hps_data <- rbind(week_17_data, week_16_data) # this *really* may take a while.

# There are a lot of variables in this, with 88,716 obs.
# Data was collected during the month prior to the election (Sep 30 - Oct 26).

# Important columns to note that we'll use later:
# SCRAM - ID
# ANYWORK - Do you have work/job/etc.?
# UI_APPLY - Applied for unemployment benefits. We won't be using this.
# UI_RECV - Received unemployment benefits 
#           (1 for yes, 2 for no, -88/-99 is no answer)
# EST_ST - State of residence of survey taker

# There is definitely a lot more you can do with this data, but
# this is one of the only sources I can get.

# Of course, if this isn't enough for some states, I can get more.

# Filter data to only get stimulus responses
# ueb = unemployment benefits received

stimulus_data <- hps_data %>% 
  mutate(code = EST_ST,
         id = SCRAM, 
         # ueb_applied = UI_APPLY, 
         ueb_received = UI_RECV) %>% 
  select(code, id, ueb_received)

stimulus_data <- stimulus_data %>% mutate(ueb_received = ifelse(ueb_received == 1, 1, 0))

# Group by/Summarize by state
# For unemployment, I have to organize by state in order for it to be consistent
# The mean is between 1 and 2, but that will give the average of those who
# DID receive unemployment checks.
stim_by_state <- stimulus_data %>% group_by(code) %>% 
  summarize(num_responses = n(), 
            pct_stimulus_recv = mean(ueb_received))

# Grouping by state to create new u/e rate for state
# Funnily enough, the first two digits of a fips code corresponds to a state code.
ue_by_state <- unemployment %>% mutate(code = floor(as.numeric(fips) / 1000)) %>%
  group_by(code, period) %>% 
  summarize(civ_labor_force = sum(civilian_labor_force), 
            employed = sum(employed)) %>%
  mutate(unemployment_rate = 100 * (civ_labor_force - employed)/civ_labor_force)

# Presidential Election filtering: filter for 2020, group into state vote
pres_by_state <- pres_election %>% filter(year==2020) %>%
  group_by(state_code) %>%
  summarize(total_vote = sum(total_vote),
            dem_votes = sum(dem_votes),
            rep_votes = sum(rep_votes),
            ind_votes = sum(ind_votes)) %>% 
  mutate(states = states$state)

# Making codes integers so we can join objects
shp <- shp %>% mutate(code = as.numeric(fips))
states <- states %>% mutate(state_code = as.numeric(state_code))

# Just a basic state graph with unemployment to gauge preliminary results
ue_by_state %>% filter(period=="Apr-20") %>% 
  left_join(shp, by="code") %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill=unemployment_rate), col="gray95", lwd=.1) +
  scale_fill_distiller(palette="Reds", 
                       direction=1) +
  theme_void()

# Pre-pandemic to mid-pandemic (and close to election) difference to U/E
ue_feb <- ue_by_state %>% filter(period == "Feb-20")
ue_apr <- ue_by_state %>% filter(period == "Apr-20")
ue_oct <- ue_by_state %>% filter(period == "Oct-20")

states <- states %>% mutate(ue_rate.feb = ue_feb$unemployment_rate,
                            ue_rate.apr = ue_apr$unemployment_rate,
                            ue_rate.oct = ue_oct$unemployment_rate)

# Assuming Feb is where unemployment was normal...

states <- states %>% mutate(ue_diff.apr = ue_rate.apr - ue_rate.feb,
                            ue_diff.oct = ue_rate.oct - ue_rate.feb,
                            stim_received = 100*stim_by_state$pct_stimulus_recv,
                            dem_pct = 100*pres_by_state$dem_votes / pres_by_state$total_vote)

pres_election <- pres_election %>% filter(!state == "AK" | !state == "DC") %>% 
  filter(year == 2020) %>% filter(!fips %in% ue_by_county)

ue_by_county <- unemployment %>% 
  mutate(code = floor(as.numeric(fips) / 1000)) %>%
  filter(!code == 2) %>%
  filter(period == "Feb-20" | period == "Apr-20") %>%
  group_by(county, fips = as.numeric(fips)) %>% 
  summarize(ue_diff = unemployment_rate[period == "Apr-20"] - 
              unemployment_rate[period == "Feb-20"]) #%>%
 # mutate(dem_pct = 100 * pres_election$dem_votes / pres_election$total_vote)

# Regression & Plots --------------------------------------------

m1 <- lm(dem_pct ~ stim_received + ue_diff.apr + ue_diff.oct, data=states)
summary(m1)

m2 <- lm(dem_pct ~ stim_received, data=states)
summary(m2)

tidy(m1, conf.int = T, conf.level=.99)

# Confidence intervals don't seem quite right. While the best regression is closer
# to October, it really doesn't seem like the regression is fitting that well.

ggplot(states, aes(x=stim_received, y=dem_pct, label=state)) +
  geom_point(aes(color=dem_pct > 50)) +
  geom_smooth(method="lm", se=TRUE, color="darkgreen") + 
  geom_hline(yintercept=50) +
  geom_text_repel(aes(label=state), max.overlaps = 5) +
  labs(x = "Percentage of receiving stimulus checks or unemployment benefits",
       y = "Democratic vote percentage per state(2020)",
       title = "Stimulus Check/Unemployment Benefits' Influence on the 2020 US Election") + 
  scale_color_manual(values = c("red", "blue"), guide=F) + 
  scale_x_continuous(limits = c(5, 25)) +
  theme_grey()

# This one happens to actually have a really cool trend. While not everything
# falls on the 95% interval, it does seem like those who did receive stimulus
# checks were more likely to be in a Democratic-voting state.
  
# It's also worth noting GA, WI, ME, PA, AZ, etc. all lay below 50% in the popular vote, 
# despite all of them leaning towards Biden.

ggplot(states, aes(x=ue_diff.apr, y=dem_pct, label=state)) +
  geom_smooth(method="lm", se=TRUE) + 
  geom_point(aes(color=dem_pct > 50)) +
  geom_hline(yintercept=50) +
  geom_text_repel(aes(label=state), max.overlaps = 3) + 
  scale_color_manual(values = c("red", "blue"), guide=F) +
  scale_x_continuous(limits = c(0, 30)) + 
  labs(y="Democratic Percentage (2020)", 
       x="Difference in Unemployment Rate of Apr 2020 from Feb 2020",
       title="Difference in Unemployment Rate Affecting the US Election") +
  theme_minimal()

ggplot(states, aes(x=ue_diff.oct, y=dem_pct, label=state)) +
  geom_smooth(method="lm", se=TRUE) + 
  geom_point(aes(color=dem_pct > 50)) +
  geom_hline(yintercept=50) +
  geom_text_repel(aes(label=state), max.overlaps = 3) +
  labs(y="Democratic Percentage (2020)", 
       x="Difference in Unemployment Rate of Oct 2020 from Feb 2020",
       title="Difference in Unemployment Rate Affecting the US Election") + 
  scale_color_manual(values = c("red", "blue"), guide=F) +
  theme_minimal()
  

# For these two, we definitely can't really reject a null hypothesis here. 
# It doesn't really seem economic factors really affect the vote very much;
# unemployment rate stays the same. That crazy point to the right is Hawaii,
# which kind of makes sense.

m3 <- lm(stim_received ~ ue_diff.apr, data=states)
summary(m3)

ggplot(states, aes(x=ue_diff.apr, y=stim_received)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE) + 
  geom_text_repel(aes(label=state), max.overlaps = 3) +
  labs(x = "Unemployment Rate Difference (from April 2020)",
       y = "Percentage of Stimulus Checks/Unemployment Insurance received",
       title = "Unemployment Rate Affecting Stimulus Check Received") +
  scale_y_continuous(limits = c(0, 25))+
  theme_grey()

# This has a very similar correlation to the above. Again, not much we can do.

# After all, we are passing in state data. Just as an experiment, let's what
# happens if we look specifically by county instead.

unemployment <- unemployment %>% 
  mutate(dem_pct = 100*pres_election$dem_votes / pres_by_state$total_vote)

# Maps & Plots ----------------------------------------------------------

shp <- shp %>% mutate(state_code = as.numeric(fips))

states %>%
  left_join(shp, by="state_code") %>% 
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill=stim_received), col="gray95", lwd=.1) +
  scale_fill_distiller(palette="Purples", 
                       direction=1, 
                       name="Percentage of Workers Receiving\nUnemployment Benefits",
                       limits=c(5, 25)) +
  theme_void(base_family="Arial Narrow") +
  theme(plot.background = element_rect(fill="steelblue4"),
        legend.text=element_text(size=5, color="gray95"),
        legend.title = element_text(size=8, color="gray95", 
                                    margin=margin(0,0,10,0)),
        legend.position=c(1,0),
        legend.direction = "horizontal",
        legend.justification = c(1,0),
        legend.key.height = unit(.25, "cm"),
        legend.margin = margin(0, 5, 5, 0))

# barebones election map
states %>% left_join(shp, by="state_code") %>% st_as_sf %>%
  ggplot() + 
  geom_sf(aes(fill=dem_pct), col="black", lwd=.05)+
  scale_fill_distiller(palette="RdBu",
                       direction=1,
                       name="Percentage of the Popular Vote for President in 2020",
                       limits=c(25, 75)) +
  theme_void(base_family="serif") +
  theme(plot.background = element_rect(fill="#a5b8e5"),
        legend.text=element_text(size=5, color="black"),
        legend.title = element_text(size=8, color="black", 
                                    margin=margin(0,0,10,0)),
        legend.position=c(1,0),
        legend.direction = "horizontal",
        legend.justification = c(1,0),
        legend.key.height = unit(.25, "cm"),
        legend.margin = margin(0, 5, 5, 0))


