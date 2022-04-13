

## From Packet 6: If two CI's overlap, the difference can still be significant

num_iver <- 679
event_iver <- 84
num_iplacebo <- 679
event_iplacebo <- 111

iver_study <- matrix(c(event_iver, num_iver - event_iver, 
                       event_iplacebo, num_iplacebo - event_iplacebo),
                     ncol=2, byrow=TRUE)
colnames(iver_study) <- c("event", "no event")
rownames(iver_study) <- c("ivermectin", "placebo")
iver_study <- as.table(iver_study)

iver_study %>% prop.test()

t(iver_study["ivermectin",]) %>% prop.test()
t(iver_study["placebo",]) %>% prop.test()

### Ivermectin Study

num_iver <- 679
event_iver <- 100
num_iplacebo <- 679
event_iplacebo <- 111

iver_study <- matrix(c(event_iver, num_iver - event_iver, 
                       event_iplacebo, num_iplacebo - event_iplacebo),
                     ncol=2, byrow=TRUE)
colnames(iver_study) <- c("event", "no event")
rownames(iver_study) <- c("ivermectin", "placebo")
iver_study <- as.table(iver_study)

iver_study %>% addmargins()
iver_study %>% prop.table(1) %>% addmargins(2)

iver_props <- iver_study %>% prop.table(1)

iver_diff_prop <- 
  iver_props["ivermectin","event"] - iver_props["placebo","event"]

iver_study %>% prop.test(alternative = "two.sided", correct = F)

iver_groups <- c(rep("ivermectin", num_iver), 
                      rep("placebo", num_iplacebo))
iver_results <- c(rep("event", event_iver),
                  rep("no event", num_iver - event_iver),
                  rep("event", event_iplacebo),
                  rep("no event", num_iplacebo - event_iplacebo))
iver_data <- data.frame("groups" = iver_groups,
                        "results" = iver_results)

iver_data %>% table() %>% addmargins()

iver_data_shuffled <- iver_data %>% 
  mutate(results = sample(results))

iver_data_shuffled %>% table() %>% addmargins()
iver_data_shuffled %>% table() %>% prop.table(1)

test <- iver_data_shuffled %>% table() %>% prop.table(1)
test["ivermectin","event"] - test["placebo","event"]

### A randomization test

num_iver <- 679
event_iver <- 100
num_iplacebo <- 679
event_iplacebo <- 111
simulations <- 10000


iver_groups <- c(rep("ivermectin", num_iver), 
                 rep("placebo", num_iplacebo))
iver_results <- c(rep("event", event_iver),
                  rep("no event", num_iver - event_iver),
                  rep("event", event_iplacebo),
                  rep("no event", num_iplacebo - event_iplacebo))
iver_data <- data.frame("groups" = iver_groups,
                        "results" = iver_results)

iver_props <- iver_data %>% table() %>% prop.table(1)

iver_diff_prop <- 
  iver_props["ivermectin","event"] - iver_props["placebo","event"]

sim_diff_prop <- numeric(simulations)

for(i in 1:simulations){
  sim_data_shuffle <- iver_data %>% 
    mutate(results = sample(results))
  sim_props <- sim_data_shuffle %>% table() %>% prop.table(1)
  sim_diff_prop[i] <- 
    sim_props["ivermectin","event"] - sim_props["placebo","event"]
}

sim_dist <- data.frame(sim_diff_prop)

sims_as_extreme <- sim_dist %>% 
  filter(sim_diff_prop <= -abs(iver_diff_prop) |
           sim_diff_prop >= abs(iver_diff_prop)) %>% 
  nrow()
sims_as_extreme
p_val <- sims_as_extreme/simulations
p_val

sim_dist %>% ggplot(aes(x = sim_diff_prop)) +
  geom_bar(color = "black", fill = "forestgreen") +
  scale_x_continuous() +
  gghighlight(sim_diff_prop <= -abs(iver_diff_prop) |
                sim_diff_prop >= abs(iver_diff_prop)
              ,
              unhighlighted_params = list(
                fill = "forestgreen",
                alpha = 0.25))


### Fluvoxamine study
  
num_fluvox <- 739
event_fluvox <- 77
num_fplacebo <- 733
event_fplacebo <- 109

fluvox_study <- matrix(c(event_fluvox, num_fluvox - event_fluvox,
                         event_fplacebo, num_fplacebo - event_fplacebo),
                       ncol=2, byrow=TRUE)
colnames(fluvox_study) <- c("event", "no event")
rownames(fluvox_study) <- c("fluvoxamine", "placebo")
fluvox_study <- as.table(fluvox_study)

fluvox_study %>% addmargins()
fluvox_study %>% prop.table(1) %>% addmargins(2)

fluvox_study %>% prop.test(correct = F, alternative = "less")
test <- fluvox_study %>% prop.test(correct = F, alternative = "less")

### Exact Test

sum(choose(679,111+(0:100))*choose(679,100-(0:100))/choose(1358,211))*2
fisher.test(iver_study)$p

## Chi sq independence

library(gssr)

num_vars <- c("age")
cat_vars <- c("sex", "degree", "partyid", "race", "grass", "owngun", "gunlaw")
my_vars <- c(num_vars, cat_vars)

gss18 <- gss_get_yr(2018)
data <- gss18
data <- data %>% 
  select(all_of(my_vars)) %>% 
  mutate(
    # Convert all missing to NA
    across(everything(), haven::zap_missing),
    # Make all categorical variables factors and relabel nicely
    across(all_of(cat_vars), forcats::as_factor)
  )

# Now, recode "degree" to a binary variable, "college"

data <- data %>% 
  mutate(
    college = recode(degree,
                     "lt high school" = "no degree",
                     "high school" = "no degree",
                     "junior college" = "degree",
                     "bachelor" = "degree",
                     "graduate" = "degree"
    )
  )

# and recode "partyid" to a simpler "party":

data <- data %>% 
  mutate(
    party = recode(partyid,
                   "strong democrat" = "DEM",
                   "not str democrat" = "DEM",
                   "ind,near dem" = "IND",
                   "independent" = "IND",
                   "ind,near rep" = "IND",
                   "not str republican" = "REP",
                   "strong republican" = "REP",
                   "other party" = "OTH"
    )
  )


table <- table(data$race, data$gunlaw)
table %>% addmargins() 
table %>% prop.table(1)
chisq.test(table)
chisq.test(table)$resid


vaccines <- c(rep("AIAN",41462),
              rep("Black",753863),
              rep("Other",1191827),
              rep("White",6826558))
vacdata <- data.frame(vaccines)

observed_gof_statistic <- vacdata %>%
  specify(response = vaccines) %>%
  hypothesize(null = "point",
              p = c("AIAN" = 0.005,
                    "Black" = 0.169,
                    "Other" = 0.053,
                    "White" = 0.773)) %>%
  calculate(stat = "Chisq")

  