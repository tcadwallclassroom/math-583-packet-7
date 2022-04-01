
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

iver_study %>% prop.test(alternative = "less", correct = F)

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
    sim_props["ivermectin","event"] - iver_props["placebo","event"]
}

sim_dist <- data.frame(sim_diff_prop)

sims_as_extreme <- sim_dist %>% 
  filter(sim_diff_prop <= iver_diff_prop) %>% 
  nrow()
sims_as_extreme
p_val <- sims_as_extreme/simulations
p_val

sim_dist %>% ggplot(aes(x = sim_diff_prop)) +
  geom_bar(color = "black", fill = "forestgreen") +
  scale_x_continuous() +
  gghighlight(sim_diff_prop <= iver_diff_prop,
              unhighlighted_params = list(
                fill = "forestgreen",
                alpha = 0.25))


### Fluvoxamine study
  
num_fluvox <- 739
event_fluvox <- 77
num_fplacebo <- 733
event_fplacebo <- 109

fluvox_study <- matrix(c(event_fluvox, num_fluvox - event_fluvox,
                         event_fplacebonum_fplacebo - event_fplacebo),
                       ncol=2, byrow=TRUE)
colnames(fluvox_study) <- c("no event", "event")
rownames(fluvox_study) <- c("fluvoxamine", "placebo")
fluvox_study <- as.table(fluvox_study)

fluvox_study %>% addmargins()
fluvox_study %>% prop.table(1) %>% addmargins(2)

fluvox_study %>% prop.test(correct = F)

