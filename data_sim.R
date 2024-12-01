
# Create fake trial data

library(tidyverse)

field_data = function(ntrials = 5,
                      ntreatments = 10,
                      reps = 5,
                      range_size = 10,
                      row_size = 5)
  {
  
   # keep this locked for now
  
  trial_names = paste('Location',1:ntrials)
  treatments = c('Control',paste('Treatment',letters[1:(ntreatments-1)]))
  
  n = range_size * row_size
  N = ntrials * reps * ntreatments
  
  Trial = rep(trial_names,each = n)
  Treatment = rep(rep(treatments,each = reps),ntrials)
  Replicate = rep(1:5,N/5)
  
  
  Row = rep(1:reps,ntrials * ntreatments)
  
  sim_data = data.frame(cbind(Trial,
                              Treatment,
                              Replicate,
                              Row))
  
  sim_data = sim_data %>% group_by(Trial) %>%
    arrange(desc(Replicate),.by_group = TRUE)
  
  Range = c()
  
  for(i in 1:(ntrials * reps)){
    
    Range = c(Range,sample(1:range_size))
    
  }
  
  sim_data$Range = Range
  
  
  cols = c(1:3)
  sim_data[cols] = lapply(sim_data[cols],factor)
  
  sim_data$Row = as.numeric(sim_data$Row)
  sim_data$Range = as.numeric(sim_data$Range)
  
  # Simulate the data, introducing some spatial variation
  
  out = sim_data %>%
    group_by(Trial) %>%
    # Get trial means
    mutate(Trial_Mean = rnorm(n = 1, 
                              mean = 2.5,
                              sd = 2),
           # Introduce range and row correlation
           row_var = runif(1,min = -1,max = 1),
           range_var = runif(1,min = -1,max = 1)) %>%
    group_by(Treatment,Trial) %>%
            # Generate treatment means for each location
    mutate(trt_trl_mean = rnorm(1,mean = mean(Trial_Mean),sd = .05),
           # win or lose per treatment
           success = runif(1,-.2,.2)) %>%
    ungroup() %>%
          # Simulate baseline and harvest data as functions of treatment mean,range and row
    mutate(baseline = round(rnorm(1,trt_trl_mean,.01) + Row * row_var + Range * range_var,2),
           # Harvest, baseline plus win/loss
           harvest = round(baseline + rnorm(1,success,.05),2))
  
  # Turn row and range to factors
  cols = c(1:5)
  out[cols] = lapply(out[cols],factor)
  
  return(out)

}

sim_data_v2 = field_data()

direc = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(direc))

write.csv(sim_data_v2,'simulated_data.csv',row.names = FALSE)


## Plot test


# Treatment locations

sim_data_v2  %>%
  filter(Trial == 'Location 1') %>%
  ggplot(aes(x = Row, y = Range)) +
  geom_tile(color = 'black',fill = 'white') +  
  geom_text(aes(label = Treatment), color = "black", size = 3) +
  theme_classic()

sim_data_v2  %>%
  filter(Trial == 'Location 1') %>%
  ggplot(aes(x = Row, y = Range, fill = baseline)) +
  geom_tile() +
  scale_fill_gradient(low="palevioletred",high="palegreen") +  
                      #limits = c(lwr,upr))+
  geom_text(aes(label = baseline), color = "black", size = 3)
            #angle = 90) 


sim_data_v2  %>%
  filter(Trial == 'Location 1') %>%
  ggplot(aes(x = Row, y = Range, fill = harvest)) +
  geom_tile() +
  scale_fill_gradient(low="palevioletred",high="palegreen") +  
  #limits = c(lwr,upr))+
  geom_text(aes(label = harvest), color = "black", size = 3)
            #angle = 90) 
