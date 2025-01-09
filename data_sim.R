


library(tidyverse)
library(scales) # percent labels

# Create fake trial data

field_data = function(ntrials = 5, # number of locations
                      ntreatments = 10, # number of treatments
                      reps = 5, # replications
                      range_size = 10, # range size (Range x Row size fields)
                      row_size = 5) # row size
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
           row_var = runif(1,min = -1,max = 1) + .001, # avoid 0's
           range_var = runif(1,min = -1,max = 1) + .001) %>%
    group_by(Treatment,Trial) %>%
            # Generate treatment means for each location
    mutate(trt_trl_mean = rnorm(1,mean = mean(Trial_Mean),sd = .05),
           # win or lose per treatment
           success = runif(1,-.2,.2)) %>%
    ungroup() %>%
          # Simulate baseline and harvest data as functions of treatment mean,range and row
    mutate(baseline = round(rnorm(1,trt_trl_mean,.01) + Row * row_var + Range * range_var,2),
           # Harvest, baseline plus win/loss
           harvest = round(baseline + rnorm(1,success,.05),2),
           CBuild.ratio = harvest/baseline)
  
  # Turn row and range to factors
  cols = c(1:5)
  out[cols] = lapply(out[cols],factor)
  
  return(out)

}

### Data tranformation functions

# Trial means, sd's

L1_stats = function(data){
  
  L1 = data %>%
    group_by(Trial,Treatment) %>%
    summarise(Mean_CBuild.ratio = mean(CBuild.ratio,na.rm = TRUE),
              SE.CBuild.ratio = sd(CBuild.ratio,na.rm = TRUE)/sqrt(n())) %>%
    group_by(Trial) %>%
    mutate(CBuild.diff = Mean_CBuild.ratio - Mean_CBuild.ratio[grepl('Control',Treatment)],
           SE.CBuild.diff = sqrt(SE.CBuild.ratio^2 + SE.CBuild.ratio[Treatment == 'Control']^2)) 
  
  
  
}

display_stats = function(data,trial){
  
  L1 = L1_stats(data) %>%
    filter(Trial == trial) %>%
    select(-Trial) %>%
    mutate(across(where(is.numeric),round,3))
}

#### Plotting functions

# Treatment locations

trt_heat = function(data,trial){
  
  p = sim_data_v2  %>%
  filter(Trial == 'Location 1') %>%
  ggplot(aes(x = Row, y = Range)) +
  geom_tile(color = 'black',fill = 'white') +  
  geom_text(aes(label = Treatment), color = "black", size = 3) +
  theme_classic()
  
  return(p)
  
}


# Baseline Heatmap
BL_heat = function(data,trial){ 

  p = sim_data_v2  %>%
  filter(Trial == 'Location 1') %>%
  ggplot(aes(x = Row, y = Range, fill = baseline)) +
  geom_tile() +
  scale_fill_gradient(low="palevioletred",high="palegreen") +  
                      #limits = c(lwr,upr))+
  geom_text(aes(label = baseline), color = "black", size = 3)
            #angle = 90) 
  
  return(p)
}

#Harvest Heatmap

H_heat = function(data,trial){

  p = sim_data_v2  %>%
  filter(Trial == 'Location 1') %>%
  ggplot(aes(x = Row, y = Range, fill = harvest)) +
  geom_tile() +
  scale_fill_gradient(low="palevioletred",high="palegreen") +  
  #limits = c(lwr,upr))+
  geom_text(aes(label = harvest), color = "black", size = 3)
            #angle = 90) 
  
  return(p)
}


# Win-Rate by Treatment

WR_bar = function(data,trt){
  
  boundry = .5
  offset = .15
  max.tc = max(abs(data$CBuild.diff))
  
  if(max.tc + offset > boundry){
    boundry = max.tc + offset
  }
  
  boundry = 100 * boundry
  
  temp = data %>% filter(Treatment == trt) %>% 
    select(Trial,CBuild.diff,SE.CBuild.diff) %>%
    mutate(WR = ifelse(CBuild.diff >= 0,1,0)) %>%
    arrange(desc(-CBuild.diff)) 
  
  
  
  locs = levels(factor(temp$Trial,level=factor(temp$Trial)))
  top = boundry - 5  
  top2 = top - 20
  
  mean_build = round(mean(temp$CBuild.diff),3)
  WR = sum(temp$WR)/nrow(temp)
  
  anno = data.frame(x = locs[1],y = c(top,top2),
                    label = c(paste('Avg', label_percent()(mean_build)),
                              paste('WR', label_percent()(WR))))
  
  # convert to %
  temp$CBuild.diff.perc = 100 * temp$CBuild.diff 
  temp$SE.CBuild.diff.perc = 100 * temp$SE.CBuild.diff
  
  p = temp %>%
    ggplot(aes(x = factor(Trial,level = Trial),
               y = CBuild.diff.perc)) + 
    geom_bar(stat = 'identity',fill = 'aquamarine3') +
    geom_errorbar(aes(x = factor(Trial, level=Trial),
                      ymin = CBuild.diff.perc - SE.CBuild.diff.perc, ymax = CBuild.diff.perc + SE.CBuild.diff.perc),
                  width = .25) +
    geom_hline(yintercept = 0) +
    xlab('') + ylab('TC H/B diff from Control (%)') +
    ylim(-boundry,boundry) +
    geom_text(data=anno,aes(x=x,y=y,label=label)) +
    #geom_text(vjust = -2,
    #          position = position_dodge(.9),size = 5) +
    ggtitle(paste('TC diff from Control - ',trt)) +
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 90))
  
  return(p)
}


# testing
 test = field_data()
 t1 = L1_stats(test)
# pwr = WR_bar(t1,'Treatment a')
# 
# print(pwr)



















