


library(tidyverse)
library(broom)
library(scales) # percent labels
library(agricolae) # Multiple Hypothesis
library(DescTools) # Dunnett's Test
library(lmerTest) # Mixed Effects Modeling
#library(RPostgreSQL)  
library(odbc) # Save to SQL server
library(DBI)

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
  
  sim_data = sim_data %>% 
    group_by(Trial) %>%
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
           # Carbon Build metric for analysis
           CBuild.ratio = harvest/baseline)
  
  # Turn row and range to factors
  cols = c(1:5)
  out[cols] = lapply(out[cols],factor)
  
  return(out)

}

### Data tranformation/stats functions

# Trial means, sd's

L1_stats = function(data){
  
  L1 = data %>%
    group_by(Trial,Treatment) %>%
    summarise(Mean_CBuild.ratio = mean(CBuild.ratio,na.rm = TRUE),
              SE.CBuild.ratio = sd(CBuild.ratio,na.rm = TRUE)/sqrt(n())) %>%
    group_by(Trial) %>%
    mutate(CBuild.diff = Mean_CBuild.ratio - Mean_CBuild.ratio[grepl('Control',Treatment)],
           SE.CBuild.diff = sqrt(SE.CBuild.ratio^2 + SE.CBuild.ratio[Treatment == 'Control']^2)) 
  
  return(L1)
  
}

# Summary statistics

display_stats = function(data,trial){
  
  L1 = L1_stats(data) %>%
    filter(Trial == trial) %>%
    select(-Trial) %>%
    mutate(across(where(is.numeric),round,3))
  
  return(L1)
}

# Fixed effects only model - Anova

display_anova = function(data,trial){
  
  trial_data = data %>% 
    filter(Trial == trial)
  
  m = lm(CBuild.ratio ~ Treatment + Replicate,trial_data)
  
  out = anova(m) %>% 
    tidy() %>%
    mutate(across(where(is.numeric),round,4))
  
  return(out)
  
}

# Dunnett's test for multiple comparisons (to control)

display_dunnet = function(data,trial){
  
  trial_data = data %>% 
    filter(Trial == trial)
  
  dt = DunnettTest(CBuild.ratio ~ Treatment,
                   data = trial_data,
                   control = 'Control')
  
  dt_df = data.frame(dt[1])
  colnames(dt_df) = c('diff','lwr.ci','upr.ci','pval')
  dt_df$trt = row.names(dt_df)
  dt_df = dt_df %>%
    separate(trt,c('Treatment.Name','ctrl'),sep = '-') %>%
    relocate(Treatment.Name, .before = diff)
  
  dt_df = dt_df %>% select(-ctrl)
  
  row.names(dt_df) = NULL
  
  out = dt_df %>% 
    #tidy() %>%
    mutate(across(where(is.numeric),round,4))
  
  return(out) 
  
}

## Mixed effects model

ME_model_trt = function(data,trial){
  
  tempdf = tempdf = data %>%
    filter(Trial == trial)
  
  model1 = lmer(baseline ~ Treatment + (1|Range) + (1|Row),tempdf)
  m1.sum = anova(model1) %>% 
    tidy()
  

  
  return(m1.sum)
  
}


## field variation

library(lme4)
field_var_BLTOC = function(data,trial) {
  
  
  tempdf = data %>%
    filter(Trial == trial)
  
  resp_mean = mean(tempdf$baseline)
  
  model1 = lmer(baseline ~ Treatment + (1|Range) + (1|Row),tempdf)
  m1.sum = summary(model1)
  sigma1 = m1.sum$sigma
  
  cv1 = sigma1/resp_mean
  sigma_range = m1.sum$varcor$Range[1]
  sigma_row = m1.sum$varcor$Row[1]
  
  # compute field variation by taking rg and row variation as a fraction of sum
  total_var = sum(sigma_range,sigma_row,sigma1^2)
  range_fv = sigma_range/total_var
  row_fv = sigma_row/total_var
  
  out = paste(round(cbind(sigma1,range_fv,row_fv,cv1),5) * 100,'%',sep = '')
  
  out = data.frame(out,
                   row.names = c('Residual','Range','Row','Coeficient of Variation'))
  colnames(out) = 'Value' 
  
}

field_var_HTOC = function(data,trial) {
  
  
  tempdf = data %>%
    filter(Trial == trial)
  
  resp_mean = mean(tempdf$harvest)
  
  model1 = lmer(harvest ~ Treatment + (1|Range) + (1|Row),tempdf)
  m1.sum = summary(model1)
  sigma1 = m1.sum$sigma
  
  cv1 = sigma1/resp_mean
  sigma_range = m1.sum$varcor$Range[1]
  sigma_row = m1.sum$varcor$Row[1]
  
  # compute field variation by taking rg and row variation as a fraction of sum
  total_var = sum(sigma_range,sigma_row,sigma1^2)
  range_fv = sigma_range/total_var
  row_fv = sigma_row/total_var
  
  out = paste(round(cbind(sigma1,range_fv,row_fv,cv1),5) * 100,'%',sep = '')
  
  out = data.frame(out,
                   row.names = c('Residual','Range','Row','Coeficient of Variation'))
  colnames(out) = 'Value' 
  
  return(out)
  
}
#### Plotting functions

# Treatment locations

trt_heat = function(data,trial){
  
  p = data  %>%
  filter(Trial == trial) %>%
  ggplot(aes(x = Row, y = Range)) +
  geom_tile(color = 'black',fill = 'white') +  
  geom_text(aes(label = Treatment), color = "black", size = 3) +
  theme_classic()
  
  return(p)
  
}


# Baseline Heatmap
BL_heat = function(data,trial){ 

  p = data  %>%
  filter(Trial == trial) %>%
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

  p = data  %>%
  filter(Trial == trial) %>%
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



## Distributions

# box plot

trial_box = function(data,trial){
  
  p = data %>%
    filter(Trial == trial) %>%
    ggplot(aes(Treatment,CBuild.ratio)) +
    geom_boxplot() +
    geom_point() +
    theme_classic()
  
  return(p)
}

# testing

#test = field_data()
#t1 = L1_stats(test)

#write.csv(test,'sample_data.csv',row.names = FALSE)
#write.csv(t1,'sample_data_L1.csv',row.names = FALSE)






