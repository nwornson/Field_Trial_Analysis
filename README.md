# Field_Trial_Analysis
Simulating typical field trial analysis for Agricultural Research and Development.  


To run the application, run this in the R console (it may take a minute to render):

library(shiny)

runGitHub('Field_Trial_Analysis','nwornson')



The purpose of this project is to simulate data that is similar to work that I have encountered in the past.  The field trial dash shows a fictional experiment where X locations were used to test Y number of treatments versus a control.  The fictional dependent variable is amount of carbon sequestered in the soil.  In a real trial, carbon measurements in the soil would be taken prior to planting, and post-harvest, with the ratio of harvest to 'baseline' being the metric of interest.  

Spatial variation is introduced for each location at random, to simulate real life fields that don't behave uniformly.  The distribution of carbon values are sampled from a normal distribution loosely based on my own experiences.  
