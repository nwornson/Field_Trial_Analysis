import sqlite3
import numpy as np
import pandas as pd
from itertools import repeat


params = pd.read_csv('C:/Users/NickW/OneDrive/Documents/Shiny_app/Field_Trial_Analysis/data/params.csv')

conn = sqlite3.connect('Field_Data.db')
params = pd.read_sql('SELECT * FROM chem_params',conn)

chems = params['chem'].tolist()
means = params['means'].tolist()
vars = params['vars'].tolist()


# Generate matix, each column sampled from a random distribution with mean and variance from params table
N = 250
ntrials = 5
ntreatments = 10

# simulated chemical values
rdata = np.random.normal(means,vars,(N,len(means)))

# rep
#rep_count = [1,2,3,4,5]
#rep = rep_count * ntrials * ntreatments

# Range and Row
Range_size = 10
Row_size = 5

range_seq = range(1,Range_size + 1)
Range = list(range_seq) * 5
Range = Range * 5

row_seq = range(1,Row_size + 1)
Row = sorted(list(row_seq) * 10)
Row = Row * 5

# Create location variable
locs = ['Location ' + str(x) for x in range(1,6)]

location = locs * 50

location = sorted(location)

chem_data_sim = pd.DataFrame(rdata,columns=chems)

chem_data_sim['Location'] = location
chem_data_sim['Range'] = Range
chem_data_sim['Row'] = Row




print(chem_data_sim.shape)

# Reshape data frame for ease in visualizations
chem_data_sim = chem_data_sim.melt(id_vars=['Location','Range','Row'])

print(chem_data_sim.shape)

# Update database
chem_data_sim.to_sql('chemical',con=conn,if_exists='replace',index=False)

# save to .csv
chem_data_sim.to_csv('chemical.csv')

# Update SQL server with R data

Carbon_Data = pd.read_csv('data/sample_data.csv')
Carbon_Data.to_sql('Carbon',con=conn,if_exists='replace',index=False)