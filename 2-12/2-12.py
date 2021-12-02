import pandas as pd
import numpy as np
df = pd.read_csv('input.txt', sep=" ", header=None)
df.columns = ['action', 'stepsize']

def main_1():

    sum_forward = df.loc[df['action'] == 'forward', 'stepsize'].sum()
    sum_down = df.loc[df['action'] == 'down', 'stepsize'].sum()
    sum_up = df.loc[df['action'] == 'up', 'stepsize'].sum()

    print(f'score = {(sum_down-sum_up)*sum_forward}')

def main_2():

    depth = 0
    dist = 0
    aim = 0
    for i, row in df.iterrows():
        action = row['action']
        stepsize = row['stepsize']
        if action == 'forward':
            dist += stepsize
            depth += aim*stepsize
        elif action == 'up':
            aim -= stepsize
        elif action == 'down':
            aim += stepsize

    print(dist*depth)

main_2()