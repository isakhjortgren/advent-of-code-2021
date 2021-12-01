import pandas as pd
import numpy as np
df = pd.read_csv('input.txt', sep="\n", header=None)
arr = df.values.flatten()

def task_1():

    diff = arr[1:] - arr[:-1]
    print(f'count {np.sum(diff > 0)}')

def task_2():
    sumarr = arr[:-2] + arr[1:-1] + arr[2:]
    diff = sumarr[1:] - sumarr[:-1]
    print(f'count {np.sum(diff > 0)}')


if __name__ == '__main__':
    task_2()