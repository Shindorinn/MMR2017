import numpy as np
import cv2
import pandas as pd
import operator

df = pd.read_csv("p01_ET_samples.txt", "\t")
print "01",
uniques = {}
for column in df.columns:
    uniques[column] = set(df[column].unique())

for i in range(2, 3):
    number = str(i)
    if(i < 10):
        number = "0" + number
    try:
        df = pd.read_csv("p" + number + "_ET_samples.txt", "\t")
    except IOError as err:
        print "Skipped", number,
        continue
    except MemoryError as err:
        print "Memory",
        continue
    print number,
    for column in df.columns:
        try:
            uniques[column].update(set(df[column].unique()))
        except MemoryError as err:
            print "Memory",
            continue
print

columns = []
for column in uniques:
    columns.append((len(uniques[column]), column))
print

columns.sort()

for column in columns:
    print column[1], ":", column[0]