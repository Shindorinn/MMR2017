import numpy as np
import cv2
import pandas as pd
import operator

def same(row):
    return row["L Event Info"] == row["R Event Info"]

def event(row, eventstr):
    return row["L Event Info"] == eventstr

maxds = 0

def analyze(df):
    mode = "Wait"
    for index, row in df.iterrows():
        if not same(row):
            continue
        if mode == "Wait":
            if event(row, "Fixation"):
                prevx, prevy = row["L POR X [px]"], row["L POR Y [px]"]
                mode = "Fixate"
        if mode == "Fixate":
            if event(row, "Fixation"):
                currx, curry = row["L POR X [px]"], row["L POR Y [px]"]
            else:
                dx = currx - prevx
                dy = curry - prevy
                ds = dx * dx + dy * dy
                if ds > maxds:
                    maxds = ds
                mode = "Wait"

for i in range(1, 2):
    number = str(i)
    if(i < 10):
        number = "0" + number
    try:
        df = pd.read_csv("p" + number + "_ET_samples.txt", "\t")
    except IOError as err:
        print "Skipped", number,
        continue
    except MemoryError as err:
        print "Memory", numebr,
        continue
    print number,
    analyze(df)
print

print maxds