import numpy as np
import cv2
import pandas as pd
import operator

def event(row, eventstr):
    return (row["L Event Info"] == eventstr) & (row["R Event Info"] == eventstr) & (not np.isnan(row["stimulus_id"])) & (not((row["Stimulus"] == "bbf32011-05e2-4a20-b06e-313857a5acea.jpg")
 & (row["stimulus_id"] == 10879)))

def analyze(df):
    maxds = 0
    mode = "Wait"
    for index, row in df.iterrows():
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
                    if maxds > 13304:
                        print index, prevx, prevy, currx, curry
                mode = "Wait"
    return maxds

maxds = 0

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
    ds = analyze(df)
    if ds > maxds:
        maxds = ds
print

print maxds