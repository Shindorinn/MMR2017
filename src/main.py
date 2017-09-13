import csv
import os
import sys

def __main__():
    datapath = 'data/sample_eyetracking_data.txt'
    with open(datapath, 'r') as csvfile:
        reader = csv.DictReader(csvfile, delimiter='\t', quotechar='"')
        
        data = []
        for row in reader:
            data += row
        print(reader.fieldnames)


__main__()    