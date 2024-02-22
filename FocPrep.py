#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec 15 19:32:03 2022

@author : ARHAM ZAKKI EDELO
@contact: edelo.arham@gmail.com
"""
import numpy as np
import os, glob, subprocess, sys
from pathlib import PurePath, Path
import pandas as pd 

print('''
Python code for preparing the FocMec input format

Before you run this program, make sure you have changed all the path correctly.      
      ''')

# List of functions 
#============================================================================================
def read_src(src_file):
    
    # initiate dict holder
    src_dict = {
    'id':[],
    'station':[],
    'azimuth':[],
    'takeoff':[]
    }
    split_data = [line.split() for line in src_file]
    for v in split_data:
        src_dict['id'].append(v[0])
        src_dict['station'].append(v[3])
        src_dict['azimuth'].append(v[5])
        src_dict['takeoff'].append(v[6])
    
    df = pd.DataFrame.from_dict(src_dict)
    return df

# End of functions 
#============================================================================================

if __name__ == "__main__" :
    prompt=str(input('Please type yes/no if you had changed the path :'))
    if prompt != 'yes':
        sys.exit("Ok, please correct the path first!")
    else:
        print("Process the program ....\n\n")
        pass

    # initialize input and output path
    pick_input  = Path(r"F:\SEML\DATA PICKING MEQ\DATA PICK 2023\PICK 2023 09\2023_09_full_test.xlsx")  # catalog picking
    hypo_src    = Path(r"F:\SEML\FOCAL MECHANISM\WORKDIR\hypoDD.src")                                   # hypodd src file
    foc_result  = Path(r"F:\SEML\FOCAL MECHANISM\WORKDIR\2023\2023 09\focmec_input")                    # focmec input directory

    # dinamic input prompt
    id_start    = int(input("Event's ID to start the Focal Mechanism determination : ")) 
    id_end      = int(input("Event's ID to end the Focal Mechanism determination : ")) 
    
    # reading pick data file
    pick_data = pd.read_excel(pick_input, index_col = None)
    
    # read the hypoDD source data
    src_data = read_src(open(hypo_src, "r"))

    
    for _id in range(id_start , id_end + 1):
    
        # select the respected pick data by the ID
        pick_handler    = pick_data[pick_data['Event ID'] == _id]
        
        # select the respected source data
        src_handler     = src_data[src_data['id'] == str(_id)]
        
        # create focmec input data
        with open (foc_result.joinpath(f"focmec_input_{_id}"), 'w') as focmec:
            print(f"Writing focmec input for events {_id}...")
            focmec.write(f"Supreme Event {_id}\n")
            for sta in list(pick_handler.get("Station")):
                # get the handler by the station
                try:
                    sta_handler = pick_handler[pick_handler["Station"] == sta]
                except Exception:
                    continue
                try:
                    # check the P polarity onset type
                    if sta_handler.P_Polarity.iloc[0] == '+' and sta_handler.P_Onset.iloc[0] == 'I':
                        polarity = 'C'
                    elif sta_handler.P_Polarity.iloc[0] == '+' and sta_handler.P_Onset.iloc[0] == 'E':
                        polarity = '+'
                    elif sta_handler.P_Polarity.iloc[0] == '-' and sta_handler.P_Onset.iloc[0] == 'I':
                        polarity = 'D'
                    elif sta_handler.P_Polarity.iloc[0] == '-' and sta_handler.P_Onset.iloc[0] == 'E':
                        polarity = '-'
                except Exception:
                    continue

                focmec.write(f"{sta}  {float(src_handler.azimuth[src_handler.station == sta].iloc[0]) + 180:6.2f}  {180 - float(src_handler.takeoff[src_handler.station == sta].iloc[0]):6.2f}{polarity}\n")
        focmec.close()
    print('-----------  The code has run succesfully! --------------')