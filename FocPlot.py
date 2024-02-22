#!/usr/bin/env python3D
# -*- coding: utf-8 -*-
"""
Created on Thu Dec 15 19:32:03 2022

@author : ARHAM ZAKKI EDELO
@contact: edelo.arham@gmail.com
"""

import subprocess as sp
import glob, os, sys
import time 
from pathlib import Path
from PIL import Image
import pandas as pd
import numpy as np
import re

print('''
Python code for FocMec Result Plotting

Before you run this program, make sure you have changed all the path correctly.      
      ''')
      
# List of functions 
#============================================================================================

# function for plotting all available focmec solutions
def plot_focal_all(program, focmec_in, focmec_out, fig_out,  ids, emerge_status = True):
    
    # call the program
    proc = sp.Popen(fortran_program, stdin=sp.PIPE, stdout=sp.PIPE, text = True)
    
    # choose upper or lower hemisphere, N => lower hemisphere
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # plot polarity data?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # input file polarity station and polarity data (focmec input)
    proc.stdin.write(f"{focmec_in}\n")
    proc.stdin.flush()
    
    # answer the question of the correct file 
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # plot impulsive P polarities?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # enter the size of the impulsive polarities
    proc.stdin.write("0.25\n")
    proc.stdin.flush()
    
    # linewidth for impulsive P polarities
    proc.stdin.write("1\n")
    proc.stdin.flush()
    
    if emerge_status:
        # answer yes emergent P polarities will be plotted
        proc.stdin.write("Y\n")
        proc.stdin.flush()
    
        # enter the size of the emergent polarities
        proc.stdin.write("0.15\n")
        proc.stdin.flush()
        
        # linewidth for emergent P polarities
        proc.stdin.write("1\n")
        proc.stdin.flush()
    else:
        pass
        
    # linewidth for circle outline
    proc.stdin.write("2\n")
    proc.stdin.flush()
        
    # add a tittle to the plot?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # enter the title
    proc.stdin.write(f"Event : {ids}\n")
    proc.stdin.flush()
    
    # linewidth for the title
    proc.stdin.write("2\n")
    proc.stdin.flush()

    # include time and file name ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # add more data to plot ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # plot focal mechanism solutions?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # print displays solution summaries?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # plot P, T and B axes ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # plot p nodal planes ?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # dashed line?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # linewidth for solutions ?
    proc.stdin.write("0.5\n")
    proc.stdin.flush()
    
    # solutions on same plot as data ?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # input solutions from a file ?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # input file name (focmec output) ?
    proc.stdin.write(f"{focmec_out}\n")
    proc.stdin.flush()
    
    # ask you whether it is correct file ?  
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # plot time and file name ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # add more solutions ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # add a plot label ?
    proc.stdin.write("n\n")
    proc.stdin.flush()

    time.sleep(4)
    stdout, error = proc.communicate()
    print(stdout, error)
    
    proc.terminate()
    return None

# functions for calculating statistical analysis from all available focmec solutions
def focal_stats(foc_input):
    foc_dict = {
        "Dip": [],
        "Strike": [],
        "Rake": []
    }
    split_line = [line.split() for line in open(foc_input, 'r').readlines()]
    breaker = 0
    for line in split_line:
        breaker += 1
        if line[0] == 'Dip':
            break

    foc_data = split_line[breaker:]

    for line in foc_data:
        foc_dict['Dip'].append(float(line[0]))
        foc_dict['Strike'].append(float(line[1]))
        foc_dict['Rake'].append(float(line[2]))
    
    # create dataframe
    df_foc = pd.DataFrame.from_dict(foc_dict)
    
    # do statistical analysis
    # find strike median
    strike_median = round(np.median(df_foc.Strike.values))
    
    # find index
    
    idx = 0
    res = np.inf
    for strike in list(df_foc.Strike.values):
        gap = strike_median - strike
        if gap < res:
            res = gap
            indice = idx
        idx +=1
    print(indice)
    foc_solution = [df_foc.Dip.values[indice], df_foc.Strike.values[indice], df_foc.Rake.values[indice]]
    foc_mean   = [np.mean(df_foc.Dip.values, dtype=np.float64), np.mean(df_foc.Strike.values, dtype=np.float64), np.mean(df_foc.Rake.values, dtype=np.float64)]
    foc_std    = [np.std(df_foc.Dip.values, dtype=np.float64), np.std(df_foc.Strike.values, dtype=np.float64), np.std(df_foc.Rake.values, dtype=np.float64)]
    
    print(df_foc.Strike.values)
    print(strike_median)
    print(foc_solution)
    return foc_solution, foc_mean, foc_std

# functions for plotting single focmec solutions after statistical analysis
def plot_focal_single(program, focmec_in, focmec_stats, fig_out,  ids, emerge_status = True):
    
    foc_median, foc_mean, foc_std = focmec_stats
    
    # call the program
    proc = sp.Popen(fortran_program, stdin=sp.PIPE, stdout=sp.PIPE, text = True)
    
    # choose upper or lower hemisphere, N => lower hemisphere
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # plot polarity data?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # input file polarity station and polarity data (focmec input)
    proc.stdin.write(f"{focmec_in}\n")
    proc.stdin.flush()
    
    # answer the question of the correct file 
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # plot impulsive P polarities?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # enter the size of the impulsive polarities
    proc.stdin.write("0.25\n")
    proc.stdin.flush()
    
    # linewidth for impulsive P polarities
    proc.stdin.write("1\n")
    proc.stdin.flush()
    
    if emerge_status:
        # answer yes emergent P polarities will be plotted
        proc.stdin.write("Y\n")
        proc.stdin.flush()
    
        # enter the size of the emergent polarities
        proc.stdin.write("0.25\n")
        proc.stdin.flush()
        
        # linewidth for emergent P polarities
        proc.stdin.write("1\n")
        proc.stdin.flush()
    else:
        pass
        
    # linewidth for circle outline
    proc.stdin.write("2\n")
    proc.stdin.flush()
        
    # add a tittle to the plot?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # enter the title
    proc.stdin.write(f"Event : {ids}\n")
    proc.stdin.flush()
    
    # linewidth for the title
    proc.stdin.write("2\n")
    proc.stdin.flush()

    # include time and file name ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # add more data to plot ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # plot focal mechanism solutions?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # print displays solution summaries?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # plot P, T and B axes ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # plot p nodal planes ?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # dashed line?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # linewidth for solutions ?
    proc.stdin.write("1\n")
    proc.stdin.flush()
    
    # solutions on same plot as data ?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # input solutions from a file ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # manually input the dip, strike, and the rake from median value ?
    proc.stdin.write(f"{foc_median[0]} {foc_median[1]} {foc_median[2]}\n")
    proc.stdin.flush()
    
    # add more solutions (mean + std)?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # plot P, T and B axes ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # plot p nodal planes ?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # dashed line?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # enter length of dash?
    proc.stdin.write("0.1\n")
    proc.stdin.flush()
    
    # spacing between dash?
    proc.stdin.write("0.1\n")
    proc.stdin.flush()
    
    # enter lindwidth for solutions?
    proc.stdin.write("1\n")
    proc.stdin.flush()
    
    # input solutions from a file ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # manually input the dip, strike, and the rake from median value ?
    proc.stdin.write(f"{foc_mean[0] + foc_std[0]} {foc_mean[1] + foc_std[1]} {foc_mean[2] + foc_std[2]}\n")
    proc.stdin.flush()
    
    # add more solutions (mean - std)?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # plot P, T and B axes ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # plot p nodal planes ?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # dashed line?
    proc.stdin.write("y\n")
    proc.stdin.flush()
    
    # enter length of dash?
    proc.stdin.write("0.1\n")
    proc.stdin.flush()
    
    # spacing between dash?
    proc.stdin.write("0.1\n")
    proc.stdin.flush()
    
    # enter lindwidth for solutions?
    proc.stdin.write("1\n")
    proc.stdin.flush()
    
    # input solutions from a file ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # manually input the dip, strike, and the rake from median value ?
    proc.stdin.write(f"{foc_mean[0] - foc_std[0]} {foc_mean[1] - foc_std[1]} {foc_mean[2] - foc_std[2]}\n")
    proc.stdin.flush()
    
    # add more solutions ?
    proc.stdin.write("n\n")
    proc.stdin.flush()
    
    # add a plot label ?
    proc.stdin.write("n\n")
    proc.stdin.flush()

    time.sleep(4)
    stdout, error = proc.communicate()
    print(stdout, error)
    
    proc.terminate()
    return None
    
# End of functions 
#============================================================================================

if __name__ == '__main__':
    prompt=str(input('Please type yes/no if you had changed the path :'))
    if prompt != 'yes':
        sys.exit("Ok, please correct the path first!")
    else:
        print("Process the program ....\n\n")
        pass
    # program path 
    fortran_program = Path(r"F:\SEML\FOCAL MECHANISM\Program\focmec\bin\focplt.exe")
    cyg_path = Path(r"C:\cygwin64\bin\bash.exe")
    
    # initialize input and output path
    focmec_input   = Path(r"F:\SEML\FOCAL MECHANISM\WORKDIR\2023\2023 09\focmec_input") # focmec input as focplot input file
    focmec_output  = Path(r"F:\SEML\FOCAL MECHANISM\WORKDIR\2023\2023 09\focmec_output") # focmec ouput as focplot input file
    fig_output     = Path(r"F:\SEML\FOCAL MECHANISM\WORKDIR\2023\2023 09\focplot_pics")
    
    # dinamic input prompt
    id_start    = int(input("Event's ID to start the Focal Mechanism determination : ")) 
    id_end      = int(input("Event's ID to end the Focal Mechanism determination : "))
    
    for _id in range(id_start , id_end + 1):
        print(f"Creating plot figure for event {_id}...")
        _input  = focmec_input.joinpath(f"focmec_input_{_id}")
        _input2 = focmec_output.joinpath(f"focmec_output_{_id}")
        
        # perform statistical analysis of the FocMec result
        _input3 = focal_stats(_input2)
        
        # check if theres emergent polarities
        emergent_status = False
        for line in open(_input, 'r').readlines():
            if re.search(r'[+-]', line):
                emergent_status = True
                break
                
        # run focal plotting for all solutions first
        if emergent_status == True:
            plot_focal_all(fortran_program , _input, _input2, fig_output,  _id)
        else:
            plot_focal_all(fortran_program , _input, _input2, fig_output, _id, emergent_status)

        # convert temp.sgf to eps and delete the temp.sgf file
        sp.run([cyg_path, '-c', 'sgftoeps temp'], check = True)
        sp.run(['rm', 'temp.sgf'])

        # rename the temp.eps and move the eps file to the disired directory
        sp.run(['mv', 'temp.eps', fig_output.joinpath(f'foc_plot_all_{_id}.eps')])
        
        # run focal plotting for single solutions with error
        if emergent_status == True:
            plot_focal_single(fortran_program , _input, _input3, fig_output,  _id)
        else:
            plot_focal_single(fortran_program , _input, _input3, fig_output, _id, emergent_status)
        
        # convert temp.sgf to eps and delete the temp.sgf file
        sp.run([cyg_path, '-c', 'sgftoeps temp'], check = True)
        sp.run(['rm', 'temp.sgf'])

        # rename the temp.eps and move the eps file to the disired directory
        sp.run(['mv', 'temp.eps', fig_output.joinpath(f'foc_plot_single_{_id}.eps')])
        
    print('-----------  The code has run succesfully! --------------')