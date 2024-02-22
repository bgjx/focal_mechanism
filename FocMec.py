#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec 15 19:32:03 2022

@author : ARHAM ZAKKI EDELO
@contact: edelo.arham@gmail.com
"""
import subprocess as sp
import glob, os
import time 
from pathlib import Path

print('''
Python code for FOCMEC sub program interfacing

Before you run this program, make sure you have changed all the path correctly.      
      ''')

def run_program(program, foc_input, foc_output, ids, emerge_avail = True, emerge_include = True):
    
    # call the program
    proc = sp.Popen(fortran_program, stdin=sp.PIPE, stdout=sp.PIPE, text = True)
    
    # output file name
    proc.stdin.write(f"{foc_output}\n")
    proc.stdin.flush()
    
    # file comment 
    proc.stdin.write(f"Supreme event {ids}\n")
    proc.stdin.flush()
    
    # input filename input
    proc.stdin.write(f"{foc_input}\n")
    proc.stdin.flush()
    
    # answer the question of the correct file 
    proc.stdin.write("Y\n")
    proc.stdin.flush()
    
    if emerge_avail and emerge_include:
        # answer yes if emergent polarity allowed
        proc.stdin.write("Y\n")
        proc.stdin.flush()
    
    if emerge_avail and not emerge_include:
        # answer no if emergent polarity allowed
        proc.stdin.write("N\n")
        proc.stdin.flush()
    
    # answert the question of the usage of relative weighting
    proc.stdin.write("N\n")
    proc.stdin.flush()
    
    # number of allowed P polarity errors
    proc.stdin.write("1\n")
    proc.stdin.flush()
    
    # the program will stop  if the solutions reach this number
    proc.stdin.write("70\n")
    proc.stdin.flush()
    
    # minimum search value B trend
    proc.stdin.write("0\n")
    proc.stdin.flush()
    
    # increment for the B trend
    proc.stdin.write("5\n")
    proc.stdin.flush()
    
    # maximum B trend - the increment
    proc.stdin.write("355\n")
    proc.stdin.flush()
    
    # minimum search of the B plunge
    proc.stdin.write("0\n")
    proc.stdin.flush()
    
    # increment for B plunge
    proc.stdin.write("5\n")
    proc.stdin.flush()
    
    # maximum of the B plunge
    proc.stdin.write("90\n")
    proc.stdin.flush()
    
    # minimum search angle in vertical plane of B trend
    proc.stdin.write("0\n")
    proc.stdin.flush()
    
    # increment for the angle
    proc.stdin.write("5\n")
    proc.stdin.flush()
    
    # maximum angle  
    proc.stdin.write("175\n")
    proc.stdin.flush()
    
    time.sleep(4)
    stdout, error = proc.communicate()
    print(stdout, error)

    return None

if __name__ == '__main__':
    prompt=str(input('Please type yes/no if you had changed the path :'))
    if prompt != 'yes':
        sys.exit("Ok, please correct the path first!")
    else:
        print("Process the program ....\n\n")
        pass
    
    # program path 
    fortran_program = Path(r"F:\SEML\FOCAL MECHANISM\Program\focmec\bin\focmec.exe")

    # initialize input and output path
    foc_input   = Path(r"F:\SEML\FOCAL MECHANISM\WORKDIR\2023\2023 09\focmec_input") # focmec input file
    foc_output  = Path(r"F:\SEML\FOCAL MECHANISM\WORKDIR\2023\2023 09\focmec_output") 
    
    # dinamic input prompt
    id_start    = int(input("Event's ID to start the Focal Mechanism determination : ")) 
    id_end      = int(input("Event's ID to end the Focal Mechanism determination : "))
    
    # exclude or include the emergent polarity
    emerge_inc = str(input("Do you want to include the emergent polarities if available ? [yes/no]: "))
    
    for _id in range(id_start, id_end +1 ):
        print(f"Calculating focal mechanism for event {_id}...")
        _input = foc_input.joinpath(f"focmec_input_{_id}")
        output = foc_output.joinpath(f"focmec_output_{_id}")
        
        # check if theres emergent polarities
        emergent_avail = False
        for line in open(_input, 'r').readlines():
            if re.search(r'[+-]', line):
                emergent_status = True
                break
        
        if emerge_avail and emerge_inc == 'yes':
            run_program(fortran_program , _input, output, _id)
        elif emerge_avail and emerge_inc != 'yes':
            run_program(fortran_program , _input, output, _id, emerge_avail = True, emerge_include = False )
        elif not emerge_avail:
            run_program(fortran_program , _input, output, _id, emerge_avail = False, emerge_include = False)
        else:
            pass
                
    print('-----------  The code has run succesfully! --------------')