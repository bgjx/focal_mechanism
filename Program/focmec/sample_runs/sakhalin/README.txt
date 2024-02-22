       Instructions for working with the Sakhalin Island event data set

  The program sequence is ratio_prep, focmec_prep, focmec, focplt (or ratplt)

  Included in this directory are only the input files that do not come from
  running an earlier program in the chain.  Output files from successful runs 
  are in ../../doc/sakhalin-doc -- it is suggested that you compare your results 
  with them.  Scripts in this directory run programs in ../../bin.  If a line 
  begins with /* that line is a comment line.  This is the only way to put in 
  comments for prompts that are character strings.  The scripts are written so 
  that terminal prompts are ported to a text file named a.junk.  This can be 
  useful if an error occurs.  Lines following EOT rename output files from their 
  default names to the user's choice.  For scripts that produce plots, lines may 
  be added to convert the .sgf plots to .pdf files and/or to display the plots.

  There is one driver file for program ratio_prep.  Text file ./stations.loc and 
  binary files that start with ../../lib/nocrust (for the nocrust velocity 
  model) used by the script rratio_prep_8sta.

  There is one driver file for program focmec_prep. File sakhalin_8sta.dat has 
  the polarity and ratio picks for the eight BB-station data set.

  There are 12 driver scripts for program focmec.  Those whose filenames start 
  with rfocmec_8sta use the eight-BB-station data set, those that start with 
  rfocmec_qed use the NEIC/QED data set, and those that start with rfocmec_all 
  use the combined data set.  Those with "fixed" in the filename restrict the 
  focmec grid search to a single point that is either for the 1990 NEIC/QED 
  solution or the final CMT published solution.

  There are six driver scripts for program focplt (all six plots are in the 
  manual).  Four are for the 8-BB data and include both polarities and fault 
  planes/surfaces.  One is for the QED data set showing polarities and 
  solutions.  One is for P-wave fault planes using the combined data.

  There are two driver scripts for program ratplt: one for the best 8-station 
  solution and one for the NEIC/QED solution.  Both use the 8-BB-station 
  dataset.

  There are two driver scripts for program dsretc.  The output can be used to 
  find the focmec input grid-search angles that restrict the search to the 
  angles for the final CMT and 1990 NEIC/QED solutions  This procedure is 
  discussed in Appendix E.
   
  Phase SKS comes in near direct S at the further station.  Driver file rttimes
  runs the iaspei-tau program ttimes for the nocrust velocity model to show the 
  S and SKS traveltimes for the eight BB stations.
