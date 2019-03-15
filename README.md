# WASA_Clim-Q-Sed_Input-Plot-Stats

For the WASA-SED model: A collection of R-scripts to generate climate input data in the format needed by WASA-SED, 
reformat and analyse observed river discharge Q, 
plot input and output Climate data, Qobs/mod, and Sediment. 

Climate timerseries (ClimTS): download NCEP data, reformat, analyse statistics &amp; plot. 

Observed river discharge (Qobs): reformat, analyse statistics/NA &amp; plot. 

Qobs_read_discharge.R can "collect" single discharge text files for each subbasins and combine them in one file "discharge_obs_24.txt", as needed for validation of model results.

Qobs-mod_summary-NAcheck-plots.R is a collection of scripts for statistical analysis (summary/NA statistics), plotting of observed discharge (Q obs), as well as joint plots with precipitation, Q obs and Q modelled; you can also create continuous time series of observation data or only plot for certain time intervals.

Authors: A. Müller, J. M. Delgado, T. Francke 

For more information please see the Wiki: https://github.com/A-Mue/WASA_Clim-Qobs/wiki
