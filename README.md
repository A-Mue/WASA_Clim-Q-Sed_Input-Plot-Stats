# WASA_Clim-Q-Sed_Input-Plot-Stats

For the WASA-SED model: A collection of R-scripts to:
- Generate climate input data in the format needed by WASA-SED, 
- Reformat observed river discharge Q (many txt-Files to 1 joint file; create continuous data, gaps are filled with NA), 
- Statistical analyses: summary and NA statistics,
- Plot input and output Climate data, Qobs/mod, and Sediment. 

Climate timerseries (ClimTS): download NCEP data, reformat, analyse statistics &amp; plot. 

Observed river discharge (Qobs): reformat, analyse statistics/NA &amp; plot. 

Qobs_read_discharge.R can "collect" single discharge text files for each subbasins and combine them into one file, create continuous time series of observation data, and save as "discharge_obs_24.txt", as needed for validation of model results.

Stats&Plot-Qobs-mod-P-NA.R is a collection of scripts for statistical analysis (summary/NA statistics), plotting of observed discharge (Q obs), as well as joint plots with precipitation, Q obs and Q modelled; you can also create continuous time series of observation data or only plot for certain time intervals.

Authors: A. Müller, J. M. Delgado, T. Francke 

For more information please see the Wiki: https://github.com/A-Mue/WASA_Clim-Qobs/wiki
