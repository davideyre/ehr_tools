# EHR tools
A collection of scripts and look up tables for analysis of electronic healthcare record data.

Contents  
1. Bug grouper - a script for grouping common bacterial pathogens [bug_grouper.R](bug_grouper.R)
2. EHR measurement limits in adults - a list of values deemed to extreme to be compatible with an actual measurement, recommended that these are set to missing in analyses [EHR Measurement Limits - Adults.xlsx](EHR%20Measurement%20Limits%20-%20Adults.xlsx)
3. Calculate Comorbidity Index - a script for Calculating Charlson and Elixhauser scores using diagnostic codes from 1-year lookback [Calculate Comorbidity Index.R](Calculate%20Comorbidity%20Index.R)


Look-up tables
1. Local subspecialties - specialty codes matched to specialties matched to specialty groups [local_subspecialties_lookup.csv](local_subspecialties_lookup.csv)
2. ICD-10 codes for primary diagnosis (needing revisions and confirmation for your own projects)
3. ICD-10 codes to CCS codes - for identifying possible sources of infection [ccs_lookup.csv](ccs_lookup.csv)
