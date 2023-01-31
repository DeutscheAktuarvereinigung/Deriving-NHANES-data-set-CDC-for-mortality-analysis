Big Data Working Group (life) DAV, NHANES-data set, January 2023

Deriving of a NHANES-data set (CDC) for a mortality analysis

Introduction
-------------

The Big Data Working Group (life) of the German Actuarial Association (DAV) has developed R-scripts to create a consistent NHANES-data set, suitable for a mortality analysis. These scripts, along with a written report detailing the work, have been published here as the first part of the working group's efforts.  

https://aktuar.de/unsere-themen/fachgrundsaetze-oeffentlich/2022-09-21-DAV-Ergebnisbericht_Big_Data_Leben_NHANES_final.pdf

Additional analysis on this NHANES-data set will be released in the future. An example of EDA-Analysis is placed in the directory EDA.

Scripts
-------

All scripts are placed in the directory NHANES_ORIGINAL. **To build the NHANES-data set locally, change to the working directory NHANES_ORIGINAL in R. Execute command "options(timeout=1200)" as it takes up to 20 minutes to download a single file. Run the script NHANES_full_run.R.**

A few R-libraries are required, and these can be installed automatically or manually. After the data set is built (about 1 h to download all the original data from the CDC web server), the output files can be found in the "data" subdirectory as NHANES_ALL.RData and NHANES_ALL.csv. The other files in 'data' are temporary files which are used if a second run is performed to avoid downloading all CDC-data again.

Unfortunately, the CDC-mortality-data have changed at the beginning of may 2022. Additional years have been added, so more deaths are now available. Therefore, the examples and results of the written report are not fully reproduceable anymore.

Contact
-------

You are invited to contact the Big-Data-Working-Group, if there are any questions or suggestions:

Stefan Heyers: sheyers@kpmg.com
Thomas Gehling: thomas.gehling@gehlingconsulting.de
Andreas Döring: adoering@scor.de

Disclaimer
-------

The German Association of Actuaries (Deutsche Aktuarvereinigung.V., DAV) is the professional representation of all actuaries in Germany. It was founded in 1993 and has more than 5,400 members today. More than 700 members are involved in thirteen committees and in over 60 working groups as a voluntary commitment.

The given repositories have been created by committees and working groups and serve as an aid for our members and interested persons to support them in their work with machine learning methods and data science issues in an actuarial context.

Please note that the repositories provided on GitHub are published by the DAV. The content of linked websites is the sole responsibility of their operators. The DAV is not responsible for the code and data linked to external sources. 
