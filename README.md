# Deriving of a NHANES-data set (CDC) for a mortality analysis

Big Data Working Group (life) DAV, NHANES-data set

Deriving of a NHANES-data set (CDC) for a mortality analysis

The Big Data Working Group (life) has developed R-scripts to build a consistent NHANES-data set which is suitable for mortality analysis. These scripts are published as a first part of the working group. A written report will be published soon to describe the work and give more details, especially of the variables. As a second part of the Working Group, additional analysis on this NHANES-data set will be published later.
All scripts are placed in the directory NHANES_ORIGINAL. To build the NHANES-data set locally, start or run the script NHANES_full_run.R. There are few R-libraries which are necessary. If they are no present, they could be installed automatically or manually. After the full run (about 1 h to download all the original data from the CDC web server), the output file is placed in the subdirectory 'data' as NHANES_ALL.RData and NHANES_ALL.csv. The other files in 'data' are temporary files which are used if a second run is performed to avoid downloading all CDC-data again.
Unfortunately, the CDC-mortality-data have changed at the beginning of may 2022. Additional years have been added, so more deaths are now available. Therefore, the examples and results of the written report are not fully reproduceable anymore.

The German Association of Actuaries (Deutsche Aktuarvereinigung.V., DAV) is the professional representation of all actuaries in Germany. It was founded in 1993 and has more than 5,400 members today. More than 700 members are involved in thirteen committees and in over 60 working groups as a voluntary commitment.

The given repositories have been created by committees and working groups and serve as an aid for our members and interested persons to support them in their work with machine learning methods and data science issues in an actuarial context.

Please note that the repositories provided on GitHub are published by the DAV. The content of linked websites is the sole responsibility of their operators. The DAV is not responsible for the code and data linked to external sources. 
