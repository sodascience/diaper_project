## Data-InfrAstructure for Parents and ChildRen (DIAPER)

Scripts pertaining to data pre-processing and analysis to investigate the role of poverty in maternal and child perinatal health outcomes in the Netherlands.

-   DIAPER rivm [website](https://www.rivm.nl/monitoren-zwangerschap-en-geboorte/diaper)

-   All of the analyses are done in the [CBS](https://www.cbs.nl/en-gb/our-services/customised-services-microdata/microdata-conducting-your-own-research) microdata environment.

### Scripts organization
The '01_preprocessing_scripts' folder contains all the scripts necessary to load data, join the dataset with CBS tables, and clean the data before further analyses. 
-   Make sure to change the 'file_paths' script to match the paths to your tables of interest.
-   If you only need a household ID and not object ID, you can skip the 03 script.
-   The '04_merge_cbs' script is the one that reads a CBS table and joins it to your dataset.
-   Script 05 is necessary in the case that you are extracting data from longitudinal tables.
-   The last script '06_cleanup' is specific to the variable processing needed for this project. NB: Income type CBS variables usually need to be processed since they contain negative values for NA. 

