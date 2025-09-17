# SMI_GLP
This repository contains code for examining usage, safety, and effectiveness of Glucagon-like peptide-1 receptor agonists (GLP-1RAs) among individuals with severe mental illness (SMI). 

## Code 
The `Code` folder contains R code organized into the following sub-folders:
- `0_Code_List_Generation`: Code to generate CPRD code lists for SMI, depression, antidiabetic medication, etc.
- `1_Data_Extraction`: Code to extract the relevant patient files from CPRD using code lists.
- `2_Data_Cleaning`: Code to clean and process the extracted data.
- `3_Power_Calculations`: Code to perform feasibility and power calculations for applying Cox proportional hazards regression models for survival outcomes.

## Code Lists
The `Code_Lists` folder contains sub-folders that each contain CPRD GOLD and Aurum code lists for specific conditions and medications. For medications, the `medication_reference.xlsx` spreadsheet provides a list of all the formulations and brand names for the medications included in the code lists. 
