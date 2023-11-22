# Species Misclassification Data Management

This repository stores the code associated with managing data associated with species misclassification evaluations across ice seal projects. Code numbered 0+ are intended to be run sequentially as the data are available for processing. Code numbered 99 are stored for longetivity, but are intended to only be run once to address a specific issue or run as needed, depending on the intent of the code.

The data management processing code is as follows:
* **SpeciesMisclass_CHESS_01_RandomSelection_4SealID_rd1.R** - code to generate random selection of seals to be reviewed during the first round of species misclassification for ChESS
* **SpeciesMisclass_CHESS_01_RandomSelection_4SealID_rd2.R** - code to generate random selection of seals to be reviewed during the second round of species misclassification for ChESS
* **SpeciesMisclass_CHESS_02_SpeciesIDFormatted4Analysis_rd1.R** - code to format the first round of species misclassification data for analysis
* **SpeciesMisclass_CHESS_02_SpeciesIDFormatted4Analysis_rd2.R** - code to format the second round of species misclassification data for analysis
* **SpeciesMisclass_CHESS_03_ImportResults2DB.R** - code to import species misclassification results into the DB
* **SpeciesMisclass_JoBSS_01_RandomSelection_1stQuartile.R** - code to generate random selection of seals to be reviewed during the first round of species misclassification for JoBSS
* **SpeciesMisclass_JoBSS_01_RandomSelection_3rdQuartile.R** - code to generate random selection of seals to be reviewed during the second round of species misclassification for JoBSS
* **SpeciesMisclass_JoBSS_02_ImportResults.R** - code to import species misclassification results into the DB
* **SpeciesMisclass_JoBSS_03_SpeciesIDFormatted4Analysis.R** - code to format the species misclassification data for analysis