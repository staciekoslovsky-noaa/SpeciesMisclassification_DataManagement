# Species Misclassification Data Management

This repository stores the code associated with managing data associated with species misclassification evaluations across ice seal projects. Code numbered 0+ are intended to be run sequentially as the data are available for processing. Code numbered 99 are stored for longetivity, but are intended to only be run once to address a specific issue or run as needed, depending on the intent of the code.

The data management processing code is as follows:
* **SpeciesMisclass_BOSS_01_ExtractDataFromDB4Import.txt** - code to extract information from BOSS database schema for import into species_misclass schema
* **SpeciesMisclass_BOSS_02_CopyFiles2MisclassFolder.R** - code to copy files used for misclassification to the central misclassification folder on the LAN
* **SpeciesMisclass_BOSS_03_GenerateImageListAndAnnotations2Review.R** - code to generate image lists and annotations that need to be reviewed by staff
* **SpeciesMisclass_BOSS_04_ImportAnnotationsAfterReview.R** - code to import annotations to the species_misclass schema after review is complete

* **SpeciesMisclass_CHESS_01_RandomSelection_4SealID_rd1.R** - code to generate random selection of seals to be reviewed during the first round of species misclassification for ChESS
* **SpeciesMisclass_CHESS_01_RandomSelection_4SealID_rd2.R** - code to generate random selection of seals to be reviewed during the second round of species misclassification for ChESS
* **SpeciesMisclass_CHESS_02_SpeciesIDFormatted4Analysis_rd1.R** - code to format the first round of species misclassification data for analysis
* **SpeciesMisclass_CHESS_02_SpeciesIDFormatted4Analysis_rd2.R** - code to format the second round of species misclassification data for analysis
* **SpeciesMisclass_CHESS_03_ImportResults2DB.R** - code to import species misclassification results into the DB

* **SpeciesMisclass_JoBSS_01_RandomSelection_1stQuartile.R** - code to generate random selection of seals to be reviewed during the first round of species misclassification for JoBSS
* **SpeciesMisclass_JoBSS_01_RandomSelection_3rdQuartile.R** - code to generate random selection of seals to be reviewed during the second round of species misclassification for JoBSS
* **SpeciesMisclass_JoBSS_02_ImportResults.R** - code to import species misclassification results into the DB
* **SpeciesMisclass_JoBSS_03_SpeciesIDFormatted4Analysis.R** - code to format the species misclassification data for analysis

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.