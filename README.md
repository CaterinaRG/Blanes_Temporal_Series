# Blanes Temporal Series

This repository contains code and data included in Giner et al. (2018), '_Quantifying long-term recurrence in planktonic microbial eukaryotes_'


#### Analysis ####
>- **R_BlanesTemporalAnalysis_CRGiner.R:** R analysis used in the manuscript.

For more details of the _recurrence index_ developed in this project look at the repository ‘recurrence index’ where the code is available.


#### Data ####
>- **EnvironmentalData_2004_2013_all.txt**: Environmental data for the samples. Containing information on the temperature, salinity, nutrients.

>- **OTU_table99_PN_noMetNuclChar_DEF2.txt**: Abundance table for the picoeukaryotic and nanoeukaryotic fraction from Blanes Bay Microbial Observaory (BBMO) from 2004 to 2013 with taxonomy. It contains the raw number of reads (i.e without rarefaction). 
Metazoan, Nucleomorph and Charophyta OTUs have been removed.

>- **OTUtable_PICO_subs7553.txt**: Abundance table for the picoeukaryotic fraction, subsampled at 7553 reads/sample.

>- **OTUtable_PN_subs5898.txt**: Abundance table for the picoeukaryotic and nanoeukaryotic fraction, subsampled at 5898 reads/sample.


The fastq raw data can be obtained from the [ENA](https://www.ebi.ac.uk/ena) under accession number: PRJEB23788
