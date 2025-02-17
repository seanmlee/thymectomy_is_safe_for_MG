## Changing rates of thymectomy in Myasthenia Gravis: an updated NSQIP analysis

<strong>Daniel Carrera et al. <em>Manuscript submitted for review</em></strong>

This repository contains reproducible code for our research manuscript titled "Changing rates of thymectomy in Myasthenia Gravis: an updated NSQIP analysis
." We will provide the full citation upon publication.

<strong>We will publish final updates to code and `.README` instructions upon publication</strong>
<br>  
<br>  

## Visual Abstract
![logistic](https://github.com/user-attachments/assets/2f5807a6-df47-4786-9d35-93b23d13a1f0)
*Figure 1. Predicted probabilities and 95% confidence intervals from logistic models fitted to data representing broad and narrow composite outcomes, as functions of myasthenia gravis (MG) diagnosis in a propensity-weighted cohort of thymectomy patients from the National Surgical Quality Improvement Program database.*
<br>  
<br>  

![image](https://github.com/user-attachments/assets/e2fcc447-4419-44b9-9acf-f9b69720c59b)
*Figure 2. Rates of a) thymectomy and b) minimally invasive thymectomy for myasthenia gravis (MG). Circles represent actual rates. Solid lines represent estimated rates from National Institutes of Health Joinpoint Regression Program Version 5.0 models. Dashed vertical lines represent publication year of Alshaik et al (2016).*
<br>  
<br>  

## To use this repository

- Clone the repository and open the `./thymectomy_is_safe_for_MG.Rproj` R Project file
- This repository uses `renv` dependency management; all software, package, and dependency version information is listed in the `./renv.lock` file and will automatically configure once prompted
- All scripts are in the `./scripts` directory
- You will need to create a new directory called `./nsqip_raw` and add your own NSQIP text files to it
  - See `scripts/00_compile_nsqip.R` to ensure that your raw NSQIP text files are labeled appropriately
- Execute scripts in their numbered order to format NSQIP data, perform all analyses, and generate all figures and tables
- Please direct questions to smlee@gwu.edu
<br>  
<br> 
