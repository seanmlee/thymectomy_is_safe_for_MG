## Changing rates of thymectomy in Myasthenia Gravis: an updated NSQIP analysis

<strong>Daniel Carrera et al. <em>Manuscript submitted for review</em></strong>

This repository contains reproducible code for our research manuscript titled "Changing rates of thymectomy in Myasthenia Gravis: an updated NSQIP analysis
." We will provide the full citation upon publication.

<strong>We will publish final updates to code and `.README` instructions upon publication</strong>
<br>  
<br>  

## Visual Abstract
![plot_logit](https://github.com/seanmlee/thymectomy_is_safe_for_MG/assets/82421211/421fbf24-9d41-440a-a7fa-ab432e2b8c32)
<br>  
<br>  

![plot_year](https://github.com/seanmlee/thymectomy_is_safe_for_MG/assets/82421211/e8ad0666-5509-447f-9a10-e3d59493414b)
<br>  
<br>  

![plot_year_minimal](https://github.com/seanmlee/thymectomy_is_safe_for_MG/assets/82421211/9acea37b-96de-45fd-9051-69d6a987d993)
<br>  
<br>  

## To use this repository

- Clone the repository and open the `./thymectomy_is_safe_for_MG.Rproj` R Project file
- This repository uses `renv` dependency management, so all software, package, and dependency version information is listed in the `./renv.lock` file and will automatically configure once prompted
- All scripts are in the `./scripts` directory
- You will need to add your own NSQIP database file into the root directory, saved as `nsqip.db`; the only table needed is `puf`; make sure all variable names are lowercase
- To run the entire analysis, use the script `00_source.R`; this will run each script in order
- Please send questions to smlee@gwu.edu
<br>  
<br> 
