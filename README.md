### System Requirements and Package Versions

**Tested Operating Systems and Hardware**:
  - Operating System: Windows 10 64-bit
  - Processor: 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz
  - Installed RAM: 32 GB (31.7 GB usable)

**Non-Standard Hardware Requirements**:
  - No special hardware is required. The analysis was performed using RStudio software.
  - Recommended: At least 16 GB of RAM for large datasets (should work with 8 GB).

**R Version and Installation Guide**:
  - R 4.0.0 or later
  - Search for "Download R" and "Download RStudio" online to find the official websites.
  - Follow the instructions on these websites to download and install R and RStudio.
  - Downloading and installing R and RStudio should take around 10 minutes, depending on internet speed, but generally less than 1 hour.


**Instructions for Use and Reproductione**:
  - The R scripts (`.R` or `.Rmd` files) are numbered and include all relevant information on how to run the analysis.
  - Open the first R script named `1_packages_trc_run_this_first.R` to set up the environment.
  - Download the input data and run the R scripts in the provided order to reproduce the results presented in the article.
  - Some key datasets are saved to Data/Intermediate_input folder to save time (see script get_intermediate_data.R)

**R Packages Used**:
  - `tidyverse` (version 2.0.0 or later)
  - `terra` (version 1.7-71 or later)
  - `tmap` (version 3.3-4 or later)
  - `scico` (version 1.5.0 or later)
  - `sf` (version 1.0-15 or later)
  - `here` (version 1.0.1 or later)
  - `tictoc` (version 1.2 or later)
  - `rmapshaper` (version 0.5.0 or later)
  - `countrycode` (version 1.5.0 or later)
  - `readxl` (version 1.4.3 or later)
  - `broom` (version 1.0.5 or later)
  - `tidyr` (version 1.3.1 or later)
  - `flextable` (version 0.9.4 or later)
  - `gridExtra` (version 2.3 or later)
  - `Rfast` (version 2.1.0 or later)
  - `matrixStats` (version 1.2.0 or later)


### Open Source Repository
  - The source code is available at: [GitHub Repository](https://github.com/jpiippon/griddedprot)

### Software License
  - Run license() in R after installing the software. See https://www.r-project.org/Licenses/
  - This software is distributed under the terms of the GNU General Public License (GPL), either Version 2 (June 1991) or Version 3 (June 2007).

