# Neophobia in Juvenile Herring Gulls

## Project Overview

This repository contains all data and code to reproduce the results from XXX study.

## Repository Structure

- **raw_data/**: Contains raw data files collected during the experiments.
- **processed_data/**: Contains processed data that has been cleaned and prepared for analysis.
- **renv/**: The directory containing the renv environment, ensuring reproducibility of the R environment used for analysis.
- **.Rprofile**: Configuration file for the R environment.
- **.gitignore**: Specifies files and directories to be ignored by git.
- **IRR.R**: Script for calculating inter-rater reliability (IRR) using Cohen’s Kappa for the coded behavioral data.
- **Neophobia_2024.Rproj**: RStudio project file for the study.
- **analysis.R**: Main analysis script where statistical models and data analysis are performed.
- **plotting.R**: Script for generating plots and visualizations of the data.
- **preprocessing.R**: Script for preprocessing the raw data before analysis.
- **renv.lock**: Lockfile for the renv environment, ensuring the same package versions are used.

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/Neophobia_2024.git

2. Set up the R environment using `renv`:
    ```bash
   renv::restore()

## Usage

1. **Data Preprocessing**:
   - Run `preprocessing.R` to prepare the raw data BORIS output for analysis.

2. **Inter-Rater Reliability**:
   - Run `IRR.R` to prepare BORIS data of the external coder, calculate the inter-rater reliability of the behavioural coding.

3. **Data Analysis**:
   - Run `analysis.R` to perform the statistical analyses. 

4. **Plotting**:
   - Run `plotting.R` to generate visualizations of the data, including figures used in the manuscript.

## Contact

For any questions or collaborations, please contact Reinoud Allaert at reinoud.allaert@ugent.be.


