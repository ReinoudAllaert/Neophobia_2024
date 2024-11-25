# OSF Project: Neophobia Across Social Contexts in Juvenile Herring Gulls

This repository contains all the files related to the study on neophobia across social contexts in juvenile Herring Gulls. Below is a description of each file, along with guidance for additional materials.

## File Descriptions

### Data
- **raw_data/**: Contains raw data files collected during the experiments. All raw data files have logically named columns and should be easy to interpret. Included files:
  - `2024_chick_data.xlsx`: Primary dataset of individual-level behavioural observations.
  - `BORIS_IRR_SB.csv`: Behavioural coding data from one observer for inter-rater reliability.
  - `BORIS_RA.csv`: Behavioural coding data from a second observer.
  - `BORIS_SK.csv`: Behavioural coding data from a third observer.
  - `metadata.xlsx`: Metadata for the raw data files.

- **processed_data/**: Contains processed data that has been cleaned and prepared for analysis, including:
  - `neophobia_data.csv`: Fully processed dataset used for statistical analysis.
  - `neophobia_data_IRR.csv`: Data prepared for inter-rater reliability analysis.
  - `videos_for_doublecoding.csv`: Information on the videos selected for double coding.

### Scripts
- **IRR.R**: Script for calculating inter-rater reliability (IRR) using Cohen’s Kappa for the coded behavioural data.
- **preprocessing.R**: Script for cleaning and preprocessing the raw data before analysis.
- **analysis.R**: Main R script containing statistical models and data analysis pipeline.
- **analysis.Rmd**: R Markdown file with detailed steps for data analysis and result interpretation.
- **analysis.html**: Rendered HTML report of the `analysis.Rmd` file.
- **plotting.R**: Script for generating plots and visualizations, including figures used in the manuscript.

### Environment and Configuration
- **renv/**: Directory containing the `renv` environment for package management, ensuring reproducibility of the R environment.
- **renv.lock**: Lockfile for the `renv` environment, ensuring the same package versions are used.
- **Neophobia_2024.Rproj**: RStudio project file for this study.
- **.Rprofile**: Configuration file for the R environment.
- **.gitignore**: Specifies files and directories to be ignored by git.
- **.gitattributes**: Defines attributes for specific files in the repository.

---

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
   - Run `analysis.Rmd` to perform the statistical analyses. 
4. **Plotting**:
   - Run `plotting.R` to generate visualizations of the data, including figures used in the manuscript.
## Contact
For any questions or collaborations, please contact Reinoud Allaert at reinoud.allaert@ugent.be.