# OSF Project: Neophobia Across Social Contexts in Juvenile Herring Gulls

This repository contains all the files related to the study on neophobia across social contexts in juvenile Herring Gulls. Below is a description of each file, along with guidance for additional materials.

<<<<<<< HEAD
This repository contains all data and code to reproduce the results from the study on neophobia in juvenile Herring Gulls, investigating the role of social context on behaviour and learning.

---
=======
## File Descriptions
>>>>>>> 8eebc8a16c33fef3e90a0ad2e1a94faeca93acad

1. **PCIRR-Stage1-Snapshot.pdf**  
   Snapshot of the Stage 1 Registered Report (RR) as submitted to PCI Registered Reports.  
   
2. **Stage1RR.pdf**  
   The initial submission of the Stage 1 Registered Report.  
  
3. **Stage1RR_revised.pdf**  
   The revised version of the Stage 1 RR, updated based on reviewer feedback.  
   
4. **Stage1RR_revision2.pdf**  
   The second revision of the Stage 1 RR after further reviewer feedback.  
  
5. **Stage2RR.pdf**  
   The final Stage 2 Registered Report, reflecting the completed study and analyses.  
   
6. **Stage_2_track_changes.pdf**  
   A version of the Stage 2 RR with changes highlighted.  
  
---

<<<<<<< HEAD
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
=======
## Additional Information
>>>>>>> 8eebc8a16c33fef3e90a0ad2e1a94faeca93acad

- **GitHub Repository**:  
  This project is linked to a GitHub repository available at:  
  [https://github.com/ReinoudAllaert/Neophobia_2024](https://github.com/ReinoudAllaert/Neophobia_2024).  
  The repository contains:  
  - Scripts and code for data analysis, all raw data to reproduce the results.  
  - A second README file with detailed information about the analysis pipeline, including instructions for reproducing results.

- **How to Cite**:  
  Please include a link to this OSF project page and follow citation guidelines provided in the manuscript when referencing this work.

---
