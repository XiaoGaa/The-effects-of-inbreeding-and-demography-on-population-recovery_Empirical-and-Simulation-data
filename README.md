# **Descriptions of the dataset**

Jia Zheng, Ella Rees-Baylis, Thijs Janzen, Zhengwang Zhang, Xiangjiang Zhan, Daiping Wang, Xiang-Yi Li Richter

 

This dataset contains all the documents that have been used to support the results and conclusion in the manuscript titled “*Inbreeding and demography interact to impact population recovery from bottlenecks*”. All the simulation data and empirical data related to this study are stored in this dataset for further scientific validation. This dataset at the peer-review stage is accessible only to the reviewers and editors. Once it is published, readers who are interested in this study are allowed to access the programming code and all the simulation data, but not the empirical data on crested ibis due to its unpublished status.  

\** **

In this dataset, you can find:

1\. **Codes (Model_scripts.zip):**

(1) All the C++ scripts (.cpp) that are used to generate the simulation outcomes of this research.

(2) R script (DataAnalysis_restoration and reintroduction.R) This is the script used to analyse all the simulation data we generate from models and the multiple species data we extracted from the COMADRE dataset.

2\. **Data:**

(1) Empirical_ibis_data.zip contains all the empirical data that we used to generate the breeding biology information and pedigree of the sampled crested ibis population.

(2) COMADRE_data.zip contains the demographic data from the 41 bird species we used to map on Figure 5 in the main text.

(3) Simulation results.zips include all the simulation results we got from running all the C++ scripts. They are also the outcomes we used in analyses to plot figures and draw conclusions. (If you want to run the R script, you have to download and decompress the series of the "Simulation results" zips. We could not manage to upload all the simulation results in one zip file due to the file size limit on GitHub. Sorry for the inconvenience!)

 

**Brief descriptions for specific files.**

(1) **All the C++ scripts and their correspondent .csv files used for simulation outcomes**

*Crested_ibis_model.cpp: *The first model tailored specifically for the ibis system. It generated

 

| **C++ scripts (.cpp)**                      | **Simulation outcomes (.csv)**            | **Results in the manuscript**                                                                                                                                           |
| :------------------------------------------ | :---------------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Crested\_ibis\_model.cpp                    | Population Info\_main                     | The first model tailored specifically for the ibis system. It generated (Figure 1 S1 and S2)                                                                            |
| Fig3. Firework                              | Population Info\_Reintro\_One.csv         | The standard firework simulation model (with no immigrant, S8)                                                                                                          |
| Fig3A. source no and with immigrant. cpp    | Population Info\_FLF\_One                 | The reintroduction model that simultaneously simulate the ‘firework’ approach with and without immigrants (Fig 3A and S9)                                               |
| Fig3B\_sequential.cpp                       | Population Info\_ReintroSeq\_One          | The simulation of sequential approach (Figure3B)                                                                                                                        |
| ParameterZones\_NOIB.cpp                    | Population Info\_NOIC\_all.csv            | Simulation model and outcomes of varying the demographic values in a broder range.  Simulation scenarios of including and excluding inbreeding effects (Figure 4 and 5) |
| S4.1\_founder parents are full siblings.cpp | Population Info\_S1.1.csv                 | Simulation model and results of exploring how relatedness between adult founders affect population restoration outcomes. (S4)                                           |
| S3\_sensitivity analysis\_S-2,-1.5,-0.5.cpp | Population Info\_Sensitivity analysis.csv | Simulation models and outcomes for the sensitivity analysis (S3)                                                                                                        |
| S5.1\_inbreeding effect\_s1.cpp             | Population Info\_S3.1.csv                 | Simulation models and outcomes for exploring the stage-depended inbreeding effects (S5)                                                                                 |
| S6\_inbreeding effect\_s1.cpp               | Population Info\_Density\_15000.csv       | Simulation models and outcomes for analysing density effect on population restoration (S6 and S7)                                                                       |

 

(2) **COMADRE_data.zip**

*long-lived_birds_mortality_clutch_size.csv*: Mortality and clutch size data for 41 species.

*IC_long-lived_birds_papers.zip*: Reference papers used to extract demographic *data.l*

*long-lived_birds.R*: The script used to extract data from the COMADRE dataset.

*analysis_Species_1.csv*: Data used for plotting Supplementary Figure 4.

 

(3) **Empirical_ibis_data.zip**

*Empirical_pedigree.csv:* A complete pedigree of the captive crested ibis population at the reserve.

*Offspring_data.csv: *Hatching and survival data of offspring.

*code for ibis.R: *R script for analysing inbreeding effect over four life stage of early life

*pedigree data analysis.csv, ibis_ped.csv, ibis_ped_summary.csv:* empirical data for creating the pedigree of crested ibises

 

 

 
