# High-Dimensional Multivariate Longitudinal Data for Survival Analysis of Cardiovascular Event Prediction in Young Adults: Insights from a Comparative Explainable Study

This repository is the code base for the following paper:  
Nguyen, H.T., Vasconcellos, H.D., Keck, K., Reis, J.P., Lewis, C.E., Sidney, S., Lloyd-Jones, D.M., Schreiner, P.J., Guallar, E., Wu, C.O. and Lima, J.A., 2022. High-Dimensional Multivariate Longitudinal Data for Survival Analysis of Cardiovascular Event Prediction in Young Adults: Insights from a Comparative Explainable Study.  
https://doi.org/10.21203/rs.3.rs-2082433/v1 


### Request Data: https://www.cardia.dopm.uab.edu/ 

##  WORKFLOW CODE:
### Main manuscript: 

Start:   
extract_outcome_space.R  
extracting outcome  
input: Mortality and Morbidity data up to 2020 in SAS format   
output: cvd outcome and mortality time-to-event in csv format  
"cvd_outcome_up_to_2020.csv"  

-->  year_datacompile.R  
data assemble  
input: exam data files in SAS format  
output: single exam dictionary and unimputed feature space  
output files: exam_year, "_unimputed_featurespace.csv", exam_year, "_all_vars_dictionary.csv"  

--> longi_data_availability.R  
combine, unite, and arrange them to make a dictionary of longitudinal variables   
input: variable dictionary from each year  
output: longitudinal variable dictionaries: one for all variables, one for all non-questionnaire variables, and one for ascvd risk factors  
output files: "longi_data_avalability_dictionary_ascvd_risk_factors2.csv", "longi_data_avalability_dictionary_no_questionnaire2.csv"  



#### Analysis using only traditional (ASCVD) risk factors:

--> longi_data_assemble_ascvd_var.R  
Input: longitudinal data dictionary, plus the actual data feature space from each year, plus the time-to-event outcome data  
input files: "longi_data_avalability_dictionary_ascvd_risk_factors2.csv", exam_year"_unimputed_featurespace.csv"  
Output: longitudinal data sheet, in long format, for all variables in the input data dictionary  
output files: "data_longi_long_format_ascvd_risk_factors.csv", "data_longi_long_format_ascvd_risk_factors_removed_missing_data.csv"  
"data_longi_long_format_ascvd_risk_factors_with_missing_data.csv"  
cohort selection: 3639 participants (including missing data)  



--> table_1.R    
Filter out cohort to get the final analysis cohort (flowchart in ppt presentation). Make table 1 for everyone in Y0 Exam, final analysis analysis cohort in Y0 and Y15 Exam    
Additional output: 'subjects_in_final_analysis_cohort.csv' (3551 subjects with filtering criteria in the flowchart)  


--> data_split.R    
split data into stratified cross-validation folds  

--> Model training and evaluation:  
Four different approaches to handle longitudinal are implemented:  
1/ running_joint_models_y15_ascvd_var.R  
2/ running_y15_models_ascvd_var.R  
3/ tsfresh_ascvd_var.ipynb --> running_time_series_extracted_features_ascvd_var_removed_correlation_other_algorithms.R  
4/ prep_data_for_dynamic_deephit.R --> CARDIA_longi_dynamic_deephit.ipynb  
  
  
--> get_aggregated_results.R  
and calculate_integratedAUC_ascvd_var.R  
Get performance results in tabulated format  
  
--> plot_dynamic_auc_c_index.R     





#### Extending analysis to more covariates:  

--> longi_data_dictionary_filter.R  
Input: longitudinal data dictionary, plus the actual data feature space from each year  
input files: "longi_data_avalability_dictionary_no_questionnaire2.csv", exam_year"_unimputed_featurespace.csv"  
Output: longitudinal data sheet, in long format, for all variables in the input data dictionary  
also output: "longi_data_availability_dictionary_filtered.csv"  

--> bubble_chart_all_vars.R  
visualize/organize longi data availability in bubble chart format  
input: 'longi_data_avalability_dictionary_filtered.csv'  
output: 'longi_data_avalability_dup_rm_bubble_format2.csv' -- lots of variables but with no variable description or variable group  

Manually go through the var dict and decide which variables to include  
longi_data_avalability_dup_rm_bubble_format3_corrected.xlsx  


--> longi_data_assemble.R  
create a combined dictionary, assemble data  
Input: longitudinal data dictionary, dictonary in bubble format, and the actual data feature space from each year  
Output: longitudinal data sheet, in long format, for all variables in the input data dictionary  

--> table_1_expanded_var.R   
Create Table 1 of the manuscript  

--> data_split_expanded_var.R  
Split data into training, validation, and testing sets for each fold of 5-times-5-fold cross-validation  


--> Model training and evaluation:  
Seven different approaches to handle longitudinal are implemented:    
1/running_y15_models_expanded_var.R  
2/running_baseline_models_expanded_var.R  
3/prep_data_for_dynamic_deephit.R --> CARDIA_longi_dynamic_deephit_expanded_var.ipynb --> evaluate_dynamic-deephit_pred_proba_expanded_var.R  
4/prep_data_for_dynamic_deephit.R --> tsfresh_ascvd_var.ipynb --> running_time_series_extracted_features_ascvd_var_removed_correlation_other_algorithms.R  
5/running_joint_models_y15_expanded_var.R  
6/running_concat_models_expanded_var.R  
7/running_traj_trcowv_models_expanded_var.R  

  
Tabulate model performance results:  
--> get_aggregated_results.R  
--> calculate_integratedAUC_expanded_var.R  


Plotting:
Fig 2 - model performance: plot_dynamic_auc_c_index.R  
Fig 3 - plot_partial_dependence.R, plot_VIMP_RSF.R, plot_traj_clusters.R  
Fig 4 - shap_rsf_traj_model.ipynb, shap_rsf_tsfresh_model.ipynb  
Fig 5 - TIME_rsf-tsfresh_expanded_var_include_demo_var_effect_size_0_1.ipynb  

--------------------------------------------------------------------------------------
### Supplementary Materials:    

#### Race-specific analysis:   
--> data_split_expanded_var_race_specific.R  
Split data into training, validation, and testing sets for each fold of 10-fold cross-validation  


--> Model training and evaluation:  
1/running_y15_models_expanded_var_race_specific.R  
2/running_time_series_extracted_features_ascvd_var_race_specific.R  
3/running_traj_trcowv_models_expanded_var_race_specific.R  
6/running_concat_models_expanded_var_race_specific.R  
  
  
--> Get performance results:  
calculate_integratedAUC_expanded_var_race_specific.R  
get_aggregated_results_race_specific.R  

--> Get VIMP and plot:  
ave_VIMP_race_specific.R  
make_heatmap_for_concat_models_race_specific.R  
plot_VIMP_RSF_race_specific.R  
plot_traj_cluster_race_specific.R    


#### Dynamic-DeepHit for all subjects:     
CARDIA_longi_dynamic_deephit_expanded_var-all_subjects.R  

#### Experiment with different clustering criteria:  
running_traj_models_expanded_var_aic_bic.R  
running_traj_models_expanded_var_exp_with_n_clusters.R  
plot_traj_clusters.R  

Miscellanea:   
The '/snippet' folder: contains helper functions for the excecution of the above code files in the main folder   
The '/csv_files' folder: contains the variable dictionaries that entail all excluded and included variables
