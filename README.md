# Multivariate Joint Modeling Approach
### Example data analysis `R` code for paper "Dynamic Prediction of Overall Survival in Patients with Solid Tumors: A Joint Modeling Framework for Disease Progression and Mortality" 

Progression-free survival (PFS) is increasingly being used as the primary endpoint in cancer clinical trials involving patients with solid tumors. Nevertheless, overall survival (OS) continues to be the gold standard for assessing patient benefit and for evaluating the cost-effectiveness of new cancer drugs. Since the primary analysis is driven by the number of patients who have progressed, mature OS data often aren't available at the time of regulatory and reimbursement decisions concerning new cancer treatments. As a result, a subsequent analysis of OS becomes necessary once the data matures. Proper planning is crucial to ascertain when the mature OS data will be ready. Accurate OS predictions hold significant value for both drug development and patients' end-of-life medical care. Inspired by an advanced renal cell carcinoma (RCC) clinical trial, we put forth a multivariate joint modeling approach that utilizes the dynamics of PFS to predict death times for trial participants. Through Bayesian model averaging, our suggested method improves the accuracy of the OS predictions compared to singular models. We demonstrate this method with an RCC trial case study, wherein our approach consistently provides the most reliable predictions in all tested scenarios, outperforming several alternative modeling strategies. In summary, our proposed method emerges as a promising tool for reliable OS prediction in solid tumor oncology studies.

Two folders are provided:
- `simulation_code` - `R` and `JAGS` code of the simulation studies conducted;
- `case_study_code` - `R` and `JAGS` code of the example data analysis;

To conduct the example data analysis yourself, please download the `case_study_code` folder, set the working directory to that folder, and run with `sbatch.txt`. Please edit the number of simulations and computing cores according to your needs. Datasets used are also provided. 

Contact: sidiwang@umich.edu
