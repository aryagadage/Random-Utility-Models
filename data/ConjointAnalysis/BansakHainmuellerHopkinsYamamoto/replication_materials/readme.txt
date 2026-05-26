This replication archive contains three main folders:

1. data
2. code
3. results

All contents of this replication archive should be maintained within their current directory structure for the R scripts to properly access necessary files. The R packages required are 'ggplot2', 'ggthemes', 'lmtest', 'parallel', 'sandwich', and 'texreg'.

(1) The 'data' folder contains 5 (csv format) data files. 

a. cand_s1.csv is the data for Stage 1 of Study 1 (political candidate application).
b. cand_s2_MTurk.csv is the data for the MTurk respondents for Stage 2 of Study 1 (political candidate application).
c. cand_s2_SSI.csv is the data for the SSI respondents for Stage 2 of Study 1 (political candidate application).
d. hotel_s1.csv is the data for Stage 1 of Study 2 (hotel application).
e. hotel_s2.csv is the data for Stage 2 of Study 2 (hotel application).

The format is such that rows correspond to respondent-profile observations (i.e. unique conjoint profiles evaluated by respondents). The variables prefixed by "r_" are respondent-level variables. 

- r_id is the (anonymous) unique respondent identifier.
- r_gender is respondent's gender, with 1 denoting male and 2 denoting female.
- r_age is respondent's age.
- r_educ is respondent's educational level, with 1 denoting less than high school, 2 denoting high school, 3 denoting some college, 4 denoting 2-year college degree, 5 denoting 4-year college degree, and 6 denoting graduate degree.
- r_income is respondent's annual household income, with 1 denoting $0 - $24,999, 2 denoting $25,000 - $49,999, 3 denoting $50,000 - $74,999, 4 denoting $75,000 - $99,999, 5 denoting $100,000 - $149,999, 6 denoting $150,000 - $199,999, and 7 denoting $200,000+.
- r_party is respondent's party affiliation, with 1 denoting Republican, 2 denoting Democrat, 3 denoting Independent, and 4 denoting Other.
- r_strength denotes whether a respondent's party affiliation is strong (1) or not very strong (2), for respondents who affiliate with either the Republican or Democratic parties.
- r_lean denotes whether a respondent leans toward the Democratic party (-1), toward the Republican party (1), or neither (0), for those respondents who do not affiliate with with either the Republican or Democratic parties.

The following variables describe key study dimensions:

- profile denotes the profile identification number for a given respondent.
- condition denotes the experimental treatment condition (the number of filler attributes included in the conjoint profiles) in the Stage 2 data files.
- wave denotes the respondent wave in cand_s2_MTurk.csv (i.e. the wave of MTurk respondents for Stage 2 of Study 1).
- partisan indicates whether or not a respondent reports a party identification (including leaning toward a party) in cand_s2_MTurk.csv and cand_s2_SSI.csv.

The following variables describe the respondents' evaluations of the individual conjoint profiles in the Stage 2 data files:

- pref is an indicator for whether or not a conjoint profile was selected as the preferred profile in the forced choice task.
- rate is a 1-7 rating of the conjoint profile.

The following variables are the fixed attributes in each study:

- cage, cparty, chealthcare, and cmarriage are the fixed attributes used in the conjoint profiles for both stages of Study 1, and cpartyp, chealthcarep, and cmarriagep are the recoded versions (in concordance with self-reported party identification) of the fixed attributes used for analysis in Stage 2 of Study 1 (see main text of study for details).
- cfloor, cfurniture, cview, and cinternet are the fixed attributes used in the conjoint profiles for both stages of Study 2.

Other variables pertain to the filler attributes. See descriptions in main text and supplementary materials for more details. In the Stage 2 data files, a value of '0' indicates that a particular filler attribute was not seen by a respondent (i.e. the respondent's set of randomly drawn filler attributes did not include the filler attribute in question), whereas a value of 'NA' indicates that a particular filler attribute was not even in the pool of available attributes for a particular respondent.


(2) The 'code' folder contains four subfolders.

The subfolder 'bootstrap_estimates' contains the code performing the bootstrapped estimates of the partial R2 results presented in Figures 5, 7, 9, and A7. These code files save the results as .Rdata files and deposit them in the 'results' folder, as described later.

The subfolder 'build_figures' contains scripts for replicating all figures showing results in both the main text and supplementary materials.

The subfolder 'build_tables' contains scripts for replicating all tables showing results in both the main text and supplementary materials.

The subfolder 'helper_scripts' contains scripts that are sourced and used by the scripts in the other folders.


(3) The 'results' folder contains three subfolders. 

The subfolder 'boots' contains the results from the partial R2 bootstrap estimation procedure.

The subfolder 'figures' contains all figures showing results in both the main text and supplementary materials.

The subfolder 'tables' contains regression tables reported in the supplementary materials.