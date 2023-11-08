* Encoding: UTF-8.

**** Analysis August 4
    
* ********Exclusion criteria
 * 1) no treatment received
 * 2) treatment with chelators (deferasirox)

FREQUENCIES VARIABLES=Wel_Geen_Proef_Behandeling
  /ORDER=ANALYSIS.


DATASET ACTIVATE DataSet1.
CROSSTABS
  /TABLES=patientID BY Wel_Geen_Proef_Behandeling
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

*exclude patients with no treatment or treated with cheletors.


USE ALL.
COMPUTE filter_$=(Wel_Geen_Proef_Behandeling = 1 | Wel_Geen_Proef_Behandeling = 2).
VARIABLE LABELS filter_$ 'Wel_Geen_Proef_Behandeling = 1 | Wel_Geen_Proef_Behandeling = 2 (FILTER)'.    
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


FREQUENCIES VARIABLES=Wel_Geen_Proef_Behandeling
  /ORDER=ANALYSIS.

* transform mobilized iron in grams
    
COMPUTE MobFe=GemobiliseerdFe_einde_depletie / 1000.
EXECUTE.


FREQUENCIES VARIABLES=MobFe
  /ORDER=ANALYSIS.

*dichotomize MobFe in  major iron overload (above 4 g) and minor iron overload (below 4g)

RECODE MobFe (MISSING=SYSMIS) (Lowest thru 4=0) (4 thru Highest=1) INTO Major_Iron_overload.
VARIABLE LABELS  Major_Iron_overload 'Major Iron Overload'.
EXECUTE.

* ******** Plots


GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Major_Iron_overload LIIMRI1 MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: Major_Iron_overload=col(source(s), name("Major_Iron_overload"), unit.category())
  DATA: LIIMRI1=col(source(s), name("LIIMRI1"))
  DATA: id=col(source(s), name("$CASENUM"), unit.category())
  GUIDE: axis(dim(1), label("Major_Iron_overload"))
  GUIDE: axis(dim(2), label("berekend door LIC/leeftijd"))
  GUIDE: text.title(label("Simple Boxplot of berekend door LIC/leeftijd by Major_Iron_overload"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: schema(position(bin.quantile.letter(Major_Iron_overload*LIIMRI1)), label(id))
END GPL.

GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Major_Iron_overload LIIMRI1 MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO SUBGROUP=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: Major_Iron_overload=col(source(s), name("Major_Iron_overload"), unit.category())
  DATA: LIIMRI1=col(source(s), name("LIIMRI1"))
  GUIDE: axis(dim(1), label("Major_Iron_overload"))
  GUIDE: axis(dim(2), label("berekend door LIC/leeftijd"))
  GUIDE: text.title(label("Scatter Plot of berekend door LIC/leeftijd by Major_Iron_overload"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: point(position(Major_Iron_overload*LIIMRI1))
END GPL.

SORT CASES  BY Major_Iron_overload.
SPLIT FILE SEPARATE BY Major_Iron_overload.

FREQUENCIES VARIABLES=LIIMRI1
  /FORMAT=NOTABLE
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN MEDIAN
  /ORDER=ANALYSIS.
SPLIT FILE OFF.


* ******** ROC curve analysis

ROC ANALYSIS LIIMRI1 BY Major_Iron_overload (1)
  /MISSING USERMISSING=EXCLUDE
  /CRITERIA CUTOFF=INCLUDE TESTPOS=LARGE DISTRIBUTION=FREE CI=95
  /DESIGN PAIR=FALSE
  /PLOT CURVE=ROC(REFERENCE) MODELQUALITY=TRUE
  /PRINT SE=TRUE COORDINATES=ROC CLASSIFIER=FALSE.

* optimal cut-off.
DATASET ACTIVATE DataSet1.
RECODE LIIMRI1 (MISSING=SYSMIS) (Lowest thru 3.4=0) (3.4 thru Highest=1) INTO LIIabove3.4.
VARIABLE LABELS  LIIabove3.4 'Optimal cut-off for LII-MRI'.
EXECUTE.

CROSSTABS
  /TABLES=LIIboven2 BY Major_Iron_overload
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=LIIabove3.4 BY Major_Iron_overload
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

** LII-MRI above 2

SORT CASES  BY Major_Iron_overload.
SPLIT FILE SEPARATE BY Major_Iron_overload.
* Compute 95% CI for  Sensitivity.
PROPORTIONS
  /ONESAMPLE LIIboven2 TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE
  /SUCCESS VALUE=LAST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.

  * Compute 95% CI for Specificity.
PROPORTIONS
  /ONESAMPLE LIIboven2 TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE
  /SUCCESS VALUE=FIRST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.


** LII-MRI above 3.4
* Compute 95% CI for  Sensitivity.
PROPORTIONS
  /ONESAMPLE LIIabove3.4 TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE
  /SUCCESS VALUE=LAST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.

  * Compute 95% CI for Specificity.
PROPORTIONS
  /ONESAMPLE LIIabove3.4 TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE
  /SUCCESS VALUE=FIRST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.
SPLIT FILE OFF.


ROC LIIabove3.4 LIIboven2 BY Major_Iron_overload (1)
  /PLOT=CURVE(REFERENCE)
  /PRINT=SE
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(LARGE) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.

*PPV and NPV.

SORT CASES  BY LIIboven2.
SPLIT FILE SEPARATE BY LIIboven2.
* Compute 95% CI for  PPV.
PROPORTIONS
  /ONESAMPLE Major_Iron_overload TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE 
  /SUCCESS VALUE=LAST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.

  * Compute 95% CI for NPV.
PROPORTIONS
  /ONESAMPLE Major_Iron_overload TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE
  /SUCCESS VALUE=FIRST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.
SPLIT FILE OFF.



SORT CASES  BY LIIabove3.4.
SPLIT FILE SEPARATE BY LIIabove3.4.
* Compute 95% CI for  PPV.
PROPORTIONS
  /ONESAMPLE Major_Iron_overload TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE 
  /SUCCESS VALUE=LAST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.

  * Compute 95% CI for NPV.
PROPORTIONS
  /ONESAMPLE Major_Iron_overload TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE
  /SUCCESS VALUE=FIRST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.
SPLIT FILE OFF.




***************************************************** Logistic regression.

FREQUENCIES VARIABLES=Alcohol_overconsumption BMI Sex HFE_Homozygoot_C282Y  Major_Iron_overload
  /ORDER=ANALYSIS.


CROSSTABS
  /TABLES=Alcohol_overconsumption BY Major_Iron_overload
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

*Collinearity.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Major_Iron_overload
  /METHOD=ENTER Alcohol_overconsumption BMI HFE_Homozygoot_C282Y Sex  LIIabove3.4.

* Are sex, BMI, alcohol overconsumption, C282Y homozygous genetic test associated with major iron overload?.
*to get the p-values based on LRI.
LOGISTIC REGRESSION VARIABLES Major_Iron_overload
   /METHOD=BSTEP(LR)   Alcohol_overconsumption BMI HFE_Homozygoot_C282Y Sex  
  /SAVE=PRED COOK
  /CASEWISE OUTLIER(2)
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(1) ITERATE(20) CUT(0.5).


COMPUTE ln_BMI=ln(BMI).
EXECUTE.

* test linearity of BMI effect.
LOGISTIC REGRESSION VARIABLES Major_Iron_overload
   /METHOD=ENTER   Alcohol_overconsumption BMI HFE_Homozygoot_C282Y Sex  
     /METHOD=ENTER  ln_BMI*BMI
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(1) ITERATE(20) CUT(0.5).


DESCRIPTIVES VARIABLES=COO_2
  /STATISTICS=MEAN STDDEV MIN MAX.

*to get the p-values based on LRI.
LOGISTIC REGRESSION VARIABLES Major_Iron_overload
   /METHOD=BSTEP(LR)  LIIabove3.4 Alcohol_overconsumption BMI HFE_Homozygoot_C282Y Sex  
  /SAVE=PRED COOK
  /CASEWISE OUTLIER(2)
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(1) ITERATE(20) CUT(0.5).


* test linearity of BMI effect.
LOGISTIC REGRESSION VARIABLES Major_Iron_overload
   /METHOD=ENTER   LIIabove3.4 Alcohol_overconsumption BMI HFE_Homozygoot_C282Y Sex  
     /METHOD=ENTER  ln_BMI*BMI
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(1) ITERATE(20) CUT(0.5).



DESCRIPTIVES VARIABLES=COO_3
  /STATISTICS=MEAN STDDEV MIN MAX.

USE ALL.
COMPUTE filter_$=(COO_3 >= 1).
VARIABLE LABELS filter_$ 'COO_3 >= 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=Alcohol_overconsumption BMI Sex HFE_Homozygoot_C282Y Major_Iron_overload 
    LIIabove3.4
  /ORDER=ANALYSIS.


* sensitivity analysis without influential outlier.


COMPUTE filter_$=(COO_3  <  1).
VARIABLE LABELS filter_$ 'COO_3  <  1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

LOGISTIC REGRESSION VARIABLES Major_Iron_overload
   /METHOD=BSTEP(LR)  LIIabove3.4 Alcohol_overconsumption BMI HFE_Homozygoot_C282Y Sex  
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(1) ITERATE(20) CUT(0.5).
* excluding the outlier, alcohol overconsumption is droped out.


USE ALL.
COMPUTE filter_$=(Wel_Geen_Proef_Behandeling = 1 | Wel_Geen_Proef_Behandeling = 2).
VARIABLE LABELS filter_$ 'Wel_Geen_Proef_Behandeling = 1 | Wel_Geen_Proef_Behandeling = 2 (FILTER)'.    
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


*to get the p-values based on LRI.
LOGISTIC REGRESSION VARIABLES Major_Iron_overload
   /METHOD=BSTEP(LR)  LIIabove3.4 Alcohol_overconsumption BMI HFE_Homozygoot_C282Y Sex  
  /SAVE=PRED 
  /CASEWISE OUTLIER(2)
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(1) ITERATE(20) CUT(0.5).

ROC ANALYSIS PRE_1 LIIabove3.4 BY Major_Iron_overload (1)
  /MISSING USERMISSING=EXCLUDE
  /CRITERIA CUTOFF=INCLUDE TESTPOS=LARGE CI=95
  /DESIGN PAIR=TRUE
  /PLOT CURVE=ROC(REFERENCE) MODELQUALITY=FALSE
  /PRINT SE=TRUE CLASSIFIER=FALSE.



DESCRIPTIVES VARIABLES=COO_3
  /STATISTICS=MEAN STDDEV MIN MAX.

********************** Multiple imputation.

*variables included in the analysis model.
FREQUENCIES VARIABLES= Major_Iron_overload GemobiliseerdFe_einde_depletie LIIMRI1 LIIabove3.4  LIIboven2 Alcohol_overconsumption Sex HFE_Homozygoot_C282Y BMI Leeftijd
   /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN MEDIAN
    /FORMAT=NOTABLE
  /ORDER=ANALYSIS.


*variables that can exlain missingness.
FREQUENCIES VARIABLES=  Depletie Wel_Geen_Proef_Behandeling Erytrocytaphferese Aantal_aderlatingen_depletie Aantal_afereses_depletie Eerste_ferritine Laatste_Ferritine Proefbehandeling_gemobiliseerd_ijzer
      /FORMAT=NOTABLE
   /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN MEDIAN
  /ORDER=ANALYSIS.


FREQUENCIES VARIABLES=  Hb MetSyn Diabetesmellitus Hypertensie Hyperlipidemie Cholesterol Triglyceriden  
   /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN MEDIAN
  /ORDER=ANALYSIS
      /FORMAT=NOTABLE.

FREQUENCIES VARIABLES=  steatose fibrosecirrose Overleden
   /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN MEDIAN
  /ORDER=ANALYSIS
      /FORMAT=NOTABLE.


* Variables asociated with missingness.

COMPUTE Missing_MajorIronOverload=MISSING(Major_Iron_overload).
EXECUTE.

*only for variables of interest.
DATASET ACTIVATE DataSet1.
COMPUTE MissingValues_persubject=NMISS(Major_Iron_overload, GemobiliseerdFe_einde_depletie, LIIMRI1, LIIabove3.4,  LIIboven2, Alcohol_overconsumption, Sex, HFE_Homozygoot_C282Y, BMI, Leeftijd).
EXECUTE.



FREQUENCIES VARIABLES=  MissingValues_persubject
   /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN MEDIAN
  /ORDER=ANALYSIS.

RECODE MissingValues_persubject (0=0) (ELSE=1).
EXECUTE.

FREQUENCIES VARIABLES=  MissingValues_persubject
   /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN MEDIAN
  /ORDER=ANALYSIS.

* continuous predictors.
T-TEST GROUPS=MissingValues_persubject(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=    LIIMRI1   BMI Leeftijd Aantal_aderlatingen_depletie Aantal_afereses_depletie Eerste_ferritine Laatste_Ferritine Proefbehandeling_gemobiliseerd_ijzer Hb Cholesterol Triglyceriden ferritine  Lengte Gewicht  
  /ES DISPLAY(FALSE)
  /CRITERIA=CI(.95).

* categorical variables.

DATASET ACTIVATE DataSet1.
PROPORTIONS
  /INDEPENDENTSAMPLES   LIIabove3.4  LIIboven2    Alcohol_overconsumption Sex HFE_Homozygoot_C282Y  Depletie Wel_Geen_Proef_Behandeling Erytrocytaphferese  MetSyn Diabetesmellitus Hypertensie Hyperlipidemie steatose fibrosecirrose Overleden
  BY MissingValues_persubject SELECT=LEVEL(1 ,0 ) CITYPES=AGRESTI_CAFFO NEWCOMBE  
    TESTTYPES=WALDH0 
  /SUCCESS VALUE=LEVEL(1 )
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.



DATASET ACTIVATE DataSet1.
LOGISTIC REGRESSION VARIABLES MissingValues_persubject
  /METHOD=BSTEP Aantal_afereses_depletie Aantal_aderlatingen_depletie Erytrocytaphferese LIIMRI1 Major_Iron_overload Diabetesmellitus Hyperlipidemie steatose fibrosecirrose GemobiliseerdFe_einde_depletie
    /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).


* In the imputation model adjust for:
    * the variables in the analysis model: Major_Iron_overload, GemobiliseerdFe_einde_depletie, LIIMRI1, LIIabove3.4,  LIIboven2, Alcohol_overconsumption, Sex, HFE_Homozygoot_C282Y, BMI, Leeftijd
    * these variables for which there are some differences between missing and non-missing values:   Eerste_ferritine Laatste_Ferritine Proefbehandeling_gemobiliseerd_ijzer
    *, Aantal_afereses_depletie, Depletie Wel_Geen_Proef_Behandeling, Erytrocytaphferese, Diabetesmellitus, Hyperlipidemie, steatose, fibrosecirrose, Overlede
    



*Analyze Patterns of Missing Values.
MULTIPLE IMPUTATION   Major_Iron_overload GemobiliseerdFe_einde_depletie LIIMRI1 LIIabove3.4  LIIboven2 Alcohol_overconsumption Sex HFE_Homozygoot_C282Y BMI Leeftijd
   Depletie Wel_Geen_Proef_Behandeling Erytrocytaphferese Aantal_aderlatingen_depletie Aantal_afereses_depletie Eerste_ferritine Laatste_Ferritine Proefbehandeling_gemobiliseerd_ijzer
   Hb MetSyn Diabetesmellitus Hypertensie Hyperlipidemie Cholesterol Triglyceriden steatose fibrosecirrose Overleden
   /IMPUTE METHOD=NONE
   /MISSINGSUMMARIES  OVERALL VARIABLES (MAXVARS=25 MINPCTMISSING=0.001) PATTERNS.

* set random generator.
SET RNG=MT MTINDEX=2000000.

* since 21/72=30% of subects with missing values, based on the rule of thumb of White et al M=30
**** FCS with PPM

*Impute Missing Data Values.
DATASET DECLARE newMRI_imputation_m30.
DATASET DECLARE M30_iteration_history.
MULTIPLE IMPUTATION Major_Iron_overload GemobiliseerdFe_einde_depletie LIIMRI1 LIIabove3.4  LIIboven2 Alcohol_overconsumption Sex HFE_Homozygoot_C282Y BMI Leeftijd
   Depletie Wel_Geen_Proef_Behandeling Erytrocytaphferese Aantal_afereses_depletie Eerste_ferritine Laatste_Ferritine Proefbehandeling_gemobiliseerd_ijzer
   Diabetesmellitus  Hyperlipidemie  steatose fibrosecirrose Overleden 
  /IMPUTE METHOD=FCS MAXITER= 10 NIMPUTATIONS=30 SCALEMODEL=PMM(5) 
    SINGULAR=1E-012 MAXPCTMISSING=NONE MAXMODELPARAM=400
  /CONSTRAINTS LIIMRI1( ROLE=IND)
   /CONSTRAINTS LIIabove3.4( ROLE=IND)
    /CONSTRAINTS LIIboven2( ROLE=IND)
        /CONSTRAINTS Leeftijd( ROLE=IND)
  /CONSTRAINTS HFE_Homozygoot_C282Y( ROLE=IND)
  /CONSTRAINTS Depletie( ROLE=IND)
  /CONSTRAINTS Sex( ROLE=IND)
   /CONSTRAINTS Wel_Geen_Proef_Behandeling( ROLE=IND)
   /CONSTRAINTS Erytrocytaphferese( ROLE=IND)
      /CONSTRAINTS Diabetesmellitus( ROLE=IND)
         /CONSTRAINTS Overleden( ROLE=IND)
  /MISSINGSUMMARIES OVERALL 
  /IMPUTATIONSUMMARIES MODELS DESCRIPTIVES 
  /OUTFILE IMPUTATIONS=newMRI_imputation_m30 FCSITERATIONS=M30_iteration_history .

***************** Sensitivity analisis: perform analysis as before.

** logistic regression.
DATASET ACTIVATE newMRI_imputation_m30.
LOGISTIC REGRESSION VARIABLES Major_Iron_overload
  /METHOD=ENTER LIIabove3.4 Alcohol_overconsumption BMI HFE_Homozygoot_C282Y Sex    
  /PRINT=CI(95)
    /SAVE=PRED 
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

* without alcohol overconsumption, since only 3 subjects (in the complete cases) have too high overconsumption.
DATASET ACTIVATE newMRI_imputation_m30.
LOGISTIC REGRESSION VARIABLES Major_Iron_overload
  /METHOD=ENTER LIIabove3.4  BMI HFE_Homozygoot_C282Y Sex    
  /PRINT=CI(95)
    /SAVE=PRED COOK
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).


LOGISTIC REGRESSION VARIABLES Major_Iron_overload
  /METHOD=BSTEP(LR) LIIabove3.4  BMI HFE_Homozygoot_C282Y Sex    
  /PRINT=CI(95)
  /CRITERIA=PIN(1) POUT(1) ITERATE(20) CUT(0.5).

* to test LIIabove3.4 with likelihood ratio test.
LOGISTIC REGRESSION VARIABLES Major_Iron_overload
  /METHOD=ENTER   BMI HFE_Homozygoot_C282Y Sex  
  /METHOD=ENTER LIIabove3.4 
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

CROSSTABS
  /TABLES=Major_Iron_overload BY LIIabove3.4 BY Sex BY HFE_Homozygoot_C282Y
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT 
  /COUNT ROUND CELL.

* Assumptions of logistic regression: no multicollinearity, linearity, no influential outliers.
   


DESCRIPTIVES VARIABLES=PRE_2 COO_2
  /STATISTICS=MEAN STDDEV MIN MAX.



USE ALL.
COMPUTE filter_$=(COO_2 < 1).
VARIABLE LABELS filter_$ 'COO_2 < 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

LOGISTIC REGRESSION VARIABLES Major_Iron_overload
  /METHOD=ENTER LIIabove3.4  BMI HFE_Homozygoot_C282Y Sex    
  /PRINT=CI(95)
    /SAVE=PRED COOK
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).


FILTER OFF.
EXECUTE.

USE ALL.
COMPUTE filter_$=(Wel_Geen_Proef_Behandeling = 1 | Wel_Geen_Proef_Behandeling = 2).
VARIABLE LABELS filter_$ 'Wel_Geen_Proef_Behandeling = 1 | Wel_Geen_Proef_Behandeling = 2 (FILTER)'.    
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Major_Iron_overload
  /METHOD=ENTER LIIabove3.4  BMI HFE_Homozygoot_C282Y Sex .


COMPUTE LnBMI=ln(BMI).
EXECUTE.

LOGISTIC REGRESSION VARIABLES Major_Iron_overload
  /METHOD=ENTER LIIabove3.4  BMI HFE_Homozygoot_C282Y Sex    
   /METHOD=ENTER BMI*LnBMI
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).


* since SPSS does not return the pooled estimates for the ROC curve, I will pooled them using Rubin's formula in Excel.

DATASET ACTIVATE newMRI_imputation_m30.
ROC ANALYSIS LIIabove3.4 BY Major_Iron_overload (1)
  /MISSING USERMISSING=EXCLUDE
  /CRITERIA CUTOFF=INCLUDE TESTPOS=LARGE CI=95
   /PLOT CURVE=ROC(REFERENCE) MODELQUALITY=FALSE
  /PRINT SE=TRUE CLASSIFIER=FALSE.

DATASET ACTIVATE newMRI_imputation_m30.
ROC ANALYSIS PRE_1 BY Major_Iron_overload (1)
  /MISSING USERMISSING=EXCLUDE
  /CRITERIA CUTOFF=INCLUDE TESTPOS=LARGE CI=95
   /PLOT CURVE=ROC(REFERENCE) MODELQUALITY=FALSE
  /PRINT SE=TRUE CLASSIFIER=FALSE.

*** Sensitivity, specificity, AUROC, PPV and NPV.


** LII-MRI above 2

DATASET ACTIVATE newMRI_imputation_m30.


SORT CASES  BY Imputation_ Major_Iron_overload.
SPLIT FILE LAYERED BY Imputation_ Major_Iron_overload.
*Compute 95% CI for  Sensitivity.
PROPORTIONS
  /ONESAMPLE LIIboven2 TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE
  /SUCCESS VALUE=LAST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.

  * Compute 95% CI for Specificity.
PROPORTIONS
  /ONESAMPLE LIIboven2 TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE
  /SUCCESS VALUE=FIRST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.


** LII-MRI above 3.4
* Compute 95% CI for  Sensitivity.
PROPORTIONS
  /ONESAMPLE LIIabove3.4 TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE
  /SUCCESS VALUE=LAST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.

  * Compute 95% CI for Specificity.
PROPORTIONS
  /ONESAMPLE LIIabove3.4 TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE
  /SUCCESS VALUE=FIRST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.
SPLIT FILE OFF.

SORT CASES  BY Imputation_ .
SPLIT FILE LAYERED BY Imputation_ .
DATASET ACTIVATE newMRI_imputation_m30.
ROC  LIIboven2 BY Major_Iron_overload (1)
  /PLOT=CURVE(REFERENCE)
  /PRINT=SE
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(LARGE) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.

*PPV and NPV.

DATASET ACTIVATE newMRI_imputation_m30.


SORT CASES  BY Imputation_ LIIboven2.
SPLIT FILE LAYERED BY Imputation_ LIIboven2.
* Compute 95% CI for  PPV.
PROPORTIONS
  /ONESAMPLE Major_Iron_overload TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE 
  /SUCCESS VALUE=LAST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.

  * Compute 95% CI for NPV.
PROPORTIONS
  /ONESAMPLE Major_Iron_overload TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE
  /SUCCESS VALUE=FIRST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.
SPLIT FILE OFF.



SORT CASES  BY Imputation_ LIIabove3.4.
SPLIT FILE LAYERED BY Imputation_ LIIabove3.4.
* Compute 95% CI for  PPV.
PROPORTIONS
  /ONESAMPLE Major_Iron_overload TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE 
  /SUCCESS VALUE=LAST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.

  * Compute 95% CI for NPV.
PROPORTIONS
  /ONESAMPLE Major_Iron_overload TESTVAL=0.5 TESTTYPES=MIDP SCORE  CITYPES=WILSON_SCORE
  /SUCCESS VALUE=FIRST
  /CRITERIA CILEVEL=95
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE.
SPLIT FILE OFF.




ROC LIIMRI1 BY Major_Iron_overload (1)
  /PLOT=NONE
  /PRINT=SE COORDINATES
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(LARGE) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.




* Investigate quasi'separation.


DATASET ACTIVATE DataSet1.
GRAPH
  /SCATTERPLOT(BIVAR)=Major_Iron_overload WITH PRE_2
  /MISSING=LISTWISE.

CROSSTABS
  /TABLES=LIIabove3.4 BY Major_Iron_overload
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.
