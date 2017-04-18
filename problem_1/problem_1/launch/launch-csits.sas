LIBNAME launch "C:\Users\jaehoon\Documents\CRMDA\IERPS\LAUNCH";


*************
* DATA STEP *
*************;

**import SPSS data into SAS;

PROC IMPORT
   DATAFILE="C:\Users\jaehoon\Documents\CRMDA\IERPS\LAUNCH\CLT_1016Launch 06-07 PreK AllUSD.sav"
   DBMS=SPSS OUT=yr0607 REPLACE;
RUN;
PROC IMPORT
   DATAFILE="C:\Users\jaehoon\Documents\CRMDA\IERPS\LAUNCH\CLT_1016Launch 07-08 PreK AllUSD.sav"
   DBMS=SPSS OUT=yr0708 REPLACE;
RUN;
PROC IMPORT
   DATAFILE="C:\Users\jaehoon\Documents\CRMDA\IERPS\LAUNCH\CLT_1016Launch 08-09 PreK AllUSD.sav"
   DBMS=SPSS OUT=yr0809 REPLACE;
RUN;
PROC IMPORT
   DATAFILE="C:\Users\jaehoon\Documents\CRMDA\IERPS\LAUNCH\CLT_1016Launch 10-11 PreK AllUSD.sav"
   DBMS=SPSS OUT=yr1011 REPLACE;
RUN;


**merge data and create varaibles;
DATA yr0611;
  MERGE yr0607 yr0708 yr0809 yr1011;
  BY year;

  *drop the case from USD355;
  IF usdno=355 THEN DELETE;

  *lunch;
  IF lunch=0 THEN lunch_re=0; *regular;
  ELSE IF lunch=1 OR lunch=2 THEN lunch_re=1; *reduced/free;
  ELSE lunch_re=.;

  *gender;
  IF gender=0 THEN male=0;
  ELSE IF gender=1 THEN male=1;
  ELSE male=.;

  *race;
  IF race=5 THEN minority=0;
  ELSE IF 1<=race<=4 OR 6<=race<=7 THEN minority=1;
  ELSE minority=.;

  *IEP;
  IF iep=0 THEN iep_re=0;
  ELSE IF 1<=iep<=12 THEN iep_re=1;
  ELSE iep_re=.;

  *KELI scores;
  phys=MEAN(OF phys1-phys7);
  soc=MEAN(OF soc8-soc13);
  sym=MEAN(OF sym14-sym18);
  gk=MEAN(OF gk19-gk23);
  oc=MEAN(OF oc24-oc28);
  wl=MEAN(OF wl29-wl35);
  mc=MEAN(OF mc36-mc41);
  wh=MEAN(OF wh42-wh46);
  att=MEAN(OF att47-att51);
  keli_s=MEAN(OF phys1s phys2s phys3s phys4s phys5s phys6s phys7s
                  sym14s sym15s sym16s sym17s sym18s
                  gk19s gk20s gk21s gk22s gk23s
                  oc24s oc25s oc26s oc27s oc28s
                  wl29s wl30s wl31s wl32s wl33s wl34s wl35s
                  mc36s mc37s mc38s mc39s mc40s mc41s
                  wh42s wh43s wh44s wh45s wh46s
                  att47s att48s att49s att50s att51s);
RUN;


**original sample;

*obtain district-level means;
PROC SORT DATA=yr0611; BY year usdno; RUN;
PROC MEANS DATA=yr0611 NOPRINT;
  BY year usdno; 
  VAR lunch_re male minority iep_re esol phys soc sym gk oc wl mc wh att;
  OUTPUT OUT=district MEAN=;
RUN;

*create variables;
DATA district;
  SET district;

  *treatment year;
  IF year=2010 THEN trtyr=1;
  ELSE trtyr=0;

  *treatment;
  IF usdno=457 THEN trt=1;
  ELSE trt=0;

  FORMAT esol BEST12.;

  DROP _type_ _freq_;
RUN;


**match 2006-2007 data;

DATA yr0607_temp;
  SET yr0611;

  *treatment;
  IF usdno=457 THEN trt=1;
  ELSE trt=0;

  IF year=2006 THEN OUTPUT yr0607_temp;
RUN;

*calculate caliper value;
PROC LOGISTIC DATA=yr0607_temp NOPRINT;
  CLASS trt /PARAM=REFERENCE;
  MODEL trt = phys sym gk wl;
  OUTPUT OUT=pred PRED=pscore;
RUN;
PROC MEANS DATA=pred; *caliper=(0.25*0.1984125)=0.049603125;
  VAR pscore;
RUN;

*matching;
DATA treatment(RENAME=(studentid=idt pscore=pscoret))
     control(RENAME=(studentid=idc pscore=pscorec));
  SET pred;

  IF trt=1 THEN OUTPUT treatment;
  ELSE OUTPUT control;
RUN;
%INC "C:\Users\jaehoon\Documents\My SAS Files\Macro\psmatching.sas"; RUN;
%PSMATCHING(DATATREATMENT=treatment,
            DATACONTROL=control,
            METHOD=caliper,
			NUMBEROFCONTROLS=1,
			CALIPER=0.049603125,
			REPLACEMENT=no);
RUN; QUIT;

*remove cases that could not be matched;
PROC SORT DATA=yr0607_temp; BY studentid; RUN;
DATA yr0607_temp_treatment yr0607_temp_control;
  SET yr0607_temp;

  IF trt=1 THEN OUTPUT yr0607_temp_treatment;
  ELSE OUTPUT yr0607_temp_control;
RUN;
DATA matched_treatment;
  SET matched;
  RENAME idselectedcontrol=studentid_match
         matchedtotreatid=studentid;  
RUN;
PROC SORT DATA=matched_treatment; BY studentid; RUN;
DATA yr0607_matched_treatment;
  MERGE yr0607_temp_treatment matched_treatment(IN=a);
  BY studentid;

  IF a;
RUN;
DATA matched_control;
  SET matched;
  RENAME idselectedcontrol=studentid
         matchedtotreatid=studentid_match;  
RUN;
PROC SORT DATA=matched_control; BY studentid; RUN;
DATA yr0607_matched_control;
  MERGE yr0607_temp_control matched_control(IN=a);
  BY studentid;

  IF a;
RUN;
DATA yr0607_matched;
  MERGE yr0607_matched_treatment yr0607_matched_control;
  BY studentid;
RUN;


**match 2007-2008 data;

DATA yr0708_temp;
  SET yr0611;

  *treatment;
  IF usdno=457 THEN trt=1;
  ELSE trt=0;

  IF year=2007 THEN OUTPUT yr0708_temp;
RUN;

*calculate caliper value;
PROC LOGISTIC DATA=yr0708_temp NOPRINT;
  CLASS trt /PARAM=REFERENCE;
  MODEL trt = phys sym gk wl;
  OUTPUT OUT=pred PRED=pscore;
RUN;
PROC MEANS DATA=pred; *caliper=(0.25*0.1539141)=0.038478525;
  VAR pscore;
RUN;

*matching;
DATA treatment(RENAME=(studentid=idt pscore=pscoret))
     control(RENAME=(studentid=idc pscore=pscorec));
  SET pred;

  IF trt=1 THEN OUTPUT treatment;
  ELSE OUTPUT control;
RUN;
%PSMATCHING(DATATREATMENT=treatment,
            DATACONTROL=control,
            METHOD=caliper,
			NUMBEROFCONTROLS=1,
			CALIPER=0.038478525,
			REPLACEMENT=no);
RUN; QUIT;

*remove cases that could not be matched;
PROC SORT DATA=yr0708_temp; BY studentid; RUN;
DATA yr0708_temp_treatment yr0708_temp_control;
  SET yr0708_temp;

  IF trt=1 THEN OUTPUT yr0708_temp_treatment;
  ELSE OUTPUT yr0708_temp_control;
RUN;
DATA matched_treatment;
  SET matched;
  RENAME idselectedcontrol=studentid_match
         matchedtotreatid=studentid;  
RUN;
PROC SORT DATA=matched_treatment; BY studentid; RUN;
DATA yr0708_matched_treatment;
  MERGE yr0708_temp_treatment matched_treatment(IN=a);
  BY studentid;

  IF a;
RUN;
DATA matched_control;
  SET matched;
  RENAME idselectedcontrol=studentid
         matchedtotreatid=studentid_match;  
RUN;
PROC SORT DATA=matched_control; BY studentid; RUN;
DATA yr0708_matched_control;
  MERGE yr0708_temp_control matched_control(IN=a);
  BY studentid;

  IF a;
RUN;
DATA yr0708_matched;
  MERGE yr0708_matched_treatment yr0708_matched_control;
  BY studentid;
RUN;


**match 2008-2009 data;

DATA yr0809_temp;
  SET yr0611;

  *treatment;
  IF usdno=457 THEN trt=1;
  ELSE trt=0;

  IF year=2008 THEN OUTPUT yr0809_temp;
RUN;

*calculate caliper value;
PROC LOGISTIC DATA=yr0809_temp NOPRINT;
  CLASS trt /PARAM=REFERENCE;
  MODEL trt = phys sym gk wl;
  OUTPUT OUT=pred PRED=pscore;
RUN;
PROC MEANS DATA=pred; *caliper=(0.25*0.1359265)=0.0339816250;
  VAR pscore;
RUN;

*matching;
DATA treatment(RENAME=(studentid=idt pscore=pscoret))
     control(RENAME=(studentid=idc pscore=pscorec));
  SET pred;

  IF trt=1 THEN OUTPUT treatment;
  ELSE OUTPUT control;
RUN;
%PSMATCHING(DATATREATMENT=treatment,
            DATACONTROL=control,
            METHOD=caliper,
			NUMBEROFCONTROLS=1,
			CALIPER=0.0339816250,
			REPLACEMENT=no);
RUN; QUIT;

*remove cases that could not be matched;
PROC SORT DATA=yr0809_temp; BY studentid; RUN;
DATA yr0809_temp_treatment yr0809_temp_control;
  SET yr0809_temp;

  IF trt=1 THEN OUTPUT yr0809_temp_treatment;
  ELSE OUTPUT yr0809_temp_control;
RUN;
DATA matched_treatment;
  SET matched;
  RENAME idselectedcontrol=studentid_match
         matchedtotreatid=studentid;  
RUN;
PROC SORT DATA=matched_treatment; BY studentid; RUN;
DATA yr0809_matched_treatment;
  MERGE yr0809_temp_treatment matched_treatment(IN=a);
  BY studentid;

  IF a;
RUN;
DATA matched_control;
  SET matched;
  RENAME idselectedcontrol=studentid
         matchedtotreatid=studentid_match;  
RUN;
PROC SORT DATA=matched_control; BY studentid; RUN;
DATA yr0809_matched_control;
  MERGE yr0809_temp_control matched_control(IN=a);
  BY studentid;

  IF a;
RUN;
DATA yr0809_matched;
  MERGE yr0809_matched_treatment yr0809_matched_control;
  BY studentid;
RUN;

*check covariane balance;
PROC TTEST DATA=yr0607_matched;
  CLASS trt;
  VAR phys sym gk wl;
RUN;
PROC TTEST DATA=yr0708_matched;
  CLASS trt;
  VAR phys sym gk wl;
RUN;
PROC TTEST DATA=yr0809_matched;
  CLASS trt;
  VAR phys sym gk wl;
RUN;

*merge data and create varaibles;
DATA launch.yr0611_matched;
  MERGE yr0607_matched yr0708_matched yr0809_matched yr1011;
  BY year;

  *drop the case from USD355;
  IF usdno=355 THEN DELETE;

  *lunch;
  IF lunch=0 THEN lunch_re=0; *regular;
  ELSE IF lunch=1 OR lunch=2 THEN lunch_re=1; *reduced/free;
  ELSE lunch_re=.;

  *gender;
  IF gender=0 THEN male=0;
  ELSE IF gender=1 THEN male=1;
  ELSE male=.;

  *race;
  IF race=5 THEN minority=0;
  ELSE IF 1<=race<=4 OR 6<=race<=7 THEN minority=1;
  ELSE minority=.;

  *IEP;
  IF iep=0 THEN iep_re=0;
  ELSE IF 1<=iep<=12 THEN iep_re=1;
  ELSE iep_re=.;

  *KELI scores;
  phys=MEAN(OF phys1-phys7);
  soc=MEAN(OF soc8-soc13);
  sym=MEAN(OF sym14-sym18);
  gk=MEAN(OF gk19-gk23);
  oc=MEAN(OF oc24-oc28);
  wl=MEAN(OF wl29-wl35);
  mc=MEAN(OF mc36-mc41);
  wh=MEAN(OF wh42-wh46);
  att=MEAN(OF att47-att51);
  keli_s=MEAN(OF phys1s phys2s phys3s phys4s phys5s phys6s phys7s
                  sym14s sym15s sym16s sym17s sym18s
                  gk19s gk20s gk21s gk22s gk23s
                  oc24s oc25s oc26s oc27s oc28s
                  wl29s wl30s wl31s wl32s wl33s wl34s wl35s
                  mc36s mc37s mc38s mc39s mc40s mc41s
                  wh42s wh43s wh44s wh45s wh46s
                  att47s att48s att49s att50s att51s);
RUN;

*obtain district-level means;
PROC SORT DATA=launch.yr0611_matched; BY year usdno; RUN;
PROC MEANS DATA=launch.yr0611_matched NOPRINT;
  BY year usdno; 
  VAR phys soc sym gk oc wl mc wh att;
  OUTPUT OUT=district_matched MEAN=;
RUN;

*create variables;
DATA district_matched;
  SET district_matched;

  *treatment year;
  IF year=2010 THEN trtyr=1;
  ELSE trtyr=0;

  *treatment;
  IF usdno=457 THEN trt=1;
  ELSE trt=0;

  DROP _type_ _freq_;
RUN;



*****************
* ANALYSIS STEP *
*****************;

**descriptive;

*original sample;
PROC FREQ DATA=yr0611;
  TABLES male*year /MISSING;
  TABLES race*year /MISSING;
  TABLES lunch*year /MISSING;
  TABLES iep*year /MISSING;
  TABLES esol*year /MISSING;
  TABLES usdno*year /MISSING;
RUN;

PROC MEANS DATA=yr0611 NOPRINT;
  CLASS year;
  VAR phys soc sym gk oc wl mc wh att;
  OUTPUT OUT=m;
RUN;
PROC SORT DATA=m; BY year; RUN;
PROC TRANSPOSE DATA=m OUT=m(KEEP=year _name_ n mean std); BY year; ID _stat_; RUN;
DATA m;
  SET m;
  IF (_name_="_TYPE_") OR (_name_="_FREQ_") THEN DELETE;
RUN;
PROC EXPORT DATA=m
  OUTFILE="C:\Users\jaehoon\Desktop\m.csv"
  DBMS=CSV REPLACE;
RUN;

*matched sample;
PROC FREQ DATA=launch.yr0611_matched;
  TABLES male*year /MISSING;
  TABLES race*year /MISSING;
  TABLES lunch*year /MISSING;
  TABLES iep*year /MISSING;
  TABLES esol*year /MISSING;
  TABLES usdno*year /MISSING;
RUN;

PROC MEANS DATA=launch.yr0611_matched NOPRINT;
  CLASS year;
  VAR phys soc sym gk oc wl mc wh att;
  OUTPUT OUT=m;
RUN;
PROC SORT DATA=m; BY year; RUN;
PROC TRANSPOSE DATA=m OUT=m(KEEP=year _name_ n mean std); BY year; ID _stat_; RUN;
DATA m;
  SET m;
  IF (_name_="_TYPE_") OR (_name_="_FREQ_") THEN DELETE;
RUN;
PROC EXPORT DATA=m
  OUTFILE="C:\Users\jaehoon\Desktop\m.csv"
  DBMS=CSV REPLACE;
RUN;


**calculate standard deviation of district means;

*original sample;
PROC MEANS DATA=yr0611;
  VAR phys soc sym gk oc wl mc wh att;
  WHERE year<=2008;
  OUTPUT OUT=sd STD=;
RUN;
PROC EXPORT DATA=sd
  OUTFILE="C:\Users\jaehoon\Desktop\sd.csv"
  DBMS=CSV REPLACE;
RUN;

*matched sample;
PROC MEANS DATA=launch.yr0611_matched;
  VAR phys soc sym gk oc wl mc wh att;
  WHERE year<=2008;
  OUTPUT OUT=sd STD=;
RUN;
PROC EXPORT DATA=sd
  OUTFILE="C:\Users\jaehoon\Desktop\sd.csv"
  DBMS=CSV REPLACE;
RUN;


**short interrupted time-series with comparison group design (CSITS) (Bloom,  2003);

*baseline balance test;
%MACRO baseline(data=, varlist=);
  %LET nvar=0;

  %DO %WHILE(%SCAN(&varlist,&nvar+1)~=);
    %LET nvar=%EVAL(&nvar+1);
    %LET var=%SCAN(&varlist,&nvar);

    PROC MIXED DATA=&data COVTEST METHOD=REML;
      CLASS usdno year;
      MODEL &var = trt /SOLUTION;
      *RANDOM intercept /SUBJECT=usdno;
      REPEATED /SUBJECT=usdno TYPE=AR(1);
      ODS OUTPUT SOLUTIONF=fixed COVPARMS=covparms;
      WHERE trtyr=0;
    RUN;

    PROC APPEND BASE=fixed_all DATA=fixed FORCE; RUN;
    PROC APPEND BASE=covparms_all DATA=covparms FORCE; RUN;

  %END;
%MEND baseline;
%baseline(data=district, varlist=phys soc sym gk oc wl mc wh att)
PROC EXPORT DATA=fixed_all
  OUTFILE="C:\Users\jaehoon\Desktop\fixed.csv"
  DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=covparms_all
  OUTFILE="C:\Users\jaehoon\Desktop\covparms.csv"
  DBMS=CSV REPLACE;
RUN;

/*
%baseline(data=launch.district_matched, varlist=phys sym gk wl)
PROC EXPORT DATA=fixed_all
  OUTFILE="C:\Users\jaehoon\Desktop\fixed.csv"
  DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=covparms_all
  OUTFILE="C:\Users\jaehoon\Desktop\covparms.csv"
  DBMS=CSV REPLACE;
RUN;
*/

*baseline mean projection model;
%MACRO impact(data=, varlist=);
  %LET nvar=0;

  %DO %WHILE(%SCAN(&varlist,&nvar+1)~=);
    %LET nvar=%EVAL(&nvar+1);
    %LET var=%SCAN(&varlist,&nvar);

    PROC MIXED DATA=&data COVTEST METHOD=REML MAXFUNC=5000 MAXITER=1000;
      CLASS usdno year;
      MODEL &var = male minority iep_re esol lunch_re trtyr trt trtyr*trt /SOLUTION;
      *RANDOM intercept /SUBJECT=usdno;
      REPEATED /SUBJECT=usdno TYPE=AR(1);
      ODS OUTPUT SOLUTIONF=fixed COVPARMS=covparms;
    RUN;

    PROC APPEND BASE=fixed_all DATA=fixed FORCE; RUN;
    PROC APPEND BASE=covparms_all DATA=covparms FORCE; RUN;

  %END;
%MEND impact;
%impact(data=district, varlist=phys soc sym gk oc wl mc wh att)
PROC EXPORT DATA=fixed_all
  OUTFILE="C:\Users\jaehoon\Desktop\fixed.csv"
  DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=covparms_all
  OUTFILE="C:\Users\jaehoon\Desktop\covparms.csv"
  DBMS=CSV REPLACE;
RUN;

%MACRO impact_simple(data=, varlist=);
  %LET nvar=0;

  %DO %WHILE(%SCAN(&varlist,&nvar+1)~=);
    %LET nvar=%EVAL(&nvar+1);
    %LET var=%SCAN(&varlist,&nvar);

    PROC MIXED DATA=&data COVTEST METHOD=REML MAXFUNC=5000 MAXITER=1000;
      CLASS usdno year;
      MODEL &var = trtyr trt trtyr*trt /SOLUTION;
      *RANDOM intercept /SUBJECT=usdno;
      REPEATED /SUBJECT=usdno;
      ODS OUTPUT SOLUTIONF=fixed COVPARMS=covparms;
    RUN;

    PROC APPEND BASE=fixed_all DATA=fixed FORCE; RUN;
    PROC APPEND BASE=covparms_all DATA=covparms FORCE; RUN;

  %END;
%MEND impact_simple;
%impact_simple(data=district_matched, varlist=phys sym gk wl)
PROC EXPORT DATA=fixed_all
  OUTFILE="C:\Users\jaehoon\Desktop\fixed.csv"
  DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=covparms_all
  OUTFILE="C:\Users\jaehoon\Desktop\covparms.csv"
  DBMS=CSV REPLACE;
RUN;
