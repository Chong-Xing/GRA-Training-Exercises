LIBNAME launch "C:\Users\jaehoon\Documents\CRMDA\IERPS\LAUNCH";


***import SPSS data into SAS;

PROC IMPORT
   DATAFILE="C:\Users\jaehoon\Documents\CRMDA\IERPS\LAUNCH\Master KELI 4R 2006 to 2010 Fall Only.sav"
   DBMS=SPSS OUT=temp REPLACE;
RUN;


***propensity score matching;

**create group variable;
DATA temp;
  SET temp;

  FORMAT group $5.;
  IF (usdno=457) THEN group="1:Int";
  ELSE group="2:Con";

  IF (gender=3) THEN gender=.;

  IF (race=0) THEN race=.;
  FORMAT race_re $10.;
  IF (race=5) THEN race_re="1:White";
  ELSE IF (race=6) THEN race_re="2:Hispanic";
  ELSE IF (race=0) THEN race_re="";
  ELSE race_re="3:other";

  FORMAT lunch_re $14.;
  IF (lunch=0) THEN lunch_re="1:regular";
  ELSE IF (lunch=1) OR (lunch=2) THEN lunch_re="2:reduced/free";
RUN;

*check covariane balance;
PROC FREQ DATA=temp; RUN;
PROC FREQ DATA=temp;
  TABLES gender*group /CHISQ;
  TABLES race*group /CHISQ;
  TABLES lunch*group /CHISQ;
  TABLES iep*group /CHISQ FISHER;
  TABLES esol*group /CHISQ;
  TABLES usdno*group;
  TABLES year*group /CHISQ;
RUN;


/*
**years 2006-2010 combined;

*calculate caliper value;
PROC LOGISTIC DATA=temp NOPRINT;
  CLASS lunch gender race iep esol /PARAM=REFERENCE;
  MODEL group(EVENT="1:Int") = lunch gender race iep esol /FIRTH MAXITER=10000;
  OUTPUT OUT=pred PRED=pscore;
RUN;
PROC MEANS DATA=pred; *caliper=(0.25*0.2514291)=0.062857275;
  VAR pscore;
RUN;

*matching;
DATA treatment(RENAME=(studentid=idt pscore=pscoret))
     control(RENAME=(studentid=idc pscore=pscorec));
  SET pred;

  IF (usdno=457) THEN OUTPUT treatment;
  ELSE OUTPUT control;
RUN;
%INC "C:\Users\jaehoon\Documents\My SAS Files\Macro\psmatching.sas"; RUN;
%PSMATCHING(DATATREATMENT=treatment,
            DATACONTROL=control,
            METHOD=caliper,
			NUMBEROFCONTROLS=1,
			CALIPER=0.062857275,
			REPLACEMENT=no);
RUN; QUIT;

*remove cases that could not be matched;
PROC SORT DATA=temp; BY studentid; RUN;
DATA usd457 other;
  SET temp;

  IF (usdno=457) THEN OUTPUT usd457;
  ELSE OUTPUT other;
RUN;
DATA matched_usd457;
  SET matched;
  RENAME idselectedcontrol=studentid_match
         matchedtotreatid=studentid;  
RUN;
PROC SORT DATA=matched_usd457; BY studentid; RUN;
DATA usd457;
  MERGE usd457 matched_usd457(IN=a);
  BY studentid;

  IF a;
RUN;
DATA matched_other;
  SET matched;
  RENAME idselectedcontrol=studentid
         matchedtotreatid=studentid_match;  
RUN;
PROC SORT DATA=matched_other; BY studentid; RUN;
DATA other;
  MERGE other matched_other(IN=a);
  BY studentid;

  IF a;
RUN;
DATA launch.matched;
  MERGE usd457 other;
  BY studentid;
RUN;

*check covariane balance;
PROC FREQ DATA=launch.matched;
  TABLES gender*group /CHISQ;
  TABLES race*group /CHISQ FISHER;
  TABLES lunch*group /CHISQ;
  TABLES iep*group /CHISQ;
  TABLES esol*group /CHISQ;
  TABLES usdno*group;
  TABLES year*group /CHISQ;
RUN;
*/


**by year;
DATA y2006 y2007 y2008 y2010;
  SET temp;

  IF (year=2006) THEN OUTPUT y2006;
  ELSE IF (year=2007) THEN OUTPUT y2007;
  ELSE IF (year=2008) THEN OUTPUT y2008;
  ELSE IF (year=2010) THEN OUTPUT y2010;
RUN;


**2006;

*calculate caliper value;
PROC LOGISTIC DATA=y2006 NOPRINT;
  CLASS lunch gender race iep esol /PARAM=REFERENCE;
  MODEL group(EVENT="1:Int") = lunch gender race iep esol /FIRTH MAXITER=10000;
  OUTPUT OUT=pred PRED=pscore;
RUN;
PROC MEANS DATA=pred; *caliper=(0.25*0.2514291)=0.062857275;
  VAR pscore;
RUN;

*matching;
DATA treatment(RENAME=(studentid=idt pscore=pscoret))
     control(RENAME=(studentid=idc pscore=pscorec));
  SET pred;

  IF (usdno=457) THEN OUTPUT treatment;
  ELSE OUTPUT control;
RUN;
%INC "C:\Users\jaehoon\Documents\My SAS Files\Macro\psmatching.sas"; RUN;
%PSMATCHING(DATATREATMENT=treatment,
            DATACONTROL=control,
            METHOD=caliper,
			NUMBEROFCONTROLS=1,
			CALIPER=0.062857275,
			REPLACEMENT=no);
RUN; QUIT;

*remove cases that could not be matched;
PROC SORT DATA=temp; BY studentid; RUN;
DATA usd457 other;
  SET temp;

  IF (usdno=457) THEN OUTPUT usd457;
  ELSE OUTPUT other;
RUN;
DATA matched_usd457;
  SET matched;
  RENAME idselectedcontrol=studentid_match
         matchedtotreatid=studentid;  
RUN;
PROC SORT DATA=matched_usd457; BY studentid; RUN;
DATA usd457;
  MERGE usd457 matched_usd457(IN=a);
  BY studentid;

  IF a;
RUN;
DATA matched_other;
  SET matched;
  RENAME idselectedcontrol=studentid
         matchedtotreatid=studentid_match;  
RUN;
PROC SORT DATA=matched_other; BY studentid; RUN;
DATA other;
  MERGE other matched_other(IN=a);
  BY studentid;

  IF a;
RUN;
DATA matched_2006;
  MERGE usd457 other;
  BY studentid;
RUN;


**2007;

*calculate caliper value;
PROC LOGISTIC DATA=y2007 NOPRINT;
  CLASS lunch gender race iep esol /PARAM=REFERENCE;
  MODEL group(EVENT="1:Int") = lunch gender race iep esol /FIRTH MAXITER=10000;
  OUTPUT OUT=pred PRED=pscore;
RUN;
PROC MEANS DATA=pred; *caliper=(0.25*0.1801943)=0.045048575;
  VAR pscore;
RUN;

*matching;
DATA treatment(RENAME=(studentid=idt pscore=pscoret))
     control(RENAME=(studentid=idc pscore=pscorec));
  SET pred;

  IF (usdno=457) THEN OUTPUT treatment;
  ELSE OUTPUT control;
RUN;
%INC "C:\Users\jaehoon\Documents\My SAS Files\Macro\psmatching.sas"; RUN;
%PSMATCHING(DATATREATMENT=treatment,
            DATACONTROL=control,
            METHOD=caliper,
			NUMBEROFCONTROLS=1,
			CALIPER=0.045048575,
			REPLACEMENT=no);
RUN; QUIT;

*remove cases that could not be matched;
PROC SORT DATA=temp; BY studentid; RUN;
DATA usd457 other;
  SET temp;

  IF (usdno=457) THEN OUTPUT usd457;
  ELSE OUTPUT other;
RUN;
DATA matched_usd457;
  SET matched;
  RENAME idselectedcontrol=studentid_match
         matchedtotreatid=studentid;  
RUN;
PROC SORT DATA=matched_usd457; BY studentid; RUN;
DATA usd457;
  MERGE usd457 matched_usd457(IN=a);
  BY studentid;

  IF a;
RUN;
DATA matched_other;
  SET matched;
  RENAME idselectedcontrol=studentid
         matchedtotreatid=studentid_match;  
RUN;
PROC SORT DATA=matched_other; BY studentid; RUN;
DATA other;
  MERGE other matched_other(IN=a);
  BY studentid;

  IF a;
RUN;
DATA matched_2007;
  MERGE usd457 other;
  BY studentid;
RUN;


**2008;

*calculate caliper value;
PROC LOGISTIC DATA=y2008 NOPRINT;
  CLASS lunch gender race iep esol /PARAM=REFERENCE;
  MODEL group(EVENT="1:Int") = lunch gender race iep esol /FIRTH MAXITER=10000;
  OUTPUT OUT=pred PRED=pscore;
RUN;
PROC MEANS DATA=pred; *caliper=(0.25*.1478120)=0.036953;
  VAR pscore;
RUN;

*matching;
DATA treatment(RENAME=(studentid=idt pscore=pscoret))
     control(RENAME=(studentid=idc pscore=pscorec));
  SET pred;

  IF (usdno=457) THEN OUTPUT treatment;
  ELSE OUTPUT control;
RUN;
%INC "C:\Users\jaehoon\Documents\My SAS Files\Macro\psmatching.sas"; RUN;
%PSMATCHING(DATATREATMENT=treatment,
            DATACONTROL=control,
            METHOD=caliper,
			NUMBEROFCONTROLS=1,
			CALIPER=0.036953,
			REPLACEMENT=no);
RUN; QUIT;

*remove cases that could not be matched;
PROC SORT DATA=temp; BY studentid; RUN;
DATA usd457 other;
  SET temp;

  IF (usdno=457) THEN OUTPUT usd457;
  ELSE OUTPUT other;
RUN;
DATA matched_usd457;
  SET matched;
  RENAME idselectedcontrol=studentid_match
         matchedtotreatid=studentid;  
RUN;
PROC SORT DATA=matched_usd457; BY studentid; RUN;
DATA usd457;
  MERGE usd457 matched_usd457(IN=a);
  BY studentid;

  IF a;
RUN;
DATA matched_other;
  SET matched;
  RENAME idselectedcontrol=studentid
         matchedtotreatid=studentid_match;  
RUN;
PROC SORT DATA=matched_other; BY studentid; RUN;
DATA other;
  MERGE other matched_other(IN=a);
  BY studentid;

  IF a;
RUN;
DATA matched_2008;
  MERGE usd457 other;
  BY studentid;
RUN;
  

**2010;

*calculate caliper value;
PROC LOGISTIC DATA=y2010 NOPRINT;
  CLASS lunch gender race iep esol /PARAM=REFERENCE;
  MODEL group(EVENT="1:Int") = lunch gender race iep esol /FIRTH MAXITER=10000;
  OUTPUT OUT=pred PRED=pscore;
RUN;
PROC MEANS DATA=pred; *caliper=(0.25*0.3692546)=0.09231365;
  VAR pscore;
RUN;

*matching;
DATA treatment(RENAME=(studentid=idt pscore=pscoret))
     control(RENAME=(studentid=idc pscore=pscorec));
  SET pred;

  IF (usdno=457) THEN OUTPUT treatment;
  ELSE OUTPUT control;
RUN;
%INC "C:\Users\jaehoon\Documents\My SAS Files\Macro\psmatching.sas"; RUN;
%PSMATCHING(DATATREATMENT=treatment,
            DATACONTROL=control,
            METHOD=caliper,
			NUMBEROFCONTROLS=1,
			CALIPER=0.09231365,
			REPLACEMENT=no);
RUN; QUIT;

*remove cases that could not be matched;
PROC SORT DATA=temp; BY studentid; RUN;
DATA usd457 other;
  SET temp;

  IF (usdno=457) THEN OUTPUT usd457;
  ELSE OUTPUT other;
RUN;
DATA matched_usd457;
  SET matched;
  RENAME idselectedcontrol=studentid_match
         matchedtotreatid=studentid;  
RUN;
PROC SORT DATA=matched_usd457; BY studentid; RUN;
DATA usd457;
  MERGE usd457 matched_usd457(IN=a);
  BY studentid;

  IF a;
RUN;
DATA matched_other;
  SET matched;
  RENAME idselectedcontrol=studentid
         matchedtotreatid=studentid_match;  
RUN;
PROC SORT DATA=matched_other; BY studentid; RUN;
DATA other;
  MERGE other matched_other(IN=a);
  BY studentid;

  IF a;
RUN;
DATA matched_2010;
  MERGE usd457 other;
  BY studentid;
RUN;


**merge matched data;
DATA launch.matched;
  MERGE matched_2006 matched_2007 matched_2008 matched_2010;
  BY studentid;
RUN;


**check covariane balance;
PROC FREQ DATA=launch.matched;
  TABLES gender*group /CHISQ;
  TABLES race*group /CHISQ FISHER;
  TABLES lunch*group /CHISQ;
  TABLES iep*group /CHISQ;
  TABLES esol*group /CHISQ;
  TABLES usdno*group;
  TABLES year*group /CHISQ;
RUN;


***export SAS data into SPSS***;

PROC EXPORT DATA=launch.matched 
  OUTFILE="C:\Users\jaehoon\Documents\CRMDA\IERPS\LAUNCH\KELI4R.0610.Matched.sav"
  DBMS=SPSS REPLACE;
RUN;



*******************
*  ANALYSIS STEP  *
*******************;


PROC IMPORT
   DATAFILE="C:\Users\jaehoon\Documents\CRMDA\IERPS\LAUNCH\KELI4R.0610.Matched.sav"
   DBMS=SPSS OUT=matched REPLACE;
RUN;
DATA matched;
  SET matched;

  FORMAT int $6.;
  IF (year<2010) THEN int="1:Pre";
  ELSE IF (year=2010) THEN int="2:Post";
RUN;


***descriptives;

**mean & sd;

*by period;
PROC MEANS DATA=matched NOPRINT;
  CLASS group int;
  VAR physfall socfall symfall gkfall ocfall wlfall mcfall whfall attfall;
  OUTPUT OUT=m;
RUN;
PROC SORT DATA=m; BY group int; RUN;
PROC TRANSPOSE DATA=m OUT=m(KEEP=group int _name_ n mean std); BY group int; ID _stat_; RUN;
DATA m;
  SET m;

  IF (_name_="_TYPE_") OR (_name_="_FREQ_") THEN DELETE;
RUN;
PROC EXPORT DATA=m
  OUTFILE="C:\Users\jaehoon\Desktop\m.csv"
  DBMS=CSV REPLACE;
RUN;

*by year;
PROC MEANS DATA=matched NOPRINT;
  CLASS group year;
  VAR physfall socfall symfall gkfall ocfall wlfall mcfall whfall attfall;
  OUTPUT OUT=m;
RUN;
PROC SORT DATA=m; BY group year; RUN;
PROC TRANSPOSE DATA=m OUT=m(KEEP=group year _name_ n mean std); BY group year; ID _stat_; RUN;
DATA m;
  SET m;

  IF (_name_="_TYPE_") OR (_name_="_FREQ_") THEN DELETE;
RUN;
PROC EXPORT DATA=m
  OUTFILE="C:\Users\jaehoon\Desktop\m.csv"
  DBMS=CSV REPLACE;
RUN;

**t-test;
PROC SORT DATA=matched; BY int; RUN;
PROC TTEST DATA=matched;
  CLASS group;
  VAR physfall socfall symfall gkfall ocfall wlfall mcfall whfall attfall;
  ODS OUTPUT STATISTICS=stat TTESTS=t;
  BY int;
RUN;
PROC EXPORT DATA=stat
  OUTFILE="C:\Users\jaehoon\Desktop\stat.csv"
  DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=t
  OUTFILE="C:\Users\jaehoon\Desktop\t.csv"
  DBMS=CSV REPLACE;
RUN;

/*
PROC SORT DATA=matched; BY group; RUN;
PROC TTEST DATA=matched;
  CLASS int;
  VAR physfall socfall symfall gkfall ocfall wlfall mcfall whfall attfall;
  ODS OUTPUT STATISTICS=stat TTESTS=t;
  BY group;
RUN;
PROC EXPORT DATA=stat
  OUTFILE="C:\Users\jaehoon\Desktop\stat.csv"
  DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=t
  OUTFILE="C:\Users\jaehoon\Desktop\t.csv"
  DBMS=CSV REPLACE;
RUN;
*/


***MANCOVA;


**assumption check;

*normality;
PROC CAPABILITY DATA=matched NORMAL;
  VAR physfall socfall symfall gkfall ocfall wlfall mcfall whfall attfall;
  *QQPLOT physfall socfall symfall gkfall ocfall wlfall mcfall whfall attfall;
  *PPPLOT physfall socfall symfall gkfall ocfall wlfall mcfall whfall attfall;
  *HISTOGRAM;
RUN;
*note: negatively skewed variables: physfall socfall gkfall whfall attfall;

*homogenity of variance;
PROC GLM DATA=matched;
  CLASS group;
  MODEL physfall socfall symfall gkfall ocfall wlfall mcfall whfall attfall = group;
  MEANS group /HOVTEST;
  WHERE (year<2010); 
RUN; QUIT;
PROC GLM DATA=matched;
  CLASS group;
  MODEL physfall socfall symfall gkfall ocfall wlfall mcfall whfall attfall = group;
  MEANS group /HOVTEST;
  WHERE (year=2010); 
RUN; QUIT;


**pre-intervention (before 2010);

PROC GLM DATA=matched;
  CLASS lunch gender race esol group;
  MODEL physfall socfall symfall gkfall ocfall wlfall mcfall whfall attfall = lunch gender race esol group /SOLUTION EFFECTSIZE;
  MANOVA H=_ALL_;
  ODS OUTPUT MODELANOVA=anova MULTSTAT=manova PARAMETERESTIMATES=parm;
  WHERE (year<2010); 
RUN; QUIT;
PROC EXPORT DATA=manova
  OUTFILE="C:\Users\jaehoon\Desktop\manova.csv"
  DBMS=CSV REPLACE;
RUN;
DATA anova;
  SET anova;

  IF (hypothesistype=3);
RUN;
PROC EXPORT DATA=anova
  OUTFILE="C:\Users\jaehoon\Desktop\anova.csv"
  DBMS=CSV REPLACE;
RUN;


**post-intervention (2010);

PROC GLM DATA=matched;
  CLASS lunch gender race iep group;
  MODEL physfall socfall symfall gkfall ocfall wlfall mcfall whfall attfall = lunch gender race iep group /SOLUTION EFFECTSIZE;
  MANOVA H=_ALL_;
  ODS OUTPUT MODELANOVA=anova MULTSTAT=manova PARAMETERESTIMATES=parm;
  WHERE (year=2010); 
RUN; QUIT;
PROC EXPORT DATA=manova
  OUTFILE="C:\Users\jaehoon\Desktop\manova.csv"
  DBMS=CSV REPLACE;
RUN;
DATA anova;
  SET anova;

  IF (hypothesistype=3);
RUN;
PROC EXPORT DATA=anova
  OUTFILE="C:\Users\jaehoon\Desktop\anova.csv"
  DBMS=CSV REPLACE;
RUN;



***intervention group only;

PROC TTEST DATA=matched;
  CLASS int;
  VAR physfall socfall symfall gkfall ocfall wlfall mcfall whfall attfall;
  ODS OUTPUT TTESTS=t;
RUN;
PROC EXPORT DATA=t
  OUTFILE="C:\Users\jaehoon\Desktop\t.csv"
  DBMS=CSV REPLACE;
RUN;
