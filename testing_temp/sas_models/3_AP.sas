/*---------------------------------------------------------------------------
 Tyler Matta, 11/21/2014 (Based on Y. M. Thum, 4/22/2014)
 Fitting a taxonomy of mixed effects models a single cohort between 4th 
 and 8th grade.
----------------------------------------------------------------------------*/

options ls=146 ;

libname progs '.\AP Model';
libname results '.\AP Model\Results';

%let pgm=			AP3;
%let Subj=			Read;
%let AnalyzeCohort= 9; * Cohort 9 spans grades 4 - 9 ;        
%let Tagg=			C9;
%let Dist=			Horry;  

*----------------------------------------------------------------------------;
* 0. setup ODS 																 ;
*----------------------------------------------------------------------------;

 Proc printto new
              log=".\AP Model\Results\&pgm._&Dist&Tagg._&Subj..log"
            print=".\AP Model\Results\&pgm._&Dist&Tagg._&Subj..lst"; run;

   PROC TEMPLATE;
     DEFINE STYLE mystyle3;
     PARENT = styles.analysis;
     REPLACE fonts /
        'TitleFont2' = ("Arial",12pt,Bold)
        'TitleFont' = ("Arial",14pt,Bold)
        'StrongFont' = ("Arial",10pt,Bold)
        'EmphasisFont' = ("Arial",9pt,Italic)
        'FixedEmphasisFont' = ("Courier New, Courier",9pt,Italic)
        'FixedStrongFont' = ("Courier New, Courier",9pt,Bold)
        'FixedHeadingFont' = ("Courier New, Courier",9pt,Bold)
        'BatchFixedFont' = ("SAS Monospace, Courier New, Courier",8pt)
        'FixedFont' = ("Courier New, Courier",9pt)
        'headingEmphasisFont' = ("Arial",10pt,Bold)
        'headingFont' = ("Arial",10pt,Bold)
        'docFont' = ("Arial",9pt);
    END;
   RUN;

  ods listing close;
  run;

  ods path (PREPEND) WORK.TEMPLAT(UPDATE);

  ods html path=".\AP Model\Results" (URL=NONE)
       file="&pgm._&Dist&Subj&Tagg..html"
       (TITLE="&Dist &Subj") style=mystyle3;

*----------------------------------------------------------------------------;
* 1. descriptives 															 ;
*----------------------------------------------------------------------------;

  proc sort data=progs.&Dist&Subj.1b out=tmp1; 
	by Cohort StudID descending Outcome;
	where Cohort in (&AnalyzeCohort) & Outcome="Y" & Test="MAP" & Grade ^= 9;
  run;

data tmp2;
  set tmp1;
  if term = 'Fall' & Grade = 4 then do;
	lin = -4.5; end;
  else if term = 'Spring' & Grade = 4 then do;
  	lin = -4.0; end;
  else if term = 'Fall' & Grade = 5 then do;
  	lin = -3.5; end;
  else if term = 'Spring' & Grade = 5 then do;
  	lin = -3.0; end;
  else if term = 'Fall' & Grade = 6 then do;
  	lin = -2.5; end;
  else if term = 'Spring' & Grade = 6 then do;
  	lin = -2.0; end;
  else if term = 'Fall' & Grade = 7 then do;
  	lin = -1.5; end;
  else if term = 'Spring' & Grade = 7 then do;
  	lin = -1.0; end;
  else if term = 'Fall' & Grade = 8 then do;
  	lin = -.5; end;
  else if term = 'Spring' & Grade = 8 then do;
  	lin = 0.0;
  end;

  quad = lin*lin ;

run;

*******************************************************************************;
** Conventional Model 1: Fixed-effects linear term	 						   ;
*******************************************************************************;


 proc nlmixed 
  data=tmp2
  maxiter=300 
  COV 
  tech=quanew;
  
  _S=_Score/_SEM;  * rescaleing the residual variance; 

  parms 
	   g100= 221.93 
       g110=   4.56;

       b0 = g100;
       b1 = g110;


       mu =  ( b0*IntS + b1*lin)/_SEM;

   model 
	   _S ~ normal(mu,1.0);

   title1 "&Dist &AnalyzeCohort MAP &Subj Scores";
   title2 "Conventional Model 1: Fixed-effects linear term";

 %put Message: 2a. -------------------------------------------------------------;
 %put Message: 2a. NLMIXED -- Conventional Model 1 &Subj;
 %put Message: 2a. Fixed-effects. linear term only
 %put Message: 2a. %SYSFUNC(DATETime(), DateTime16.0) ;
RUN;

*******************************************************************************;
* Conventional Model 2: Fixed-effects linear & quadratic terms 				   ;
*******************************************************************************;


 proc nlmixed 
  data=tmp2
  maxiter=300 
  COV 
  tech=quanew;
  
  _S=_Score/_SEM;  * rescaleing the residual variance; 

  parms 
	   g00= 220.30 
       g10=   2.09
       g20=  -0.55;

       b0 = g00;
       b1 = g10;
	   b2 = g20;

       mu =  ( b0*IntS + b1*lin + b2*quad )/_SEM;

   model 
	   _S ~ normal(mu,1.0);

   title1 "&Dist &AnalyzeCohort MAP &Subj Scores";
   title2 "Conventional Model 2";

 %put Message: 2a. -------------------------------------------------------------;
 %put Message: 2a. NLMIXED -- &Subj Conventional Model 1;
 %put Message: 2a. Fixed-effects. linear and quadratic terms;
 %put Message: 2a. %SYSFUNC(DATETime(), DateTime16.0) ;
RUN;

*******************************************************************************;
* Conventional Model 3: Random-intercept with fixed linear & quadratic terms   ;
*******************************************************************************;

 proc nlmixed 
  data=tmp2
  maxiter=300 
  COV 
  tech=quanew;
  
  _S=_Score/_SEM;  * rescaleing the residual variance; 

  parms 
	   g00= 218.86 
       g10=   2.03
	   g20=  -0.56
       t00= 215.65;

       b0 = g00 + u00 ;
       b1 = g10;
	   b2 = g20;

       mu =  ( b0*IntS + b1*lin + b2*quad )/_SEM;

   model 
	   _S ~ normal(mu,1.0);

   random
   		u00 ~ normal([0],
					 [t00]) subject = StudID;

   title1 "&Dist &AnalyzeCohort MAP &Subj Scores";
   title2 "Conventional Model 3: Random-intercept with fixed linear & quadratic terms";

 %put Message: 2a. -------------------------------------------------------------;
 %put Message: 2a. NLMIXED -- &Subj Conventional Model 3;
 %put Message: 2a. Random-intercept with fixed linear & quadratic terms;
 %put Message: 2a. %SYSFUNC(DATETime(), DateTime16.0);

run;


*******************************************************************************;
* Conventional Model 4: Random-intercept linear term with fixed quadratic term ;
*******************************************************************************;

 proc nlmixed 
  data=tmp2
  maxiter=300 
  COV 
  tech=quanew;
  
  _S=_Score/_SEM;  * rescaleing the residual variance; 

  parms 
	   g00= 218.86 
       g10=   2.03
	   g20=  -0.56
       t00= 215.65
	   t10=	  7.58  t11= 4.78;

       b0 = g00 + u00 ;
       b1 = g10 + u10;
	   b2 = g20;

       mu =  ( b0*IntS + b1*lin + b2*quad )/_SEM;

   model 
	   _S ~ normal(mu,1.0);

   random
   		u00 u10 ~ normal([0, 0],
					 	 [t00,
					  	  t10, t11]) subject = StudID;

   title1 "&Dist &AnalyzeCohort MAP &Subj Scores";
   title2 "Conventional Model 4: Random-intercept linear term with fixed quadratic term";

 %put Message: 2a. -------------------------------------------------------------;
 %put Message: 2a. NLMIXED -- &Subj Conventional Model 4;
 %put Message: 2a. Random-intercept linear term with fixed quadratic term;
 %put Message: 2a. %SYSFUNC(DATETime(), DateTime16.0);

run;


*******************************************************************************;
* Conventional Model 5: Full random-coefficent model		 				   ;
*******************************************************************************;

 proc nlmixed 
  data=tmp2
  maxiter=300 
  COV 
  tech=quanew;
  
  _S=_Score/_SEM;  * rescaleing the residual variance; 

  parms 
	   g00= 218.94 
       g10=   2.08
	   g20=  -0.56
       t00= 215.61
	   t10=	 10.06  t11= 34.85
	   t20=   0.68	t21=  7.14	t22= 1.65;	

       b0 = g00 + u00;
       b1 = g10 + u10;
	   b2 = g20 + u20;

       mu =  ( b0*IntS + b1*lin + b2*quad )/_SEM;

   model 
	   _S ~ normal(mu,1.0);

   random
   		u00 u10 u20 ~ normal([0, 0, 0],
					 	 	 [t00,
					  	  	  t10, t11,
						      t20, t21, t22]) subject = StudID;

   title1 "&Dist &AnalyzeCohort MAP &Subj Scores";
   title2 "Conventional Model 5: Full random-coefficent model";

 %put Message: 2a. -------------------------------------------------------------;
 %put Message: 2a. NLMIXED -- &Subj Conventional Model 5: ;
 %put Message: 2a. Full random-coefficent model;
 %put Message: 2a. %SYSFUNC(DATETime(), DateTime16.0) ;

run;

