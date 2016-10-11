/*---------------------------------------------------------------------------
 Tyler Matta, 11/21/2014 (Based on Y. M. Thum, 4/22/2014)
 Fitting a taxonomy of mixed effects AP models a single cohort between 4th 
 and 9th grade.
----------------------------------------------------------------------------*/

options ls=146 ;

libname progs ".";
libname results ".\Results";

%let pgm=AP2;
%let Subj=Read;
%let AnalyzeCohort= 9; * Cohort 9 spans grades 4 - 9 ;        
%let Tagg=C9;
%let Dist=Horry;  

*----------------------------------------------------------------------------;
* 0. setup ODS 																 ;
*----------------------------------------------------------------------------;

 Proc printto new
              log=".\Results\&pgm._&Dist&Tagg._&Subj..log"
            print=".\Results\&pgm._&Dist&Tagg._&Subj..lst"; run;

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

  ods html path=".\Projects\AP_Model\Results" (URL=NONE)
       file="&pgm._&Dist&Subj&Tagg..html"
	   gpath=".\Projects\AP_Model\Graphs"
       (TITLE="&pgm &Dist &Subj") style=mystyle3;

*----------------------------------------------------------------------------;
* 1. descriptives 															 ;
*----------------------------------------------------------------------------;

  proc sort data=progs.&Dist&Subj.1b out=tmp; 
	by Cohort StudID descending Outcome;
	where Cohort in (&AnalyzeCohort) & Outcome="Y" & Test="MAP" & Grade ^= 9;
  run;


  proc print data= tmp (Obs=10);
  run ;


  proc freq data=tmp;
	tables Grade*Term;
  run;


  proc means data=tmp maxdec=3 fw=6
	mean std min max;
	class Grade Term;
	var _Score ;
  run;


*******************************************************************************;
** AP Model 1: Fixed-effects												   ;
*******************************************************************************;


 proc nlmixed 
  data=tmp 
  maxiter=300 
  COV 
  tech=quanew;
  

  _S=_Score/_SEM;  * rescaleing the residual variance; 

  parms 
	   g100= 221.32 
       g110=   2.54 
       g120=  -0.36 
       g200=   3.64 
       g210=   0.26 
       g220=  -0.30;


       b10 = g100;
       b11 = g110;
       b12 = g120;
	   b20 = g200;
       b21 = g210;
       b22 = g220;

       mu =  ( b10*IntS + b11*LinS + b12*QuaS + b20*IntG + b21*LinG + b22*QuaG )/_SEM;

   model 
	   _S ~ normal(mu,1.0);

   title1 "&Dist &AnalyzeCohort MAP &Subj Scores";
   title2 "AP Model 1: Fixed-effects";

 %put Message: 2a. -------------------------------------------------------------;
 %put Message: 2a. NLMIXED -- AP Model 1: Fixed-effects &Subj;
 %put Message: 2a. %SYSFUNC(DATETime(), DateTime16.0) ;
RUN;


*******************************************************************************;
** AP Model 2: Between-Year Random Intercept								   ;
*******************************************************************************;


 proc nlmixed 
  data=tmp 
  maxiter=300 
  COV 
  method=guass
  tech=quanew;
 

  _S=_Score/_SEM;  * rescaleing the residual variance; 

  parms 
	   g100= 218.97 
       g110=   2.61 
       g120=  -0.32
       g200=   3.78 
       g210=   0.47 
       g220=  -0.37
       t11=  215.74;

   bounds
       t11 > 0 ;

  	   b10 = g100 + u10;
   	   b11 = g110;
   	   b12 = g120;
       b20 = g200;
       b21 = g210;
       b22 = g220;

       mu =  ( b10*IntS + b11*LinS + b12*QuaS + b20*IntG + b21*LinG + b22*QuaG )/_SEM;

   model 
	   _S ~ normal(mu,1.0);

   random 
	   u10 ~ normal([0],
                    [t11]) subject=StudID;

   title1 "&Dist &AnalyzeCohort MAP &Subj Scores";
   title2 "AP Model 2: Between-Year Random Intercept";

 %put Message: 2a. -------------------------------------------------------------;
 %put Message: 2a. NLMIXED -- AP Model 2: Between-Year Random Intercept, &Subj;
 %put Message: 2a. %SYSFUNC(DATETime(), DateTime16.0) ;

 run;


*******************************************************************************;
** AP Model 3: Between-Year Random Intercept & Linear Slope					   ;
*******************************************************************************;

 proc nlmixed 
  data=tmp 
  maxiter=300 
  COV  
  tech=quanew;

  _S=_Score/_SEM;  * rescaleing the residual variance; 

   parms 
	   g100= 219.95 
       g110=   2.54 
       g120=  -0.35
       g200=   3.76 
       g210=   0.48 
       g220=  -0.38
       t11=  230.39
	   t21=    7.69	t22=    4.49;

   bounds
       t11 t22 > 0 ;

  	   b10 = g100 + u10;
   	   b11 = g110 + u11;
   	   b12 = g120;
       b20 = g200;
       b21 = g210;
       b22 = g220;

       mu =  ( b10*IntS + b11*LinS + b12*QuaS + b20*IntG + b21*LinG + b22*QuaG )/_SEM;

   model 
	   _S ~ normal(mu,1.0);

   random 
	   u10 u11 ~ normal([0,0],
                        [t11, t21, t22]) subject=StudID;

   title1 "&Dist &AnalyzeCohort MAP &Subj Scores";
   title2 "AP Model 3: Between-Year Random Intercept & Linear Slope";

 %put Message: 2a. -------------------------------------------------------------;
 %put Message: 2a. NLMIXED -- AP Model 3: Between-Year Random Intercept & Linear Slope, &Subj;
 %put Message: 2a. %SYSFUNC(DATETime(), DateTime16.0) ;

 run;

 

*******************************************************************************;
** AP Model 4: Between-Year Random Intercept, Linear Slope & Curvature		   ;
*******************************************************************************;

  proc nlmixed 
  data=tmp 
  maxiter=300 
  COV  
  tech=quanew;

  _S=_Score/_SEM;  * rescaleing the residual variance; 

   parms 
	   g100= 220.02 
       g110=   2.70 
       g120=  -0.30
       g200=   3.77 
       g210=   0.50 
       g220=  -0.38
       t11=  218.54
	   t21=    1.99	t22= 28.27
	   t31=   -1.38	t32=  6.27	t33= 1.60;

   bounds
       t11 t22 t33 > 0;

  	   b10 = g100 + u10;
   	   b11 = g110 + u11;
   	   b12 = g120 + u12;
       b20 = g200;
       b21 = g210;
       b22 = g220;

       mu =  ( b10*IntS + b11*LinS + b12*QuaS + b20*IntG + b21*LinG + b22*QuaG )/_SEM;


   model 
	   _S ~ normal(mu,1.0);

   random 
	   u10 u11 u12 ~ normal([0,0,0],
                            [t11, 
						     t21,t22,
						     t31,t32,t33]) subject=StudID;

   title1 "&Dist &AnalyzeCohort MAP &Subj Scores";
   title2 "AP Model 4: Between-Year Random Intercept, Linear Slope & Curvature";

 %put Message: 2a. -------------------------------------------------------------;
 %put Message: 2a. NLMIXED -- AP Model 4: Between-Year Random Intercept, Linear 
				   Slope and Curvature, &Subj;
 %put Message: 2a. %SYSFUNC(DATETime(), DateTime16.0) ;

 run;


*******************************************************************************;
** AP Model 5: Between-Year Random Intercept, Linear Slope & Curvature 		   ;
**              Within-Year Random Intercept								   ;
*******************************************************************************;


  proc nlmixed 
  data=tmp 
  maxiter=300 
  COV  
  tech=quanew;

  _S=_Score/_SEM;  * rescaleing the residual variance; 

   parms 
	   g100= 220.10 
       g110=   2.68 
       g120=  -0.30
       g200=   3.91 
       g210=   0.50 
       g220=  -0.38
       t11=  218.07
	   t21=    4.92	 t22=  28.41
	   t31=   -0.99	 t32=   6.26	t33=  1.59
	   t41=	  -7.12	 t42=   1.87	t43=  0.07	t44=  14.54;

   bounds
       t11 t22 t33 t44 > 0;

  	   b10 = g100 + u10;
   	   b11 = g110 + u11;
   	   b12 = g120 + u12;
       b20 = g200 + u20;
       b21 = g210;
       b22 = g220;

       mu =  ( b10*IntS + b11*LinS + b12*QuaS + b20*IntG + b21*LinG + b22*QuaG )/_SEM;


   model 
	   _S ~ normal(mu,1.0);

   random 
	   u10 u11 u12 u20 ~ normal([0,0,0,0],
                                [t11, 
						         t21,t22,
						         t31,t32,t33,
  								 t41,t42,t43,t44]) subject=StudID;

   title1 "&Dist &AnalyzeCohort MAP &Subj Scores";
   title2 "AP Model 5: Between-year Random Intercept, Linear Slope and Curvature 
					  & Within-year Random Intercept";

 %put Message: 2a. -------------------------------------------------------------;
 %put Message: 2a. NLMIXED -- AP Model 5: Between Year Random Intercept, Linear 
				   Slope and Curvature & Within-year Random Intercept, &Subj;
 %put Message: 2a. %SYSFUNC(DATETime(), DateTime16.0) ;

 run;


*******************************************************************************;
** AP Model 6: Between-Year Random Intercept, Linear Slope and Curvature &	   ;
**              Within-Year Random Intercept and Linear Slope				   ;
*******************************************************************************;


  proc nlmixed 
  data=tmp 
  maxiter=300 
  COV  
  tech=quanew;

  _S=_Score/_SEM;  * rescaleing the residual variance; 

   parms 
	   g100= 220.06 
       g110=   2.64 
       g120=  -0.31
       g200=   3.82 
       g210=   0.45 
       g220=  -0.38
       t11=  215.68
	   t21=    9.76	 t22=  31.85
	   t31=   -0.20	 t32=   6.67	t33=  1.62
	   t41=	   3.82	 t42=  10.59	t43=  1.30	t44= 34.82
	   t51=	   6.30	 t52=   4.64	t53=  0.63	t54= 11.52	t55=6.07;

   bounds
       t11 t22 t33 t44 > 0;

  	   b10 = g100 + u10;
   	   b11 = g110 + u11;
   	   b12 = g120 + u12;
       b20 = g200 + u20;
       b21 = g210 + u21;
       b22 = g220;

       mu =  ( b10*IntS + b11*LinS + b12*QuaS + b20*IntG + b21*LinG + b22*QuaG )/_SEM;


   model 
	   _S ~ normal(mu,1.0);

   random 
	   u10 u11 u12 u20 u21 ~ normal([0,0,0,0,0],
                                    [t11, 
						             t21,t22,
						             t31,t32,t33,
  								     t41,t42,t43,t44,
								     t51,t52,t53,t54,t55]) subject=StudID;

   title1 "&Dist &AnalyzeCohort MAP &Subj Scores";
   title2 "AP Model 6: Between-year Random Intercept, Linear Slope and Curvature
					  & Within-year Random Intercept and Linear Slope";
 %put Message: 2a. -------------------------------------------------------------;
 %put Message: 2a. AP Model 6: Between Year Random Intercept, Linear Slope and 
				   Curvature & Within-year Random Intercept and Linear Slope, &Subj;
 %put Message: 2a. %SYSFUNC(DATETime(), DateTime16.0) ;

 run;



*******************************************************************************;
** AP Model 7: Between-Year Random Intercept, Linear Slope and Curvature &	   ;
**              Within-Year Random Intercept, Linear Slope and Curvature	   ;
*******************************************************************************;


  proc nlmixed 
  data=tmp 
  maxiter=300 
  COV  
  tech=quanew;

  _S=_Score/_SEM;  * rescaleing the residual variance; 

   parms 
	   g100= 220.08 
       g110=   2.63 
       g120=  -0.31
       g200=   3.85 
       g210=   0.43 
       g220=  -0.37
       t11=  214.01
	   t21=    7.48	 t22= 38.83
	   t31=   -0.52	 t32=  8.75	t33=  2.21
	   t41=	   0.32	 t42= 19.70	t43=  3.97	t44= 44.44
	   t51=	   6.91  t52= 27.43	t53=  6.88  t54= 37.95	t55= 65.68
	   t61=	  -0.61	 t62= -6.13	t63= -1.65  t64= -7.20	t65=-15.65	t66= 4.10;

   bounds
       t11 t22 t33 t44 > 0;

  	   b10 = g100 + u10;
   	   b11 = g110 + u11;
   	   b12 = g120 + u12;
       b20 = g200 + u20;
       b21 = g210 + u21;
       b22 = g220 + u22;

       mu =  ( b10*IntS + b11*LinS + b12*QuaS + b20*IntG + b21*LinG + b22*QuaG )/_SEM;


   model 
	   _S ~ normal(mu,1.0);

   random 
	   u10 u11 u12 u20 u21 u22 ~ normal([0,0,0,0,0,0],
                                    [t11, 
						             t21,t22,
						             t31,t32,t33,
  								     t41,t42,t43,t44,
								     t51,t52,t53,t54,t55,
									 t61,t62,t63,t64,t65,t66]) subject=StudID;

   title1 "&Dist &AnalyzeCohort MAP &Subj Scores";
   title2 "AP Model 6: Between-year Random Intercept, Linear Slope and Curvature 
		  & Within-year Random Intercept, Linear Slope and Curvature";
 %put Message: 2a. -------------------------------------------------------------;
 %put Message: 2a. NLMIXED -- AP Model 6: Between-year Random Intercept, Linear 
				   Slope and Curvature & Within-year Random Intercept, Linear 
				   Slope and Curvature, &Subj;
 %put Message: 2a. %SYSFUNC(DATETime(), DateTime16.0) ;

 run;
