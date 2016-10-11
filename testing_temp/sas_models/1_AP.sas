
/*---------------------------------------------------------------------------
 T. H. Matta, 12/8/2014

Model Comparison 1: Recovering Means
----------------------------------------------------------------------------*/

options ls=146 ;

libname l1 ".";
libname l2 ".\Results";

%let pgm=AP1;
 
%let Tagg=C9;
%let Dist=Horry;  



*----------------------------------------------------------------------------;
* 0. setup ODS                     ; 
*----------------------------------------------------------------------------;

 Proc printto new
              log=".\Results\&pgm._&Dist&Tagg..log"
            print=".\Results\&pgm._&Dist&Tagg..lst"; run;



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

  ods html path=".\Results" (URL=NONE) 
           file="&pgm._&Dist&Tagg..html"
         (title="&pgm &Dist") style=mystyle3;

*----------------------------------------------------------------------------;
* 1. Data                      ;
*----------------------------------------------------------------------------;

DATA growth;
 INPUT grade term $ math read int time time2 fInt fLin fQuad fsInt fsLin fsQuad;
 CARDS;
9 Spring 239 224 1  1.9  3.61 1  1  1 -0.9 -0.9  -0.81
9 Fall   233 218 1  1.0  1.00 1  1  1  0.0  0.0   0.00
8 Spring 236 222 1  0.9  0.81 1  0  0 -0.9  0.0   0.00
8 Fall   231 218 1  0.0  0.00 1  0  0  0.0  0.0   0.00
7 Spring 232 218 1 -0.1  0.01 1 -1  1 -0.9  0.9  -0.81
7 Fall   228 214 1 -1.1  1.00 1 -1  1  0.0  0.0   0.00
6 Spring 227 214 1 -2.0  1.21 1 -2  4 -0.9  1.8  -3.24
6 Fall   222 210 1 -2.1  4.00 1 -2  4  0.0  0.0   0.00
5 Spring 225 212 1 -3.0  4.41 1 -3  9 -0.9  2.7  -7.29
5 Fall   216 205 1 -3.1  9.00 1 -3  9  0.0  0.0   0.00
4 Spring 216 205 1 -4.0  9.61 1 -4 16 -0.9  3.6 -12.96
4 Fall   206 198 1 -4.1 16.00 1 -4 16  0.0  0.0   0.00
;
RUN; 


PROC PRINT DATA=growth;
RUN; 


*----------------------------------------------------------------------------;
* 1. Math Models                    ;
*----------------------------------------------------------------------------;

* Conventional Polynomial Models;
  PROC REG DATA=growth;
      model math = int time / noint p dwProb;
    title "Conventional Polynomial 1 MAP Math Scores";
  RUN;


  PROC REG DATA=growth;
      model math = int time time2 / noint p dwProb;
    title "Conventional Polynomial 2 MAP Math Scores";
  RUN;

* AP Models;
   PROC REG DATA=growth;
      model math = fInt fLin fQuad fsInt fsLin fsQuad / noint p dwProb;
    title "Addative Polynomial 1 MAP Math Scores";

   RUN;


   PROC REG DATA=growth;
      model math = fInt fLin fQuad fsInt fsQuad / noint p dwProb;
    title "Addative Polynomial 2 MAP Math Scores";
   RUN;


*----------------------------------------------------------------------------;
* 1. Reading Models                    ;
*----------------------------------------------------------------------------;

* Conventional Polynomial Models ;
  PROC REG DATA=growth;
      model read = int time / noint p dwProb;
    title "Conventional Polynomial 1 MAP Reading Scores";
  RUN;


  PROC REG DATA=growth;
      model read = int time time2 / noint p dwProb;
    title "Conventional Polynomial 2 MAP Reading Scores";
  RUN;

* AP Models;
   PROC REG DATA=growth;
      model read = fInt fLin fQuad fsInt fsLin fsQuad / noint p dwProb;
    title "Addative Polynomial 1 MAP Reading Scores";
   RUN;

   
   PROC REG DATA=growth;
      model read = fInt fLin fQuad fsInt fsQuad / noint p dwProb;
    title "Addative Polynomial 2 MAP Reading Scores";
  RUN;

endsas;
