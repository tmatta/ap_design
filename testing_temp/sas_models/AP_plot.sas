options ls=132;
 

data tst;
  input SchYr Term $ iDays RIT;
  iWks1=(iDays-200)/5;
  iWks2=iWks1*iWks1;
  iWks3=iWks2*iWks1;
  iWks4=iWks3*iWks1;
cards;
2011 F  15.52  177.67
2011 W  81.45  186.91
2011 S 155.27  194.03
2012 F 192.38  191.74
2012 W 259.21  199.28
2012 S 332.25  205.73
2013 F 367.67  202.84
2013 W 437.17  209.54
2013 S 509.49  216.08
;

proc mixed data=tst;
  model RIT = iWks1 iWks2 iWks3 iWks4 /s ;
 run;

proc mixed data=tst;
  model RIT = iWks1 /s outp=polyn;
 run;

proc print data=polyn round;
run;

    proc export dbms=excelcs data=polyn
         OUTFILE= "D:\NWEA\Norms 2015\Model\AP Compare.xls" replace;
         SHEET="Polyn2";
    run;

data tst;
   set tst;

   * for array a, a1-3 is the 2011 school year, b1-3 is the 2012 school year, and c1-3 is the 2012 school year;
   * 1= fall, 2=winter, 3=spring;

	array a a1-a3 b1-b3 c1-c3;
  
  do over a; a=0; end;

  if SchYr=2011 then do; 
	a1=1; 
	a2=(iDays - 0.0*180 - 20)/5; 
	a3=a2**2; 
  end;

  else if SchYr=2012 then do; 
	b1=1; 
	b2=(iDays - 1.0*180 - 20)/5; 
	b3=b2**2; 
  end;

  else if SchYr=2013 then do; 
	c1=1; 
	c2=(iDays - 2.0*180 - 20)/5; 
	c3=c2**2; 
  end;

  Fall_Int =  1.0*a1 + 1.0*b1 + 1.0*c1;
  Fall_Lin = -1.0*a1 + 0.0*b1 + 1.0*c1;
  Fall_Qua =  1.0*a1 + 0.0*b1 + 1.0*c1;
  Lin_Int  =  1.0*a2 + 1.0*b2 + 1.0*c2;
  Lin_Lin  = -1.0*a2 + 0.0*b2 + 1.0*c2;
  Qua_Int  =  1.0*a3 + 1.0*b3 + 1.0*c3;
  
  run;

 proc print data=tst round;
 run;

 proc mixed data=tst;
  model RIT = Fall_Int Fall_Lin Fall_Qua Lin_Int Lin_Lin Qua_Int / s noint outp=polyn;
 run;

 proc print data=polyn round;
 run;

    proc export dbms=excelcs data=polyn
         OUTFILE= "D:\NWEA\Norms 2015\Model\AP Compare.xls" replace;
         SHEET="AP";
    run;

proc iml;

 A ={ 1 0 0 0 0 0 0 0 0,
      1 1 1 0 0 0 0 0 0,
      1 2 4 0 0 0 0 0 0,
      0 0 0 1 0 0 0 0 0,
      0 0 0 1 1 1 0 0 0,
      0 0 0 1 2 4 0 0 0,
      0 0 0 0 0 0 1 0 0,
      0 0 0 0 0 0 1 1 1,
      0 0 0 0 0 0 1 2 4};

 B ={ 1 -1 1 0  0 0,
      0  0 0 1 -1 0,
      0  0 0 0  0 1,
      1  0 0 0  0 0,
      0  0 0 1  0 0,
      0  0 0 0  0 1,
      1  1 1 0  0 0,
      0  0 0 1  1 0,
      0  0 0 0  0 1};

 K = A*B;

 print A B, K;

quit;

endsas;

 K = 
1 -1  1  0  0  0
1 -1  1  1 -1  1
1 -1  1  2 -2  4
1  0  0  0  0  0
1  0  0  1  0  1
1  0  0  2  0  4
1  1  1  0  0  0
1  1  1  1  1  1
1  1  1  2  2  4
