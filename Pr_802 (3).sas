
data rptm_means;
input Inoculation_Method $ Thickness $ @@; 
do Week=1 to 5 by 1; 
	input mu @@; 
	output; 
end;
datalines;
Dry 1/4 4.26 4.25 4.47 4.33 4.54
Dry 1/8 4.91 4.95 4.67 4.56 4.97
Wet 1/4 4.21 4.57 4.65 4.49 4.38
Wet 1/8 4.86 4.78 4.62 4.32 4.22
;
data rptm_design;
 set rptm_means;
 do Batches = 1 to 5; /* Creating 5 blocks (batches) */
  output;
 end;
run;
proc print data=rptm_design;
run;

/* Creating Model */
proc glimmix data=rptm_design;
    class Batches Inoculation_Method Thickness Week;
    model mu = Inoculation_Method|Thickness|Week;
    random intercept / subject=Batches;
    random Week / subject=Batches*Inoculation_Method*Thickness type=ar(1) residual;
    parms (.029)(0.017)(.028)/hold=1,2,3;  /* Provide 3 parameters for variance components */
    lsmeans Inoculation_Method*Thickness*Week / slicediff=Week cl;
    /* Define main effect contrasts */
    contrast 'Dry vs Wet' 
        Inoculation_Method 1 -1;
    contrast '1/4 vs 1/8 inches' 
        Thickness 1 -1;
    /* Define interaction contrasts */
   contrast 'Dry vs Wet at 1/4 Inches' 
        Inoculation_Method 1 -1 Inoculation_Method*Thickness 1 0 -1 0; 
    contrast 'Dry vs Wet at 1/8 Inches' 
        Inoculation_Method 1 -1 Inoculation_Method*Thickness 0 1 0 -1; 
    contrast '1/4 vs 1/8 inches for Dry inoculation'
        Thickness 1 -1 Inoculation_Method*Thickness 1 -1 0 0;
    contrast '1/4 vs 1/8 inches for Wet inoculation'
        Thickness 1 -1 Inoculation_Method*Thickness 0 0 1 -1;
               
    ods output contrasts=f_contrast tests3=f_anova;
run;
/*Power*/
data power;
    set f_contrast f_anova;
    ncparm = numdf * fvalue;
    alpha = 0.05;
    fcrit = finv(1-alpha, numdf, dendf, 0);
    power = 1 - probf(fcrit, numdf, dendf, ncparm);
run;
proc print data=power;
run;




