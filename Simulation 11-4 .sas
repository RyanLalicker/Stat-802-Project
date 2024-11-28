/* Step 1: Define AR(1) Covariance Structure in PROC IML */
proc iml;
    n = 20;                    /* Number of subjects per treatment, updated to 20 */
    mean = {0 0 0 0 0};        /* Mean for each week */
    T = 5;                     /* Number of repeated measures (weeks) */
    rho = 0.2;                 /* AR(1) correlation parameter */
    sigma2 = {2 2 2 2 2};      /* Variance for each week */
    
    /* Construct AR(1) covariance matrix */
    cov = j(T, T, 0);
    do i = 1 to T;
        do j = 1 to T;
            cov[i, j] = sqrt(sigma2[i] * sigma2[j]) * rho**abs(i - j);
        end;
    end;
    
    /* Print covariance matrix */
    print "Covariance Matrix:", cov;

    /* Generate simulated data using the covariance matrix */
    call randseed(12345);      /* Set random seed */
    x = randnormal(n, mean, cov); /* Simulate AR(1) correlated data */
    cname = {"t1", "t2", "t3", "t4", "t5"};
    
    /* Print the simulated data matrix directly */
    print "Simulated Data Matrix (x):", x;
    /* Print Sample mean */
    samplemean = x[:,];
    print samplemean n;

    /* Create dataset from simulated data */
    create inputdatacb from x[colname=cname];
    append from x;
close inputdatacb;
quit;

/* Step 2: Display the Simulated Data as a SAS Table */
proc print data=inputdatacb label;
    title "Simulated Data with AR(1) Covariance Structure";
run;

/* Step 3: Define Treatment Structure and Random Effects */
data rptm_simulation;
    retain Subject 0;
    keep Inoculation_Method Thickness Week Batches Response;

    array weeks[5] t1-t5;

    /* Define mean values for each combination of factors and week */
    if _n_ = 1 then do;
        array mean_values[4,2,5] _temporary_ (
            /* Dry, 1/4 inch */
            4.26, 4.25, 4.47, 4.33, 4.54,
            /* Dry, 1/8 inch */
            4.91, 4.95, 4.67, 4.56, 4.97,
            /* Wet, 1/4 inch */
            4.21, 4.57, 4.65, 4.49, 4.38,
            /* Wet, 1/8 inch */
            4.86, 4.78, 4.62, 4.32, 4.22
        );
    end;

    /* Simulation parameters */
    sigma_batch = sqrt(0.029); /* Batch variance */
    sigma_resid = sqrt(0.017); /* Residual variance */

    /* Loop through each combination of factors */
    do Batches = 1 to 5; /* Number of batches */
        batch_effect = rand("Normal", 0, sigma_batch); /* Random batch effect */

        do Inoculation_Method = "Dry", "Wet";
            do Thickness = "1/4-inch", "1/8-inch";
                Subject + 1;
                set inputdatacb;

                /* Generate response for each week with AR(1) structure */
                do Week = 1 to 5;
                    Mean_Value = mean_values[
                        (Inoculation_Method="Dry")*1 + (Inoculation_Method="Wet")*2,
                        (Thickness="1/4-inch")*1 + (Thickness="1/8-inch")*2,
                        Week
                    ];
                    Response = Mean_Value + batch_effect + weeks[Week];
                    output;
                end;
            end;
        end;
    end;
run;

/* Step 4: Display the Simulated Data in a Structured Format */
proc print data=rptm_simulation label;
    title "Simulated Data for 2x2 Factorial Design with Repeated Measures";
run;



/* Step 5: Analyze the Simulated Data Using PROC GLIMMIX */
proc glimmix data=rptm_simulation;
    class Batches Inoculation_Method Thickness Week;
    model Response = Inoculation_Method|Thickness|Week;
    random intercept / subject=Batches;
    random Week / subject=Batches*Inoculation_Method*Thickness type=ar(1) residual;
    
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