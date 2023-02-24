# Check print.xpose_data returns a proper message

    Code
      capture_output(print(xpdb_ex_pk))
    Output
      [1] "run001.lst overview: \n - Software: nonmem 7.3.0 \n - Attached files (memory usage 1.4 Mb): \n   + obs tabs: $prob no.1: catab001.csv, cotab001, patab001, sdtab001 \n   + sim tabs: $prob no.2: simtab001.zip \n   + output files: run001.cor, run001.cov, run001.ext, run001.grd, run001.phi, run001.shk \n   + special: <none> \n - gg_theme: theme_readable \n - xp_theme: theme_xp_default \n - Options: dir = data, quiet = FALSE, manual_import = NULL"

---

    Code
      capture_output(print(xpdb_modified))
    Output
      [1] "run001.lst overview: \n - Software: nonmem 7.3.0 \n - Attached files (memory usage 425.4 Kb): \n   + obs tabs: $prob no.1 (modified): catab001.csv, cotab001, patab001, sdtab001 \n   + sim tabs: $prob no.2 (modified): simtab001.zip \n   + output files: run001.cor, run001.cov, run001.ext (modified), run001.grd, run001.phi, run001.shk \n   + special: vpc continuous (#3, modified) \n - gg_theme: theme_readable \n - xp_theme: theme_xp_default \n - Options: dir = data, quiet = FALSE, manual_import = NULL"

# Check prm_table returns a proper message

    Code
      capture_output(prm_table(xpdb_ex_pk, .problem = 1))
    Output
      [1] "\nReporting transformed parameters:\nFor the OMEGA and SIGMA matrices, values are reported as standard deviations for the diagonal elements and as correlations for the off-diagonal elements. The relative standard errors (RSE) for OMEGA and SIGMA are reported on the approximate standard deviation scale (SE/variance estimate)/2. Use `transform = FALSE` to report untransformed parameters.\n\nEstimates for $prob no.1, subprob no.0, method foce\n Parameter  Label      Value        RSE\n THETA1     TVCL       26.29        0.03391\n THETA2     TVV        1.348        0.0325\n THETA3     TVKA       4.204        0.1925\n THETA4     LAG        0.208        0.07554\n THETA5     Prop. Err  0.2046       0.1097\n THETA6     Add. Err   0.01055      0.3466\n THETA7     CRCL on CL 0.007172     0.2366\n OMEGA(1,1) IIV CL     0.2701       0.08616\n OMEGA(2,2) IIV V      0.195        0.1643\n OMEGA(3,3) IIV KA     1.381        0.1463\n SIGMA(1,1)            1        fix  - "

---

    Code
      capture_output(prm_table(xpdb_ex_pk, transform = FALSE))
    Output
      [1] "\nReporting untransformed parameters:\nFor the OMEGA and SIGMA matrices, values are reported as variances for the diagonal elements and as covariances for the off-diagonal elements.\n\nEstimates for $prob no.1, subprob no.0, method foce\n Parameter  Label      Value        SE\n THETA1     TVCL       26.29        0.8915\n THETA2     TVV        1.348        0.04381\n THETA3     TVKA       4.204        0.8091\n THETA4     LAG        0.208        0.01571\n THETA5     Prop. Err  0.2046       0.02244\n THETA6     Add. Err   0.01055      0.003658\n THETA7     CRCL on CL 0.007172     0.001697\n OMEGA(1,1) IIV CL     0.07295      0.01257\n OMEGA(2,2) IIV V      0.03802      0.0125\n OMEGA(3,3) IIV KA     1.907        0.5582\n SIGMA(1,1)            1        fix  - "

# Check summary.xpose_data returns a proper message

    Code
      capture_output(summary(xpdb_ex_pk))
    Output
      [1] "\nSummary for problem no. 0 [Global information] \n - Software                      @software   : nonmem\n - Software version              @version    : 7.3.0\n - Run directory                 @dir        : data\n - Run file                      @file       : run001.lst\n - Run number                    @run        : run001\n - Reference model               @ref        : 000\n - Run description               @descr      : NONMEM PK example for xpose\n - Run start time                @timestart  : Mon Oct 16 13:34:28 CEST 2017\n - Run stop time                 @timestop   : Mon Oct 16 13:34:35 CEST 2017\n\nSummary for problem no. 1 [Parameter estimation] \n - Input data                    @data       : ../../mx19_2.csv\n - Number of individuals         @nind       : 74\n - Number of observations        @nobs       : 476\n - ADVAN                         @subroutine : 2\n - Estimation method             @method     : foce-i\n - Termination message           @term       : MINIMIZATION SUCCESSFUL\n - Estimation runtime            @runtime    : 00:00:02\n - Objective function value      @ofv        : -1403.905\n - Number of significant digits  @nsig       : 3.3\n - Covariance step runtime       @covtime    : 00:00:03\n - Condition number              @condn      : 21.5\n - Eta shrinkage                 @etashk     : 9.3 [1], 28.7 [2], 23.7 [3]\n - Epsilon shrinkage             @epsshk     : 14.9 [1]\n - Run warnings                  @warnings   : (WARNING 2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.\n\nSummary for problem no. 2 [Model simulations] \n - Input data                    @data       : ../../mx19_2.csv\n - Number of individuals         @nind       : 74\n - Number of observations        @nobs       : 476\n - Estimation method             @method     : sim\n - Number of simulations         @nsim       : 20\n - Simulation seed               @simseed    : 221287\n - Run warnings                  @warnings   : (WARNING 2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.\n                                               (WARNING 22) WITH $MSFI AND \"SUBPROBS\", \"TRUE=FINAL\" ..."

# Check list_vars returns a proper message

    Code
      capture_output(list_vars(xpdb_ex_pk, .problem = 1))
    Output
      [1] "\nList of available variables for problem no. 1 \n - Subject identifier (id)               : ID\n - Dependent variable (dv)               : DV\n - Independent variable (idv)            : TIME\n - Dose amount (amt)                     : AMT\n - Event identifier (evid)               : EVID\n - Model typical predictions (pred)      : PRED\n - Model individual predictions (ipred)  : IPRED\n - Model parameter (param)               : KA, CL, V, ALAG1\n - Eta (eta)                             : ETA1, ETA2, ETA3\n - Residuals (res)                       : CWRES, IWRES, RES, WRES\n - Categorical covariates (catcov)       : SEX, MED1, MED2\n - Continuous covariates (contcov)       : CLCR, AGE, WT\n - Compartment amounts (a)               : A1, A2\n - Not attributed (na)                   : DOSE, SS, II, TAD, CPRED"

# Check list_data returns a proper message

    Code
      capture_output(list_data(xpdb_ex_pk))
    Output
      [1] "Data:\n  problem simtab data                   modified\n    <int> <lgl>  <list>                 <lgl>   \n1       1 FALSE  <tibble [550 × 31]>    FALSE   \n2       2 TRUE   <tibble [11,000 × 12]> FALSE   "

# Check list_files returns a proper message

    Code
      capture_output(list_files(xpdb_ex_pk))
    Output
      [1] "Files:\n  name       extension problem subprob method data               modified\n  <chr>      <chr>       <dbl>   <dbl> <chr>  <list>             <lgl>   \n1 run001.cor cor             1       0 foce   <tibble [14 × 15]> FALSE   \n2 run001.cov cov             1       0 foce   <tibble [14 × 15]> FALSE   \n3 run001.ext ext             1       0 foce   <tibble [28 × 16]> FALSE   \n4 run001.grd grd             1       0 foce   <tibble [21 × 11]> FALSE   \n5 run001.phi phi             1       0 foce   <tibble [74 × 12]> FALSE   \n6 run001.shk shk             1       0 foce   <tibble [7 × 5]>   FALSE   "

# Check list_special returns a proper message

    Code
      capture_output(list_special(xpdb_ex_pk %>% vpc_data(quiet = TRUE)))
    Output
      [1] "Specials:\n  problem method type       data              modified\n    <dbl> <chr>  <chr>      <list>            <lgl>   \n1       3 vpc    continuous <named list [20]> FALSE   "

