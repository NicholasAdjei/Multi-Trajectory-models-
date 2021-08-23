
set maxvar 30000
clear all 
use "G:\Abt_PrEv\Projekte\Nicholas ADJEI\Liverpool MCS data\mcs1to6_singletons.dta"
set more off

drop if fovwt2==-1
svyset, clear
svyset [pweight=fovwt2], strata(pttype2) psu(sptn00) fpc( nh2 )  // surveyweight


***Exposures for trajectory analysis**************************
/*The main exposures were trajectories family adversity and poverty from age 9 months to 14 years. 
Longitudinal measures of exposure were created from the indicators of family adversity of interest (i.e., poor parental mental health, domestic violence and abuse, and frequent parental alcohol use)and poverty from 9 months to age 14 years.
A binary score was constructed for all exposures throughout childhood1(for full details see Box 1 in the paper)*/


****Generate a set of time variables for trajectory groups********

forval i = 1/6 { 
  generate t_`i' = `i'
}
recode t_1 (1= 0) // 9 months
recode t_2 (2= 3) // 3 years
recode t_3 (3= 5) // 5 years
recode t_4 (4= 7) // 7 years
recode t_5 (5= 11) // 11 years 
recode t_6 (6= 14) // 14 years


/Group based trajectory analysis//

//1. Choosing number of groups// The models were selected based on the Bayesian information criterion (BIC) 

//Fitting 1 trajectory group//

traj, multgroups(1) var1(M_MentalH_*) indep1(t_*) order1(3) model1(logit) ///
      var2(M_Alcohol_*) indep2(t_*) order2(3) model2(logit) ///
      var3(IPV_*) indep3(t_*) order3(3) model3(logit) ///
	  var4(poverty_*) indep4(t_*) order4(3) model4(logit)
	  
****** Fit statstics: average posterior probability, the odds of correct classification, and the observed classification proportion versus the expected classification proportion ******
set more off

*I made a function to print out summary stats
program summary_table_procTraj_Toxictrio
    preserve
    *now lets look at the average posterior probability
	gen Mp = 0
	foreach i of varlist _traj_ProbG* {
	    replace Mp = `i' if `i' > Mp 
	}
    sort _traj_Group
    *and the odds of correct classification
    by _traj_Group: gen countG = _N
    by _traj_Group: egen groupAPP = mean(Mp)
    by _traj_Group: gen counter = _n
    gen n = groupAPP/(1 - groupAPP)
    gen p = countG/ _N
    gen d = p/(1-p)
    gen occ = n/d
    *Estimated proportion for each group
    scalar c = 0
    gen TotProb = 0
    foreach i of varlist _traj_ProbG* {
       scalar c = c + 1
       quietly summarize `i'
       replace TotProb = r(sum)/ _N if _traj_Group == c 
    }
	gen d_pp = TotProb/(1 - TotProb)
	gen occ_pp = n/d_pp
    *This displays the group number [_traj_~p], 
    *the count per group (based on the max post prob), [countG]
    *the average posterior probability for each group, [groupAPP]
    *the odds of correct classification (based on the max post prob group assignment), [occ] 
    *the odds of correct classification (based on the weighted post. prob), [occ_pp]
    *and the observed probability of groups versus the probability [p]
    *based on the posterior probabilities [TotProb]
    list _traj_Group countG groupAPP occ occ_pp p TotProb if counter == 1
	
	list _traj_Group countG groupAPP occ occ_pp  p if counter == 1
	
    restore
end

summary_table_procTraj_Toxictrio


//Fitting 2 trajectory groups//

traj, multgroups(2) var1(M_MentalH_*) indep1(t_*) order1(3 3) model1(logit) ///
      var2(M_Alcohol_*) indep2(t_*) order2(3 3) model2(logit) ///
      var3(IPV_*) indep3(t_*) order3(3 3) model3(logit) ///
	  var4(poverty_*) indep4(t_*) order4(3 3) model4(logit) 

****** Fit statstics: average posterior probability, the odds of correct classification, and the observed classification proportion versus the expected classification proportion ******
set more off

*I made a function to print out summary stats
program summary_table_procTraj_Toxictrio
    preserve
    *now lets look at the average posterior probability
	gen Mp = 0
	foreach i of varlist _traj_ProbG* {
	    replace Mp = `i' if `i' > Mp 
	}
    sort _traj_Group
    *and the odds of correct classification
    by _traj_Group: gen countG = _N
    by _traj_Group: egen groupAPP = mean(Mp)
    by _traj_Group: gen counter = _n
    gen n = groupAPP/(1 - groupAPP)
    gen p = countG/ _N
    gen d = p/(1-p)
    gen occ = n/d
    *Estimated proportion for each group
    scalar c = 0
    gen TotProb = 0
    foreach i of varlist _traj_ProbG* {
       scalar c = c + 1
       quietly summarize `i'
       replace TotProb = r(sum)/ _N if _traj_Group == c 
    }
	gen d_pp = TotProb/(1 - TotProb)
	gen occ_pp = n/d_pp
    *This displays the group number [_traj_~p], 
    *the count per group (based on the max post prob), [countG]
    *the average posterior probability for each group, [groupAPP]
    *the odds of correct classification (based on the max post prob group assignment), [occ] 
    *the odds of correct classification (based on the weighted post. prob), [occ_pp]
    *and the observed probability of groups versus the probability [p]
    *based on the posterior probabilities [TotProb]
    list _traj_Group countG groupAPP occ occ_pp p TotProb if counter == 1
	
	list _traj_Group countG groupAPP occ occ_pp  p if counter == 1
	
    restore
end

summary_table_procTraj_Toxictrio


//Fitting 3 trajectory groups//

traj, multgroups(3) var1(M_MentalH_*) indep1(t_*) order1(3 3 3) model1(logit) ///
      var2(M_Alcohol_*) indep2(t_*) order2(3 3 3) model2(logit) ///
      var3(IPV_*) indep3(t_*) order3(3 3 3) model3(logit) ///
	  var4(poverty_*) indep4(t_*) order4(3 3 3) model4(logit) 

****** Fit statstics: average posterior probability, the odds of correct classification, and the observed classification proportion versus the expected classification proportion ******

set more off

*I made a function to print out summary stats
program summary_table_procTraj_Toxictrio
    preserve
    *now lets look at the average posterior probability
	gen Mp = 0
	foreach i of varlist _traj_ProbG* {
	    replace Mp = `i' if `i' > Mp 
	}
    sort _traj_Group
    *and the odds of correct classification
    by _traj_Group: gen countG = _N
    by _traj_Group: egen groupAPP = mean(Mp)
    by _traj_Group: gen counter = _n
    gen n = groupAPP/(1 - groupAPP)
    gen p = countG/ _N
    gen d = p/(1-p)
    gen occ = n/d
    *Estimated proportion for each group
    scalar c = 0
    gen TotProb = 0
    foreach i of varlist _traj_ProbG* {
       scalar c = c + 1
       quietly summarize `i'
       replace TotProb = r(sum)/ _N if _traj_Group == c 
    }
	gen d_pp = TotProb/(1 - TotProb)
	gen occ_pp = n/d_pp
    *This displays the group number [_traj_~p], 
    *the count per group (based on the max post prob), [countG]
    *the average posterior probability for each group, [groupAPP]
    *the odds of correct classification (based on the max post prob group assignment), [occ] 
    *the odds of correct classification (based on the weighted post. prob), [occ_pp]
    *and the observed probability of groups versus the probability [p]
    *based on the posterior probabilities [TotProb]
    list _traj_Group countG groupAPP occ occ_pp p TotProb if counter == 1
	
	list _traj_Group countG groupAPP occ occ_pp  p if counter == 1
	
    restore
end

summary_table_procTraj_Toxictrio


//Fitting 4 trajectory groups//

traj, multgroups(4) var1(M_MentalH_*) indep1(t_*) order1(3 3 3 3) model1(logit) ///
      var2(M_Alcohol_*) indep2(t_*) order2(3 3 3 3) model2(logit) ///
      var3(IPV_*) indep3(t_*) order3(3 3 3 3) model3(logit) ///
	  var4(poverty_*) indep4(t_*) order4(3 3 3 3) model4(logit) 

****** Fit statstics: average posterior probability, the odds of correct classification, and the observed classification proportion versus the expected classification proportion ******

set more off

*I made a function to print out summary stats
program summary_table_procTraj_Toxictrio
    preserve
    *now lets look at the average posterior probability
	gen Mp = 0
	foreach i of varlist _traj_ProbG* {
	    replace Mp = `i' if `i' > Mp 
	}
    sort _traj_Group
    *and the odds of correct classification
    by _traj_Group: gen countG = _N
    by _traj_Group: egen groupAPP = mean(Mp)
    by _traj_Group: gen counter = _n
    gen n = groupAPP/(1 - groupAPP)
    gen p = countG/ _N
    gen d = p/(1-p)
    gen occ = n/d
    *Estimated proportion for each group
    scalar c = 0
    gen TotProb = 0
    foreach i of varlist _traj_ProbG* {
       scalar c = c + 1
       quietly summarize `i'
       replace TotProb = r(sum)/ _N if _traj_Group == c 
    }
	gen d_pp = TotProb/(1 - TotProb)
	gen occ_pp = n/d_pp
    *This displays the group number [_traj_~p], 
    *the count per group (based on the max post prob), [countG]
    *the average posterior probability for each group, [groupAPP]
    *the odds of correct classification (based on the max post prob group assignment), [occ] 
    *the odds of correct classification (based on the weighted post. prob), [occ_pp]
    *and the observed probability of groups versus the probability [p]
    *based on the posterior probabilities [TotProb]
    list _traj_Group countG groupAPP occ occ_pp p TotProb if counter == 1
	
	list _traj_Group countG groupAPP occ occ_pp  p if counter == 1
	
    restore
end

summary_table_procTraj_Toxictrio


//Fitting 5 trajectory groups//

traj, multgroups(5) var1(M_MentalH_*) indep1(t_*) order1(3 3 3 3 3) model1(logit) ///
      var2(M_Alcohol_*) indep2(t_*) order2(3 3 3 3 3) model2(logit) ///
      var3(IPV_*) indep3(t_*) order3(3 3 3 3 3) model3(logit) ///
	  var4(poverty_*) indep4(t_*) order4(3 3 3 3 3) model4(logit) 

****** Fit statstics: average posterior probability, the odds of correct classification, and the observed classification proportion versus the expected classification proportion ******

set more off

*I made a function to print out summary stats
program summary_table_procTraj_Toxictrio
    preserve
    *now lets look at the average posterior probability
	gen Mp = 0
	foreach i of varlist _traj_ProbG* {
	    replace Mp = `i' if `i' > Mp 
	}
    sort _traj_Group
    *and the odds of correct classification
    by _traj_Group: gen countG = _N
    by _traj_Group: egen groupAPP = mean(Mp)
    by _traj_Group: gen counter = _n
    gen n = groupAPP/(1 - groupAPP)
    gen p = countG/ _N
    gen d = p/(1-p)
    gen occ = n/d
    *Estimated proportion for each group
    scalar c = 0
    gen TotProb = 0
    foreach i of varlist _traj_ProbG* {
       scalar c = c + 1
       quietly summarize `i'
       replace TotProb = r(sum)/ _N if _traj_Group == c 
    }
	gen d_pp = TotProb/(1 - TotProb)
	gen occ_pp = n/d_pp
    *This displays the group number [_traj_~p], 
    *the count per group (based on the max post prob), [countG]
    *the average posterior probability for each group, [groupAPP]
    *the odds of correct classification (based on the max post prob group assignment), [occ] 
    *the odds of correct classification (based on the weighted post. prob), [occ_pp]
    *and the observed probability of groups versus the probability [p]
    *based on the posterior probabilities [TotProb]
    list _traj_Group countG groupAPP occ occ_pp p TotProb if counter == 1
	
	list _traj_Group countG groupAPP occ occ_pp  p if counter == 1
	
    restore
end

summary_table_procTraj_Toxictrio


//Fitting 6 trajectory groups//

traj, multgroups(6) var1(M_MentalH_*) indep1(t_*) order1(3 3 3 3 3 3) model1(logit) ///
      var2(M_Alcohol_*) indep2(t_*) order2(3 3 3 3 3 3) model2(logit) ///
      var3(IPV_*) indep3(t_*) order3(3 3 3 3 3 3) model3(logit) ///
	  var4(poverty_*) indep4(t_*) order4(3 3 3 3 3 3) model4(logit) 
	  
***** Traj Plot********
multtrajplot, xtitle(Age) ytitle1(Mentalhealth) ytitle2(Alcohol) ytitle3(DVA ) ytitle4(poverty) ylabel1(0(0.1)0.8) ylabel2(0(0.1)0.7) ylabel3(0(0.1)0.8) ylabel4(0(0.1)1)
	
	
****** Fit statstics: average posterior probability, the odds of correct classification, and the observed classification proportion versus the expected classification proportion ******

set more off

*I made a function to print out summary stats
program summary_table_procTraj_Toxictrio
    preserve
    *now lets look at the average posterior probability
	gen Mp = 0
	foreach i of varlist _traj_ProbG* {
	    replace Mp = `i' if `i' > Mp 
	}
    sort _traj_Group
    *and the odds of correct classification
    by _traj_Group: gen countG = _N
    by _traj_Group: egen groupAPP = mean(Mp)
    by _traj_Group: gen counter = _n
    gen n = groupAPP/(1 - groupAPP)
    gen p = countG/ _N
    gen d = p/(1-p)
    gen occ = n/d
    *Estimated proportion for each group
    scalar c = 0
    gen TotProb = 0
    foreach i of varlist _traj_ProbG* {
       scalar c = c + 1
       quietly summarize `i'
       replace TotProb = r(sum)/ _N if _traj_Group == c 
    }
	gen d_pp = TotProb/(1 - TotProb)
	gen occ_pp = n/d_pp
    *This displays the group number [_traj_~p], 
    *the count per group (based on the max post prob), [countG]
    *the average posterior probability for each group, [groupAPP]
    *the odds of correct classification (based on the max post prob group assignment), [occ] 
    *the odds of correct classification (based on the weighted post. prob), [occ_pp]
    *and the observed probability of groups versus the probability [p]
    *based on the posterior probabilities [TotProb]
    list _traj_Group countG groupAPP occ occ_pp p TotProb if counter == 1
	
	list _traj_Group countG groupAPP occ occ_pp  p if counter == 1
	
    restore
end

summary_table_procTraj_Toxictrio


*Best fit model: The six-group trajectory model had the best fit 

*******************************************************************************************		  
*Rename Trajectory groups*******
cap drop traj
gen traj=_traj_Group
recode traj (1=1) (2=2) (3=3)(4=4) (5=5) (6=6)
label define  traj 1 "Low poverty and adversity" 2 "persistent poverty " 3 "persistent poor mental health"  4 "persistent poverty and poor mental health"  5 "Persistent alcohol use"  6 "persistent domestic violence and abuse", modify 
label var traj  "Trajectory"
lab value traj traj 
tab traj 
		  

//Fitting 7 trajectory groups//

traj, multgroups(7) var1(M_MentalH_*) indep1(t_*) order1(3 3 3 3 3 3 3) model1(logit) ///
      var2(M_Alcohol_*) indep2(t_*) order2(3 3 3 3 3 3 3) model2(logit) ///
      var3(IPV_*) indep3(t_*) order3(3 3 3 3 3 3 3) model3(logit) ///
	  var4(poverty_*) indep4(t_*) order4(3 3 3 3 3 3 3) model4(logit) 

****** Fit statstics: average posterior probability, the odds of correct classification, and the observed classification proportion versus the expected classification proportion ******

set more off

*I made a function to print out summary stats
program summary_table_procTraj_Toxictrio
    preserve
    *now lets look at the average posterior probability
	gen Mp = 0
	foreach i of varlist _traj_ProbG* {
	    replace Mp = `i' if `i' > Mp 
	}
    sort _traj_Group
    *and the odds of correct classification
    by _traj_Group: gen countG = _N
    by _traj_Group: egen groupAPP = mean(Mp)
    by _traj_Group: gen counter = _n
    gen n = groupAPP/(1 - groupAPP)
    gen p = countG/ _N
    gen d = p/(1-p)
    gen occ = n/d
    *Estimated proportion for each group
    scalar c = 0
    gen TotProb = 0
    foreach i of varlist _traj_ProbG* {
       scalar c = c + 1
       quietly summarize `i'
       replace TotProb = r(sum)/ _N if _traj_Group == c 
    }
	gen d_pp = TotProb/(1 - TotProb)
	gen occ_pp = n/d_pp
    *This displays the group number [_traj_~p], 
    *the count per group (based on the max post prob), [countG]
    *the average posterior probability for each group, [groupAPP]
    *the odds of correct classification (based on the max post prob group assignment), [occ] 
    *the odds of correct classification (based on the weighted post. prob), [occ_pp]
    *and the observed probability of groups versus the probability [p]
    *based on the posterior probabilities [TotProb]
    list _traj_Group countG groupAPP occ occ_pp p TotProb if counter == 1
	
	list _traj_Group countG groupAPP occ occ_pp  p if counter == 1
	
    restore
end

summary_table_procTraj_Toxictrio







































































