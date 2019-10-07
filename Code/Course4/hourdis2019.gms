$Title Simple Hourly Dispatch LP
* Maxwell Brown
* Maxwell.L.Brown@gmail.com
* Updated September 12 2017

$ontext
  This model is a simple LP for hourly RMPA electricity dispatch
  It has two different policies that can be modeled through the first
  set of switches. The references amounts of carbon (either average or
  aggregate) are from the base case. These can also be set to match the tax
  scenario's resulting emissions (average or aggregate)

  The model solves for several scenarios:
  1. No policy
  2. Carbon cap
  3. Tradable performance standard
  4. Trading with 2 and 3
  5. CO only cap and tps 

  
  This model also demonstrates how to import data from a CSV or an Excel file
  and requires the dataimp.csv and demand.xlsx files to be in the same directory
  that the GMS code is being run from
  
  Another extension of this modeling exercise would be to include elastic demand.
  This is very important for the counterfactual cases... we will discuss why

  The model presented here is very simple, a non-exhaustive list of extensions would be:
  1. operating reserves
  2. capacity expansion
  3. frequency regulation 
  4. stochasticity of VRE
  5. fuel supply curves
  6. demand response
  7. explicit transmission representation
  8. minimum generation requirements

$offtext

$if not set GSwOpres $setglobal GSwOpRes 0

*Policy Switches
Scalar SwSeason "Switch to have average of annual demand [0] or average of summer demand [1]" /0/;
Scalar CAPTrade "Switch to turn on [1] or off [0] a carbon cap with trading" /0/;
Scalar TPSTrade "Switch to turn on [1] or off [0] a carbon TPS with trading" /0/;
Scalar SwOpRes  "Switch to  requirement for operating reserves" /%GSwOpRes%/;

*=======================
*  Begin sets
*=======================

set h "hour" /h1*h24/;
set k "season" /day, summer/;

*table dem(h,k) "demand by season, MWH"
$include refdem.inc

set s "states"
/
  CO "Colorado",
  WY "Wyoming"
/;


Set SwTPS(s)  "Switch to enable or disable a TPS";
Set SwCAP(s)  "Switch to enable or disable a carbon cap";
*disable these for now
swtps(s) = no;
swcap(s) = no;

set f "fuels, generation technology"
        /
        bit "bituminous coal"
        dfo "distillate fuel oil"
        ng  "natural gas"
        sub "subbituminous coal"
        sun "solar"
        wat "water"
        wnd "wind"
        /;

set f_opres(f) "technologies that can provide operating reserves" /bit, ng, sub, wat/;

set pc  "plant characteristics" /cap, hr, onm/;

set pid "plant id" /1*328/;

set genfeas(s,f,pid,h) "general feasibility set, determines when a plant can generate";

set ort "operating reserve types" /spin,flex/;


*=======================
*  End sets
*=======================

*=======================
*  Begin data
*=======================

*Table plantdata(s,f,pid,pc);
$include plantdata.inc
;


*set the generation feasibility set such that it is enabled
*when the indicated s/f/pid/h combination has capacity
genfeas(s,f,pid,h)$(plantdata(s,f,pid,"CAP")>0) = yes;
*can't use solar when it's dark out
genfeas(s,"sun",pid,h)$((ord(h)<8) or (ord(h)>18)) = NO;

*set temp /1*100/;
*parameter test;
*test("ord",temp) = ord(temp);
*test("val",temp) = temp.val;
*display test;
*genfeas(s,"sun",pid,h)$((h.val<8) or (ord(h)>18)) = NO;

*note that onm costs are already in $ per mwh but we need to
*convert heat rate from btu / kwh to mmbtu / mwh

*following should really be by technology
scalar hr_adj "heat rate adjustment to take it from full load factor to partial load factor" /1.07/;
plantdata(s,f,pid,"hr") = hr_adj * plantdata(s,f,pid,"hr") / 1000;


parameter capfac(f) "capacity availability by fuel type"
         /
         bit 0.8,
         dfo 0.65,
         ng  0.85,
         sub 0.8,
         sun 0.5,
         wat 0.55,
         wnd 0.45
         /;
* offhand guesses -- do not cite/quote


parameter fcost(f) "costs by fuel ($ / MMBTU)"
         /
         bit  2.9,
         dfo  11.1,
         ng   2.97,
         sub  2.9
         /;
*estimates from EIA, taken from short term energy outlook

parameter emit(f) "lbs co2 per mmbtu for fuel burning"
         /
         bit  202,
         dfo  161,
         ng   117,
         sub  209
         /;
*taken from EPA's model legislation
*see results browser documentation for table

*Policy Parameters
*Unless otherwise defined, the Psi parameters are relative to the base case
*note that we cannot be too stringent w/o elastic demand, safety valve credits, or some capacity expansion capabilities
Parameter  Psi(s) "Percentage reduction of average CI in a TPS" /CO 10, WY 7.5/;
psi(s) = psi(s)/100;


*These start out as large values just in case the switch is activated
*without being properly calibrated to the reference case's reduction
*thus making it a non-binding constraint
Parameter REFC(s) "Reference Aggregate Carbon level at which the standard is met";
Parameter REFCI(s) "Reference average carobn intensity at which the standard is met";
refc(s) = 1e10;
refci(s) = 1e10;

table or_req(ort,*) "percentage requirement of operating reserves for load and renewables"
      load  renew
spin  0.05
flex         0.05


execute_unload 'inputs.gdx';

*=======================
*  End data
*=======================


*=======================
*  Begin Model
*=======================


Equations
CostEq                             "Objective function",
DemEq(h)                           "Demand needs to be met by sum of generation over fuels",
FuelCapEq(s,f,pid,h)               "Generation by fuel cannot exceed capacity",
RegCAPEq(s)                        "Carbon cap regulation constraint (aggregate carbon emissions)",
CAPTradeEQ					   	           "Carbon cap with trade"
RegTPSEq(s)                        "TPS regulation constraint (average carbon emissions)"
TPSTradeEQ                         "TPS regulation with trading"
OpResEQ(ort,s,h)                         "TPS regulation with trading"
;

Positive Variables
GEN(s,f,pid,h)                     "Generation by technology by hour (MWH)"
OPRES(ort,s,f,pid,h)
;

Variable
COST                               "Cost of delivering electricity ($)";


*sum of costs are those from operating (onm) and fuel (hr * fcost)
CostEQ.. COST =E= sum((s,f,pid,h)$genfeas(s,f,pid,h),GEN(s,f,pid,h) * (
                                                 plantdata(s,f,pid,"onm")
                                                 + plantdata(s,f,pid,"hr") * fcost(f)  )
                      );

*generation from all states must equal the demand, depending on day
DemEQ(H).. 
	SUM((s,f,pid)$genfeas(s,f,pid,h),GEN(s,f,pid,h)) 
		=G= 
    (1-SwSeason) * dem(h,"day") + SwSeason * dem(h,"summer")  ;


*carbon emissions must be less than the cap (if activated)
RegCapEQ(s)$SwCAP(s).. 
	(1-Psi(s)) * RefC(s) 
		=g= 
	sum((f,pid,h)$genfeas(s,f,pid,h),GEN(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f));


RegTPSEQ(s)$SwTPS(s).. 
	(1-Psi(s)) * RefCI(s) * sum((f,pid,h)$genfeas(s,f,pid,h),GEN(s,f,pid,h))
        =g=
    sum((f,pid,h)$genfeas(s,f,pid,h),GEN(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f));

FuelCapEq(s,f,pid,h)$(genfeas(s,f,pid,h)).. 
	capfac(f) * plantdata(s,f,pid,"CAP") 
		=g= 
	GEN(s,f,pid,h) + sum(ort,OPRES(ort,s,f,pid,h))$(SwOpRes$f_opres(f));


CAPTradeEQ$CAPTrade.. 
	sum(s,(1-Psi(s)) * RefC(s)) 
		=g= 
	sum((s,f,pid,h)$genfeas(s,f,pid,h),GEN(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f));


TPSTradeEQ$TPSTrade..    
	(sum(s,(1-psi(s)) * REFC(s)) / sum(h,(1-SwSeason) * dem(h,"day") + SwSeason * dem(h,"summer"))) * sum((s,f,pid,h)$genfeas(s,f,pid,h),GEN(s,f,pid,h)) 
 						    =g= sum((s,f,pid,h)$genfeas(s,f,pid,h),GEN(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f));

OpResEQ(ort,s,h)$SwOpRes..
  sum((f,pid)$(genfeas(s,f,pid,h)$f_opres(f)),OPRES(ort,s,f,pid,h))
  =g=
  or_req(ort,"load") * ((1-SwSeason) * dem(h,"day") + SwSeason * dem(h,"summer"))
  + or_req(ort,"renew") * 
  sum((f,pid)$((sameas(f,"sun") or sameas(f,"wnd"))$genfeas(s,f,pid,h)),GEN(s,f,pid,h));



Model HourlyDis /all/;

*=======================
*  End Model
*=======================

*=======================
*  Begin solving
*=======================

Parameters
  rep_cost  "total cost by case (Dollars)",
  rep_genh  "generation by fuel by hour by state (MWh)",
  rep_tgenh "total generation by fuel by hour (MWh)",
  rep_gend  "generation by fuel over the entire day (MWh)",
  rep_c     "aggregate carbon missions (million lbs)",
  rep_ci    "average carbon intensity (lbs / MWh)",
  rep_p     "hourly price based on marginal of demand equation ($/MWh)",
  rep_permp "permit price ($/1000 lbs co2)"
  leakage   "leakage of WY for CO policy"
;




*\\\\\
* BAU Scenario
*\\\\\

*solve the reference case
*turn off all policy switches
SwCAP(s) = no;
SwTPS(s) = no;
CAPTrade = 0;
TPSTrade = 0;

solve hourlydis using lp minimizing COST;

*discuss solving methods here

  rep_cost("BAU") = COST.l;
  rep_genh(s,f,h,"BAU") = sum(pid$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_tgenh(f,h,"BAU") = sum(s,rep_genh(s,f,h,"BAU"));
  rep_gend(s,f,"BAU") = sum((pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_c(s,"BAU") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f));
  rep_ci(s,"BAU") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f)) / sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_p(h,"BAU")=demeq.m(h);


*Record REFC and REFCI
*Note that neither of these should affect outcome
*as long as SwCAP and SwTPS are set to zero
REFC(s) = rep_c(s,"BAU");
REFCI(s) = rep_ci(s,"BAU");

*\\\\\
* CAP Scenario
*\\\\\


SwTPS(s) = no;
SwCAP(s) = yes;

Solve HourlyDis minimizing COST using LP;


  rep_cost("CAP") = COST.l;
  rep_genh(s,f,h,"CAP") = sum(pid$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_tgenh(f,h,"CAP") = sum(s,rep_genh(s,f,h,"CAP"));
  rep_gend(s,f,"CAP") = sum((pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_c(s,"CAP") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f));
  rep_ci(s,"CAP") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f)) / sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_p(h,"CAP")=demeq.m(h);
  rep_permp(s,"CAP") = RegCapEQ.m(s) * 1000;



*\\\\\
* TPS Scenario
*\\\\\

SwTPS(s) = YES;
SwCAP(s) = no;

Solve HourlyDis minimizing COST using LP;

display gen.l;

  rep_cost("TPS") = COST.l;
  rep_genh(s,f,h,"TPS") = sum(pid$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_tgenh(f,h,"TPS") = sum(s,rep_genh(s,f,h,"TPS"));
  rep_gend(s,f,"TPS") = sum((pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_c(s,"TPS") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f));
  rep_ci(s,"TPS") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f)) / sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_p(h,"TPS")=demeq.m(h);
  rep_permp(s,"TPS") = RegTPSEQ.m(s) * 1000;



*\\\\\
* TRADE CAP Scenario
*\\\\\

*turn off all other policies but activate the cap trade eqwuation
SwTPS(S) = no;
SwCAP(s) = no;
CAPTrade = 1;
TPSTrade = 0;


Solve HourlyDis minimizing COST using lp;

display gen.l;

  rep_cost("CAP_T") = COST.l;
  rep_genh(s,f,h,"CAP_T") = sum(pid$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_tgenh(f,h,"CAP_T") = sum(s,rep_genh(s,f,h,"CAP_T"));
  rep_gend(s,f,"CAP_T") = sum((pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_c(s,"CAP_T") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f));
  rep_ci(s,"CAP_T") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f)) / sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_p(h,"CAP_T") = demeq.m(h);
  rep_permp(s,"CAP_T") = CAPTradeEq.M * 1000;


*\\\\\
* TRADE TPS Scenario
*\\\\\


*turn off all other policies but activate the TPS trade eqwuation
SwTPS(S) = no;
SwCAP(s) = no;
CAPTrade = 0;
TPSTrade = 1;

solve hourlydis using lp minimizing COST;

display gen.l;

  rep_cost("TPS_T") = COST.l;
  rep_genh(s,f,h,"TPS_T") = sum(pid$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_tgenh(f,h,"TPS_T") = sum(s,rep_genh(s,f,h,"TPS_T"));
  rep_gend(s,f,"TPS_T") = sum((pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_c(s,"TPS_T") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f));
  rep_ci(s,"TPS_T") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f)) / sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_p(h,"TPS_T") = demeq.m(h);
  rep_permp(s,"TPS_T") = TPSTradeEq.M * 1000;


*\\\\\
* CO only CAP Scenario
*\\\\\

*turn off all other policies but activate the TPS trade eqwuation
SwTPS(S) = no;
SwCAP(s) = no;
SwCAP("CO") = yes;
CAPTrade = 0;
TPSTrade = 0;

solve hourlydis using lp minimizing COST;

display gen.l;

  rep_cost("CAP_CO") = COST.l;
  rep_genh(s,f,h,"CAP_CO") = sum(pid$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_tgenh(f,h,"CAP_CO") = sum(s,rep_genh(s,f,h,"CAP_CO"));
  rep_gend(s,f,"CAP_CO") = sum((pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_c(s,"CAP_CO") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f));
  rep_ci(s,"CAP_CO") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f)) / sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_p(h,"CAP_CO") = demeq.m(h);
  rep_permp("CO","CAP_CO") = RegCapEQ.m("CO") * 1000;
  leakage("CAP_CO") = (rep_c("WY","CAP_CO") - rep_c("WY","BAU")) / (rep_c("CO","BAU")-rep_c("CO","CAP_CO"));


*\\\\\
* CO only TPS Scenario
*\\\\\

*turn off all other policies but activate the TPS trade eqwuation
SwTPS(S) = no;
SwCAP(s) = no;
SwTPS("CO") = yes;
CAPTrade = 0;
TPSTrade = 0;

solve hourlydis using lp minimizing COST;

display gen.l;

  rep_cost("TPS_CO") = COST.l;
  rep_genh(s,f,h,"TPS_CO") = sum(pid$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_tgenh(f,h,"TPS_CO") = sum(s,rep_genh(s,f,h,"TPS_CO"));
  rep_gend(s,f,"TPS_CO") = sum((pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_c(s,"TPS_CO") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f));
  rep_ci(s,"TPS_CO") = sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h) * plantdata(s,f,pid,"hr") * emit(f)) / sum((f,pid,h)$genfeas(s,f,pid,h),gen.l(s,f,pid,h));
  rep_p(h,"TPS_CO") = demeq.m(h);
  rep_permp("CO","TPS_CO") = RegTPSEQ.m("CO") * 1000;
  leakage("TPS_CO") = (rep_c("WY","TPS_CO") - rep_c("WY","BAU")) / (rep_c("CO","BAU")-rep_c("CO","TPS_CO"));

*=======================
*  End solving
*=======================



*CONVERT AGGREGATE CARBON EMISSION TO MILLION POUNDS
rep_c(s,"BAU") = rep_c(s,"BAU") / 1000000;
rep_c(s,"CAP") = rep_c(s,"CAP") / 1000000;
rep_c(s,"TPS") = rep_c(s,"TPS") / 1000000;
rep_c(s,"CAP_T") = rep_c(s,"CAP_T") / 1000000;
rep_c(s,"TPS_T") = rep_c(s,"TPS_T") / 1000000;
rep_c(s,"CAP_CO") = rep_c(s,"CAP_CO") / 1000000;
rep_c(s,"TPS_CO") = rep_c(s,"TPS_CO") / 1000000;




display
         rep_cost,
*         rep_genh,
         rep_gend,
         rep_c,
         rep_ci,
         rep_p,
         rep_permp;


*Unload the data to a GDX File
*note that here we are not specifying which parameters to export
*this will give us access to 
execute_unload "hourout_%GSwOpRes%.gdx";



