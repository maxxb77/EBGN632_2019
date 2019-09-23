$TITLE Louis Workshop

set 
i "products sold" /bowls, tables, chairs/,
j "inputs"        /blanks, cherry, maple/,
i_d(i) "subset of i requiring other products for production" /tables/;

alias(i,ii);

parameter
p(i)   "Price of products sold ($ / product)"
/
bowls 30,
tables 200,
chairs 50
/, 

c(j)   "Costs of inputs used ($ / input)" 
/
blanks 5,
maple 15,
cherry 10
/,

h(i)   "Hours need to produce one unit (hours / product)"
/
bowls 3,
tables 20,
chairs 4
/

d(i,i) "products required to produce another product"
;

d("tables","chairs") = 1/4;

table a(i,j) "Inputs of j required to produce one unit of i (input / products sold)"
$ondelim
$include data\a.csv
$offdelim
;

scalar hbar   "Total hours in a week (equal to forty hours)" /40/;
scalar Sw_UnitReq "turn unit requirement off or on" /0/;

Positive Variables
X(i) "production of outputs (units)",
Y(j) "use of inputs (inputs)";

Variables Z "objective function value";



Equations
ObjFn        "computed objective function value",
Input_Req(i) "outputs required input - transformation",
Hours_Limit  "louis can only work 40 hours in a week",
Unit_req(i)  "each table requires four chair"
;

*Signs that can be used in GAMS
**=e= -- equal to
**=g= -- greater than
**=l= -- less than


ObjFn.. Z =e= sum(i,p(i) * X(i))
             -sum(j,c(j) * Y(j));

Input_Req(i)..
		sum(j,a(i,j) * Y(j)) =g= 
		X(i);

Hours_limit.. 
		hbar =g=
		sum(i,h(i) * X(i))
		;

Unit_req(i)$i_d(i)..
		sum(ii,d(i,ii) * X(ii)) =g= 
		X(i);

Model workshop /all/;

solve workshop using lp maximizing z;

*appendices for variable reporting..
*X.l -- level value
*X.m -- marginal value
*X.lo -- lower bound
*X.up -- upper bound

parameter rep "reporting";
rep("profit") =  sum(i,p(i) * X.l(i))
             	-sum(j,c(j) * Y.l(j));
 	
rep("revenue") = sum(i,p(i) * X.l(i));

rep("cost") = sum(j,c(j) * Y.l(j));

display rep;

*execute_unload "alldata.gdx";


















