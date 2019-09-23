$TITLE Louis Workshop

set 
i      "products sold" /bowls, tables, chairs/,
j      "inputs"        /blanks, cherry, maple/,
i_d(i) "subset of i requiring other products for production" /tables/,
e      "customer sets" /1 * 10/;

alias(i,ii);

table
consumer_Characteristics(i,e,*)   "characteristics "
$ondelim
$include data\demand.csv
$offdelim
;

parameter p(i,e) "Price of products sold ($ / product)",
		  q(i,e) "quantity of produces customer is willing to purchase (products)";

p(i,e) = consumer_Characteristics(i,e,"price");
q(i,e) = consumer_Characteristics(i,e,"quantity");


parameter
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
X(i,e) "production of outputs for each customer(units)",
Y(j) "use of inputs (inputs)";

Variables Z "objective function value";



Equations
ObjFn        "computed objective function value",
Input_Req(i) "outputs required input - transformation",
Hours_Limit  "louis can only work 40 hours in a week",
Unit_req(i,e)  "each table requires four chair"
Quantity_Limit_Customer(i,e) "cant sell more to customer c than they are willing to buy"
;

*Signs that can be used in GAMS
**=e= -- equal to
**=g= -- greater than
**=l= -- less than


ObjFn.. Z =e= sum((i,e),p(i,e) * X(i,e))
             -sum(j,c(j) * Y(j));

Input_Req(i)..
		sum(j,a(i,j) * Y(j)) =g= 
		 	sum(e,X(i,e));

Hours_limit.. 
		hbar =g=
		sum((i,e),h(i) * X(i,e))
		;

Unit_req(i,e)$i_d(i)..
		sum((ii),d(i,ii) * X(ii,e)) =g= 
		X(i,e);

Quantity_Limit_Customer(i,e).. 
		q(i,e) =g= X(i,e);


Model workshop /all/;

solve workshop using lp maximizing z;

execute_unload "alldata_customers.gdx";

$exit

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


















