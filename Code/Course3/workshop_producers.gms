$TITLE Louis Workshop

set 
i      "products sold" /bowls, tables, chairs/,
j      "inputs"        /blanks, cherry, maple/,
i_d(i) "subset of i requiring other products for production" /tables/,
e      "customer sets" /1 * 10/,
s      "lumber supplier" /1*10/;

alias(i,ii);

table
consumer_Characteristics(i,e,*)   "characteristics "
$ondelim
$include data\demand.csv
$offdelim
;

table
supplier_Characteristics(j,s,*)   "characteristics "
$ondelim
$include data\supply.csv
$offdelim
;


parameter p(i,e) "Price of products sold ($ / product)",
		  q(i,e) "quantity of produces customer is willing to purchase (products)";

p(i,e) = consumer_Characteristics(i,e,"price");
q(i,e) = consumer_Characteristics(i,e,"quantity") / 10;


p("bowls",e) = p("bowls",e) / 2;

parameter
c(j,s)   "Costs of inputs used by supplier($ / input)",
q_s(j,s) "quantity available at different suppliers (inputs / supplier)",
h(i)   "Hours need to produce one unit (hours / product)"
/
bowls 3,
tables 20,
chairs 4
/

d(i,i) "products required to produce another product"
;

*assign characteristics to supplier parameters
c(j,s) =  supplier_Characteristics(j,s,"cost");
q_s(j,s) = supplier_Characteristics(j,s,"quantity");


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
Y(j,s) "use of inputs (inputs)";

Variables Z "objective function value";



Equations
ObjFn          "computed objective function value",
Input_Req(i)   "outputs required input - transformation",
Hours_Limit    "louis can only work 40 hours in a week",
Unit_req(i,e)  "each table requires four chair"
Quantity_Limit_Customer(i,e) "cant sell more to customer c than they are willing to buy"
Quantity_Limit_Supplier(j,s) "cant sell more to customer c than they are willing to buy"
;



*end of homework 3

*Signs that can be used in GAMS
**=e= -- equal to
**=g= -- greater than
**=l= -- less than


ObjFn.. Z =e= sum((i,e),p(i,e) * X(i,e))
             -sum((j,s),c(j,s) * Y(j,s));


Input_Req(i)..
		sum((j,s),a(i,j) * Y(j,s)) =g= 
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

Quantity_Limit_Supplier(j,s).. 
		q_s(j,s) =g= Y(j,s)
		;

Model workshop /all/;

solve workshop using lp maximizing z;

parameter rep, rep_unit, rep_supplier;

rep("profit","withconstraint") = sum((i,e),p(i,e) * X.l(i,e))
             -sum((j,s),c(j,s) * Y.l(j,s));

rep_unit(i,e,"withconstraint") = x.l(i,e);

rep_supplier(j,s,"withconstraint") = y.l(j,s);

*========================
* turning off constraint
*========================

*running counterfactual without requirement of tables for chairs
i_d("tables") = no;

solve workshop using lp maximizing z;

rep("profit","withoutconstraint") = sum((i,e),p(i,e) * X.l(i,e))
             -sum((j,s),c(j,s) * Y.l(j,s));

rep_unit(i,e,"withoutconstraint") = x.l(i,e);

rep_supplier(j,s,"withoutconstraint") = y.l(j,s);



execute_unload "alldata_customers_suppliers.gdx";


