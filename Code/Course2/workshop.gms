$TITLE Louis Workshop

set 
i "products sold" /bowls, tables, chairs/,
j "inputs"        /blanks, cherry, maple/,
i_d(i) "subset of i requiring other products for production" /tables/;

parameter
p(i)  "Price of products sold ($ / product)", 
c(j)  "Costs of inputs used ($ / input)" ,
h(i)  "Hours need to produce one unit (hours / product)",
h     "Total hours in a week (equal to forty hours)",
d(i,i) "products required to produce another product",
a(i,j) "Inputs of j required to produce one unit of i (input / products sold)"
;


