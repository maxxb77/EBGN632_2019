
library(gdxrrw)
library(ggplot2)

datin = rgdx.param("c:\\users\\mbrown1\\ebgn632\\code\\course3\\alldata_customers.gdx","rep_unit")

ggplot(datin,aes(x=i,y=value,group=j,color=j,fill=j)) + 
  geom_bar(stat="identity")


