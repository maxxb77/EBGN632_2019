#load the necessary libraries
#note that gdxrrw requires manual installation as well as the installation of reshape2
#it will auto-load reshape2 but we do explictly so here to use a few functions, specifically:
# melt
# dcast
# merge

library(gdxrrw)
library(ggplot2)
library(reshape2)
library(plotly)


#i need to tell r where to find my gdx utilities
#this must be from an installation of GAMS that is greater than or equal to GAMS 24.0
igdx("c:\\gams\\win64\\24.2")

#set my working directory to where we stored the hourout.gdx file
setwd("c:\\users\\mbrown1\\ebgn632\\code\\course4")

gdxfile = "hourout_noopres.gdx"


#for reference, here are the parameters and their descriptions that we will be loading:
#     rep_cost  "total cost by case (Dollars)",
#     rep_genh  "generation by fuel by hour by state (MWh)",
#     rep_tgenh "total generation by fuel by hour (MWh)",
#     rep_gend  "generation by fuel over the entire day (MWh)",
#     rep_c     "aggregate carbon emissions (million lbs)",
#     rep_ci    "average carbon intensity (lbs / MWh)",
#     rep_p     "hourly price based on marginal of demand equation ($/MWh)",
#     rep_permp "permit price ($/1000 lbs co2)"
#     leakage   "leakage of WY for CO policy"


######
#let's start simple, we'll plot cost by each case

cost = rgdx.param(gdxfile,"rep_cost")

ggplot(cost,aes(x=i,y=value))+geom_point(size=2)

#we can also store the plot as an object and add some features to it 

costplot = ggplot(cost,aes(x=i,y=value))+geom_point(size=2)

#here add a y axis label and an x axis label
costplot = costplot + ylab("Cost (2016$)") + xlab("Case")

costplot

#the plot looks a little out of order and 'cap' is (intentionally) not capitalized
#let's replace 'cap' with "CAP"... 
cost$i = gsub("cap","CAP",cost$i)

#and rearrange the order
caseorder = c("BAU","CAP","TPS","CAP_T","TPS_T","CAP_CO","TPS_CO")

cost$i = factor(cost$i,levels=caseorder)

#recreate the plot using the same lines we used before
costplot = ggplot(cost,aes(x=i,y=value))+geom_point(size=3)
costplot + ylab("Cost (2016$)") + xlab("Case")


#What if we wanted to plot the relative change of costs
#with respect to the reference level costs?
#we can do so by subsetting then merging the data

#first we need the value of the costs from the BAU case...
costbau = subset(cost,i=="BAU")
cbau = costbau[1,2]

#then we need all the other costs
costnbau = subset(cost,i!="BAU")

#the relative change in costs is just the value [minus] 
#the costs from the bau [divided by] the costs from bau
costnbau$value = (costnbau$value - cbau) / cbau

#let's plot this again - now we can add percent scales to the y axis and change the label
costplotchange = ggplot(costnbau,aes(x=i,y=value))+geom_point(size=3)

costplotchange+
  ylab("Change from reference case")+
  xlab("Case")+
  scale_y_continuous(labels = scales::percent)


#that's all easy stuff... let's start looking at generation (rep_genh)
genh = rgdx.param(gdxfile,"rep_genh")

#rename the columns to something more friendly
colnames(genh) = c("s","f","h","case","value")

#again, change the name and factor out the order
genh$case = gsub("cap","CAP",genh$case)
genh$case = factor(genh$case,levels=caseorder)


#R can have trouble discerning labels when they are not numbers
#therefore, we can replace the 'h' in hour to be more accomodating
genh$h = as.numeric(as.character(gsub("h","",genh$h)))


#now it is nice to plot the generation but the data is a mess
#let's first plot just for the BAU case
genh_bau = subset(genh,case=="BAU")

#we first designate the dataset and aesthetics
#note that for bar plots, fill designates the fill
ggplot(genh_bau,aes(x=h,y=value,fill=f))+
  geom_bar(position="stack",stat="identity")+
  ylab("MWh")+xlab("hour of day")

#that was neat and all, but perhaps we want to see the generation by state
#we can do that via facet_wrap:
ggplot(genh_bau,aes(x=h,y=value,fill=f))+
  geom_bar(position="stack",stat="identity")+
  ylab("MWh")+xlab("hour of day")+facet_wrap(~s)

#interesting in that it gives us the total amount of generation, 
#but what if we wanted to look at the share of generation?
#we can do that here by adjusting the position = "stack" to position="fill"
ggplot(genh_bau,aes(x=h,y=value,fill=f))+
  geom_bar(position="fill",stat="identity")+
  ylab("MWh")+xlab("hour of day")+facet_wrap(~s)+
  scale_y_continuous(labels = scales::percent)

ggplot(genh_bau,aes(x=h,y=value,fill=f))+
  geom_bar(position="fill",stat="identity")+
  ylab("MWh")+xlab("hour of day")+facet_wrap(~s)+
  scale_y_continuous(labels = scales::percent)


#that's really cool - but what about if we wanted to look at 
#generatoin by state and by case? we can use facet_grid on the main 
#genh dataset to do just that:
ggplot(genh,aes(x=h,y=value,fill=f))+
  geom_bar(position="fill",stat="identity")+
  ylab("MWh")+xlab("hour of day")+facet_grid(s~case)+
  scale_y_continuous(labels = scales::percent)

#bar charts look a little weird - let's try an area plot:
genplot = ggplot(genh,aes(x=h,y=value,fill=f))+
  geom_area(position="stack")+
  ylab("MWh")+xlab("hour of day")+facet_grid(s~case,scales="free_y")

genplot

#that's great and all - but what if we wanted to look at 
#the relative change with respect to BAU generation?

#we can do that similarly to cost but now we need to merge the bau and other data sets
genh_bau = subset(genh,case=="BAU")
genh_nbau = subset(genh,case!="BAU")

genh_m = merge(genh_bau,genh_nbau,by=c("s","f","h"))

#now we see that value.x refers to the BAU value and value.y refers to the case value
genh_m$case = genh_m$case.y
genh_m$value = (genh_m$value.y - genh_m$value.x) 

ggplot(genh_m,aes(x=h,y=value,fill=f))+
  geom_area(position="stack")+
  ylab("MWh")+xlab("hour of day")+facet_grid(s~case,scales="free_y")


#your adviser has asked for the total carbon and average carbon 
#intensity by state in a table - how can we go about doing this?

#first, we need the total carbon and average carbon emissions:
totcarb = rgdx.param(gdxfile,"rep_c")
avcarb = rgdx.param(gdxfile,"rep_ci")

#rename the columns so they're easier to work with:
colnames(totcarb) = c("s","case","value")
colnames(avcarb) = c("s","case","value")

#let's give totcarb a variable name "total emissions"
totcarb$var = "Total Emissions"
#and a variable name to average emissions
avcarb$var = "Average Emissions"

#since the two have the same dimensions, we can 'stack' them via rbind:
emit = rbind(totcarb,avcarb)
emit$case = gsub("cap","CAP",emit$case)

#that table looks terrible! - how do we break it out?
#we'd like case on the rows and state/variable on the columns:
#to do so, we use dcast which will create separate columns
#for each variable as indicated by the formula

emitd = dcast(emit,case~s+var,value.var="value")

#let's write that as a csv that we can use to put in a report:
write.csv(emitd,"emitd.csv",row.names=FALSE)

#well now your adviser wants a graph of the data you just put into a table
#how do we take the data and revert it to 'long' format?
#we use the melt function:
emitm = melt(emitd,id=c("case"))

#that is OK but we really want our state and variable names back
#never fear = substr is here!
emitm$s = substr(emitm$variable,1,2)
emitm$var = substr(emitm$variable,4,100)

#let's get rid of that ugly old 'variable'
emitm_fin = emitm[,c("case","s","var","value")]

#we can make a plot of these data using the previous examples:
emitplot = ggplot(emitm_fin,aes(x=case,y=value,color=s))+
  geom_point(size=3)+
  facet_wrap(~var,scales="free_y")

#those colors are a little ugly - let's change them ourselves:
emitplot = emitplot + scale_color_manual(values=c("red","blue"))
emitplot

#that's pretty neat - what would we do if we wanted to make 
#an interactive plot of just total emissions?
#well we can simply use ggplotly(...)

emitm_fin2 = subset(emitm_fin,var=="Total Emissions")
emitplot2 = ggplot(emitm_fin2,aes(x=case,y=value,color=s))+geom_point(size=3)+scale_color_manual(values=c("red","blue"))
ggplotly(emitplot2)



#finally, what if we wanted to save one of our previous graphs?
#easy - use ggsave(...) and we can also specify some parameters to make it high resolution
#remove the extension from the input file and use that to prepend to your output file
fname = gsub(".gdx","",gdxfile)

ggsave(paste(fname,"_gencase.png",sep=""),genplot,dpi=600,height=6,width=8)







