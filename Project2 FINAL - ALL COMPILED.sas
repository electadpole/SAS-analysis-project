/*Part 1:  Preprocessing*/
libname Project2 "C:\sasfiles";
/*Create count dataset*/
data Project2.bn (drop=domain date product price); /*bn only contains the customer info for BN*/
set Project2.books; /*Books is the original dataset*/
if domain="amazon.com" then qty=0; /*if the book is purchased in Amazon.com, 
we set the qty to 0, because we are calculating the number of book purchased in BN*/
run;

proc sort data=project2.bn;
by userid;
proc sql noprint;
create table project2.count as 
/*create a dataset counting the total quantity of books that every customer purchased in BN*/
select userid, education, region, hhsz, age, income, child, race, country, sum(qty) as count from project2.bn
group by userid, education, region, hhsz, age, income, child, race, country;
quit;
proc print data=project2.count (obs=10);/*Print the first 10 obs as required by the project*/
run;

/*impute age and region*/
data project2.count;
set project2.count;
if age=99 then age=.;
run;
proc means data=project2.count;
var region age;
run;
data project2.count (drop=education);
set project2.count;
if age=. then age=7;
if region=. then region=3;
run;

/*Sample distribution of book purchase*/
proc sort data=project2.count;
by count;
proc sql noprint;
create table Book_Purchase as /*Book_Purchase is similar to the one on Slide 5*/
select count, count(userid) as Num_People from project2.count
group by count
order by count;
quit;

/* NBD Model*/
PROC NLMIXED DATA=Book_purchase;/*Book_Purchase is in work library*/
  PARMS r=1 a=1;
  ll = Num_People*log((gamma(r+Count)/(gamma(r)*fact(Count)))*((a/(a+1))**r)*((1/(a+1))**Count));
  MODEL Num_People ~ general(ll);
RUN;

/*****Poisson Regression Model*****/
proc nlmixed data=project2.count;
  /* m stands for lamdha */
  parms m0=1 b1=0 b2=0 b3=0 b4=0 b5=0 b6=0 b7=0;
  m=m0*exp(b1*region+b2*hhsz+b3*age+b4*income+b5*child+b6*race+b7*country);
  ll = count*log(m)-m-log(fact(count));
  model count ~ general(ll);
run;

/*****NBD Regression Model*****/
proc nlmixed data=project2.count;
  parms r=1 a=1 b1=0 b2=0 b3=0 b4=0 b5=0 b6=0 b7=0;
  b=exp(b1*region+b2*hhsz+b3*age+b4*income+b5*child+b6*race+b7*country);
  ll = log((gamma(r+count)/(gamma(r)*fact(count)))*((a/(a+b))**r)*((b/(a+b))**count));
  model count ~ general(ll);
run;

/*Part 2:  Model Improvement*/

/*Variable Selection*/
/*drop the most insignificant variable, and rerun the regression, until all parms are significant*/
/*drop b2*/
proc nlmixed data=project2.count;
  parms r=1 a=1 b1=0 b3=0 b4=0 b5=0 b6=0 b7=0;
  b=exp(b1*region+b3*age+b4*income+b5*child+b6*race+b7*country);
  ll = log((gamma(r+count)/(gamma(r)*fact(count)))*((a/(a+b))**r)*((b/(a+b))**count));
  model count ~ general(ll);
run;
/*drop b5*/
proc nlmixed data=project2.count;
  parms r=1 a=1 b1=0 b3=0 b4=0 b6=0 b7=0;
  b=exp(b1*region+b3*age+b4*income+b6*race+b7*country);
  ll = log((gamma(r+count)/(gamma(r)*fact(count)))*((a/(a+b))**r)*((b/(a+b))**count));
  model count ~ general(ll);
run;
/*drop b4*/
proc nlmixed data=project2.count;
  parms r=1 a=1 b1=0 b3=0 b6=0 b7=0;
  b=exp(b1*region+b3*age+b6*race+b7*country);
  ll = log((gamma(r+count)/(gamma(r)*fact(count)))*((a/(a+b))**r)*((b/(a+b))**count));
  model count ~ general(ll);
run;
/*drop b7*/
proc nlmixed data=project2.count;
  parms r=1 a=1 b1=0 b3=0 b6=0 ;
  b=exp(b1*region+b3*age+b6*race);
  ll = log((gamma(r+count)/(gamma(r)*fact(count)))*((a/(a+b))**r)*((b/(a+b))**count));
  model count ~ general(ll);
run;
/*all variables left in the model are significant at this moment*/

/*Custom Variables:  Holiday, Weekend, Loyalty*/

/*Holiday Variable*/
/*mark the purchase date as holiday or non-holiday:
0: non-holiday
1: one week before labor day 8/27-9/3
   one week before Thanksgiving and black Friday 11/15-11/23
   one week bofore Christmas 12/18-12/25 */
data holiday (keep=userid domain holiday qty);
set project2.books;
Holiday=0;
if 20070827<=date<=20070903 then holiday=1;
if 20071115<=date<=20071123 then holiday=1;
if 20071218<=date<=20071225 then holiday=1;
run;
/*calculate the total number of books purchased on holidays*/
proc sort data=holiday;
by userid holiday;
run;
proc sql noprint;
create table holiday_purchase as
select userid, holiday, sum(qty) as TotalHoliday
from holiday
group by userid, holiday
order by userid, holiday;
delete from holiday_purchase where holiday=0;
quit;
proc sql noprint;
create table total_purchase as
select userid, sum(qty) as Total
from holiday
group by userid;
quit; 
/*get the percentage of books that are purchased on holidays*/
proc sql noprint;
create table project2.holiday_percent as
select c.userid, c.region, c.hhsz, c.age, c.income, c.child, c.race, c.country, holiday_percent, c.count
from project2.count c left join 
(select t.userid, TotalHoliday/Total as holiday_percent from total_purchase t left join holiday_purchase h on t.userid=h.userid) p
on p.userid=c.userid;
update project2.holiday_percent 
set holiday_percent=0 where holiday_percent is missing;
quit;

data project2.holiday_percent;
set project2.holiday_percent;
if holiday_percent=0 then holiday=0;
else if holiday_percent<=0.33 then holiday=1;
        else if holiday_percent<=0.66 then holiday=2;
		      else holiday=3;
run;

/*update NBD model with percentage of holidays*/
proc nlmixed data=project2.holiday_percent;
  parms r=1 a=1 b1=0 b2=0 b3=0 b4=0 b5=0 b6=0 b7=0 b8=0;
  b=exp(b1*region+b2*hhsz+b3*age+b4*income+b5*child+b6*race+b7*country+b8*holiday);
  ll = log((gamma(r+count)/(gamma(r)*fact(count)))*((a/(a+b))**r)*((b/(a+b))**count));
  model count ~ general(ll);
run;


/*Weekend Variable*/
/*mark the purchase date as weekday/weekend
0: weekday 1:weekend */

Data weekday (keep= userid date qty x weekday);
set project2.books;
date=input(put(date, 8.),yymmdd8.);
format date yymmddn8.;
x=weekday(date);
weekday=0;
if x=7 then weekday=1; /*x=7 is Saturday*/
if x=1 then weekday=1; /*x=1 is Sunday*/
run;
proc sort data=weekday;
by userid weekday;
run;
proc sql noprint;
create table weekend_purchase as
select userid, weekday, sum(qty) as TotalWeekend
from weekday
group by userid, weekday; 
delete from weekend_purchase where weekday=0;
quit;
proc sql noprint;
create table total_purchase as
select userid, sum(qty) as Total
from weekday
group by userid;
quit;

proc sql noprint;
create table project2.weekend_percent as
select c.userid, c.region, c.hhsz, c.age, c.income, c.child, c.race, c.country, weekend_percent, c.count
from project2.count c left join 
(select w.userid, TotalWeekend/Total as weekend_percent from weekend_purchase w left join total_purchase t on w.userid=t.userid) p
on c.userid=p.userid;
update project2.weekend_percent 
set weekend_percent=0 where weekend_percent is missing;
quit;

data project2.weekend_percent;
set project2.weekend_percent;
if weekend_percent=0 then weekend=0;
else if weekend_percent<=0.33 then weekend=1;
        else if weekend_percent<=0.66 then weekend=2;
		      else weekend=3;
run;

/*update NBD model with percentage of weekend purchase*/
proc nlmixed data=project2.weekend_percent;
  parms r=1 a=1 b1=0 b2=0 b3=0 b4=0 b5=0 b6=0 b7=0 b8=0;
  b=exp(b1*region+b2*hhsz+b3*age+b4*income+b5*child+b6*race+b7*country+b8*weekend);
  ll = log((gamma(r+count)/(gamma(r)*fact(count)))*((a/(a+b))**r)*((b/(a+b))**count));
  model count ~ general(ll);
run;


/*Loyalty Variable*/
/*calculate the loyalty of customer*/
data loyalty;
set project2.books;
run;
proc sort data=loyalty;
by userid;
run;
proc sql noprint;
create table loyalty_purchase as
select userid, sum(qty) as total /*calculate the total quantity*/
from loyalty
group by userid;
quit;
proc sql noprint;
create table project2.loyalty_percent as
select c.userid, c.education, c.region, c.hhsz, c.age, c.income, c.child, c.race, c.country, l.Total, c.count, count/total as bn_percent
from project2.count c left join loyalty_purchase l
on l.userid=c.userid;
quit;

data project2.loyalty_percent;
set project2.loyalty_percent;
if bn_percent=0 then bn=0;
else if bn_percent<=0.33 then bn=1;
        else if bn_percent<=0.66 then bn=2;
		      else bn=3;
run;

/*update NBD regression model*/
proc nlmixed data=project2.loyalty_percent;
  parms r=1 a=1 b1=0 b2=0 b3=0 b4=0 b5=0 b6=0 b7=0 b8=0;
  b=exp(b1*region+b2*hhsz+b3*age+b4*income+b5*child+b6*race+b7*country+b8*bn);
  ll = log((gamma(r+count)/(gamma(r)*fact(count)))*((a/(a+b))**r)*((b/(a+b))**count));
  model count ~ general(ll);
run;


/*Interaction Effects:  Region*Race, Age*Income, Hhsz*Child*/
/*Interaction Effect = Region*Race */
proc nlmixed data=project2.count;
  parms r=1 a=1 b1=0 b2=0 b3=0 b4=0 b5=0 b6=0 b7=0 b8=0;
  b=exp(b1*region+b2*hhsz+b3*age+b4*income+b5*child+b6*race+b7*country+b8*region*race);
  ll = log((gamma(r+count)/(gamma(r)*fact(count)))*((a/(a+b))**r)*((b/(a+b))**count));
  model count ~ general(ll);
run;

/*Interaction Effect = Age*Income */
proc nlmixed data=project2.count;
  parms r=1 a=1 b1=0 b2=0 b3=0 b4=0 b5=0 b6=0 b7=0 b8=0;
  b=exp(b1*region+b2*hhsz+b3*age+b4*income+b5*child+b6*race+b7*country+b8*age*income);
  ll = log((gamma(r+count)/(gamma(r)*fact(count)))*((a/(a+b))**r)*((b/(a+b))**count));
  model count ~ general(ll);
run;

/*Interaction Effect = Hhsz*Child*/
proc nlmixed data=project2.count;
  parms r=1 a=1 b1=0 b2=0 b3=0 b4=0 b5=0 b6=0 b7=0 b8=0;
  b=exp(b1*region+b2*hhsz+b3*age+b4*income+b5*child+b6*race+b7*country+b8*hhsz*child);
  ll = log((gamma(r+count)/(gamma(r)*fact(count)))*((a/(a+b))**r)*((b/(a+b))**count));
  model count ~ general(ll);
run;

/*Part 3:  Why Customers Choose Amazon over Barnes & Noble*/

data Project2.logistic(drop=date product qty price count);
set Project2.count;
if count=0 then domain=0;
else domain=1;
run;
proc logistic data=Project2.logistic;
   /*class education region hhsz age income child race country ;*/
   model domain = education region hhsz age income child race country / expb;
run;
