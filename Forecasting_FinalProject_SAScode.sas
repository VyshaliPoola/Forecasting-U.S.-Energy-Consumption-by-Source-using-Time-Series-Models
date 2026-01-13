/* Importing Excel file into SAS */
proc import out=energy datafile="/home/u64128357/sasuser.v94/Energy_Consumption_Production.xlsx"
dbms=xlsx replace; 
run;

/* Descriptive statistics */

proc means data=energy chartype mean std min max n vardef=df;
	var Biomass_Energy_Consumption Renewable_Energy_Consumption;
run;

/* Biomass Energy Consumption */
/* Creating a time series plot */
proc sgplot data=energy;
     series x= month y=Biomass_Energy_Consumption;
     title "Monthly Consumption for Biomass Energy Consumption (1994-2025)";
run;
     
/*Creating an ACF plot*/
proc timeseries data=energy plots=acf out=_null_;
     var Biomass_Energy_Consumption	;
     corr acf/nlag=36;
run;

/* Renewable Energy Consumption */
/* Creating a time series plot */
proc sgplot data=energy;
     series x= month y=Renewable_Energy_Consumption;
     title "Monthly Consumption for Renewable Energy Consumption (1994-2025)";
run;
     
/*Creating an ACF plot*/
proc timeseries data=energy plots=acf out=_null_;
     var Renewable_Energy_Consumption	;
     corr acf/nlag=36;
run;

/* 90:10 split */
data energy;
	set energy;
	t=_n_;
	Biomass_Energy_Consumption_new=Biomass_Energy_Consumption;
	Renewable_Energy_Consumption_new=Renewable_Energy_Consumption;
	if t>341 then Biomass_Energy_Consumption_new=. ;
 	if t>341 then Renewable_Energy_Consumption_new=.; 	
	mth=month(month);
	if mth=2 then m2=1; else m2=0;
	if mth=3 then m3=1; else m3=0;
	if mth=4 then m4=1; else m4=0;
	if mth=5 then m5=1; else m5=0;
	if mth=6 then m6=1; else m6=0;
	if mth=7 then m7=1; else m7=0;
	if mth=8 then m8=1; else m8=0;
	if mth=9 then m9=1; else m9=0;
	if mth=10 then m10=1; else m10=0;
	if mth=11 then m11=1; else m11=0;
	if mth=12 then m12=1; else m12=0;
	t2=t*t;
/* 	t3=t*t*t; */
/* 	t4=t*t*t*t; */
/* 	t5=t*t*t*t*t; */
run;

proc freq data=energy;
	table mth m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12;
run;

/* Biomass Energy Consumption */
/* Deseasonalizing  (90:10)*/
proc timeseries data=energy outdecomp=sa_energy1 out=null;
	decomp sa;
	id month interval=month;
	var Biomass_Energy_Consumption;
run;

data sa_energy1a;
	merge energy sa_energy1(keep=sa);
	si = Biomass_Energy_Consumption/sa;
	t=_n_;
	sa1=sa;
	if t>341 then sa1=.;
run;

proc sgplot data=sa_energy1a;
	series x=month y=Biomass_Energy_Consumption;
	series x=month y=sa;
	title "Monthly Biomass Energy Consumption (Desasonalized) (90:10)";
run;

proc reg data=sa_energy1a;
	model sa1=t;
	output out=sa_energyout1 r=sa_resid p=sa_predict;
run;

data sa_energyout1;
	set sa_energyout1;
	energy_reseason=sa_predict*si;
	if t<=341 then
		do;
			mape_fit=abs(sa_resid/sa1)*100;
			mae_fit=abs(sa_resid);
			mse_fit=sa_resid**2;
		end;
	else if t>341 then
		do;
			mape_acc=abs((sa-sa_predict)/sa)*100;
			mae_acc=abs(sa-sa_predict);
			mse_acc=(sa-sa_predict)**2;
		end;
run;

proc means data=sa_energyout1 n mean maxdec=3;
	var mape_fit mae_fit mse_fit mape_acc mae_acc mse_acc;
	title "Error measures for Biomass Energy Consumption Deseasonalizing for 90:10 split ";
run;

proc sgplot data=sa_energyout1;
	series x=month y=Biomass_Energy_Consumption;
	series x=month y=energy_reseason;
	title "Monthly Biomass Energy Consumption (Resasonalized)(90:10)";
run;

/* Linear Regression using dummy variables for Biomass_Energy_Consumption */
proc reg data=energy;
	model Biomass_Energy_Consumption_new=t m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12/clb vif dwprob;
	output out=energyout1a p=biomass_predict r=biomass_resid;
run;

proc sgplot data=energyout1a;
	series x=month y=Biomass_Energy_Consumption;
	series x=month y=biomass_predict;
	title "Linear Trend for Biomass Energy Consumption for 90:10 split ";
run;

data energyout1a;
	set energyout1a;
	if t<=341 then
		do;
			mape_fit=abs(biomass_resid/Biomass_Energy_Consumption_new)*100;
			mae_fit=abs(biomass_resid);
			mse_fit=(biomass_resid)**2;
		end;
	else if t>341 then
		do;
			mape_acc=abs((Biomass_Energy_Consumption-biomass_predict)/Biomass_Energy_Consumption)*100;
			mae_acc=abs(Biomass_Energy_Consumption-biomass_predict);
			mse_acc=(Biomass_Energy_Consumption-biomass_predict)**2;
		end;
run;
	
proc means data=energyout1a n mean maxdec=3;
	var mape_fit mae_fit mse_fit mape_acc mae_acc mse_acc;
	title "Error measures for Biomass Energy Consumption Linear Trend for 90:10 split ";
run;

/* Non-linear Trend for Biomass_Energy_Consumption */
proc reg data=energy;
	model Biomass_Energy_Consumption_new=t t2 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12/clb vif dwprob;
	output out=energyout1b p=biomass_predict r=biomass_resid;
run;

proc sgplot data=energyout1b;
	series x=month y=Biomass_Energy_Consumption;
	series x=month y=biomass_predict;
	title "Non-Linear Trend for Biomass Energy Consumption for 90:10 split ";
run;

data energyout1b;
	set energyout1b;
	if t<=341 then
		do;
			mape_fit=abs(biomass_resid/Biomass_Energy_Consumption_new)*100;
			mae_fit=abs(biomass_resid);
			mse_fit=(biomass_resid)**2;
		end;
	else if t>341 then
		do;
			mape_acc=abs((Biomass_Energy_Consumption-biomass_predict)/Biomass_Energy_Consumption)*100;
			mae_acc=abs(Biomass_Energy_Consumption-biomass_predict);
			mse_acc=(Biomass_Energy_Consumption-biomass_predict)**2;
		end;
run;
	
proc means data=energyout1b n mean maxdec=3;
	var mape_fit mae_fit mse_fit mape_acc mae_acc mse_acc;
	title "Error measures for Biomass Energy Consumption Non Linear Trend for 90:10 split ";
run;

/* Holt's Exponential Smoothing Method (90:10 split) */

/* Additive Winter's */

proc esm data=energy print=all lead=38 back=38 outfor=energyout1c out=_null_;
	id month interval=month;
	forecast Biomass_Energy_Consumption/model=addwinters;
run;

proc sgplot data=energyout1c;
	series x=month y=actual;
	series x=month y=predict;
	title " Actual vs Predicted values for Biomass Energy Consumption ";
run;

/* Multiplicative Winter's */

proc esm data=energy print=all lead=38  back=38 outfor=energyout1d out=_null_;
	id month interval=month;
	forecast Biomass_Energy_Consumption/model=winters;
run;

proc sgplot data=energyout1d;
	series x=month y=actual;
	series x=month y=predict;
	title " Actual vs Predicted values for Biomass Energy Consumption ";
run;

/* Seasonal ARIMA   (90:10 split)*/

/*Model 1*/

data energy;
	set energy;
	t=_n_;
	Biomass_Energy_Consumption1=Biomass_Energy_Consumption;
	if t>341 then Biomass_Energy_Consumption1=.;
run;

proc arima data=energy;
	identify var=Biomass_Energy_Consumption1(12,1) whitenoise=ignoremiss nlag=36;
	estimate p=(11)(12)(24)(36) q=(9)(12) whitenoise=ignoremiss; /*(11,1,9)(3,1,1)*/ 
	*estimate p=(2)(12)(24)(36) q=(2)(12) whitenoise=ignoremiss; /*(2,1,2)(3,1,1)*/ 
	*estimate p=(2)(12)(24)(36) q=(9)(12) whitenoise=ignoremiss; /*(2,1,9)(3,1,1)*/ 
	forecast id=month interval=month lead=38 out=energyout1e;
run;

data energyout1e;
	merge energy energyout1e;
	if t<=341 then
		do;
			mape_fit=abs(residual/Biomass_Energy_Consumption1)*100;
			mae_fit=abs(residual);
			mse_fit=residual**2;
		end;
	else if t>341 then
		do;
			mape_acc=abs((Biomass_Energy_Consumption-forecast)/Biomass_Energy_Consumption)*100;
			mae_acc=abs(Biomass_Energy_Consumption-forecast);
			mse_acc=abs(Biomass_Energy_Consumption-forecast)**2;
		end;
run;

proc means data=energyout1e n mean maxdec=3;
	var mape_fit mae_fit mse_fit mape_acc mae_acc mse_acc;
run;

data energyout1e;
      merge energy energyout1e;
run;

proc sgplot data= energyout1e;
     series x=month y=Biomass_Energy_Consumption;
     series x=month y=forecast;
     title "Actual Vs  Predicted plot for Biomass Energy Consumption";
run;  

/* Model 2*/

data energy;
	set energy;
	t=_n_;
	Biomass_Energy_Consumption1=Biomass_Energy_Consumption;
	if t>341 then Biomass_Energy_Consumption1=.;
run;

proc arima data=energy;
	identify var=Biomass_Energy_Consumption1(12,1) whitenoise=ignoremiss nlag=36;
	estimate p= (14)(12)(24)(36) q=(2)(12) whitenoise=ignoremiss; /*(14,1,2)(3,1,1)*/
	forecast id=month interval=month lead=38 out=energyout1f;
run;

data energyout1f;
	merge energy energyout1f;
	if t<=341 then
		do;
			mape_fit=abs(residual/Biomass_Energy_Consumption1)*100;
			mae_fit=abs(residual);
			mse_fit=residual**2;
		end;
	else if t>341 then
		do;
			mape_acc=abs((Biomass_Energy_Consumption-forecast)/Biomass_Energy_Consumption)*100;
			mae_acc=abs(Biomass_Energy_Consumption-forecast);
			mse_acc=abs(Biomass_Energy_Consumption-forecast)**2;
		end;
run;

proc means data=energyout1f n mean maxdec=3;
	var mape_fit mae_fit mse_fit mape_acc mae_acc mse_acc;
run;

data energyout1f;
      merge energy energyout1f;
run;

proc sgplot data= energyout1f;
     series x=month y=Biomass_Energy_Consumption;
     series x=month y=forecast;
     title "Actual Vs  Predicted plot for Biomass Energy Consumption";
run;  

/*****************************************************************************************************************************/

/* Renewable Energy Consumption */
/* Deseasonalizing  (90:10)*/
proc timeseries data=energy outdecomp=sa_energy2 out=null;
	decomp sa;
	id month interval=month;
	var renewable_energy_consumption;
run;

data sa_energy2a;
	merge energy sa_energy2(keep=sa);
	si = renewable_energy_consumption/sa;
	t=_n_;
	sa1=sa;
	if t>341 then sa1=.;
run;

proc sgplot data=sa_energy2a;
	series x=month y=renewable_energy_consumption;
	series x=month y=sa;
	title "Monthly Renewable Energy Consumption (Desasonalized) (90:10)";
run;

proc reg data=sa_energy2a;
	model sa1=t;
	output out=sa_energyout2 r=sa_resid p=sa_predict;
run;

data sa_energyout2;
	set sa_energyout2;
	energy_reseason=sa_predict*si;
	if t<=341 then
		do;
			mape_fit=abs(sa_resid/sa1)*100;
			mae_fit=abs(sa_resid);
			mse_fit=sa_resid**2;
		end;
	else if t>341 then
		do;
			mape_acc=abs((sa-sa_predict)/sa)*100;
			mae_acc=abs(sa-sa_predict);
			mse_acc=(sa-sa_predict)**2;
		end;
run;

proc means data=sa_energyout2 n mean maxdec=3;
	var mape_fit mae_fit mse_fit mape_acc mae_acc mse_acc;
	title "Error measures for Renewable Energy Consumption Deseasonalizing for 90:10 split ";
run;

proc sgplot data=sa_energyout2;
	series x=month y=renewable_energy_consumption;
	series x=month y=energy_reseason;
	title "Monthly Energy Consumption (Resasonalized)(90:10)";
run;

/* Linear Regression using dummy variables for Renewable_Energy_Consumption */
proc reg data=energy;
	model Renewable_Energy_Consumption_new=t m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12/clb vif dwprob;
	output out=energyout2a p=renew_predict r=renew_resid;
run;

proc sgplot data=energyout2a;
	series x=month y=Renewable_Energy_Consumption;
	series x=month y=renew_predict;
	title "Linear Trend for Renewable Energy Consumption for 90:10 split ";
run;

data energyout2a;
	set energyout2a;
	if t<=341 then
		do;
			mape_fit=abs(renew_resid/Renewable_Energy_Consumption_new)*100;
			mae_fit=abs(renew_resid);
			mse_fit=(renew_resid)**2;
		end;
	else if t>341 then
		do;
			mape_acc=abs((Renewable_Energy_Consumption-renew_predict)/Renewable_Energy_Consumption)*100;
			mae_acc=abs(Renewable_Energy_Consumption-renew_predict);
			mse_acc=(Renewable_Energy_Consumption-renew_predict)**2;
		end;
run;
	
proc means data=energyout2a n mean maxdec=3;
	var mape_fit mae_fit mse_fit mape_acc mae_acc mse_acc;
	title "Error measures for Renewable Energy Consumption Linear Trend for 90:10 split ";
run;

/* Non-linear Trend for Renewable_Energy_Consumption */
proc reg data=energy;
	model Renewable_Energy_Consumption_new=t t2 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12/clb vif dwprob;
	output out=energyout2b p=renew_predict r=renew_resid;
run;

proc sgplot data=energyout2b;
	series x=month y=Renewable_Energy_Consumption;
	series x=month y=renew_predict;
	title "Non-Linear Trend for Renewable Energy Consumption for 90:10 split ";
run;

data energyout2b;
	set energyout2b;
	if t<=341 then
		do;
			mape_fit=abs(renew_resid/Renewable_Energy_Consumption_new)*100;
			mae_fit=abs(renew_resid);
			mse_fit=(renew_resid)**2;
		end;
	else if t>341 then
		do;
			mape_acc=abs((Renewable_Energy_Consumption-renew_predict)/Renewable_Energy_Consumption)*100;
			mae_acc=abs(Renewable_Energy_Consumption-renew_predict);
			mse_acc=(Renewable_Energy_Consumption-renew_predict)**2;
		end;
run;

proc means data=energyout2b n mean maxdec=3;
	var mape_fit mae_fit mse_fit mape_acc mae_acc mse_acc;
	title "Error measures for Renewable Energy Consumption Non Linear Trend for 90:10 split ";
run;

/* Holt's Exponential Smoothing Method (90:10 split) */

/* Additive Winter's */

proc esm data=energy  print=all lead=38 back=38 outfor=energyout2c out=_null_;
     id month interval=month;
     forecast renewable_energy_consumption/model=addwinters;
run;

proc sgplot data=energyout2c;
      series x= month y= actual;
      series x= month y =predict;
      title "Actual vs Predicted values for Renewable Energy Consumption";
run;

/* Multiplicative Winter's */

proc esm data=energy print=all lead=38  back=38 outfor=energyout2d out=_null_;
     id month interval=month;
     forecast renewable_energy_consumption/model=winters;
run;

proc sgplot data=energyout2d;
      series x= month y= actual;
      series x= month y =predict;
      title "Actual Vs Predicted values for Renwable Energy Consumption";
run;

/* Seasonal ARIMA   (90:10 split)*/
/* Model 1 */

data energy;
     set energy;
     t=_n_;
     renewable_energy_consumption1=renewable_energy_consumption;
     if t>341 then renewable_energy_consumption1=.;
run;
    
proc arima data=energy;
	identify var=renewable_energy_consumption1(12,1) whitenoise=ignoremiss nlag=36; 
	estimate p=(11)(12)(24)(36) q=(5)(12) whitenoise=ignoremiss; /*ARIMA(11,1,5)(3,1,1) */  
	*estimate p=(3)(12)(24)(36) q=(12) whitenoise=ignoremiss; /*ARIMA(3,1,0)(3,1,1) */
	*estimate p=(3)(12)(24)(36) q=(2)(12) whitenoise=ignoremiss; /*ARIMA(3,1,2)(3,1,1) */
    forecast id=month interval=month lead=38 out=energyout2e;
run;

data energyout2e;
     merge energy energyout2e;
     if t<=341 then
      do;
          mape_fit=(abs((residual)/renewable_energy_consumption1))*100;
          mae_fit=abs(residual);
          mse_fit=residual**2;
     end;
   else if t>341 then
     do;
          mape_acc=abs((renewable_energy_consumption-forecast)/renewable_energy_consumption)*100;
          mae_acc=abs(renewable_energy_consumption-forecast);
          mse_acc=(renewable_energy_consumption-forecast) **2;
     end;
run;

proc means data=energyout2e n mean maxdec=3;
   var mape_fit mae_fit mse_fit mape_acc mae_acc mse_acc;
run;

data energyout2e;
      merge energy energyout2e;
run;

proc sgplot data= energyout2e;
     series x=month y=renewable_energy_consumption;
     series x=month y=forecast;
     title "Actual Vs  Predicted values for Renewable Energy Consumption";
run;  

/* Model 2 */

data energy;
     set energy;
     t=_n_;
     renewable_energy_consumption1=renewable_energy_consumption;
     if t>341 then renewable_energy_consumption1=.;
run;

proc arima data=energy;
	identify var=renewable_energy_consumption1(12,1) whitenoise=ignoremiss nlag=36; 
	estimate p=(14)(12)(24)(36) q=(17)(12) whitenoise=ignoremiss; /*ARIMA(14,1,17)(3,1,1) */  
    forecast id=month interval=month lead=38 out=energyout2f;
run;

data energyout2f;
     merge energy energyout2f;
     if t<=341 then
      do;
          mape_fit=(abs((residual)/renewable_energy_consumption1))*100;
          mae_fit=abs(residual);
          mse_fit=residual**2;
     end;
   else if t>341 then
     do;
          mape_acc=abs((renewable_energy_consumption-forecast)/renewable_energy_consumption)*100;
          mae_acc=abs(renewable_energy_consumption-forecast);
          mse_acc=(renewable_energy_consumption-forecast) **2;
     end;
run;

proc means data=energyout2f n mean maxdec=3;
   var mape_fit mae_fit mse_fit mape_acc mae_acc mse_acc;
run;

data energyout2f;
      merge energy energyout2f;
run;

proc sgplot data= energyout2f;
     series x=month y=renewable_energy_consumption;
     series x=month y=forecast;
     title "Actual Vs  Predicted values for Renewable Energy Consumption";
run; 

/*****************************************************************************************************************************/

/* Forecast for next 12-month period using Holt's Winter Exponential Smoothing model*/
/* Renewable Energy Consumption */

proc esm data=energy lead=12  print=all outfor=energyout3 out=null;
	id month interval=month;
	forecast renewable_energy_consumption/model=winters;
run;

proc sgplot data=energyout3;
      series x= month y= actual;
      series x= month y =predict;
      title " Actual vs Predicted values for Renewable Energy Consumption ";
run;

/* Biomass Energy Consumption */

proc esm data=energy print=all lead=12 outfor=energyout4 out=_null_;
	id month interval=month;
	forecast Biomass_Energy_Consumption/model=addwinters;
run;

proc sgplot data=energyout4;
	series x=month y=actual;
	series x=month y=predict;
	title " Actual vs Predicted values for Biomass Energy Consumption ";
run;


