#include <R.h>
#include <math.h>

# include "brent.h"

//-------------------------------------------------------
// TODO:
// - [DONE] split premium frequency (number of premium per year) / default leg discretisation (Timestep) 
// - [DONE] accruedpremium
// - [DONE] effor handling when bootstrap not working
//
//
//-------------------------------------------------------

// internal functions
double getDiscountFactor(double *yieldcurve, int nyieldcurve, double t) 
{
	double result = -1.0;
	double *yieldcurveTenor = &yieldcurve[0];
	double *yieldcurveRate = &yieldcurve[nyieldcurve];
	
	int min_time_index = 0;
	int max_time_index = nyieldcurve - 1;
	int i;
	
	if (t < 0) {
		result = -1.0;
	}
	else if (t == 0) {
		result = 1.0;
	}
	else if (t > 0 && t < yieldcurveTenor[min_time_index]) {
		result = exp(-t * yieldcurveRate[min_time_index]);
	}
	else if (t >= yieldcurveTenor[max_time_index]) {
		result = exp(-t * yieldcurveRate[max_time_index]);
	}
	else {
		for (i=0; i < nyieldcurve - 1; i++) {
			if (t >= yieldcurveTenor[i] && t < yieldcurveTenor[i+1]) {
				double dr = yieldcurveRate[i] + (yieldcurveRate[i+1]-yieldcurveRate[i]) * (t-yieldcurveTenor[i]) / (yieldcurveTenor[i+1]-yieldcurveTenor[i]);
				
				result = exp(-t * dr);
			}
		}
	}
	
	return(result);
}

double getSurvivalProbability(double *creditcurve, int ncreditcurve, double t) 
{
	double result = -1.0;
	double *creditcurveTenor = &creditcurve[0];
	double *creditcurveSurvivalProbability = &creditcurve[ncreditcurve];
	
	int min_time_index = 0;
	int max_time_index = ncreditcurve - 1;
	int i;
	
	if (t < 0) {
		result = -1.0; // undefined case
	}
	else if (t == 0) {
		result = 1.0;
	}
	else if (t > 0 && t < creditcurveTenor[min_time_index]) {
		double h = (-1/creditcurveTenor[min_time_index])*log(creditcurveSurvivalProbability[min_time_index]);
		result = exp(-t * h);
	}
	else if (t == creditcurveTenor[max_time_index]) {
		result = creditcurveSurvivalProbability[max_time_index];
	}
	else if (t > creditcurveTenor[max_time_index]) {
		double h = 0;
		if (max_time_index == 0) {
			h = -1/(creditcurveTenor[max_time_index]-0) * log(creditcurveSurvivalProbability[max_time_index]/1.0);
		}
		else {
			h = -1/(creditcurveTenor[max_time_index]-creditcurveTenor[max_time_index-1]) * log(creditcurveSurvivalProbability[max_time_index]/creditcurveSurvivalProbability[max_time_index-1]);
		}
				
		result = creditcurveSurvivalProbability[max_time_index]*exp(-(t-creditcurveTenor[max_time_index]) * h);
	}
	else {
		for (i=0; i < ncreditcurve - 1; i++) {
			if (t >= creditcurveTenor[i] && t < creditcurveTenor[i+1]) {
				double h = -1/(creditcurveTenor[i+1]-creditcurveTenor[i]) * log(creditcurveSurvivalProbability[i+1]/creditcurveSurvivalProbability[i]);
				
				result = creditcurveSurvivalProbability[i]*exp(-(t-creditcurveTenor[i]) * h);
			}
		}
	}
	
	return(result);
}

void initializeCreditCurve(double** creditcurve, int *ncreditcurve) {
	*ncreditcurve = 0;
	(*creditcurve) = malloc((*ncreditcurve) * 2 * sizeof(double));
}

void addTenorToCreditCurve(double** creditcurve, int *ncreditcurve, double tenor, double h){	
	double* tmp = malloc((*ncreditcurve) * 2 * sizeof(double));	
	for (int i=0; i < (*ncreditcurve)*2 ; i++) {
		tmp[i] = (*creditcurve)[i];
	}
	
	(*creditcurve) = realloc(*creditcurve, ((*ncreditcurve)+1) * 2 * sizeof(double));
	for (int i=0; i < ((*ncreditcurve)+1)*2 ; i++) {
		(*creditcurve)[i] = 0;
	}
	
	for (int i=0; i < *ncreditcurve ; i++) {
		//Rprintf("i=%d\n",i);
		(*creditcurve)[i] = tmp[i];
		(*creditcurve)[i + (*ncreditcurve) + 1] = tmp[i + (*ncreditcurve)];
	}
	(*creditcurve)[*ncreditcurve] = tenor;
	(*creditcurve)[(*ncreditcurve)*2+1] = h;
	
	(*ncreditcurve) = (*ncreditcurve) + 1;
	
	// free tmp memory
	free(tmp);
}

void printCreditCurve(double *creditcurve, int ncreditcurve){
	Rprintf("tenor,hazardrate\n");
	for (int i=0; i < ncreditcurve; i++) {
		Rprintf("%f,%f \n",creditcurve[i],creditcurve[ i + ncreditcurve]);
	}
}

double calculatePremiumLeg(double *creditcurve, int ncreditcurve, double *yieldcurve, int nyieldcurve, double cdsMaturity, int numberPremiumPerYear,int accruedPremiumFlag, double spread, double h) {
	double *creditcurveTenor = &creditcurve[0];
	//double *creditcurveSurvivalProbability = &creditcurve[ncreditcurve];
	int max_time_index = ncreditcurve - 1;
	
	// if creditcurve not empty (i.e ncreditcurve > 0) and 
	// if cdsMaturity <= creditcurveTenor[max_time_index]
	// i.e. if hazard rate already know for cdsMaturity 
	// ==> simple calculation, no bootstrap required
	if (ncreditcurve > 0 && cdsMaturity <= creditcurveTenor[max_time_index]) {
		//Rprintf("calculatePremiumLeg ==> no boostrap,ncreditcurve=: %d\n",ncreditcurve);
		double annuity = 0;
		double accruedPremium = 0;
		int N = (int) (cdsMaturity * (double) numberPremiumPerYear);
		
		int n;
		for (n = 1; n <=N; n++) {
			double tn = ((double) n)/((double) numberPremiumPerYear);
			double tnm1 = ((double) (n-1))/((double) numberPremiumPerYear);			
			double delta_t = 1.0/((double) numberPremiumPerYear);
			annuity += delta_t*getDiscountFactor(yieldcurve,nyieldcurve,tn)*getSurvivalProbability(creditcurve,ncreditcurve,tn);

			if (accruedPremiumFlag) {
				accruedPremium += 0.5 * delta_t * getDiscountFactor(yieldcurve,nyieldcurve,tn) * 
							  (getSurvivalProbability(creditcurve,ncreditcurve,tnm1)-getSurvivalProbability(creditcurve,ncreditcurve,tn));
			}
		}
		
		//Rprintf("calculatePremiumLeg (h=%f) ==> %f\n",h,spread*annuity);
		return(spread * (annuity + accruedPremium));
	}
	// if cdsMaturity > creditcurveTenor[max_time_index]
	// i.e. hazard rate not know for cdsMaturity 
	// ==> we will use the CDS spread to imply the non-cumulative hazard rate (h) (bootstrapping method)
	else {
		//Rprintf("calculatePremiumLeg ==> boostrap,ncreditcurve=: %d\n",ncreditcurve);
		double annuity = 0;
		double accruedPremium = 0;
		int N = (int) (cdsMaturity * (double) numberPremiumPerYear);
		int M = (ncreditcurve > 0 ? (int) (creditcurveTenor[max_time_index] * (double) numberPremiumPerYear) : 0 );
		
		for (int n = 1; n <=N; n++) {
			if (n <=M) {
				double tn = ((double) n)/((double) numberPremiumPerYear);
				double tnm1 = ((double) (n-1))/((double) numberPremiumPerYear);		
				double delta_t = 1.0/((double) numberPremiumPerYear);
				
				annuity += delta_t*getDiscountFactor(yieldcurve,nyieldcurve,tn) * getSurvivalProbability(creditcurve,ncreditcurve,tn);		 
				
				if (accruedPremiumFlag) {
					accruedPremium += 0.5 * delta_t * getDiscountFactor(yieldcurve,nyieldcurve,tn) * 
									(getSurvivalProbability(creditcurve,ncreditcurve,tnm1) - getSurvivalProbability(creditcurve,ncreditcurve,tn));
				}
			}
			else {
				double tn = ((double) n)/((double) numberPremiumPerYear);
				double tnm1 = ((double) (n-1))/((double) numberPremiumPerYear);		
				double tM = ((double) M)/((double) numberPremiumPerYear);
				double delta_t = 1.0/((double) numberPremiumPerYear);				
				
				double survivalProbabilityn = getSurvivalProbability(creditcurve,ncreditcurve,tM) * exp(-h*(tn-tM));
				double survivalProbabilitynm1;
				if (tnm1 <= tM) survivalProbabilitynm1 = getSurvivalProbability(creditcurve,ncreditcurve,tnm1);
				else survivalProbabilitynm1 = getSurvivalProbability(creditcurve,ncreditcurve,tM) * exp(-h*(tnm1-tM));
				
				annuity += delta_t * getDiscountFactor(yieldcurve,nyieldcurve,tn) * survivalProbabilityn;

				if (accruedPremiumFlag) {
					accruedPremium += 0.5 * delta_t * getDiscountFactor(yieldcurve,nyieldcurve,tn) * 
									(survivalProbabilitynm1 - survivalProbabilityn);
				}
			}
		}
		
		//Rprintf("calculatePremiumLeg (h=%f) ==> %f\n",h,spread*annuity);
		return(spread * (annuity + accruedPremium));
	}	
}

double calculateDefaultLeg(double *creditcurve, int ncreditcurve, double *yieldcurve, int nyieldcurve, double cdsMaturity, int numberDefaultIntervalPerYear, double recoveryRate, double h) {
	double *creditcurveTenor = &creditcurve[0];
	//double *creditcurveSurvivalProbability = &creditcurve[ncreditcurve];
	int max_time_index = ncreditcurve - 1;
	
	// if creditcurve not empty (i.e ncreditcurve > 0) and 
	// if cdsMaturity <= creditcurveTenor[max_time_index]
	// i.e. if hazard rate already know for cdsMaturity 
	// ==> simple calculation, no bootstrap required
	if (ncreditcurve > 0 && cdsMaturity <= creditcurveTenor[max_time_index]) {
		//Rprintf("calculateDefaultLeg (%f) ==> no boostrap\n",cdsMaturity,ncreditcurve);
		double annuity = 0;
		int N = (int) (cdsMaturity * (double) numberDefaultIntervalPerYear);
		
		for (int n = 1; n <=N; n++) {
			double tn = ((double) n)/((double) numberDefaultIntervalPerYear);
			double tnm1 = ((double) (n-1))/((double) numberDefaultIntervalPerYear);
			annuity += getDiscountFactor(yieldcurve,nyieldcurve,tn)*(getSurvivalProbability(creditcurve,ncreditcurve,tnm1)-getSurvivalProbability(creditcurve,ncreditcurve,tn));		 
		}
		
		return((1.0-recoveryRate)*annuity);
	}
	// if cdsMaturity > creditcurveTenor[max_time_index]
	// i.e. hazard rate not know for cdsMaturity 
	// ==> we will use the CDS spread to imply the non-cumulative hazard rate (h) (bootstrapping method)
	else {
		//Rprintf("calculateDefaultLeg (%f/%f) ==> boostrap\n",cdsMaturity,creditcurveTenor[max_time_index]);
		double annuity = 0;
		int N = (int) (cdsMaturity * (double) numberDefaultIntervalPerYear);
		int M = (ncreditcurve > 0 ? (int) (creditcurveTenor[max_time_index] * (double) numberDefaultIntervalPerYear) : 0 );
		
		for (int n = 1; n <=N; n++) {
			if (n <= M) {
				double tn = ((double) n)/((double) numberDefaultIntervalPerYear);
				double tnm1 = ((double) (n-1))/((double) numberDefaultIntervalPerYear);
				annuity += getDiscountFactor(yieldcurve,nyieldcurve,tn)*(getSurvivalProbability(creditcurve,ncreditcurve,tnm1)-getSurvivalProbability(creditcurve,ncreditcurve,tn));
				
				//Rprintf("SP[%d]={%f}, SP[%d]={%f}\n",n,getSurvivalProbability(creditcurve,ncreditcurve,tn),n-1,getSurvivalProbability(creditcurve,ncreditcurve,tnm1));	
				//Rprintf("n=%d,M=%d,N=%d ==> annuity = %f\n",n,M,N,annuity);
			}
			else {
				double tM = ((double) M)/((double) numberDefaultIntervalPerYear);
				double tn = ((double) n)/((double) numberDefaultIntervalPerYear);
				double tnm1 = ((double) (n-1))/((double) numberDefaultIntervalPerYear);
				
				double survivalProbabilityn = getSurvivalProbability(creditcurve,ncreditcurve,tM) * exp(-h*(tn-tM));
				double survivalProbabilitynm1;
				if (tnm1 <= tM) survivalProbabilitynm1 = getSurvivalProbability(creditcurve,ncreditcurve,tnm1);
				else survivalProbabilitynm1 = getSurvivalProbability(creditcurve,ncreditcurve,tM) * exp(-h*(tnm1-tM));
				
				annuity += getDiscountFactor(yieldcurve,nyieldcurve,tn)*(survivalProbabilitynm1-survivalProbabilityn);
				
				//Rprintf("SP[%d]={%f}, SP[%d]={%f}\n",n,survivalProbabilityn,n-1,survivalProbabilitynm1);	
				//Rprintf("n=%d,M=%d,N=%d ==> annuity = %f\n",n,M,N,annuity);				
			}
		}		
		return((1.0-recoveryRate)*annuity);		
	}	
}

void priceCreditDefaultSwapSpreads(double *yieldcurve, int *nyieldcurve, double *creditcurve, int *ncreditcurve, double *cdsTenors, int *ncdsTenors, int *numberPremiumPerYear, int *numberDefaultIntervalPerYear, int *accruedPremiumFlag, double *recoveryRate, double *spreads, int * warningFlag) {
	int yieldcurveLength = nyieldcurve[0];
	int creditcurveLength = ncreditcurve[0];
	int cdsTenorsLength = ncdsTenors[0];
	int premiumFrequency = numberPremiumPerYear[0];
	int defaultFrequency = numberDefaultIntervalPerYear[0];
	int accruedPremium = accruedPremiumFlag[0];
	double RR = recoveryRate[0];
	
	double lower = 0.0;
	double upper = 30.0;
	double matchep = 2.220446049250313E-016;
	double t = matchep;
	
	for (int i=0; i < cdsTenorsLength; i++) {
		double objfun(double spread) {
			double result = calculatePremiumLeg(creditcurve,creditcurveLength,yieldcurve,yieldcurveLength,cdsTenors[i],premiumFrequency,accruedPremium, spread,0) - 
			calculateDefaultLeg(creditcurve,creditcurveLength,yieldcurve,yieldcurveLength,cdsTenors[i],defaultFrequency,RR,0);
			return(result);
		};
	
		/*double premleg = calculatePremiumLeg(yieldcurve,yieldcurveLength,creditcurve,creditcurveLength,cdsTenors[i],4,0.0050);
		double defaultleg = calculateDefaultLeg(yieldcurve,yieldcurveLength,creditcurve,creditcurveLength,cdsTenors[i],4,0.40);
		double spread = calculateCreditDefaultSwapSpread(yieldcurve,yieldcurveLength,creditcurve,creditcurveLength,cdsTenors[i],4,0.40);
		Rprintf("Premium Leg: %f\n",premleg);
		Rprintf("Default Leg: %f\n",defaultleg);
		Rprintf("Spread: %f\n",spread);	
		*/
		spreads[i] = zero(lower,upper,matchep,t,objfun);
		if (spreads[i] == lower || spreads[i] == upper) *warningFlag = 1;
		
		//double premleg = calculatePremiumLeg(yieldcurve,yieldcurveLength,creditcurve,creditcurveLength,cdsTenors[i],freq,spreads[i],0);
		//double defaultleg = calculateDefaultLeg(yieldcurve,yieldcurveLength,creditcurve,creditcurveLength,cdsTenors[i],freq,RR,0.05);
		//Rprintf("Premium Leg: %f, Default Leg: %f\n",premleg, defaultleg);
	}	
}

void bootstrapCreditDefaultSwapSpreads(double *yieldcurve, int *nyieldcurve, double *cdsTenors, int *ncdsTenors, double *spreads, int *numberPremiumPerYear, int *numberDefaultIntervalPerYear, int *accruedPremiumFlag, double *recoveryRate, double *output, int * warningFlag) {
	int yieldcurveLength = nyieldcurve[0];
	int cdsTenorsLength = ncdsTenors[0];
	int premiumFrequency = numberPremiumPerYear[0];
	int defaultFrequency = numberDefaultIntervalPerYear[0];
	int accruedPremium = accruedPremiumFlag[0];
	double RR = recoveryRate[0];
	
	int newcreditcurveLength = 0;
	double *newcreditcurve;
	initializeCreditCurve(&newcreditcurve,&newcreditcurveLength);	
	
	double lower = 0.0;
	double upper = 30.0;
	double matchep = 2.220446049250313E-016;
	double t = matchep;
	
	double survprob[cdsTenorsLength];
	double hazrate[cdsTenorsLength];
	
	for (int i=0; i < cdsTenorsLength; i++) {		
		double objfun(double h) {
		double result = calculatePremiumLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,cdsTenors[i],premiumFrequency,accruedPremium, spreads[i],h) - calculateDefaultLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,cdsTenors[i],defaultFrequency,RR,h);
			//Rprintf("objfun (h=%f) ==> %f\n",h,result);
			return(result);
		};
		
		double h = zero(lower,upper,matchep,t,objfun);
		if (h == lower || h == upper) *warningFlag = 1;
		
		/*Rprintf("hazardrate=%f, PremiumLeg=%f , DefaultLeg=%f\n",
		h,
		calculatePremiumLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,cdsTenors[i],freq,spreads[i],h),
		calculateDefaultLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,cdsTenors[i],freq,RR,h));*/
		
		hazrate[i] = h;
		if (i == 0) survprob[i] = exp(-hazrate[i]*cdsTenors[i]);
		else survprob[i] = survprob[i-1]*exp(-hazrate[i]*(cdsTenors[i]-cdsTenors[i-1]));
		
		addTenorToCreditCurve(&newcreditcurve,&newcreditcurveLength,cdsTenors[i],survprob[i]);		
	}
	
	for (int i=0; i < cdsTenorsLength; i++) {
		output[i] = survprob[i];
		output[i + cdsTenorsLength] = hazrate[i];
	}	
}


// -----------------------------------
// Tests
// -----------------------------------

void test1(double *yieldcurve, int *nyieldcurve, double *creditcurve, int *ncreditcurve) {
	int i;
	int yieldcurveLength = nyieldcurve[0];
	int creditcurveLength = ncreditcurve[0];
	
	// browse yieldcurve
	for (i=0; i < yieldcurveLength; i++) {
		Rprintf("tenor: %f, disc. rate: %f\n",yieldcurve[i],yieldcurve[yieldcurveLength + i]);
	}
	
	// browse creditcurve
	for (i=0; i < creditcurveLength; i++) {
		Rprintf("tenor: %f, surv. prob: %f\n",creditcurve[i],creditcurve[creditcurveLength + i]);
	}
	
	Rprintf("getDiscountFactor\n");
	Rprintf("tenor: %f, df: %f\n",2.5,getDiscountFactor(yieldcurve,yieldcurveLength,2.5));
	Rprintf("tenor: %f, df: %f\n",0.0,getDiscountFactor(yieldcurve,yieldcurveLength,0.0));
	Rprintf("tenor: %f, df: %f\n",5.0,getDiscountFactor(yieldcurve,yieldcurveLength,5.0));
	Rprintf("tenor: %f, df: %f\n",4.0,getDiscountFactor(yieldcurve,yieldcurveLength,4.0));
	Rprintf("tenor: %f, df: %f\n",0.5,getDiscountFactor(yieldcurve,yieldcurveLength,0.5));
	Rprintf("tenor: %f, df: %f\n",3.5,getDiscountFactor(yieldcurve,yieldcurveLength,3.5));
	
	Rprintf("getSurvivalProbability\n");
	Rprintf("tenor: %f, df: %f\n",2.5,getSurvivalProbability(creditcurve,creditcurveLength,2.5));
	Rprintf("tenor: %f, df: %f\n",7.0,getSurvivalProbability(creditcurve,creditcurveLength,7.0));
	Rprintf("tenor: %f, df: %f\n",8.0,getSurvivalProbability(creditcurve,creditcurveLength,8.0));
	Rprintf("tenor: %f, df: %f\n",0.5,getSurvivalProbability(creditcurve,creditcurveLength,0.5));
	Rprintf("tenor: %f, df: %f\n",6.0,getSurvivalProbability(creditcurve,creditcurveLength,6.0));
	Rprintf("tenor: %f, df: %f\n",5.0,getSurvivalProbability(creditcurve,creditcurveLength,5.0));
	Rprintf("tenor: %f, df: %f\n",6.5,getSurvivalProbability(creditcurve,creditcurveLength,6.5));
	
	//double zero ( double a, double b, double machep, double t, 
    //double f ( double x ) )
	double f(double x) {
		return(2*x*x*x-5*x*x+4*x-3);
	};
	double sol;
	sol = zero(0.0,10.0,10e-8,10E-8,f);
	Rprintf("solution: %f\n",sol);
	
	double premleg = calculatePremiumLeg(yieldcurve,yieldcurveLength,creditcurve,creditcurveLength,3.0,4,0,0.0050,0);
	double defaultleg = calculateDefaultLeg(yieldcurve,yieldcurveLength,creditcurve,creditcurveLength,3.0,4,0.40,0);
	Rprintf("Premium Leg: %f\n",premleg);
	Rprintf("Default Leg: %f\n",defaultleg);
	

	
}

void test2(double *yieldcurve, int *nyieldcurve) {
	// test bootstraping
	Rprintf("- - - - - - - - - - - - - - - - \n");	
	Rprintf("Bootstraping test\n");	
	Rprintf("- - - - - - - - - - - - - - - - \n");
	
	int yieldcurveLength = nyieldcurve[0];
	int newcreditcurveLength = 0;
	double *newcreditcurve;
	newcreditcurve = malloc(newcreditcurveLength * 2 * sizeof(double));
	memset(newcreditcurve , 0 , newcreditcurveLength * 2 * sizeof(* newcreditcurve));
	
	double RR = 0.40;
	int freq = 4;
	double lower = 0.0;
	double upper = 30.0;
	double matchep = 2.220446049250313E-016;
	double t = matchep;
	
	// first tenor 
	double cdsMaturity = 1;
	double cdsSpread = 0.0050; // 50bp
	
	double objfun1(double h) {
		double result = calculatePremiumLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,cdsMaturity,freq,0,cdsSpread,h) - 
		calculateDefaultLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,cdsMaturity,freq,RR,h);
		Rprintf("objfun1 (h=%f) ==> %f\n",h,result);
		return(result);
	};		
	double h1 = zero(lower,upper,matchep,t,objfun1);
	double sp1 = exp(-h1*cdsMaturity);
	Rprintf("h1=: %f\n",h1);
	Rprintf("SurvProb1=: %f\n",sp1);
	/*
	newcreditcurveLength = 1;
	newcreditcurve = malloc(newcreditcurveLength * 2 * sizeof(double));
	newcreditcurve[0] = cdsMaturity;
	newcreditcurve[0 + newcreditcurveLength] = sp1;
	*/
	addTenorToCreditCurve(&newcreditcurve,&newcreditcurveLength,cdsMaturity,sp1);
	
	// second tenor
	cdsMaturity = 2;
	cdsSpread = 0.0050; // 80bp
	
	double objfun2(double h) {
		double result = calculatePremiumLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,cdsMaturity,freq,0,cdsSpread,h) - 
		calculateDefaultLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,cdsMaturity,freq,RR,h);
		Rprintf("objfun2 (h=%f) ==> %f\n",h,result);
		return(result);
	};
	
	double h2 = zero(lower,upper,matchep,t,objfun2);
	double sp2 = exp(-h2*cdsMaturity)*sp1;
	Rprintf("h2=: %f\n",h2);
	Rprintf("SurvProb2=: %f\n",sp2);
	
	/*
	newcreditcurveLength = 2;
	double *tmp = malloc(newcreditcurveLength * 2 * sizeof(double));
	tmp[0] = newcreditcurve[0];
	tmp[0 + newcreditcurveLength] = newcreditcurve[1];
	tmp[1] = cdsMaturity;
	tmp[1 + newcreditcurveLength] = h2;
	newcreditcurve = tmp;
	*/
	addTenorToCreditCurve(&newcreditcurve,&newcreditcurveLength,cdsMaturity,sp2);
	
	// third tenor
	cdsMaturity = 3;
	cdsSpread = 0.0050; // 100bp
	
	double objfun3(double h) {
		double result = calculatePremiumLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,cdsMaturity,freq,0,cdsSpread,h) - 
		calculateDefaultLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,cdsMaturity,freq,RR,h);
		Rprintf("objfun3 (h=%f) ==> %f\n",h,result);
		return(result);
	};
	
	double h3 = zero(lower,upper,matchep,t,objfun3);
	double sp3 = exp(-h3*cdsMaturity)*sp2;
	Rprintf("h3=: %f\n",h3);
	Rprintf("SurvProb2=: %f\n",sp3);
	
}

void test3() {
	/*
	int bnlcurvelength = 0;
	double *blncurve;
	blncurve = malloc(bnlcurvelength * 2 * sizeof(double));
	printCreditCurve(blncurve,bnlcurvelength);
	
	addTenorToCreditCurve(blncurve,&bnlcurvelength,0.5,0.002);
	printCreditCurve(blncurve,bnlcurvelength);
	
	addTenorToCreditCurve(blncurve,&bnlcurvelength,1.0,0.007);
	printCreditCurve(blncurve,bnlcurvelength);
*/
	int bnlcurve2length = 0;
	double* blncurve2 = calloc(bnlcurve2length * 2, sizeof(double));
	
	/*
	int bnlcurve2length = 1;
	double* blncurve2 = calloc(bnlcurve2length * 2, sizeof(double));
	blncurve2[0] = 1.0;
	blncurve2[1] = 0.07;
	printCreditCurve(blncurve2,bnlcurve2length);
	*/
	/*
	bnlcurve2length = 2;
	blncurve2 = realloc(blncurve2, bnlcurve2length * 2 * sizeof(double));
	blncurve2[0] = 1.0;
	blncurve2[2] = 0.07;
	
	blncurve2[1] = 2.0;
	blncurve2[3] = 0.09;
	printCreditCurve(blncurve2,bnlcurve2length);
	*/
	
	addTenorToCreditCurve(&blncurve2,&bnlcurve2length,2.0,0.008);
	printCreditCurve(blncurve2,bnlcurve2length);
	
	addTenorToCreditCurve(&blncurve2,&bnlcurve2length,3.0,0.00666);
	printCreditCurve(blncurve2,bnlcurve2length);
	
	//
	//printCreditCurve(blncurve2,bnlcurve2length);

}

void test4(double *yieldcurve, int *nyieldcurve) {
	// test bootstraping
	Rprintf("- - - - - - - - - - - - - - - - \n");	
	Rprintf("Bootstraping test\n");	
	Rprintf("- - - - - - - - - - - - - - - - \n");
	
	int yieldcurveLength = nyieldcurve[0];
	
	int newcreditcurveLength = 0;
	double *newcreditcurve;
	initializeCreditCurve(&newcreditcurve,&newcreditcurveLength);	
	printCreditCurve(newcreditcurve,newcreditcurveLength);
	
	double tenors[5] = {0.5,1.0,2.0,3.0,5.0};
	//double spreads[5] = {0.0050,0.0060,0.0070,0.0080,0.0090};
	double spreads[5] = {0.3000,0.1200,0.0900,0.0850,0.0850};
	double survprob[5] = {};
	double hazrate[5] = {};
	
	double RR = 0.40;
	int freq = 4;
	double lower = 0.0;
	double upper = 30.0;
	double matchep = 2.220446049250313E-016;
	double t = matchep;
		
	
	for (int i=0; i<5; i++) {
		double objfun(double h) {
		double result = calculatePremiumLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,tenors[i],freq,0,spreads[i],h) - 
			calculateDefaultLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,tenors[i],freq,RR,h);
			Rprintf("objfun (h=%f) ==> %f\n",h,result);
			return(result);
		};

		double h = zero(lower,upper,matchep,t,objfun);
		hazrate[i] = h;
		if (i == 0) survprob[i] = exp(-hazrate[i]*tenors[i]);
		else survprob[i] = survprob[i-1]*exp(-hazrate[i]*tenors[i]);
		
		Rprintf("h[%d]= %f / %f\n",i,hazrate[i],survprob[i]);
		
		addTenorToCreditCurve(&newcreditcurve,&newcreditcurveLength,tenors[i],survprob[i]);
	}
	
	printCreditCurve(newcreditcurve,newcreditcurveLength);
}

void test5() {
	
	double yieldcurve[12] = {1.0, 2.0, 3.0, 4.0, 5.0, 7.0, 0.0050, 0.0070, 0.0080, 0.0100, 0.0120, 0.0150};
	int nyieldcurve = 6;
	int yieldcurveLength = nyieldcurve;
	
	int newcreditcurveLength = 0;
	double *newcreditcurve;
	initializeCreditCurve(&newcreditcurve,&newcreditcurveLength);	
	printCreditCurve(newcreditcurve,newcreditcurveLength);
	
	double tenors[1] = {1.0};
	double spreads[1] = {0.0050};
	double survprob[1] = {};
	double hazrate[1] = {};
	
	double RR = 0.40;
	int freq = 4;
	double lower = 0.0;
	double upper = 30.0;
	double matchep = 2.220446049250313E-016;
	double t = matchep;
	
	for (int i=0; i<1; i++) {
		double objfun(double h) {
		double result = calculatePremiumLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,tenors[i],freq,0,spreads[i],h) - 
			calculateDefaultLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,tenors[i],freq,RR,h);
			Rprintf("objfun (h=%f) ==> %f\n",h,result);
			return(result);
		};

		double h = zero(lower,upper,matchep,t,objfun);
		hazrate[i] = h;
		if (i == 0) survprob[i] = exp(-hazrate[i]*tenors[i]);
		else survprob[i] = survprob[i-1]*exp(-hazrate[i]*tenors[i]);
		
		Rprintf("h[%d]= %f / %f\n",i,hazrate[i],survprob[i]);		
		addTenorToCreditCurve(&newcreditcurve,&newcreditcurveLength,tenors[i],survprob[i]);
	}
	printCreditCurve(newcreditcurve,newcreditcurveLength);
	
	Rprintf("1Y SP:{%f}\n",getSurvivalProbability(newcreditcurve,newcreditcurveLength,1.0));
	
	double h = 0.150;
    double spread = 0.0080;
	double tenor = 2.0;
	
	double premLeg = calculatePremiumLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,tenor,freq,0,spread,h);
	double defaultLeg = calculateDefaultLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,tenor,freq,RR,h);
	
	Rprintf("h= %f, spread=%f, premLeg=%f, defaultLeg=%f\n",h,spread,premLeg,defaultLeg);		

}
