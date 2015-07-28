#include <R.h>
#include <math.h>

# include "brent.h"
# include "credule.h"

//-------------------------------------------------------
// TODO:
// - [DONE] split premium frequency (number of premium per year) / default leg discretisation (Timestep) 
// - [DONE] accruedpremium
// - [DONE] effor handling when bootstrap not working
//
//
//-------------------------------------------------------

// Global static variable
static struct paramFindParSpread globalParamFindParSpread; 
static struct paramFindHazardRate globalParamFindHazardRate; 

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
	/*Rprintf("creditcurve[0]=%f, ncreditcurve=%d, yieldcurve[0]=%f, nyieldcurve=%d, cdsMaturity=%f, numberPremiumPerYear=%d, numberPremiumPerYear=%d, accruedPremiumFlag=%, h=%f",creditcurve[0],ncreditcurve,yieldcurve[0],nyieldcurve,cdsMaturity,numberPremiumPerYear, numberPremiumPerYear,accruedPremiumFlag,h);*/
	//Rprintf("creditcurve[0]=%f, ncreditcurve=%d, yieldcurve[0]=%f, nyieldcurve=%d\n",creditcurve[0],ncreditcurve,yieldcurve[0],nyieldcurve);
	
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


double objfunFindParSpread(double spread) {
	double result = 
	calculatePremiumLeg(globalParamFindParSpread.creditcurve,
						globalParamFindParSpread.creditcurveLength,
						globalParamFindParSpread.yieldcurve,
						globalParamFindParSpread.yieldcurveLength,
						globalParamFindParSpread.cdsTenor,
						globalParamFindParSpread.premiumFrequency,
						globalParamFindParSpread.accruedPremium,
						spread,
						globalParamFindParSpread.hazardRate) -
	calculateDefaultLeg(globalParamFindParSpread.creditcurve,
						globalParamFindParSpread.creditcurveLength,
						globalParamFindParSpread.yieldcurve,
						globalParamFindParSpread.yieldcurveLength,
						globalParamFindParSpread.cdsTenor,
						globalParamFindParSpread.defaultFrequency,
						globalParamFindParSpread.recoveryRate,
						globalParamFindParSpread.hazardRate);
	return(result);
}

double objfunFindHazardRate(double h) {
	double result = 
	calculatePremiumLeg(globalParamFindHazardRate.creditcurve,
						globalParamFindHazardRate.creditcurveLength,
						globalParamFindHazardRate.yieldcurve,
						globalParamFindHazardRate.yieldcurveLength,
						globalParamFindHazardRate.cdsTenor,
						globalParamFindHazardRate.premiumFrequency,
						globalParamFindHazardRate.accruedPremium,
						globalParamFindHazardRate.spread,
						h) -
	calculateDefaultLeg(globalParamFindHazardRate.creditcurve,
						globalParamFindHazardRate.creditcurveLength,
						globalParamFindHazardRate.yieldcurve,
						globalParamFindHazardRate.yieldcurveLength,
						globalParamFindHazardRate.cdsTenor,
						globalParamFindHazardRate.defaultFrequency,
						globalParamFindHazardRate.recoveryRate,
						h);
	return(result);
}

// functions exposed to R
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
		/*double premleg = calculatePremiumLeg(yieldcurve,yieldcurveLength,creditcurve,creditcurveLength,cdsTenors[i],4,0.0050);
		double defaultleg = calculateDefaultLeg(yieldcurve,yieldcurveLength,creditcurve,creditcurveLength,cdsTenors[i],4,0.40);
		double spread = calculateCreditDefaultSwapSpread(yieldcurve,yieldcurveLength,creditcurve,creditcurveLength,cdsTenors[i],4,0.40);
		Rprintf("Premium Leg: %f\n",premleg);
		Rprintf("Default Leg: %f\n",defaultleg);
		Rprintf("Spread: %f\n",spread);	
		*/
		globalParamFindParSpread.creditcurve = creditcurve;
		globalParamFindParSpread.creditcurveLength = creditcurveLength;
		globalParamFindParSpread.yieldcurve = yieldcurve;
		globalParamFindParSpread.yieldcurveLength = yieldcurveLength;
		globalParamFindParSpread.cdsTenor = cdsTenors[i];
		globalParamFindParSpread.premiumFrequency = premiumFrequency;
		globalParamFindParSpread.defaultFrequency = defaultFrequency;
		globalParamFindParSpread.accruedPremium = accruedPremium;
		globalParamFindParSpread.recoveryRate = RR;
		globalParamFindParSpread.hazardRate = 0;
		
		spreads[i] = zero(lower,upper,matchep,t,objfunFindParSpread);
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
		/*double objfun(double h) {
		double result = calculatePremiumLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,cdsTenors[i],premiumFrequency,accruedPremium, spreads[i],h) - calculateDefaultLeg(newcreditcurve,newcreditcurveLength,yieldcurve,yieldcurveLength,cdsTenors[i],defaultFrequency,RR,h);
			//Rprintf("objfun (h=%f) ==> %f\n",h,result);
			return(result);
		};*/
		
		globalParamFindHazardRate.creditcurve = newcreditcurve;
		globalParamFindHazardRate.creditcurveLength = newcreditcurveLength;
		globalParamFindHazardRate.yieldcurve = yieldcurve;
		globalParamFindHazardRate.yieldcurveLength = yieldcurveLength;
		globalParamFindHazardRate.cdsTenor = cdsTenors[i];
		globalParamFindHazardRate.premiumFrequency = premiumFrequency;
		globalParamFindHazardRate.defaultFrequency = defaultFrequency;
		globalParamFindHazardRate.accruedPremium = accruedPremium;
		globalParamFindHazardRate.recoveryRate = RR;
		globalParamFindHazardRate.spread = spreads[i];
		
		double h = zero(lower,upper,matchep,t,objfunFindHazardRate);
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

