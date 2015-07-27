struct quadratic_params
{
	double a, b, c;
};

struct paramFindParSpread
{
	double *creditcurve;
	int creditcurveLength;
	double *yieldcurve;
	int yieldcurveLength;
	double cdsTenor;
	double premiumFrequency;
	double defaultFrequency;
	int accruedPremium;
	double recoveryRate;
	double hazardRate;	
};

struct paramFindHazardRate
{
	double *creditcurve;
	int creditcurveLength;
	double *yieldcurve;
	int yieldcurveLength;
	double cdsTenor;
	double premiumFrequency;
	double defaultFrequency;
	int accruedPremium;
	double recoveryRate;
	double spread;	
};
