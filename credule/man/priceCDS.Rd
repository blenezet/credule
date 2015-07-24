\name{priceCDS}
\alias{priceCDS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Credit Default Swap Pricing}
\description{
    A function that calculate the spreads of a list of Credit Default Swap from a yield curve and a credit curve.
}
\usage{
priceCDS(yieldcurveTenor,
		 yieldcurveRate,
		 creditcurveTenor,
		 creditcurveSP,
		 cdsTenors,
		 recoveryRate,
		 numberPremiumPerYear = c(4,2,1,12),
		 numberDefaultIntervalPerYear = 12,
		 accruedPremium = c(TRUE,FALSE)) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{yieldcurveTenor}{
    A double vector. Each value represents a tenor of the yield curve expressed in year (e.g. 1.0 for 1Y, 0.5 for 6M)
	}
\item{yieldcurveRate}{
    A double vector. Each value represents the discount rate (continuously compounded) for a partical tenor (e.g. 0.005 means 0.5\%, 0.02 means 2\%)
	}
\item{creditcurveTenor}{
    A double vector. Each value represents a tenor of the credit curve expressed in year (e.g. 1.0 for 1Y, 0.5 for 6M)
	}
\item{creditcurveSP}{
    A double vector. Each value represents the survival probability for a partical tenor (e.g. 0.98 means 98\%)
	}
\item{cdsTenors}{
    A double vector. Each value represents the maturity expressed in year of a Credit Default Swap which we want to price (e.g 5.0 means 5Y)
	}
\item{recoveryRate}{
    A double. It represents the Recovery Rate in case of default (e.g 0.40 means 40\% recovery which is a standard value for Senior Unsecured debt)
	}
\item{numberPremiumPerYear}{
    An Integer. It represents the number of premiums paid per year. CDS premiums paid quaterly (i.e. numberPremiumPerYear=4) and sometimes semi-annually (i.e. numberPremiumPerYear=2)
	}	
\item{numberDefaultIntervalPerYear}{
    An Integer. It represents the number of timesteps used to perform the numerical integral required while computing the default leg value. It is shown that a monthly discretisation usually gives a good approximation (Ref. Valuation of Credit Default Swaps, Dominic O Kane and Stuart Turnbull)
	}	
\item{accruedPremium}{
    A boolean. If set to TRUE, the accrued premium will be taken into account in the calculation of the premium leg value.
	}	
}
\value{
    Returns a Dataframe with 2 columns: tenor and spread. The tenor column contains the tenor value given in parameter cdsTenors, the spread column give the Credit Default Swap spreads (in decimal) for each tenor (e.g. 0.0050 is equivalent to 0.5\% or 50 bp).
}
\author{
    Bertrand Le Nezet
}

\examples{
yieldcurveTenor = c(1,2,3,4,5,7)
yieldcurveRate = c(0.0050,0.0070,0.0080,0.0100, 0.0120,0.0150)
creditcurveTenor = c(1,3,5,7)
creditcurveSP = c(0.99,0.98,0.95,0.92)
cdsTenors = c(1,3,5,7)
cdsSpreads = c(0.0050,0.0070,0.00100,0.0120)
premiumFrequency = 4
defaultFrequency = 12
accruedPremium = TRUE
RR = 0.40

priceCDS(yieldcurveTenor,
            yieldcurveRate,
            creditcurveTenor,
            creditcurveSP,
            cdsTenors,
            RR,
            premiumFrequency,
            defaultFrequency,
            accruedPremium
          )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{CDS, Credit Default Swap, pricing}
