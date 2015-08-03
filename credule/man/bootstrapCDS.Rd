\name{bootstrapCDS}
\alias{bootstrapCDS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bootstrap a Credit Curve}
\description{
    A function that bootstrap a credit curve from a set of Credit Default Swap spreads givent for various maturity.
}
\details{
In brief, a CDS is used to transfer the credit risk of a reference entity (corporate or sovereign) from one party to another. In a standard CDS contract one party purchases credit protection from another party, to cover the loss of the face value of an asset following a credit event. A credit event is a
legally defined event that typically includes bankruptcy, failure-to-pay and restructuring. This protection lasts until some specified maturity date. To pay for this protection, the protection buyer makes a regular stream of payments, known as the premium leg, to the protection seller. This size of these premium payments is calculated from a quoted default swap spread which is paid on the face value of the protection. These payments are made until a credit event occurs or until maturity, whichever occurs first.

\bold{Modeling Credit Using a Reduced-Form Approach}
The world of credit modelling is divided into two main approaches, one called the structural and the other called the reduced-form. In the structural approach, the idea is to characterize the default as being the consequence of some company event such as its asset value being insufficient to cover a repayment of debt.

Structural models are generally used to say at what spread corporate bonds should trade based on the internal structure of the firm. They therefore require information about the balance sheet of the firm and can be used to establish a link between pricing in the equity and debt markets. However, they are limited 
in at least three important ways: they are hard to calibrate because internal company data is only published at most four times a year; they generally lack the flexibility to fit exactly a given term structure of spreads; and they cannot be easily extended to price credit derivatives.

In the reduced-form approach, the credit event process is modeled directly by modeling the probability of the credit event itself. Using a security pricing model based on this approach, this probability of default can be extracted from market prices. Reduced form models also generally have the flexibility to refit the prices of a variety of credit instruments of different maturities. They can also be extended to price more exotic credit derivatives. It is for these reasons that they are used for credit derivative pricing.

See \code{vignette("credule")} for more details.

}
\usage{
bootstrapCDS(yieldcurveTenor,
			 yieldcurveRate,
			 cdsTenors,
			 cdsSpreads,
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
    A double vector. Each value represents the discount rate (continuously compounded) for a partical tenor (e.g. 0.005 means 0.5\%, 0.02 means 2%)
	}
\item{cdsTenors}{
    A double vector. Each value represents the maturity expressed in year of a Credit Default Swap which we want to price (e.g 5.0 means 5Y)
	}
\item{cdsSpreads}{
    A double vector. Each value represents the CDS spread (expressed in decimal, e.g. 0.0050 represent 0.5\% or 50 bp) for a given maturity
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
    Returns a Dataframe with 3 columns: tenor, survprob and hazrate. The tenor column contains the tenor value given in parameter cdsTenors, the survprob column gives the survival probability (in decimal) for each tenor (e.g. 0.98 menas 98\%) and the hazrate column gives the non-cumulative hazard rate (intensity of the poisson process) for each tenor (e.g. 0.01 means 1\% hazard rate).
}
\author{
    Bertrand Le Nezet
}

\examples{
library(credule)

yieldcurveTenor = c(1,2,3,4,5,7)
yieldcurveRate = c(0.0050,0.0070,0.0080,0.0100, 0.0120,0.0150)
cdsTenors = c(1,3,5,7)
cdsSpreads = c(0.0050,0.0070,0.0090,0.0110)
premiumFrequency = 4
defaultFrequency = 12
accruedPremium = TRUE
RR = 0.40

bootstrapCDS(yieldcurveTenor,
             yieldcurveRate,
             cdsTenors,
             cdsSpreads,
             RR,
             premiumFrequency,
             defaultFrequency,
             accruedPremium)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Credit Default Swap, Credit Default Swaps, CDS, spread, survival probability, survival probabilities, default probability, default probabilities, pricing, credit curve, bootstrapping, hazard rate, poisson process}
