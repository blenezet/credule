\name{credule-package}
\alias{credule-package}
\docType{package}
\title{
Credit Default Swap pricing and Credit Curve bootstrapping
}
\description{
Credit Curve boostrapping from market quotes (Credit Default Swap - CDS - spreads) and Credit Default Swap - CDS - pricing
}
\details{
\tabular{ll}{
Package: \tab credule\cr
Type: \tab Package\cr
Version: \tab 0.1.3\cr
Date: \tab 2015-08-03\cr
License: \tab MIT + file LICENSE\cr
}

A Credit Default Swap (CDS) is a financial swap agreement that the seller of the CDS will compensate the buyer (usually the creditor of the reference loan) in the event of a loan default (by the debtor) or other credit event. This is to say that the seller of the CDS insures the buyer against some reference loan defaulting. The buyer of the CDS makes a series of payments (the CDS "fee" or "spread") to the seller and, in exchange, receives a payoff if the loan defaults. It was invented by Blythe Masters from JP Morgan in 1994.

April 8 2009 saw a "Big Bang" in the market for credit default swap (CDS) contracts and the way in which they are traded. Both contract and convention
changes were implemented simultaneously. These changes were designed to
make CDS more standardised to help support efforts for central clearing of
CDS trades, make strides towards T+0 trade processing and facilitate
operational efficiency.

The CDS "Big Bang" has introduced standardized coupon dates and maturity dates. The functions provided in this package do not use these "new" conventions (i.e. they use the conventions pre-April 2008); therefore it's not recommended to use them in a production context. In most cases the differences are marginal, therefore these functions are good enough for preliminary research. 

See \code{vignette("credule")} for more details.

Note. I'm planning to update this package with proper CDS ISDA conventions handling at a later stage.
}
\author{
Bertrand Le Nezet
Maintainer: Bertrand Le Nezet <bertrand dot lenezet at gmail dot com>
}
\keyword{ Credit Default Swap, Credit Default Swaps, CDS, spread, survival probability, survival probabilities, default probability, default probabilities, pricing, credit curve, bootstrapping, hazard rate, poisson process}
\examples{
library(credule)

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

# CDS pricing
res_price = priceCDS(yieldcurveTenor,
            yieldcurveRate,
            creditcurveTenor,
            creditcurveSP,
            cdsTenors,
            RR,
            premiumFrequency,
            defaultFrequency,
            accruedPremium
          )
res_price

# Credit curve bootstrapping from CDS spreads
res_bootstrap = bootstrapCDS(yieldcurveTenor,
             yieldcurveRate,
             res_price$tenor,
             res_price$spread,
             RR,
             premiumFrequency,
             defaultFrequency,
             accruedPremium)
res_bootstrap

}
\references{
  ISDA Standard CDS Example \url{http://www.cdsmodel.com/cdsmodel/assets/cds-model/docs/Standard\%20CDS\%20Examples.pdf}
  
  CDS Bing Bang \url{http://www.markit.com/assets/en/docs/markit-magazine/issue-4/60-cds-big-bang.pdf}
}
