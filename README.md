# MF-BVAR
This project aims to implement a Mixed-Frequency Bayesian VAR (MF-BVAR) model to the problem of predicting the Singapore Nominal Effective Exchange Rate (SNEER).

Unlike any other country, the Monetary Authority of Singapore runs a exchange-rate centered monetary police, in which the SNEER is the intermediate target of monetary police. The MAS makes regular interventions on the FX market to ensure that the SNEER stays within a band only known to the MAS. This inteventions guarantees mediun-term price stability on the SNEER. The SNEER is a weekly variable which is known to be a  trade-weighted  basket  of  currencies of Singapore major trade partners. On the other hand, the exchange rates of the countries within the SNEER have higher frequancy data, which can be used for prediction.

There are four sources of uncertainty into the MAS exchange rate police:

> (1) Daily vaule of the SNEER

> (2) Slope of the SNNER

> (3) Bands of the SNEER

This project seeks to adress this issues in the following steps:

> (1) Build a MF-BVAR to connect the daily value of the FX data of the major trade partners of Singapore to make daily predictions of SNEER as long as the best weekly predictions of the same index

> (2) Address the problem of estimating the Bands of the SNEER


# References

