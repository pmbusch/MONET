# Scripts

The key engine of MONET are the trade ratios (matrix), which relates sales of vehicles in a country with production in other countries.

Using trade ratios and a sales vector, MONET allocates production: Production = f (Trade Ratios, Sales Vector)

MONET also contains a dimension of market segments, associated with vehicle size, which allows to model the heterogeneity in sales and production by vehicle, and the different battery capacity requirements.

The main code to run the baseline scenario is **ProdAllocation_Baseline**. Other scripts are modifications of this code, with different inputs used in MONET.

## Folders

* **00-Functions**: Common functions used in the analysis.
* **Trade Ratios Creation**: Scripts to estimate baseline LDV trade ratios from historical trade flows, production and sales by country. Contains all the matrix balancing process, with the validation analysis.
* **Trade Scenarios Example**: Code to create new trade ratios, using modeller assumptions on deviations from the historical LDV trade ratios. 


## Scripts

* **00-Libraries.R**: Common libraries used in the analysis.
* **ProdAllocation_Baseline.R**: **Baseline code to run an EV product allocation based on inputs: trade ratios and sales vector**. It produces a production vector by country. 
* **ProdAllocation_BatteryScen.R**: Example of MONET scenario modelling by modifying **battery capacity** (input). 
* **ProdAllocation_DemandScen.R**: Example of MONET scenario modelling by modifying **sales forecast** (input).
* **ProdAllocation_SegmentScen.R**: Example of MONET scenario modelling by modifying **market share sales** (input).
* **ProdAllocation_TradeRatiosScen.R**: Example of MONET scenario modelling by modifying **trade ratios** (input).
* **BatRequirements_Demand.R**: Calculate battery requirements based only on the demand, only for comparison.

## Figures

* **Figure_Battery.R**: Figure of battery size per region and market segment.
* **Figure_ProdAllocation_Scenarios.R**: Figure that compares production at country level based on different trade scenarios.
* **Figure_Sales.R**: Figure for forecasted sales in 2035.
* **Figure_Share_Segments.R**: Figure for share of sales and production by country and market segments.
