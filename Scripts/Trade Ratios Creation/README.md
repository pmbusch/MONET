# Trade Ratios Creation

Code to generate the key input for MONET vehicle production allocation: **Trade Ratios**.

The trade ratios are estimated from the balanced vehicle trade flows matrix, which is estimated from historical LDV trade flows, production and sales by country.

The method requires balancing as the data sources have conflicting information.

* **01-Trade_Balance.R**: Balancing of the original 2D matrix of trade vehicle flows.
* **02a-ProportionalAllocation.R**: Method to balance the 3D matrix (with market segments) using **Proportional Allocation**.
* **02b-BucketsAlg.R**: Method to balance the 3D matrix (with market segments) using **Buckets Algorithm**.
* **03-Trade_Balance_Multidimensional.R**: 3D Matrix balancing using multidimensional RAS method.
* **04-CrossLDVRatios.R**: Validation of allocation method. It compares historical production with predicted production using trade ratios from different years.
