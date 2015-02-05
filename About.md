# Simulation of Disease Transmission in a Population


This app simulates the selected number of personas and uses Transmission-Probability matrix for the spread of disease among these simulated personas. Every time a new value is selected for the "Number of Simulated Personas" from the slider on the left, a new simulation is generated. When new simulations are generated new figures and tables are generated and the app figures/table are updated automatically. 

The app contains several tabs to browse between the several outputs. To change to a new tab just click on the name of the tab near the top of the page. 

The app has four tabs:

* __About__ tab, this tab. 
* __Distributions of Personas and Exposure Levels__ tab: this tab shows the following distributions:
  + Distribution of simulated personas for the recipients group
  + Distribution of simulated personas for the exposers group
  + Distribution of simulated exposure levels
  + Distribution of simulated personas for the recipients group after their interaction with the exposers group 
* __Post-Exposure Status of Recipients__

This tab shows a the distribution of the recipients group after their interaction with the exposers group but with much more details than the one shown in the first tab. The numbers on the right side of the figure (1 through 6) represent the health status of the recipient group prior to their exposure to the exposers group. The number on top side of the figure (1 through 6) represent the health status of the exposers group. The bar plots shown are color coded by the level of exposure between the recipient and exposers groups. For example if we take the first row and first column --recipients with level 1 health status interacting with exposers with level 1 health status-- we see that none of these personas move to level 2 on the health status scale. On the other hand if we take the last column in the first row --recipients with level 1 health status interacting with exposers with level 6 health status-- we see that a big chunk of those recipients advanced to level 2 on the health status scale, i.e. got sicker. Another example, if we take the third column in the fourth row --recipients with level 4 health status interacting with exposers with level 3 health status-- again, we see that none of these personas move to level 5 on the health status scale. From the figure this is always the case for any recipient interacting with an exposer with a lower level on the health status level (All lower off-diagonal panels). 

* __Simulated Data (Table)__: A data table showing the simulated personas of recipients and exposers, exposure levels, and the recipiens health status after exposure. This table is searchable and sortable by any of its columns.

* __Transmission Probability matrix__: Shows the disease transmission probability matrix used in the simulations