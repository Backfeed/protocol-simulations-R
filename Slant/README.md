R - simulations of the Backfeed protocol
--------------------------------------------

The R scripts in this repository allow for defining a scenario of consequential evaluations of contributions, executing the scenario and playing with different functions within the main "evaluate" function. At the end or during a simulation run plots can be made.

sim.R - contains the simulation run.
evaluate.R - contains the main evaluate function which updates the reputation of users as well as the contributions' data frame.
scenario.R - defines all the relevant parameters as well as functional forms for the stake and the dividend.

The Slant version of the protocol includes the following points:
1) reputation changes only based on upvotes.
2) stake payment decreases with previously upvoted ratio and with the time from the opening of the bid.


