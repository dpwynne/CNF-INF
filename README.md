# CNF-INF
Shiny app to work with cumulative proportion/probability problems using the normal distribution. Supports values out to 4 standard deviations away from the mean.
Designed as a teaching aid for CSU Fullerton Math 338 and similar upper-division statistics classes.

## Known Bugs
Sometimes the cumulative proportion (left tab) outputs will refresh twice. It shouldn't be a problem, though.

## Left Tab (Values to Proportions)
Specify a range of values on the original scale, and whether you want to compute the proportion above X (upper tail), below Y (lower tail), or between X and Y (middle).
The output shows the four steps of performing this type of problem:

Step 1: Sketch the normal distribution and shade the area of interest.

Step 2: Compute the z-score(s) at the boundary of the area of interest.

Step 3: Find the cumulative proportions corresponding to the z-score(s).

Step 4: Answer the question in context.

## Right Tab (Percentiles to Values)
Specify a range of percentiles. For lower tail problems, keep the left side of the slider at 0. For upper tail problems, keep the right side of the slider at 100.
The output shows the four steps of performing this type of problem:

Step 1: Sketch the standard normal distribution and shade the area of interest.

Step 2: Compute the cumulative proportions at the boundary of the area of interest and use them to find the corresponding z-scores.

Step 3: Convert the z-scores back to the original scale.

Step 4: Answer the question in context.
