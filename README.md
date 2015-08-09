# R-qualityExperiments
## R code to track, compare and graph relationship (contact) frequency and compare quality of relationships

I investigated the quality of one particulalr relatiosnhip, using contact frequency as a metric, and the other relationship as control.

The particular question for which I am seeking answer is: are the two relationships qualitatively different? Is one relationship inherently stronger and more resiliant than the other?

To answer these question, I designed an experiment, in which I used the contact frequency embedded in my relationship with one friend as control, and contrast it with the contact frequency embedded in the other relationship. In this experiment, I keep both relationships on an even keel, in that both parties are suposed to take turns contacting each other. For exmaple, if the friend contacts me this time, then next time, it will my turn to initiate the contact. After I initiate the contact, I keep track of the number of days lapsed, if any, until the friend reciprocates. Therefore, if the friend reciprocates in one day, then the number of days prior to a contact is 1. If a friend reciproctes in five days, then the number of days prioir to a contact is 5, etc. Conceptually, the number of days prior to a day can be a unbounded number. 

There is an interesting feature in this experiment: If the exact value were known for both relationships, then the sample average time (in days) PRIOR TO receiving a contact would be a siimple estimator for the average number of days PRIOR TO receiving a contact. But what should be done if we have incomplete data (such as the last number of days). This is an example of censored data where there is information, but it is not obvious how to use it. Just using the value of any arbitrary number will lead to an underestimate of the average time to receive a contact and an overestimate of the probability of receiving a contact in a day. You could substitute in a value for the last number of days before computing an average, but what number should you use? 11, 12, 13, 15, 27?

This problem is amenable to maximum likelihood estiamtion. The number of days prior to receiving a contact is a random variable, which forms a geometric distribution. Using the results obtained from this experiment (on-going), I then calculated the maximum likelihood estimator of the probabiltiy of receiving a contact in a given day for both relationships. 

The R code in this repository goes through the algebra of calculating the log likelihood. After this is done, it graphs the probability value for each censored day (namely, for each day of receiving no contact) from both relationships. 

The goal of this experiment is then to compare the profile confidence interval resulted from both relationship at the 95% confidence level. If the two confidence intervals computed using the same method forr the two relationships overlap significantly, then we can conclude that there is no evidence that the two relationships differ in quality, as measured in the contact frequency. If, however, the two confidence intervals do not overlap, then we have strong evidence that the two relationships differ.


