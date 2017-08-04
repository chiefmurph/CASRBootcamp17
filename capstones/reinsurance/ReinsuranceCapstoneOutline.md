# RBootcamp 2017 -- Reinsurance Capstone

## Outline

* Familiarize yourself with the data provided
    * A data frame of policy data ('policytbl')
    * Another data frame of associated claim level data at annual evaluations ('claimtbl')
    * Other
    * Anything missing will be discussed and created or provided
    
* Divide data into 3 sets/tranches 
    * Partition policies into 3 sets:
        1. test
        2. train
        3. validate
        
    * Divide the claim data into three tranches based on the tranche of their policy

    * (Optional) If any tranche has < 25,000 policies, bootstrap a sample of at least that size

* Using the test tranche {policytbl, claimtbl, ...}
    * Experiment with writing a program to decide a fair price for
        * An excess of loss (XOL) cover for an arbitrary retention and limit
        * An aggregate stop loss (ASL) cover for an arbitrary retention and limit
    
    * Reproduce Gary Venter's paper that compares two such covers based on Net Benefit of Reinsurance (NBOR) 
        * See Gary Venter's paper 
        *Measuring Value in Reinsurance* 
        at this link
            [https://www.casact.org/pubs/forum/01sforum/01sf179.pdf](https://www.casact.org/pubs/forum/01sforum/01sf179.pdf) We will cover the first part only.
        * Through Gary's generosity, a rmarkdown version of his paper will be provided. 
        Your task will be to replace all of Gary's output with 
        values and illustrations based on the test tranch (later the test and validate tranches)
        * Which cover do you think cedant would choose?
    
    * (Optional) Investigate the sensitivity of the results to 
    changes in assumptions (frequency, severity, ...)
    * Once you're happy with your selected covers and pricing approach, 
    move to the next tranche.
    
* Using the train tranche {policytbl, claimtbl, ...}
    * Set your prices for the covers selected ("parameterize your model")
    * Compare the covers using NBOR
        * Did *E(NBOR)* and *sd(NBOR)* change significantly?
        * Did NBOR by policy year change significantly?
        * What do those observations tell you about the cedant?
    * Re-knit Gary's paper.
    Does the knitted paper from the new data still read well?

* Using the validate tranche {policytbl, claimtbl, ...}
    * Apply your covers and prices to the data
    * Recompare *E(NBOR)*, *sd(NBOR)*, and NBOR by PY
    * Re-knit Gary's paper
    * Summarize differences
    
* Takeaways
    * Does dividing your data using three tranches add anything
    to an actuarial reinsurance pricing exercise?
    * Other takeaways



