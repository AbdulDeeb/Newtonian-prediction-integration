# Newtonian-prediction-integration
Data and stimuli form  Deeb, Cesanek, &amp; Domini's "Newtonian predictions are integrated with sensory information in 3D motion perception


Steps to reproduce

-Data Analysis 
The raw data can be opened in R using the load function. Each data file (Exp1-5). Data from Exp1 and  2 are used to  make  the model fit in the "Data_analysis.R" Code. The code is well commented and can potentially  be recreated in programs like MATLAB or Python. 
To fit the model, you may need to install/load the third-party R library 'nloptr'. All other functions should be included in base R (version 3.3.1, 'Bug in Your Hair'). To reproduce the analyses and figures, just load the R workspace data files and run the code. 

CSV files have also been included for interested  readers who  are unfamiliar with R.

-Experimental Stimuli

Code used to generate the experiments were written in C++ and the OpenGL library. The coordinate system has three dimensions, with the Z coordinates away from the observer in the negatives (i.e the target ball stimuli were initially 400 mm away from the observer and are coded as -400). Participants viewed stereoscopic renderings of 3D objects by looking into a half-silvered mirror arranged at a 45° angle to a 19” CRT monitor directly to the left of the mirror. The mirror reflected the image displayed on the monitor such that the rendered objects appeared to be floating in space beyond the mirror. Therefore, the code generates a mirror image of what subjects actually witnessed. This is especially important when considering how the response controls were coded. 


