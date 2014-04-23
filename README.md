##Info about data and varibles gathered from the UCI website and work I've done to clean up, reshape and summarized
###Version 1.0
======================================
##The data
The data contains the experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years old. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS,
SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded 
accelerometer and gyroscope, the team captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The obtained dataset has been randomly partitioned into two sets, 
where 70% of the volunteers was selected for generating the training data and 30% the test data.  The 
training data and test data were both split into three seperate file:subject_.txt(responding subject information),X_.txt(all the data) and Y_.txt(responding activity).   

##For each record it is provided:
======================================

* Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
* Triaxial Angular velocity from the gyroscope. 
* A 561-feature vector with time and frequency domain variables. 
* Its activity label. 
* An identifier of the subject who carried out the experiment.

##Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw 
signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were 
captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd 
order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, 
the acceleration signal was then separated into body and gravity acceleration signals 
(tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner 
frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain 
Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these 
three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag,
 tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing 
fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag.
 (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

follwing are the shortname:t-time;body-body;acc-acceleration;mag-magnitide;XYZ-3 axis;

* tBodyAcc-XYZ  (time body acceleration- X Y Z-axis)
* tGravityAcc-XYZ (time gravity acceleration- XYZ-axis)
* tBodyAccJerk-XYZ (time body accelreration jerk -XYZ-axis)
* tBodyGyro-XYZ     (time body gyroscope XYZ-axis)
* tBodyGyroJerk-XYZ (... ...)
* tBodyAccMag       
* tGravityAccMag    
* tBodyAccJerkMag   
* tBodyGyroMag      
* tBodyGyroJerkMag  
* fBodyAcc-XYZ      
* fBodyAccJerk-XYZ  
* fBodyGyro-XYZ     
* fBodyAccMag       
* fBodyAccJerkMag   
* fBodyGyroMag      
* fBodyGyroJerkMag  

The set of variables that were estimated from these signals are: 

* mean(): Mean value
* std(): Standard deviation
* mad(): Median absolute deviation 
* max(): Largest value in array
* min(): Smallest value in array
* sma(): Signal magnitude area
* energy(): Energy measure. Sum of the squares divided by the number of values. 
* iqr(): Interquartile range 
* entropy(): Signal entropy
* arCoeff(): Autorregresion coefficients with Burg order equal to 4
* correlation(): correlation coefficient between two signals
* maxInds(): index of the frequency component with largest magnitude
* meanFreq(): Weighted average of the frequency components to obtain a mean frequency
* skewness(): skewness of the frequency domain signal 
* kurtosis(): kurtosis of the frequency domain signal 
* bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
* angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on
the angle() variable:

* gravityMean
* tBodyAccMean
* tBodyAccJerkMean
* tBodyGyroMean
* tBodyGyroJerkMean

##Work outline to clean up the data. run_anaysis.R file contains more details about steps and code 
* read all the data to R using read.table()
* match and combine the activity code file and activity file
* combine three seperate traning and test files into one horizontally using cbind()
* merge the training and test data vertically using rbind()
* select the data of interest using subseting skills, and idetify position using "find" function of text
* reshape the data and further summarize it,"reshape2" package melt and dcast function used