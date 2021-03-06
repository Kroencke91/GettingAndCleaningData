# Code Book - Getting and Cleaning Data Course Project

Feature to Column Name Mapping
=================

index	|	feature					|	colname

1	|	tBodyAcc-mean()-X			|	tbodyaccmeanx

2	|	tBodyAcc-mean()-Y			|	tbodyaccmeany

3	|	tBodyAcc-mean()-Z			|	tbodyaccmeanz

4	|	tBodyAcc-std()-X			|	tbodyaccstdx

5	|	tBodyAcc-std()-Y			|	tbodyaccstdy

6	|	tBodyAcc-std()-Z			|	tbodyaccstdz

41	|	tGravityAcc-mean()-X			|	tgravityaccmeanx

42	|	tGravityAcc-mean()-Y			|	tgravityaccmeany

43	|	tGravityAcc-mean()-Z			|	tgravityaccmeanz

44	|	tGravityAcc-std()-X			|	tgravityaccstdx

45	|	tGravityAcc-std()-Y			|	tgravityaccstdy

46	|	tGravityAcc-std()-Z			|	tgravityaccstdz

81	|	tBodyAccJerk-mean()-X			|	tbodyaccjerkmeanx

82	|	tBodyAccJerk-mean()-Y			|	tbodyaccjerkmeany

83	|	tBodyAccJerk-mean()-Z			|	tbodyaccjerkmeanz

84	|	tBodyAccJerk-std()-X			|	tbodyaccjerkstdx

85	|	tBodyAccJerk-std()-Y			|	tbodyaccjerkstdy

86	|	tBodyAccJerk-std()-Z			|	tbodyaccjerkstdz

121	|	tBodyGyro-mean()-X			|	tbodygyromeanx

122	|	tBodyGyro-mean()-Y			|	tbodygyromeany

123	|	tBodyGyro-mean()-Z			|	tbodygyromeanz

124	|	tBodyGyro-std()-X			|	tbodygyrostdx

125	|	tBodyGyro-std()-Y			|	tbodygyrostdy

126	|	tBodyGyro-std()-Z			|	tbodygyrostdz

161	|	tBodyGyroJerk-mean()-X			|	tbodygyrojerkmeanx

162	|	tBodyGyroJerk-mean()-Y			|	tbodygyrojerkmeany

163	|	tBodyGyroJerk-mean()-Z			|	tbodygyrojerkmeanz

164	|	tBodyGyroJerk-std()-X			|	tbodygyrojerkstdx

165	|	tBodyGyroJerk-std()-Y			|	tbodygyrojerkstdy

166	|	tBodyGyroJerk-std()-Z			|	tbodygyrojerkstdz

201	|	tBodyAccMag-mean()			|	tbodyaccmagmean

202	|	tBodyAccMag-std()			|	tbodyaccmagstd

214	|	tGravityAccMag-mean()			|	tgravityaccmagmean

215	|	tGravityAccMag-std()			|	tgravityaccmagstd

227	|	tBodyAccJerkMag-mean()			|	tbodyaccjerkmagmean

228	|	tBodyAccJerkMag-std()			|	tbodyaccjerkmagstd

240	|	tBodyGyroMag-mean()			|	tbodygyromagmean

241	|	tBodyGyroMag-std()			|	tbodygyromagstd

253	|	tBodyGyroJerkMag-mean()			|	tbodygyrojerkmagmean

254	|	tBodyGyroJerkMag-std()			|	tbodygyrojerkmagstd

266	|	fBodyAcc-mean()-X			|	fbodyaccmeanx

267	|	fBodyAcc-mean()-Y			|	fbodyaccmeany

268	|	fBodyAcc-mean()-Z			|	fbodyaccmeanz

269	|	fBodyAcc-std()-X			|	fbodyaccstdx

270	|	fBodyAcc-std()-Y			|	fbodyaccstdy

271	|	fBodyAcc-std()-Z			|	fbodyaccstdz

294	|	fBodyAcc-meanFreq()-X			|	fbodyaccmeanfreqx

295	|	fBodyAcc-meanFreq()-Y			|	fbodyaccmeanfreqy

296	|	fBodyAcc-meanFreq()-Z			|	fbodyaccmeanfreqz

345	|	fBodyAccJerk-mean()-X			|	fbodyaccjerkmeanx

346	|	fBodyAccJerk-mean()-Y			|	fbodyaccjerkmeany

347	|	fBodyAccJerk-mean()-Z			|	fbodyaccjerkmeanz

348	|	fBodyAccJerk-std()-X			|	fbodyaccjerkstdx

349	|	fBodyAccJerk-std()-Y			|	fbodyaccjerkstdy

350	|	fBodyAccJerk-std()-Z			|	fbodyaccjerkstdz

373	|	fBodyAccJerk-meanFreq()-X		|	fbodyaccjerkmeanfreqx

374	|	fBodyAccJerk-meanFreq()-Y		|	fbodyaccjerkmeanfreqy

375	|	fBodyAccJerk-meanFreq()-Z		|	fbodyaccjerkmeanfreqz

424	|	fBodyGyro-mean()-X			|	fbodygyromeanx

425	|	fBodyGyro-mean()-Y			|	fbodygyromeany

426	|	fBodyGyro-mean()-Z			|	fbodygyromeanz

427	|	fBodyGyro-std()-X			|	fbodygyrostdx

428	|	fBodyGyro-std()-Y			|	fbodygyrostdy

429	|	fBodyGyro-std()-Z			|	fbodygyrostdz

452	|	fBodyGyro-meanFreq()-X			|	fbodygyromeanfreqx

453	|	fBodyGyro-meanFreq()-Y			|	fbodygyromeanfreqy

454	|	fBodyGyro-meanFreq()-Z			|	fbodygyromeanfreqz

503	|	fBodyAccMag-mean()			|	fbodyaccmagmean

504	|	fBodyAccMag-std()			|	fbodyaccmagstd

513	|	fBodyAccMag-meanFreq()			|	fbodyaccmagmeanfreq

516	|	fBodyBodyAccJerkMag-mean()		|	fbodybodyaccjerkmagmean

517	|	fBodyBodyAccJerkMag-std()		|	fbodybodyaccjerkmagstd

526	|	fBodyBodyAccJerkMag-meanFreq()		|	fbodybodyaccjerkmagmeanfreq

529	|	fBodyBodyGyroMag-mean()			|	fbodybodygyromagmean

530	|	fBodyBodyGyroMag-std()			|	fbodybodygyromagstd

539	|	fBodyBodyGyroMag-meanFreq()		|	fbodybodygyromagmeanfreq

542	|	fBodyBodyGyroJerkMag-mean()		|	fbodybodygyrojerkmagmean

543	|	fBodyBodyGyroJerkMag-std()		|	fbodybodygyrojerkmagstd

552	|	fBodyBodyGyroJerkMag-meanFreq()		|	fbodybodygyrojerkmagmeanfreq

555	|	angle(tBodyAccMean,gravity)		|	angletbodyaccmeangravity

556	|	angle(tBodyAccJerkMean),gravityMean)	|	angletbodyaccjerkmeangravitymean

557	|	angle(tBodyGyroMean,gravityMean)	|	angletbodygyromeangravitymean

558	|	angle(tBodyGyroJerkMean,gravityMean)	|	angletbodygyrojerkmeangravitymean

559	|	angle(X,gravityMean)			|	anglexgravitymean

560	|	angle(Y,gravityMean)			|	angleygravitymean

561	|	angle(Z,gravityMean)			|	anglezgravitymean



Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean
