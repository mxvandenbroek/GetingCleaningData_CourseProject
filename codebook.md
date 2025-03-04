Introduction
------------

The purpose of this project is to demonstrate the ability to collect,
work with, and clean a data set. The goal is to prepare tidy data that
can be used for later analysis. As a result of this analysis, input data
files are transformed to two new tidy datasets. This Codebook describes:

-   the input data
-   the process to transform the data (for a more detailed description,
    we refer to the analysis.md file)
-   the tidy data

The data that is analyzed comes from the are of wearable computing. This
is one of the most exciting areas in all of data science right now.
Companies like Fitbit, Nike, and Jawbone Up are racing to develop the
most advanced algorithms to attract new users. The data represent data
collected from the accelerometers from the Samsung Galaxy S smartphone.

Input Data description
----------------------

### Data Source

The data files that we will be using for this analysis can be found
here:

<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

A full description is available at the site where the data was obtained:

<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>

### Some background on the input data

Some background on the input data and how they were obtained and
preprocessed:

The experiments have been carried out with a group of 30 volunteers
within an age bracket of 19-48 years. Each person performed six
activities (WALKING, WALKING\_UPSTAIRS, WALKING\_DOWNSTAIRS, SITTING,
STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the
waist. Using its embedded accelerometer and gyroscope, we captured
3-axial linear acceleration and 3-axial angular velocity at a constant
rate of 50Hz. The experiments have been video-recorded to label the data
manually. The obtained dataset has been randomly partitioned into two
sets, where 70% of the volunteers was selected for generating the
training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by
applying noise filters and then sampled in fixed-width sliding windows
of 2.56 sec and 50% overlap (128 readings/window). From each window, a
vector of features was obtained by calculating variables from the time
and frequency domain. See 'features\_info.txt' for more details.

### More detail on the input data

The zip archive contains a number of files: data files and explanatory
files. I refer to the explanatory files for more detail. In particular
the readme.txt file and features\_info.txt contain detailed information.

I will now give a summary that is specific to this subsequent data
analysis. The following files are used:

Two files containing feature names and class labels respectively:

-   'features.txt': List of all features.
-   'activity\_labels.txt': Links the class labels with their activity
    name.

Six files containing data, three training data sets and three test data
sets.

-   'train/X\_train.txt': Training set.
-   'train/y\_train.txt': Training labels.
-   'train/subject\_train.txt': Each row identifies the subject who
    performed the activity for each window sample. Its range is from 1
    to 30.
-   'test/X\_test.txt': Test set.
-   'test/y\_test.txt': Test labels.
-   'test/subject\_train.txt': Each row identifies the subject who
    performed the activity for each window sample. Its range is from 1
    to 30.

For this analysis we will be using a subset of the features in x\_train
(and x\_test): namely only the features that correspond to a mean or
standard deviation of the original data measurements.

A few last notes:

-   Features are normalized and bounded within [-1,1].
-   Each feature vector is a row on the text file.

### Feature definitions

The following was taken from the file features\_info.txt and gives
detailed information on the individual features in the data:

The features selected for this database come from the accelerometer and
gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain
signals (prefix 't' to denote time) were captured at a constant rate of
50 Hz. Then they were filtered using a median filter and a 3rd order low
pass Butterworth filter with a corner frequency of 20 Hz to remove
noise. Similarly, the acceleration signal was then separated into body
and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ)
using another low pass Butterworth filter with a corner frequency of 0.3
Hz.

Subsequently, the body linear acceleration and angular velocity were
derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and
tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional
signals were calculated using the Euclidean norm (tBodyAccMag,
tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

Finally a Fast Fourier Transform (FFT) was applied to some of these
signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ,
fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to
indicate frequency domain signals).

These signals were used to estimate variables of the feature vector for
each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

-   tBodyAcc-XYZ
-   tGravityAcc-XYZ
-   tBodyAccJerk-XYZ
-   tBodyGyro-XYZ
-   tBodyGyroJerk-XYZ
-   tBodyAccMag
-   tGravityAccMag
-   tBodyAccJerkMag
-   tBodyGyroMag
-   tBodyGyroJerkMag
-   fBodyAcc-XYZ
-   fBodyAccJerk-XYZ
-   fBodyGyro-XYZ
-   fBodyAccMag
-   fBodyAccJerkMag
-   fBodyGyroMag
-   fBodyGyroJerkMag

The set of variables that were estimated from these signals are:

-   mean(): Mean value
-   std(): Standard deviation
-   mad(): Median absolute deviation
-   max(): Largest value in array
-   min(): Smallest value in array
-   sma(): Signal magnitude area
-   energy(): Energy measure. Sum of the squares divided by the number
    of values.
-   iqr(): Interquartile range
-   entropy(): Signal entropy
-   arCoeff(): Autorregresion coefficients with Burg order equal to 4
-   correlation(): correlation coefficient between two signals
-   maxInds(): index of the frequency component with largest magnitude
-   meanFreq(): Weighted average of the frequency components to obtain a
    mean frequency
-   skewness(): skewness of the frequency domain signal
-   kurtosis(): kurtosis of the frequency domain signal
-   bandsEnergy(): Energy of a frequency interval within the 64 bins of
    the FFT of each window.
-   angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window
sample. These are used on the angle() variable:

-   gravityMean
-   tBodyAccMean
-   tBodyAccJerkMean
-   tBodyGyroMean
-   tBodyGyroJerkMean

The complete list of variables of each feature vector is available in
'features.txt'

Description of the final two tidy datasets
------------------------------------------

We have created, based on the input data, two tidy datasets that we
describe here in more detail. To see the steps that we have taken to
create these datasets from the input data, please see the
run\_analysis.R script and the analysis.md file that shows the steps
taken in a markdown file with extensive comments.

The two tidy datasets are: - tidyTotalData.txt - tidyMeanData.txt

### Description of tidyTotalData

This dataset is created by:

-   joining the three x\_train, y\_train and subject\_train files;
    similar for the three corresponding test files
-   merging the test and training data
-   adding an activity description column to the data
-   selecting from these data a subset of the features that correspond
    to a mean or std (see below)

For completeness, we list the columns of the dataset below. For a
description of most of the variables, we refer to the original
features\_info.txt and readme.md files, since these features are
unchanged. These files contain all the relevant information. For newly
added columns, we describe below the interpretation:

-   ActivityId: 1,..6; see activity\_labels.txt for decoding.
-   SubjectId: id of the subject; ranges from 1 to 30
-   ActivityName: see activity\_labels.txt for possible values.
-   tBodyAcc.mean.X
-   tBodyAcc.mean.Y
-   tBodyAcc.mean.Z
-   tGravityAcc.mean.X
-   tGravityAcc.mean.Y
-   tGravityAcc.mean.Z
-   tBodyAccJerk.mean.X
-   tBodyAccJerk.mean.Y
-   tBodyAccJerk.mean.Z
-   tBodyGyro.mean.X
-   tBodyGyro.mean.Y
-   tBodyGyro.mean.Z
-   tBodyGyroJerk.mean.X
-   tBodyGyroJerk.mean.Y
-   tBodyGyroJerk.mean.Z
-   tBodyAccMag.mean
-   tGravityAccMag.mean
-   tBodyAccJerkMag.mean
-   tBodyGyroMag.mean
-   tBodyGyroJerkMag.mean
-   fBodyAcc.mean.X
-   fBodyAcc.mean.Y
-   fBodyAcc.mean.Z
-   fBodyAcc.meanFreq.X
-   fBodyAcc.meanFreq.Y
-   fBodyAcc.meanFreq.Z
-   fBodyAccJerk.mean.X
-   fBodyAccJerk.mean.Y
-   fBodyAccJerk.mean.Z
-   fBodyAccJerk.meanFreq.X
-   fBodyAccJerk.meanFreq.Y
-   fBodyAccJerk.meanFreq.Z
-   fBodyGyro.mean.X
-   fBodyGyro.mean.Y
-   fBodyGyro.mean.Z
-   fBodyGyro.meanFreq.X
-   fBodyGyro.meanFreq.Y
-   fBodyGyro.meanFreq.Z
-   fBodyAccMag.mean
-   fBodyAccMag.meanFreq
-   fBodyBodyAccJerkMag.mean
-   fBodyBodyAccJerkMag.meanFreq
-   fBodyBodyGyroMag.mean
-   fBodyBodyGyroMag.meanFreq
-   fBodyBodyGyroJerkMag.mean
-   fBodyBodyGyroJerkMag.meanFreq
-   tBodyAcc.std.X
-   tBodyAcc.std.Y
-   tBodyAcc.std.Z
-   tGravityAcc.std.X
-   tGravityAcc.std.Y
-   tGravityAcc.std.Z
-   tBodyAccJerk.std.X
-   tBodyAccJerk.std.Y
-   tBodyAccJerk.std.Z
-   tBodyGyro.std.X
-   tBodyGyro.std.Y
-   tBodyGyro.std.Z
-   tBodyGyroJerk.std.X
-   tBodyGyroJerk.std.Y
-   tBodyGyroJerk.std.Z
-   tBodyAccMag.std
-   tGravityAccMag.std
-   tBodyAccJerkMag.std
-   tBodyGyroMag.std
-   tBodyGyroJerkMag.std
-   fBodyAcc.std.X
-   fBodyAcc.std.Y
-   fBodyAcc.std.Z
-   fBodyAccJerk.std.X
-   fBodyAccJerk.std.Y
-   fBodyAccJerk.std.Z
-   fBodyGyro.std.X
-   fBodyGyro.std.Y
-   fBodyGyro.std.Z
-   fBodyAccMag.std
-   fBodyBodyAccJerkMag.std
-   fBodyBodyGyroMag.std
-   fBodyBodyGyroJerkMag.std

### Description of tidyMeanData

This dataset is created by taking the tidyTotalData as starting point
and calculating the mean for each feature, grouped by subject and
activity. This leads to the following columns (see below). These columns
are not again described in detail, since most of them are just the mean
of the columns of the first data set. We refer to the description of
these columns.

-   SubjectId
-   ActivityId
-   tBodyAcc.mean.X.avg.avg
-   tBodyAcc.mean.Y.avg.avg
-   tBodyAcc.mean.Z.avg.avg
-   tGravityAcc.mean.X.avg.avg
-   tGravityAcc.mean.Y.avg.avg
-   tGravityAcc.mean.Z.avg.avg
-   tBodyAccJerk.mean.X.avg.avg
-   tBodyAccJerk.mean.Y.avg.avg
-   tBodyAccJerk.mean.Z.avg.avg
-   tBodyGyro.mean.X.avg.avg
-   tBodyGyro.mean.Y.avg.avg
-   tBodyGyro.mean.Z.avg.avg
-   tBodyGyroJerk.mean.X.avg.avg
-   tBodyGyroJerk.mean.Y.avg.avg
-   tBodyGyroJerk.mean.Z.avg.avg
-   tBodyAccMag.mean.avg.avg
-   tGravityAccMag.mean.avg.avg
-   tBodyAccJerkMag.mean.avg.avg
-   tBodyGyroMag.mean.avg.avg
-   tBodyGyroJerkMag.mean.avg.avg
-   fBodyAcc.mean.X.avg.avg
-   fBodyAcc.mean.Y.avg.avg
-   fBodyAcc.mean.Z.avg.avg
-   fBodyAcc.meanFreq.X.avg.avg
-   fBodyAcc.meanFreq.Y.avg.avg
-   fBodyAcc.meanFreq.Z.avg.avg
-   fBodyAccJerk.mean.X.avg.avg
-   fBodyAccJerk.mean.Y.avg.avg
-   fBodyAccJerk.mean.Z.avg.avg
-   fBodyAccJerk.meanFreq.X.avg.avg
-   fBodyAccJerk.meanFreq.Y.avg.avg
-   fBodyAccJerk.meanFreq.Z.avg.avg
-   fBodyGyro.mean.X.avg.avg
-   fBodyGyro.mean.Y.avg.avg
-   fBodyGyro.mean.Z.avg.avg
-   fBodyGyro.meanFreq.X.avg.avg
-   fBodyGyro.meanFreq.Y.avg.avg
-   fBodyGyro.meanFreq.Z.avg.avg
-   fBodyAccMag.mean.avg.avg
-   fBodyAccMag.meanFreq.avg.avg
-   fBodyBodyAccJerkMag.mean.avg.avg
-   fBodyBodyAccJerkMag.meanFreq.avg.avg
-   fBodyBodyGyroMag.mean.avg.avg
-   fBodyBodyGyroMag.meanFreq.avg.avg
-   fBodyBodyGyroJerkMag.mean.avg.avg
-   fBodyBodyGyroJerkMag.meanFreq.avg.avg
-   tBodyAcc.std.X.avg.avg
-   tBodyAcc.std.Y.avg.avg
-   tBodyAcc.std.Z.avg.avg
-   tGravityAcc.std.X.avg.avg
-   tGravityAcc.std.Y.avg.avg
-   tGravityAcc.std.Z.avg.avg
-   tBodyAccJerk.std.X.avg.avg
-   tBodyAccJerk.std.Y.avg.avg
-   tBodyAccJerk.std.Z.avg.avg
-   tBodyGyro.std.X.avg.avg
-   tBodyGyro.std.Y.avg.avg
-   tBodyGyro.std.Z.avg.avg
-   tBodyGyroJerk.std.X.avg.avg
-   tBodyGyroJerk.std.Y.avg.avg
-   tBodyGyroJerk.std.Z.avg.avg
-   tBodyAccMag.std.avg.avg
-   tGravityAccMag.std.avg.avg
-   tBodyAccJerkMag.std.avg.avg
-   tBodyGyroMag.std.avg.avg
-   tBodyGyroJerkMag.std.avg.avg
-   fBodyAcc.std.X.avg.avg
-   fBodyAcc.std.Y.avg.avg
-   fBodyAcc.std.Z.avg.avg
-   fBodyAccJerk.std.X.avg.avg
-   fBodyAccJerk.std.Y.avg.avg
-   fBodyAccJerk.std.Z.avg.avg
-   fBodyGyro.std.X.avg.avg
-   fBodyGyro.std.Y.avg.avg
-   fBodyGyro.std.Z.avg.avg
-   fBodyAccMag.std.avg.avg
-   fBodyBodyAccJerkMag.std.avg
-   fBodyBodyGyroMag.std.avg
-   fBodyBodyGyroJerkMag.std.avg

\`\`\`
