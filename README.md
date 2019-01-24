# Kaggle_Reduce_Aviation_Fatality_RandomForrest

Overview
--------

Most flight-related fatalities stem from a loss of “airplane state awareness.” That is, ineffective attention management on the part of pilots who may be distracted, sleepy or in other dangerous cognitive states.

Your challenge is to build a model to detect troubling events from aircrew’s physiological data. You'll use data acquired from actual pilots in test situations, and your models should be able to run calculations in real time to monitor the cognitive states of pilots. With your help, pilots could then be alerted when they enter a troubling state, preventing accidents and saving lives.

Reducing aircraft fatalities is just one of the complex problems that Booz Allen Hamilton has been solving for business, government, and military leaders for over 100 years. Through devotion, candor, courage, and character, they produce original solutions where there are no roadmaps. Now you can help them find answers, save lives, and change the world.

Data
----

In this dataset, you are provided with real physiological data from eighteen pilots who were subjected to various distracting events. The benchmark training set is comprised of a set of controlled experiments collected in a non-flight environment, outside of a flight simulator. The test set (abbreviated LOFT = Line Oriented Flight Training) consists of a full flight (take off, flight, and landing) in a flight simulator.

The pilots experienced distractions intended to induce one of the following three cognitive states:

Channelized Attention (CA) is, roughly speaking, the state of being focused on one task to the exclusion of all others. This is induced in benchmarking by having the subjects play an engaging puzzle-based video game.

Diverted Attention (DA) is the state of having one’s attention diverted by actions or thought processes associated with a decision. This is induced by having the subjects perform a display monitoring task. Periodically, a math problem showed up which had to be solved before returning to the monitoring task.

Startle/Surprise (SS) is induced by having the subjects watch movie clips with jump scares.

For each experiment, a pair of pilots (each with its own crew id) was recorded over time and subjected to the CA, DA, or SS cognitive states. The training set contains three experiments (one for each state) in which the pilots experienced just one of the states. For example, in the experiment = CA, the pilots were either in a baseline state (no event) or the CA state. The test set contains a full flight simulation during which the pilots could experience any of the states (but never more than one at a time). The goal of this competition is to predict the probability of each state for each time in the test set.

Each sensor operated at a sample rate of 256 Hz. Please note that since this is physiological data from real people, there will be noise and artifacts in the data.

Data fields
-----------

Variables with the eeg prefix are electroencephalogram recordings.

id - (test.csv and sample_submission.csv only) A unique identifier for a crew + time combination. You must predict probabilities for each id.

crew - a unique id for a pair of pilots. There are 9 crews in the data.

experiment - One of CA, DA, SS or LOFT. The first 3 comprise the training set. The latter the test set.
time - seconds into the experiment

seat - is the pilot in the left (0) or right (1) seat

eeg_fp1

eeg_f7

eeg_f8

eeg_t4

eeg_t6

eeg_t5

eeg_t3

eeg_fp2

eeg_o1

eeg_p3

eeg_pz

eeg_f3

eeg_fz

eeg_f4

eeg_c4

eeg_p4

eeg_poz

eeg_c3

eeg_cz

eeg_o2


ecg - 3-point Electrocardiogram signal. The sensor had a resolution/bit of .012215 µV and a range of -100mV to +100mV. The data are provided in microvolts.

r - Respiration, a measure of the rise and fall of the chest. The sensor had a resolution/bit of .2384186 µV and a range of -2.0V to +2.0V. The data are provided in microvolts.

gsr - Galvanic Skin Response, a measure of electrodermal activity. The sensor had a resolution/bit of .2384186 µV and a range of -2.0V to +2.0V. The data are provided in microvolts.

event - The state of the pilot at the given time: one of A = baseline, B = SS, C = CA, D = DA

