IMBALANCED CLASSIFICATION EXAMPLE USING XGBOOST

Task/Purpose
- The jupyter notebook file illustrates the training of a gradient-boosted tree classifier using the xgboost library available in python.

Instructions
- Unzip the two zip files, test_sample.csv.zip and train_sample.csv.zip to get the data files.
- Run xgboost_classification_example_py3.ipynb on jupyter notebook to see the analysis.

Data
- The datasets are composed of 15 features (9 quantitative, 6 qualitative) and 1 response variable indicating a positive/negative outcome.
- The datasets are highly imbalanced, with "negative" labels overwhelming the "positive" labels in numbers ("positive" rate close to 0.001)
- The training set and test set are non-overlapping but are in identical structures.

Results
- Recall of 86% was achieved with default cutoff threshold of 0.5
- Given the imbalance of the data and how it was downsampled for training, the model is going to generate a good amount of False Positives, leading to poor precision figures for positive predictions.
- However, as is the case with many imbalanced studies(disease assessment, subscription purchases, etc), the cost of a FP(misclassifying an actual negative) tends to be very low compared to the cost of a FN(misclassifying an actual positive), so in this case we would be willing to accept the trade-off.
