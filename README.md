## inpatient_CBG_xgb

code to process files from the Abbott unipoc and aegis systems, process into datasets for building xgboost classifiers to predict the likelyhood of a CBG below a defined threshold on an arbitrary day of admission

code order and flow is shown in the diagram: [cbg_prediction_code_flow.pdf](https://github.com/csainsbury/inpatient_CBG_xgb/blob/main/cbg_prediction_code_flow.pdf)

a set of comparisons of the model performance (C-statistic) between internal cross validation score on training data (unipoc) and test score on the validation data (aegis) is given [here](https://github.com/csainsbury/inpatient_CBG_xgb/blob/main/CBG_performance.pdf). These plots show the perfmance predicting a CBG of <3, <4 and <5mmol/L on days 3,4,5,6,7 and 21 of admission.

10-fold internal cross validation [AUROCs](https://github.com/csainsbury/inpatient_CBG_xgb/tree/main/unipoc_model_auroc_threshold). predictions from each fold combined before AUROC calculated to give an overall auroc. threshold is the automatically calculated best threshold for this combined set of predictions.

[best parameters](https://github.com/csainsbury/inpatient_CBG_xgb/tree/main/unipoc_model_best_params) from bayesian optimisation per day/threshold
