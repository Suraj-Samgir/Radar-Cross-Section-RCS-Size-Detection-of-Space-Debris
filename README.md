# Space Debris Radar Cross-Section (RCS) Prediction

This project aims to predict the radar cross-section (RCS) size of space debris using data science techniques. The approach integrates machine learning methods with space science to model the RCS behavior of various debris objects.

## Data Collection and Preprocessing

1. **Data Source**: A large dataset collected from [Kaggle](https://www.kaggle.com/code/kandhalkhandeka/predicting-rcs-size-of-space-debris).
2. **Preprocessing Steps**:
    - Invalid columns removal.
    - Null values removal.
    - Conversion of categorical variables.
    - Class imbalance treatment.
    - Normalization using the Min-Max method.
    - Outlier detection and treatment.
    - Feature selection using Recursive Feature Elimination (RFE) and Mutual Information (MI).

## Model Training

Various machine learning algorithms were explored, including Random Forests, Support Vector Machines (SVM), Boosting Techniques, and others. The dataset was split into training and testing sets for model evaluation.

## Results

The performance of different models was evaluated, and the following results were obtained:

| Algorithm                          | Accuracy | Precision | Recall | F1 Score | Specificity |
|------------------------------------|:--------:|:---------:|:------:|:--------:|:-----------:|
| Gradient Boosting Machine (GBM)    |   0.89   |   0.89    |  0.88  |   0.88   |    0.93     |
| Random Forest                      | **0.97** | **0.97**  |**0.97**|**0.97**  |  **0.98**   |
| Multinomial Logistic Regression    |   0.80   |   0.79    |  0.80  |   0.79   |    0.90     |
| Naive Bayes                        |   0.68   |   0.69    |  0.69  |   0.69   |    0.85     |
| Support Vector Machine (SVM)       |   0.81   |   0.81    |  0.80  |   0.80   |    0.90     |
| Decision Trees                     |   0.77   |   0.78    |  0.76  |   0.76   |    0.88     |
| Voting Based                       |   0.85   |   0.85    |  0.84  |   0.84   |    0.90     |

The Random Forest algorithm achieved the highest accuracy of 97%, demonstrating the effectiveness of the proposed method in predicting the RCS size of space debris under various observation conditions.

## Conclusion

This research highlights the potential benefits of combining machine learning with electromagnetic simulations to enhance the tracking and characterization of space debris. Improved prediction of RCS contributes to better space situational awareness and reduced risk of collisions, thereby safeguarding the sustainability of space operations.
