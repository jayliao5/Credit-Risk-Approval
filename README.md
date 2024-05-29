# Credit Risk Score Calculation and Anomaly Detection

## Project Overview
This project, part of DECISION 520Q: Data Science for Business, is developed by Team 3. The focus is on developing a robust model for predicting credit card approval based on consumer behavior and economic trends. The project integrates various data science techniques to manage credit risk effectively.

### Team Members
- Mia Kwon
- Zhixuan Li
- Jason Liao
- Yitian Wang
- Sihan Zhou

## Business Understanding
The credit card industry plays a crucial role in consumer spending and credit accessibility but faces risks like payment defaults. Our model aims to mitigate these risks by accurately predicting credit card approvals.

## Data Preparation
Data preparation involved creating a response variable from customer credit records and cleaning predictor variables. We tackled the imbalances in the dataset and prepared it for modeling through various data transformation techniques.

## Modeling
We used multiple supervised models including Logistic Regression, LASSO Regression, Decision Tree, and Random Forest, alongside an unsupervised model, K-means, to predict credit card approval:

- **Logistic Regression**: Baseline model, using various variables related to creditworthiness.
- **LASSO Regression**: To handle the large number of variables and avoid overfitting.
- **Decision Tree and Random Forest**: For their ability to handle non-linear relationships and provide high accuracy.
- **K-Means Clustering**: An unsupervised model that was initially tested for clustering traits but was not ultimately used.

## Evaluation
Models were evaluated based on their F1 score to balance precision and recall, crucial for dealing with the class imbalance in approval predictions.

## Deployment
The best-performing model, Random Forest, will be integrated into the credit card application processing pipeline for real-time predictions.

## Ethical Considerations
Ensures the model does not discriminate based on sensitive attributes. Regular audits and fairness-enhancing interventions are recommended.

## Risks and Mitigation
- **Model Drift**: Regular updates and monitoring.
- **Over-reliance on the Model**: Manual review for borderline cases.
- **Data Privacy**: Adherence to regulations like GDPR.
- **Model Bias**: Implementing fairness checks.

## Appendix
Each team member contributed equally to the project. All data used was treated in strict compliance with ethical standards.

---

For more details on the methodologies and results, please refer to our project report or contact any of the team members.
