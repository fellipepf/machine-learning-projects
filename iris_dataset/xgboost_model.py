import numpy as np
from sklearn import datasets
from sklearn.metrics import precision_score
from sklearn.model_selection import train_test_split

import xgboost as xgb
#from xgboost_model import XGBClassifier

iris = datasets.load_iris()
X = iris.data
y = iris.target



X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)


print(xgb)
dtrain = xgb.DMatrix(data=X_train, label=y_train)
dtest = xgb.DMatrix(data=X_test, label=y_test)

param = {
    'max_depth': 3,  # the maximum depth of each tree
    'eta': 0.3,  # the training step for each iteration
    'silent': 1,  # logging mode - quiet
    'objective': 'multi:softprob',  # error evaluation for multiclass training
    'num_class': 3}  # the number of classes that exist in this datset
num_round = 20  # the number of training iterations

bst = xgb.train(param, dtrain, num_round)

bst.dump_model('dump.raw.txt')
preds = bst.predict(dtest)

best_preds = np.asarray([np.argmax(line) for line in preds])
print (precision_score(y_test, best_preds, average='macro'))