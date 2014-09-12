__author__ = 'nilesh'

# load_data
# create_data_partition
# create_feature_set

import os
import pandas as pd
import numpy as np
from sklearn import linear_model
from sklearn.linear_model import LogisticRegression
import random
import sklearn
from sklearn.preprocessing import Imputer
from sklearn import metrics
from sklearn import cross_validation
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestClassifier

#os.chdir('/mnt/hgfs/D/0. Nilesh Files/Dropbox/SkyDrive/2. Projects/7. Perosonal/51. Python Machine Learning')
os.chdir('D:\\0. Nilesh Files\\1. Kaggle\\70. Fire Peril Loss Cost')

train_file_path = '1. Data/train.csv'
test_file_path = '1. Data/test.csv'
sample_submission_file = '1. Data/sampleSubmission.csv'

train_sample_pct = 0.8
ver_name_target = 'target'
ver_name_id = 'id'

print os.getcwd()

def load_data(train_file_path, test_file_path, sample_submission_file):
    train = pd.read_table(train_file_path, sep=',')
    test = pd.read_table(test_file_path, sep=',')
    sample_submission = pd.read_table(sample_submission_file, sep=',')

    train_row_count = train.shape[0]
    test_row_count = test.shape[0]
    
    print 'Train & Test Column Names :', list(set(train.columns.values) | 
    set(test.columns.values))
    print 'Train only Column Names :', \
    list(set(train.columns.values) - set(test.columns.values))
    print 'Test only Column Names :', \
    list(set(train.columns.values) - set(train.columns.values))
    
    data = train.append(test)
    

    data.describe()

    data = np.nan_to_num(np.array(data))
    
    data_columns = data.columns
    data_index = data.index
    data = data.values
    return data, data_columns, data_index, train_row_count, test_row_count, sample_submission


def create_data_partition(train_row_count, text_row_count, train_sample_pct):
    idx_train_temp = range(train_row_count)
    idx_train = [idx_train_temp[i] for i in sorted(random.sample(xrange(len(idx_train_temp)), int(train_row_count* train_sample_pct)))]
    idx_valid = list(set(idx_train_temp) - set(idx_train))
    idx_test = list(train_row_count+np.asanyarray(range(test_row_count) ))
    return idx_train, idx_valid, idx_test    


def create_feature_set(data_columns, ver_name_target, ver_name_id):
    fs_target = [ver_name_target]
    fs_id = [ver_name_id]
    fs_all = list(data_columns.values)
    fs_features = list(set(fs_all) - set(set(fs_target) | set(fs_id)))
    return fs_target, fs_id, fs_all, fs_features
        

def get_matching_item_indices(list1, list2):
    idx = list()    
    if len(list2) == 1:
        idx.append(list(list1).index(list2[0]))        
    else:
        for item in list2:
            idx.append(list(list1).index(item))
    idx.sort()
    return idx
    
def create_feature_set_index(fs_target, fs_id, fs_all, fs_features):
    fs_idx_target = get_matching_item_indices(data_columns.values, fs_target)
    fs_idx_id = get_matching_item_indices(data_columns.values, fs_id)
    fs_idx_all= get_matching_item_indices(data_columns.values, fs_all)
    fs_idx_features= get_matching_item_indices(data_columns.values, fs_features) 
    return fs_idx_target, fs_idx_id, fs_idx_all, fs_idx_features
    
def pre_process_data(data):
    imp = Imputer(missing_values='NaN', strategy='mean', axis=0)
    imp.fit(data)
    data = imp.transform(data)
    return data


def write_submisstion_file(output_test, sample_submission):
    sample_submission['Probability'] = output_test
    sample_submission.to_csv('2. Data Processing/Python Submission.csv', index=False)


def train_logistic_model(data, idx_train,idx_valid, idx_test):
    print 'Training Model'
    model= linear_model.LogisticRegression()
    model.fit(data[idx_train,][:, fs_idx_features], data[idx_train,][:, fs_idx_target])
    output_train = model.predict_proba(data[idx_train,][:, fs_idx_features])[:,1]
    output_valid = model.predict_proba(data[idx_valid,][:, fs_idx_features])[:,1]
    output_test = model.predict_proba(data[idx_test,][:, fs_idx_features])[:,1]
    print 'AUC Train:', metrics.roc_auc_score(data[idx_train,][:, fs_idx_target], np.asarray(output_train))
    print 'AUC Valid:', metrics.roc_auc_score(data[idx_valid,][:, fs_idx_target], np.asarray(output_valid))
    return model

def train_random_forest_model(data, idx_train,idx_valid, idx_test):
    print 'Training Model'
    model= RandomForestClassifier(n_estimators=100, n_jobs=4, verbose=1)
    model.fit(data[idx_train,][:, fs_idx_features], data[idx_train,][:, fs_idx_target])
    output_train = model.predict_proba(data[idx_train,][:, fs_idx_features])[:,1]
    output_valid = model.predict_proba(data[idx_valid,][:, fs_idx_features])[:,1]
    output_test = model.predict_proba(data[idx_test,][:, fs_idx_features])[:,1]
    print 'AUC Train:', metrics.roc_auc_score(data[idx_train,][:, fs_idx_target], np.asarray(output_train))
    print 'AUC Valid:', metrics.roc_auc_score(data[idx_valid,][:, fs_idx_target], np.asarray(output_valid))
    write_submisstion_file(output_test,sample_submission)
    return model



[data, data_columns, data_index,  train_row_count, test_row_count, sample_submission] = load_data(train_file_path, test_file_path, sample_submission_file)

[idx_train, idx_valid, idx_test] = create_data_partition(train_row_count, test_row_count, train_sample_pct)

[fs_target, fs_id, fs_all, fs_features] = create_feature_set(data_columns, ver_name_target, ver_name_id)
[fs_idx_target, fs_idx_id, fs_idx_all, fs_idx_features] = create_feature_set_index(fs_target, fs_id, fs_all, fs_features)

print 'Data Loading Complete'

data = pre_process_data(data)

model = train_logistic_model(data,idx_train, idx_valid, idx_test)

model = train_random_forest_model(data, idx_train,idx_valid, idx_test)






