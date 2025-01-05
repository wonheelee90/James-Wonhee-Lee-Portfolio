function [err_train_5, err_test_5, err_train_10, err_test_10, err_train_15, err_test_15, err_train_30, err_test_30] = knn(train, test)
%err_train is the error rate on the train data
%err_test is the error rate on the test data

% train = zscore(train);
% test = zscore(test);


[~, d] = size(train);

xtrain = double(train(:, 1: d-1));
ytrain = double(train(:, d));

xtest = double(test(:, 1: d-1));
ytest = double(test(:, d));

xtrain = double(xtrain./255);
xtest = double(xtest ./255);

pos_idx = find(ytrain == 1);
neg_idx = find(ytrain == 0);

pi_pos = length(pos_idx);
pi_neg = length(neg_idx);

mdl_5 = fitcknn(xtrain, ytrain, 'NumNeighbors', 5);
pred_train = predict(mdl_5, xtrain);
pred_test = predict(mdl_5, xtest);

err_train_5 = sum(pred_train ~= ytrain)/length(xtest);
err_test_5 = sum(pred_test ~= ytest)/length(ytest);

mdl_10 = fitcknn(xtrain, ytrain, 'NumNeighbors', 10);
pred_train = predict(mdl_10, xtrain);
pred_test = predict(mdl_10, xtest);

err_train_10 = sum(pred_train ~= ytrain)/length(xtest);
err_test_10 = sum(pred_test ~= ytest)/length(ytest);

mdl_15 = fitcknn(xtrain, ytrain, 'NumNeighbors', 15);
pred_train = predict(mdl_15, xtrain);
pred_test = predict(mdl_15, xtest);

err_train_15 = sum(pred_train ~= ytrain)/length(xtest);
err_test_15 = sum(pred_test ~= ytest)/length(ytest);

mdl_30 = fitcknn(xtrain, ytrain, 'NumNeighbors', 30);
pred_train = predict(mdl_30, xtrain);
pred_test = predict(mdl_30, xtest);

err_train_30 = sum(pred_train ~= ytrain)/length(xtest);
err_test_30 = sum(pred_test ~= ytest)/length(ytest);

end