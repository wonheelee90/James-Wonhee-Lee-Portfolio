function [err_train, err_test] = ModelFull(train, test)
%You implment this function by assuming a full covariance matrix. 
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

pos_mean = mean(xtrain(pos_idx, :));
neg_mean = mean(xtrain(neg_idx, :));

pos_cov = cov(xtrain(pos_idx, :));
neg_cov = cov(xtrain(neg_idx, :));


[U_pos, S_pos] = eigs(pos_cov, d-1);
[U_neg, S_neg] = eigs(neg_cov, d-1);

epsilon = 1e-2;

V_pos = U_pos * diag(1./sqrt(diag(S_pos) + epsilon));
V_neg = U_neg * diag(1./sqrt(diag(S_neg) + epsilon));
S_pos = diag(S_pos);
S_neg = diag(S_neg);

pos_train = -sum(((xtrain - repmat(pos_mean, size(train, 1), 1)) * V_pos).^2, 2)/2 - log(sum(S_pos))/2 + log(pi_pos);
neg_train = -sum(((xtrain - repmat(neg_mean, size(train, 1), 1)) * V_neg).^2, 2)/2 - log(sum(S_neg))/2 + log(pi_neg);

pos_test = -sum(((xtest - repmat(pos_mean, size(test, 1), 1)) * V_pos).^2, 2)/2 - log(sum(S_pos))/2 + log(pi_pos);
neg_test = -sum(((xtest - repmat(neg_mean, size(test, 1), 1)) * V_neg).^2, 2)/2 - log(sum(S_neg))/2 + log(pi_neg);

ytrain_label = max(sign(pos_train - neg_train), 0);
ytest_label = max(sign(pos_test - neg_test), 0);

err_train = sum(abs(ytrain_label - (ytrain)))/length(ytrain);
err_test = sum(abs(ytest_label - ytest))/length(ytest);

end