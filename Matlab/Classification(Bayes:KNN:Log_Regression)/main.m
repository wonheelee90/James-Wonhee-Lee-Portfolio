%This is the main routine for homework 4
%You are asked to plugin your implementation for the funciton ModelFull,
%ModelDiagonal, and ModelSpherical

%Repeat the experiments for 100 times
N = 100;

err_Full = zeros(N,2);
err_Diagonal = zeros(N,2);
err_Spherical = zeros(N,2);
err_knn = zeros(N,8);

%Let p change from 0.1, 0.2, 0.5, 0.8, 0.9 to compare the performance of each classifier
p = 0.9;

for i = 1 : N
	
	[train, test] = SplitData(p);
	
	[err_train, err_test] = ModelFull(train, test);
	err_Full(i,:) = [err_train, err_test];
	
	[err_train, err_test] = ModelDiagonal(train, test);
	err_Diagonal(i,:) = [err_train, err_test];
	
	[err_train, err_test] = ModelSpherical(train, test);
	err_Spherical(i,:) = [err_train, err_test];
	
    [err_train_5, err_test_5, err_train_10, err_test_10, err_train_15, err_test_15, err_train_30, err_test_30] = knn(train, test);
	err_knn(i,:) = [err_train_5, err_test_5, err_train_10, err_test_10, err_train_15, err_test_15, err_train_30, err_test_30];
    
end

mean_err_Full = mean(err_Full);
mean_err_Diagonal = mean(err_Diagonal);
mean_err_Spherical = mean(err_Spherical);
mean_err_knn = mean(err_knn);

fprintf('err_Full : %g, %g\n', mean_err_Full(1), mean_err_Full(2));
fprintf('err_Diagonal : %g, %g\n', mean_err_Diagonal(1), mean_err_Diagonal(2));
fprintf('err_Spherical : %g, %g\n', mean_err_Spherical(1), mean_err_Spherical(2));
fprintf('err_knn : %g, %g\n', mean_err_knn(1), mean_err_knn(2), mean_err_knn(3), mean_err_knn(4), mean_err_knn(5), mean_err_knn(6), mean_err_knn(7), mean_err_knn(8));
