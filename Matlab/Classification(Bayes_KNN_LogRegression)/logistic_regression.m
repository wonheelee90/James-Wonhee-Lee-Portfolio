N = 100;
P = [0.1, 0.2, 0.5, 0.8, 0.9];
avg_error_rates = zeros(5, 2);

for m = 1:length(P)
    p = P(m);

    error_rates = zeros(100, 2);
    for n=1:N

        [train, test] = SplitData(p);

        [~, d] = size(train);

        xtrain = double(train(:, 1: d-1));
        ytrain = logical(train(:, d));

        xtest = double(test(:, 1: d-1));
        ytest = logical(test(:, d));

        xtrain = double(xtrain./255);
        xtest = double(xtest ./255);

        % get coefficients of logistic regression model
        GLM = glmfit(xtrain, ytrain, 'binomial', 'link', 'logit');

        % fit on training data
        data = xtrain;
        coefs = GLM;

        % fit regression linearly
        linfit = zeros(size(data, 1), 1);
        for i=1:size(data, 1)
            row_val = coefs(1);
            for j=2:length(coefs)
                row_val = row_val + data(i, j-1)*coefs(j); 
            end
            linfit(i, :) = row_val;
        end

        % convert to logistic regression prediction values
        logfit = 1./(1+exp(-linfit));

        % predict based on threshold of 0.5 (0.5 or higher means label of '1')
        predictions = logfit;
        for k=1:size(logfit, 1)
            if logfit(k) >= 0.5
                predictions(k) = 1;
            else
                predictions(k) = 0;
            end
        end
        
        % compute training error
        err_train = sum(predictions ~= ytrain)/length(ytrain);

        % now fit the model on the test data
        data = xtest;

        % fit regression linearly
        linfit = zeros(size(data, 1), 1);
        for i=1:size(data, 1)
            row_val = coefs(1);
            for j=2:length(coefs)
                row_val = row_val + data(i, j-1)*coefs(j); 
            end
            linfit(i, :) = row_val;
        end

        % convert to logistic regression prediction values
        logfit = 1./(1+exp(-linfit));

        % predict based on threshold of 0.5 (0.5 or higher means label of '1')
        predictions = logfit;
        for k=1:size(logfit, 1)
            if logfit(k) >= 0.5
                predictions(k) = 1;
            else
                predictions(k) = 0;
            end
        end
   
        % compute test error
        err_test = sum(predictions ~= ytest)/length(ytest);

        error_rates(n, :) = [err_train, err_test];
        
    end
    avg_error_rate = mean(error_rates);
    avg_error_rates(m, :) = avg_error_rate;

end

avg_error_rates