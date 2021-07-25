function [ class, centroid ] = mykmeans( pixels, K )
%
% Your goal of this assignment is implementing your own K-means.
%
% Input:
%     pixels: data set. Each row contains one data point. For image
%     dataset, it contains 3 columns, each column corresponding to Red,
%     Green, and Blue component.
%
%     K: the number of desired clusters. Too high value of K may result in
%     empty cluster error. Then, you need to reduce it.
%
% Output:
%     class: the class assignment of each data point in pixels. The
%     assignment should be 1, 2, 3, etc. For K = 5, for example, each cell
%     of class should be either 1, 2, 3, 4, or 5. The output should be a
%     column vector with size(pixels, 1) elements.
%
%     centroid: the location of K centroids in your result. With images,
%     each centroid corresponds to the representative color of each
%     cluster. The output should be a matrix with size(pixels, 1) rows and
%     3 columns. The range of values should be [0, 255].
%     
%
% You may run the following line, then you can see what should be done.
% For submission, you need to code your own implementation without using
% the kmeans matlab function directly. That is, you need to comment it out.

    centroids = rand(K, 3)*255; % randomly assign initial centroids
    old_centroids = zeros(K, 3); % variable for storing centroids
    while ~isequal(old_centroids, centroids) % loop runs until there is no change in centroids
        for i=1:size(pixels,1) % for each data point
            pixel = pixels(i, 1:3);
            distances = zeros(K, 3);
            for j=1:K % for each cluster
                for l=1:3 % compute distances in R, G, B
                    distances(j,l) = (centroids(j,l)-pixel(l)).^2; % compute squared R, G, B distances
                end
            end
            sum_dist = sqrt(sum(distances, 2)); % take sqrt of sum to get euclidean distances to each centroid
            [M,I] = min(sum_dist); % get minimum distance, corresponding centroid
            assignment(i) = I; % assign data point to that centroid
        end
        old_centroids = centroids;
        for k=1:K % for each cluster
            idx = find(assignment(:)==k); % subset indices belonging to each cluster
            if isempty(idx) % if there are no data points in the cluster
                centroids = old_centroids(1:end ~= k, :); % remove the centroid of the empty cluster
                K= K-1; % reduce the number of clusters by 1
                break; % revert back to the step of finding new centroids
            end
            part = pixels(idx, :); % subset cluster
            centroids(k, :) = mean(part); % set new centroid as mean of cluster
        end
        disp(K)
    end
    class = assignment;
    centroid = centroids;
end
