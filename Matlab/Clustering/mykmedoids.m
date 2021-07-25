function [ class, centroid ] = mykmedoids( pixels, K )
%
% Your goal of this assignment is implementing your own K-medoids.
% Please refer to the instructions carefully, and we encourage you to
% consult with other resources about this algorithm on the web.
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

    npix = length(pixels);
    inds = randi([1,npix], 1,K);
    medoids = pixels(inds, :); % initialize medoids
    %medoids = pixels(1:K, :); % pick first K data points as medoids for question 4
    cost = 0;
    for i=1:npix % for each data point
        for k=1:K % for each medoid
            dist(k, :) = sum(abs(medoids(k, :) - pixels(i, :))); % Manhattan Distance
        end
        [M, I] = min(dist); % find closest medoid
        cost = cost + M; % add cost of data point
        assignment(i) = I; % assign data point to that medoid's cluster
    end
    prev_cost = cost; % initial cost (distance)
    prev_assignment = assignment; % initial cluster assignment
    for l=1:K
        idx = find(prev_assignment(:)==l); % indices of cluster l
        subset = pixels(idx, :); % subset cluster l
        nsubs = length(subset); % number of data points in l
        compression_degree = 1/100; % compression(sampling) factor
        rand_idx = randi([1, nsubs], 1, round(nsubs*compression_degree)); % randomly select sample indices
        rand_sample = subset(rand_idx, :); % create random sample for swapping
        count = 0; % count for # of consecutive swaps where the medoid did not change
        for j=1:length(rand_sample) % for each swap
            new_medoids = medoids;
            new_medoids(l) = rand_sample(j); % swap "l" medoid with data point j
            new_cost = 0;
            for t=1:npix % for each data point
                for s=1:K % for each medoid
                    sub_dist(s, :) = sum(abs(new_medoids(s, :) - pixels(t, :))); % Manhattan Distance
                end
                [MIN, IND] = min(sub_dist); % find closest medoid
                new_cost = new_cost + MIN; % add cost of data point
                new_assignment(t) = IND; % assign data point to that medoid's cluster
            end
            if new_cost < prev_cost % if cost after swap is less than the previous cost
                prev_cost = new_cost; % update the previous cost with new cost
                prev_assignment = new_assignment; % update previous assignment with new assignment
                medoids = new_medoids; % update medoids with new medoids
                count = 0; % reset counter
            else
                count = count+1; % consecutive non-medoid-change counter + 1
                convergence_factor = 0.2; % portion of consecutive non-changes of medoids required for convergence
                if count >= round(convergence_factor*length(rand_sample)) % if the algorithm swaps through x-portion of the cluster's random sample without changing medoids
                    break; % consider the algorithm converged for this cluster
                end
            end
            disp(prev_cost) % this displays the distance metric for each iteration
        end
    end
    class = prev_assignment;
    centroid = medoids;
end