clear all;
close all;
%load subject14 images, downsize by factor of 4
im14 = imresize(imread('subject14.gif'), 1/4);
glasses14 = imresize(imread('subject14.glasses.gif'), 1/4);
happy14 = imresize(imread('subject14.happy.gif'), 1/4);
leftlight14 = imresize(imread('subject14.leftlight.gif'), 1/4);
noglasses14 = imresize(imread('subject14.noglasses.gif'), 1/4);
normal14 = imresize(imread('subject14.normal.gif'), 1/4);
rightlight14 = imresize(imread('subject14.rightlight.gif'), 1/4);
sad14 = imresize(imread('subject14.sad.gif'), 1/4);
sleepy14 = imresize(imread('subject14.sleepy.gif'), 1/4);
wink14 = imresize(imread('subject14.wink.gif'), 1/4);

% reshape into h*w*n
yalefaces14 = reshape([im14,glasses14,happy14,leftlight14,noglasses14,normal14,rightlight14,sad14,sleepy14,wink14], [size(im14,1) size(im14,2) 10]);
[h14,w14,n14] = size(yalefaces14);
% reshape each image into a vector
d14 = h14*w14;
x14 = reshape(yalefaces14,[d14, n14]);
x14 = double(x14);
% mean vector/face
mu14 = mean(x14,2);
figure
colormap gray
imagesc(reshape(mu14, [h14, w14]))
% subtract mean
x14=bsxfun(@minus, x14, mu14);
% get covariance matrix
s14 = cov(x14');
% determine eigenvalue & eigenvector
[V14,D14] = eig(s14);
eigval14 = diag(D14);
% sort eigenvalues, corresponding eigenfaces in descending order
eigval14 = eigval14(end:-1:1);
V14 = fliplr(V14);
% plot first 6 eigenfaces
figure,subplot(3,2,1)
colormap gray
for i = 1:6
    subplot(3,2,i)
    imagesc(reshape(V14(:,i),h14,w14))
end

% repeat same process as above for subject01

%load subject01 images, downsize by factor of 4
im01 = imresize(imread('subject01.gif'), 1/4);
glasses01 = imresize(imread('subject01.glasses.gif'), 1/4);
happy01 = imresize(imread('subject01.happy.gif'), 1/4);
leftlight01 = imresize(imread('subject01.leftlight.gif'), 1/4);
noglasses01 = imresize(imread('subject01.noglasses.gif'), 1/4);
normal01 = imresize(imread('subject01.normal.gif'), 1/4);
rightlight01 = imresize(imread('subject01.rightlight.gif'), 1/4);
sad01 = imresize(imread('subject01.sad.gif'), 1/4);
sleepy01 = imresize(imread('subject01.sleepy.gif'), 1/4);
wink01 = imresize(imread('subject01.wink.gif'), 1/4);

% reshape into h*w*n
yalefaces01 = reshape([im01,glasses01,happy01,leftlight01,noglasses01,normal01,rightlight01,sad01,sleepy01,wink01], [size(im01,1) size(im01,2) 10]);
[h01,w01,n01] = size(yalefaces01);
% reshape each image into a vector
d01 = h01*w01;
x01 = reshape(yalefaces01,[d01, n01]);
x01 = double(x01);
% mean vector/face
mu01 = mean(x01,2);
figure
colormap gray
imagesc(reshape(mu01, [h01, w01]))
% subtract mean
x01=bsxfun(@minus, x01, mu01);
% get covariance matrix
s01 = cov(x01');
% determine eigenvalue & eigenvector
[V01,D01] = eig(s01);
eigval01 = diag(D01);
% sort eigenvalues, corresponding eigenfaces in descending order
eigval01 = eigval01(end:-1:1);
V01 = fliplr(V01);
% plot first 6 eigenfaces
figure,subplot(3,2,1)
colormap gray
for i = 1:6
    subplot(3,2,i)
    imagesc(reshape(V01(:,i),h01,w01))
end

% Face recognition
% load test14 image
test14 = double(imresize(imread('subject14.test.gif'), 1/4));
test_reshape14 = reshape(test14, [4880, 1]);

% face recognition using PCAs for subject14
norm_test14 = bsxfun(@minus, reshape(test14, d14, 1), mu14);
weights14 = V14'*(norm_test14);
distance14 = norm(weights14 - test_reshape14);

% face recognition using PCAs for subject01
norm_test01 = bsxfun(@minus, reshape(test14, d14, 1), mu01);
weights01 = V01'*(norm_test01);
distance01 = norm(weights01 - test_reshape14);

% compare distances
[distance14, distance01]

% since distance14 is smaller, we conclude that we are able to recognize
% subject14 through principal components of subject 14.