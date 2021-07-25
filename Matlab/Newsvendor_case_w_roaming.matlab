
%Choose input
input=[6,7,8,9,10,11,12,13,14,15,16,17,18,19,20];
N_Q=length(input);
N_lane=10; %number of lane

lambda=100/N_lane; % weekly mean demand per lane

N_iter = 4000;

cost=zeros(1,N_Q);
left = zeros(1,N_lane);
shortage = zeros(1,N_lane);
demand = zeros(1,N_lane); 

for i_iter = 1:N_iter	% for each iteration(week)

	for i=1:N_Q            % for each possible ordering quantity
		Q=input(i);
		left = zeros(1,N_lane) + Q; % start with Q items in each lane
		T_shortage = zeros(1,N_lane); % total shortage per lane

		for day = 1:7 % iterate for each day
			demand = poissrnd(lambda/7,[1,N_lane]); %daily demand D~Pois(lambda/7)
			current_left = left;
			left = max(0,current_left - demand);
			shortage = max(0, demand - current_left);

			for j = 1:N_lane  % ROAMING
				if shortage(j) > 0   %if there is shortage in lane j
	    			if j > 1    %if the current lane is 2, 3, .... , 8, then check the lane on the left side. 
	    				if left(j-1) >= shortage(j)
	        				left(j-1) = left(j-1) - shortage(j);
	        				shortage(j) = 0;
	           			else
	           				left(j-1) = 0;
	           				shortage(j) = shortage(j) - left(j-1);
	           			end
	     			end
	     			if j < N_lane     %if the current lane is 1, 2, ..., 7, then check the lane on the right side. 
	          			if left(j+1) >= shortage(j)
	        				left(j+1) = left(j+1) - shortage(j);
	        				shortage(j) = 0;
	           			else
	           				left(j+1) = 0;
	           				shortage(j) = shortage(j) - left(j+1);
	           			end
	     			end
				end
			end
		
			T_shortage = T_shortage + shortage;   

		end

		%at the end of week, calculate cost
		cost(i) = cost(i) + 4.99*Q*N_lane + 1.99*sum(left) + 11.99*sum(T_shortage);%unit cost per lane

		if nnz(~left) > 0  %if there are any empty pockets
			cost(i) = cost(i) + 1000; %add penalty $1000
		end
		
	end
end
cost = cost/N_iter;




h1 = plot(input, cost, '--b*');

xlabel('ordering quantity q');
ylabel('expected weekly cost');
legend([h1], {'w/o roaming'});

