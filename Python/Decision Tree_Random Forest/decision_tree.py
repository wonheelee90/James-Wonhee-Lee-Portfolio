from util import entropy, information_gain, partition_classes
import numpy as np 
import ast

class DecisionTree(object):
    def __init__(self):
        # Initializing the tree as an empty dictionary or list, as preferred
        #self.tree = []
        self.tree = {}
        pass

    def learn(self, X, y):
        # TODO: Train the decision tree (self.tree) using the the sample X and labels y
        # You will have to make use of the functions in utils.py to train the tree
        
        # One possible way of implementing the tree:
        #    Each node in self.tree could be in the form of a dictionary:
        #       https://docs.python.org/2/library/stdtypes.html#mapping-types-dict
        #    For example, a non-leaf node with two children can have a 'left' key and  a 
        #    'right' key. You can add more keys which might help in classification
        #    (eg. split attribute and split value)

        def split(X, y):
            if len(set(y)) == 1:
                return y[0]
            i = 0 
            abs_max_i = 0
            abs_max_IG = 0
            abs_max_X_left = []
            abs_max_X_right = []
            abs_max_y_left = []
            abs_max_y_right = []
            abs_max_j = 0
            while i < len(X[0]):
                j = 0
                current_H = entropy(y)
                max_j = 0
                max_IG = 0
                max_X_left = []
                max_X_right = []
                max_y_left = []
                max_y_right = []
                while j < len(y):           
                    X_left, X_right, y_left, y_right = partition_classes(X, y, i, X[j][i])
                    if information_gain(y, [y_left, y_right]) > max_IG:
                        max_IG = information_gain(y, [y_left, y_right])
                        max_j = j
                        max_X_left = X_left
                        max_X_right = X_right
                        max_y_left = y_left
                        max_y_right = y_right
                    j += 1
                if max_IG > abs_max_IG:
                    abs_max_IG = max_IG
                    abs_max_i = i
                    abs_max_X_left = max_X_left
                    abs_max_X_right = max_X_right
                    abs_max_y_left = max_y_left
                    abs_max_y_right = max_y_right
                    abs_max_j = max_j
                i += 1
            return {'X_left': abs_max_X_left, 'X_right': abs_max_X_right, 'y_left': abs_max_y_left, 'y_right': abs_max_y_right, 'split_attr': abs_max_i, 'split_val': X[abs_max_j][abs_max_i]}

        def recursive_split(X, y):
            current_dict = split(X,y)
            if (current_dict != 0) and (current_dict != 1):
                current_dict['X_left'] = recursive_split(current_dict['X_left'], current_dict['y_left'])
            if (current_dict != 0) and (current_dict != 1):
                current_dict['X_right'] = recursive_split(current_dict['X_right'], current_dict['y_right'])
            return current_dict

        self.tree = recursive_split(X,y)
        pass


    def classify(self, record):
        # TODO: classify the record using self.tree and return the predicted label
        dict = self.tree
        def route(dict, record):
            if dict == 0 or dict == 1:
                return dict
            if type(record[dict['split_attr']]) == int:
                if record[dict['split_attr']] <= dict['split_val']:
                    return route(dict['X_left'], record)
                else:
                    return route(dict['X_right'], record)
            else:
                if record[dict['split_attr']] == dict['split_val']:
                    return route(dict['X_left'], record)
                else:
                    return route(dict['X_right'], record)

        #def predict(self, record):
        #    predictions = []
         #   for i in record:
          #      predictions.append(route(dict, i))
           # return predictions

        return route(dict, record)
    
        pass
