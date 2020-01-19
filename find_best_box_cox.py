from scipy.stats import boxcox


def find_best_box_cox(data,target_var,predictor_var):
    
    box_cox = [-2,-1, 0, 1, 2]
    var_tar = {}
    var_pred = {}
    for bc in box_cox:
        var_tar[bc] = boxcox(data[target_var],bc)
        var_pred[bc] = boxcox(data[predictor_var],bc)
    
    dct = {}
    for k1 in var_tar.keys():
        for k2 in var_pred.keys():
            dct[k1,k2] = np.corrcoef(var_tar[k1],var_pred[k2])[0][1]
        
    return(min(dct, key=dct.get))
	
	
for target_var in ['Price']:
    for predictor_var in data.columns[2:].tolist():
        print(find_best_box_cox(data = data, target_var = target_var , predictor_var = predictor_var))