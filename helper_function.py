import pandas as pd

def export_reg_result_csv(ols, path):
    '''
    This function takes a statsmodels OLS regression result and exports the results to a CSV file. 
    It includes a comment at the top indicating the dependent variable.

    Parameters:
    ols: statsmodels OLS regression result
    path: string / the path where the CSV file should be saved

    Returns: 
    None. The function saves a CSV file with the results of the regression at the specified path.
    '''
    
    # Extract the regression results into a DataFrame
    results_df = pd.DataFrame({
        'Variable': ols.model.exog_names,
        'Coefficient': ols.params,
        'Standard Errors': ols.bse,
        't-values': ols.tvalues,
        'p-values': ols.pvalues
    })

    dependent_variable = ols.model.endog_names

    # Format the comment to include the dependent variable
    comment = f"# This file contains the OLS regression summary results. The dependent variable is '{dependent_variable}'."

    # Write the comment and the DataFrame to a CSV file at the given path
    with open(path, 'w') as f:
        f.write(comment + "\n")
        results_df.to_csv(f, index=False)
    
    print(f"OLS regression results saved to {path}")
