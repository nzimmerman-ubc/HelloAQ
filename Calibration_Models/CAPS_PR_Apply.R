CAPS_PR_Apply = function(model_object,m_data) {
  # This function applies polynomial regression models
  
  # INPUTS
  
  # model_object: vector of coefficients of the polynomial regression model.
  
  # m_data: matrix of input data to fit to target.
  
  # OUTPUT
  
  # v_data: vector of fitted data.
  
  if ((is_empty(m_data)) | (is_empty(model_object))) {
    v_data = as.matrix(rep(NaN,dim(m_data)[1]))
  } else {

  ## Setup
  # Augment data matrix
  m_data_new = matrix(1,dim(m_data)[1],(dim(m_data)[2]+1))
  m_data_new[,2:(dim(m_data)[2]+1)] = m_data
  m_data = m_data_new
  
  # determine model order
  parameters = as.matrix(model_object)
  n_parameters = length(parameters)
  
  n_order = 0;
  if (n_parameters == 1) {
    n_order = 0
  } else if (n_parameters == dim(m_data)[2]) {
    n_order = 1
  } else if (n_parameters == sum(1:(dim(m_data)[2]))) {
    n_order = 2
  } else {
    i_order = 3
    while (n_order == 0) {
      m_combinations = combinations(dim(m_data)[2],i_order,repeats.allowed = TRUE)
      n_combinations = dim(m_combinations)[1]
      if (n_parameters == n_combinations) {
        n_order = i_order
      } else {
        i_order = i_order + 1
      }
    }
  }
    
  # create factor matrix
  m_combinations = combinations(dim(m_data)[2],n_order,repeats.allowed = TRUE)
  
  X = matrix(NA,dim(m_data)[1],dim(m_combinations)[1]) 
  
  for (i_combin in 1:(dim(m_combinations)[1])) {
    if (dim(m_combinations)[2] == 1) {
      X[,i_combin] = m_data[,m_combinations[i_combin,]]
    } else {
      X[,i_combin] = apply(m_data[,m_combinations[i_combin,]],-2,prod)
    }
  }
  
  ## Remove columns with zero coefficient
  id_zero_columns = which(parameters == 0)
  if (!is_empty(id_zero_columns)) {
    X = X[,-id_zero_columns]
    parameters = parameters[-id_zero_columns]
    parameters = as.matrix(parameters)
  }
  
  ## Apply Model
  v_data = X %*% parameters
  
  v_data[is.na(v_data)] = NaN
  
  }
  
  ## Output
  return(v_data)
}