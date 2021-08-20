CAPS_Hybrid_Apply = function(model_object,m_data) {
  # The function applies a hybrid random forest and linear regression model
  
  # INPUTS:
  
  # model_object: The R hybrid random forest and linear regression model to be applied
  # model_object[[1]][1]: lower bound for application of RF model
  # model_object[[1]][2]: upper bound for application of RF model
  # model_object[[2]]: RF model object
  # model_object[[3]]: lower LR model object
  # model_object[[4]]: upper LR model object
  
  # m_data: matrix of input data to processed with the model.
  
  # OUTPUT:
  
  # v_data: output of model application. Length matches first dimension of m_data
  
  ## Setup
  v_data = as.matrix(rep(NaN,dim(m_data)[1]))
  
  ## Apply Model
  if ((!is_empty(model_object)) & (!is_empty(m_data))) {
    # Extract model information
    n_RF_lower_bound = model_object[[1]][1]
    n_RF_upper_bound = model_object[[1]][2]
      
    RF_model = model_object[[2]]
    lower_LR_model = model_object[[3]]
    upper_LR_model = model_object[[4]]
    
    
    # rename input types for consistency
    c_input_names = c()
    for (i_input in 1:dim(m_data)[2]) {
      c_input_names = c(c_input_names,paste('input',i_input,sep=''))
    }
    colnames(m_data) = c_input_names
    
    ## Apply RF model
    if (!is_empty(RF_model)) {
      v_data = CAPS_RF_Apply(RF_model,m_data)
    }
    
    ## Apply lower LR model if needed
    id_lower = which(v_data < n_RF_lower_bound)
    if (!is_empty(id_lower)) {
      if (!is_empty(lower_LR_model)) {
        m_lower = m_data[id_lower,]
        dim(m_lower) = c(length(id_lower),dim(m_data)[2])
        v_data[id_lower] = CAPS_PR_Apply(lower_LR_model,m_lower)
      } else {
        v_data[id_lower] = NaN
      }
    }
    
    ## Apply upper LR model if needed
    id_upper = which(v_data > n_RF_upper_bound)
    if (!is_empty(id_upper)) {
      if (!is_empty(upper_LR_model)) {
        m_upper = m_data[id_upper,]
        dim(m_upper) = c(length(id_upper),dim(m_data)[2])
        v_data[id_upper] = CAPS_PR_Apply(upper_LR_model,m_upper)
      } else {
        v_data[id_upper] = NaN
      }
    }
    
  }
  
  ## Output
  v_data = as.matrix(v_data)
  return(v_data)
  
}