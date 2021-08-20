CAPS_RF_Apply = function(model_object,m_data) {
  # The function applies a random forest model
  
  # INPUTS:
  
  # model_object: The R random forest model to be applied
  
  # m_data: matrix of input data to processed with the model.
  
  # OUTPUT:
  
  # v_data: output of model application. Length matches first dimension of m_data
  
  ## Setup
  warnstate_save = getOption('warn')
  options(warn = -1)
  
  library(randomForest)
  v_data = as.matrix(rep(NaN,dim(m_data)[1]))
  
  ## Apply Model
  if ((!is_empty(model_object)) & (!is_empty(m_data))) {
    # rename input types for consistency
    c_input_names = c()
    for (i_input in 1:dim(m_data)[2]) {
      c_input_names = c(c_input_names,paste('input',i_input,sep=''))
    }
    
    f_data = as.data.frame(m_data)
    colnames(f_data) = c_input_names
    
    id_complete_cases = complete.cases(f_data)
    if (!is_empty(which(id_complete_cases))) {
      v_data[id_complete_cases,] = predict(model_object,newdata = f_data)
    }
  }
  
  ## Output
  detach("package:randomForest",unload = TRUE)
  options(warn = warnstate_save)
  
  v_data = as.matrix(v_data)
  return(v_data)
  
}