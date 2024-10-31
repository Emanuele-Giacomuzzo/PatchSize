#Construct the full model by trying different optimisers until you find the one that works

try.different.optimizers.metaecos = function(data,
                                             formula){
  
  # List of optimizers and methods to try
  
  optimizers <- list(
    list(optimizer = "bobyqa"),                                       # Boundary optimization
    list(optimizer = "optimx", method = "BFGS"),                    # Quasi-Newton method
    list(optimizer = "optimx", method = "L-BFGS-B"),                # Limited-memory version of BFGS
    list(optimizer = "nlminbwrap"),                                  # Non-linear minimization with bounds
    list(optimizer = "Nelder_Mead"),                                 # Nelder-Mead simplex method
    list(optimizer = "nloptwrap", method = "NLOPT_LN_NELDERMEAD"),  # NLOPT Nelder-Mead method
    list(optimizer = "nloptwrap", method = "NLOPT_LN_BOBYQA"),      # NLOPT Bobyqa method
    list(optimizer = "nloptwrap", method = "NLOPT_LN_NEWUOA"),      # NLOPT Newuoa method
    list(optimizer = "nloptwrap", method = "NLOPT_GD_STEEPEST_DESCENT"),  # NLOPT Steepest Descent
    list(optimizer = "nloptwrap", method = "NLOPT_GD_CG"),          # NLOPT Gradient Descent with Conjugate Gradient
    list(optimizer = "nloptwrap", method = "NLOPT_LD_MMA"),         # Method of Moving Asymptotes
    list(optimizer = "optimx", method = "CG"),                       # Conjugate Gradient method
    list(optimizer = "optimx", method = "SANN"),                     # Simulated Annealing
    list(optimizer = "optimx", method = "Powell"),                   # Powell's method
    list(optimizer = "nloptwrap", method = "NLOPT_LN_SBPLX"),       # Gradient-free optimization using Nelder-Mead
    list(optimizer = "nloptwrap", method = "NLOPT_LN_COBYLA"),      # Constrained optimization by linear approximation
    list(optimizer = "nloptwrap", method = "NLOPT_LN_PRAXIS"),      # Derivative-free optimization
    list(optimizer = "nloptwrap", method = "NLOPT_LN_BOBYQA"),      # Bound Optimization BY Quadratic Approximation
    list(optimizer = "optimx", method = "tnewton"),                # Truncated Newton method
    list(optimizer = "optim", method = "SANN"),                      # Simulated Annealing
    list(optimizer = "optim", method = "Nelder-Mead"),              # Nelder-Mead method
    list(optimizer = "nloptwrap", method = "NLOPT_LD_TNEWTON"),     # NLOPT TNewton method
    list(optimizer = "nloptwrap", method = "NLOPT_LD_TNEWTON_RESTART"),  # Restart version of TNewton
    list(optimizer = "nloptwrap", method = "NLOPT_LD_CCSAQ")        # NLOPT Constrained optimization by linear approximation
  )
  
  # Initialize a variable to store whether a warning occurs
  
  warning_flag <- "no"
  successful_model <- NULL  # Will store the successfully fitted model
  optimizer_used <- NULL    # Will store the optimizer that worked
  
  # Loop through each optimizer
  
  for (opt in optimizers) {
    
    warning_flag <- "no"  # Reset warning flag before each attempt
    
    # Try to fit the model with the current optimizer
    
    successful_model <- tryCatch({
      
      # Dynamically choose optimizer and method (if applicable)
      
      if (!is.null(opt$method)) {
        lmer(formula,
             data = data,
             REML = FALSE,
             control = lmerControl(optimizer = opt$optimizer, optCtrl = list(method = opt$method)))
      } else {
        lmer(formula,
             data = data,
             REML = FALSE,
             control = lmerControl(optimizer = opt$optimizer))
      }
      
    }, warning = function(w) {
      
      # If a warning occurs, set the flag to "yes"
      
      warning_flag <<- "yes"
      message("Warning with optimizer: ", opt$optimizer, if(!is.null(opt$method)) paste0(" (", opt$method, ")"))
      
      # Return NULL to indicate the model failed with this optimizer
      
      return(NULL)
      
    })
    
    # If the model was fitted successfully (i.e., no warning), break out of the loop
    
    if (!is.null(successful_model)) {
      
      optimizer_used <- opt  # Store the successful optimizer
      
      break
      
    }
  }
  
  # Check if a model was successfully fitted and display the optimizer used
  
  if (!is.null(successful_model)) {
    
    print(paste("Model successfully fitted with optimizer:", optimizer_used$optimizer, 
                if(!is.null(optimizer_used$method)) paste0(" (", optimizer_used$method, ")")))
    
  } else {
    
    print("Model fitting failed with all optimizers.")
    
  }
  
  return(successful_model)
}
