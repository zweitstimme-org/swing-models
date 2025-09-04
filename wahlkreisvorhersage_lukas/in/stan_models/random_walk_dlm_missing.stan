functions {
    matrix build_F(int K) {
        matrix[K, K] F;  // Declare a K x K matrix
        F = diag_matrix(rep_vector(1, K));  // Create a diagonal matrix with 1s on the diagonal
        return F;
    }
    matrix build_G(int K) {
        matrix[K, K] G;  // Declare a K x K matrix
        G = diag_matrix(rep_vector(1, K));  // Create a diagonal matrix with 1s on the diagonal
        return G;
    }
    matrix build_V(vector noise_theta, int K) {
        matrix[K, K] V;  // Declare a K x K matrix
        vector[K] noise_variances = square(noise_theta[1:K]);  // Square the first K elements of noise_theta
        V = diag_matrix(noise_variances);  // Create a diagonal matrix with noise variances
        return V;
    }
    matrix build_W(vector noise_theta, int K) {
        matrix[K, K] W;  // Declare a K x K matrix
        vector[K] noise_variances = square(noise_theta[1:K]);  // Square the first K elements of noise_theta
        W = diag_matrix(noise_variances);  // Create a diagonal matrix with noise variances
        return W;
    }
}
    
data {
    int N;                    // Number of observations
    int T;                    // Number of time steps
    int K;                    // Dimensions
    int time[N];              // Time indices for observations
    matrix[K,N] y;            // Observed data
    int update[T];            // Missing indicator: 0 = missing, 1 = observed
    matrix[K,K] C0;           // Initial covariance
    vector[K] m0;             // Initial mean
    int<lower=0, upper=1> run_backward_sampling;  // Boolean flag (0 = false, 1 = true)
}

parameters {
    vector<lower=0>[K] meas_var;  // Process and observation noise parameters
    vector<lower=0>[K] evo_var;  // Process and observation noise parameters
}

transformed parameters {
    // Paramters for Kalmann Filter
    array[T] vector[K] f;
    array[T] matrix[K,K] Q;
    matrix[K, K] F = build_F(K);  // Latent States
    matrix[K, K] G = build_G(K);  // Measurment Equation
    matrix[K, K] V = build_V(meas_var, K); // Error Variance
    matrix[K, K] W = build_W(evo_var, K); // Evaluation Variance
    
    // For Kalmann Filter
    matrix[K, K] Kt; 
    vector[K] et;
    vector[K] at;
    matrix[K, K] Rt;
    array[T+1] vector[K] m; 
    array[T+1] matrix[K, K] C;

    m[1] = m0;
    C[1] = C0;

    for (t in 1:T) {
        // Predicted state
        // Petris et al. p.53
        at = G * m[t];
        Rt = G* C[t] * G' + W;

        // Predicted observations
        f[t] = F * at;  
        Q[t] = F * Rt * F' + V;  

        if (update[t] != 0) {
            // Update with Kalman filter
            Kt = (Rt * F')/ Q[t];
            et = y[,update[t]] - f[t];
            m[t+1] = at + Kt * et;
            C[t+1] = Rt - Kt * F * Rt;
        } else {
            // Use predictions as current estimates
            m[t+1] = at;
            C[t+1] = Rt;
        }
    }
}
model {
    for (i in 1:N) {
       y[,i] ~ multi_normal(f[time[i]], Q[time[i]]);
    }
}

generated quantities {
  
    if (run_backward_sampling) {
      
      array[T] vector[K] theta;  // Smoothed latent states
      matrix[K, K] Rt1;
      matrix[K, K] Ht;
      vector[K] ht;
      int t;
  
      // Initialize theta_T
      theta[T] = multi_normal_rng(m[T+1], C[T+1]);  // Sample final state
  
      // Backward sampling
      for (i in 1:(T-1)) {
          t = T - i;  // Backward index
  
          // Compute R_t(t+1)
          Rt1 = G * C[t+1] * G' + W;
  
          // Compute smoothed covariance
          ht = m[t+1] + C[t+1] * (G' / Rt1) * (theta[t+1] - G * m[t+1]);
  
          // Compute smoothed mean
          Ht = C[t+1] - C[t+1] * (G' / Rt1) * G * C[t+1];
          
          // Sample smoothed state
          theta[t] = multi_normal_rng(ht, Ht);
      }
    }
    
}


