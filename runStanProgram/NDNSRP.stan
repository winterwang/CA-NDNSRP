data {
  int N;
  int I;  
  int<lower = 0, upper = 1> DM4cat_1[N]; 
  int<lower = 0, upper = 1> DM4cat_2[N]; 
  int<lower = 0, upper = 1> DM4cat_3[N];
  int<lower = 0, upper = 1> Agecat_1[N];
  int<lower = 0, upper = 1> Agecat_2[N];
  int<lower = 0, upper = 1> Agecat_3[N];
  int<lower = 0, upper = 1> Agecat_4[N];
  int<lower = 0, upper = 1> Agecat_5[N];
  int<lower = 0, upper = 1> nssec8_1[N];
  int<lower = 0, upper = 1> nssec8_2[N];
  int<lower = 0, upper = 1> nssec8_3[N];
  int<lower = 0, upper = 1> nssec8_4[N];
  int<lower = 0, upper = 1> nssec8_5[N];
  int<lower = 0, upper = 1> nssec8_6[N];
  int<lower = 0, upper = 1> nssec8_8[N];
  int<lower = 1, upper = N> seriali[I];
  int<lower = 0, upper = 1> Time3g_Afternoon[I];
  int<lower = 0, upper = 1> Time3g_Evening[I];
  int<lower = 0, upper = 1> YesPudding[I];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b[18];
  real b_P[N];
  real<lower = 0> s_P;
}

transformed parameters {
  real x_P[N];
  real x_J[I];
  real x[I];
  real q[I];
  for (n in 1:N)
    x_P[n] = b[2]*DM4cat_1[n] + b[3]*DM4cat_2[n] + b[4]*DM4cat_3[n] + 
             b[5]*Agecat_1[n] + b[6]*Agecat_2[n] + b[7]*Agecat_3[n] + 
             b[8]*Agecat_4[n] + b[9]*Agecat_5[n] + b[10]*nssec8_1[n] + 
             b[11]*nssec8_2[n] + b[12]*nssec8_3[n] + b[13]*nssec8_4[n] + 
             b[14]*nssec8_5[n] + b[15]*nssec8_6[n] + b[16]*nssec8_8[n] + 
             b_P[n];
  for (i in 1:I) {
    x_J[i] = b[17]*Time3g_Afternoon[i] + b[18]*Time3g_Evening[i];
    x[i]   = b[1] + x_P[seriali[i]] + x_J[i];
    q[i]   = inv_logit(x[i]);
  }
}

model{
  for (n in 1:N)
    b_P[n] ~ normal(0, s_P);
  for (i in 1:I)
    YesPudding[i]  ~ bernoulli(q[i]);
}


generated quantities{
  real OR_Eve_vs_Morn; 
  real OR_Eve_vs_After;
  OR_Eve_vs_Morn = exp(b[18]);
  OR_Eve_vs_After = exp(b[18] - b[17]);
}
