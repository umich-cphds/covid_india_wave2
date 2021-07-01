data 
{
  int <lower=1> M;               // number of regions
  int <lower=1> N0;              // number of days in init period
  int<lower=1> N[M];             // number of days in entire period
  int<lower=1> N2;               // max number of days across regions
  int deaths[N2, M];             // observed deaths 
  matrix[N2, M] f;               // infection-to-death distribution
  int ll_len;                    // length of indices for likelihood
  int ll_idxs[ll_len];           // indices for likelihood
  real pop[M];                   // population count
  int W;                         // maximum number of weeks
  int week_index[M,N2];
  real SI[N2];                   // generation time distribution
  real WI[N2];                   // waning immunity distribution
  real AR_SD_MEAN;
  int T2;                        // starting index of the second strain
  int phylo_N_len; 
  int phylo_N[phylo_N_len]; 
  int phylo_PSamples[phylo_N_len];
  int phylo_NSamples[phylo_N_len];
  matrix[N2, M] PCR_pos_prob;     // PCR positivity distribution
  matrix[N2, M] seroconv_cdf;     // cumululative seroconverted distribution
  real <lower=0,upper=1> ur;
  real ifr1_prior[M];
  real <lower=0,upper=1> cross_protection;
}

transformed data {
  vector[N2] SI_rev; // SI in reverse order
  vector[N2] WI_rev; // waning immunity in revered order
  matrix[N2, M] seroconv_cdf_rev; // cumululative seroconverted distribution in reverse order
  matrix[N2, M] PCR_pos_prob_rev; // PCR positivity distribution in reverse order
  matrix[N2, M] f_rev; // infection-to-death distribution in reversed order
  real <lower=0> ifr1[M] = ifr1_prior;
  for(i in 1:N2){
    SI_rev[i] = SI[N2-i+1];
    WI_rev[i] = WI[N2-i+1];
    seroconv_cdf_rev[i,:] = seroconv_cdf[N2-i+1,:];
    PCR_pos_prob_rev[i,:] = PCR_pos_prob[N2-i+1,:];
    f_rev[i,:] = f[N2-i+1,:];
  }
}

parameters 
{
  real<lower=0> R_difference; // relative transmissibility of strain 2 compared to strain 1 
  real<lower=1> y_v1[M]; // infections in intial period strain 1
  real<lower=1> y_v2[M]; // infections in intial period strain 2
  real<lower=0> phi;
  real<lower=0> tau; // NegBin dispersion
  real <lower=0> RR[M]; // relative risk
  matrix[W+1,M] weekly_effect;
  real<lower=0, upper=1> weekly_rho;
  real<lower=0, upper=1> weekly_rho1;
  real<lower=0> weekly_sd;
}


transformed parameters 
{
  matrix[N2,M] cumm_sum = rep_matrix(0,N2,M);
  matrix[N2,M] prediction = rep_matrix(0,N2,M);
  matrix[N2,M] E_deaths  = rep_matrix(0,N2,M);
  matrix[N2,M] immune = rep_matrix(0,N2,M);
  matrix[N2,M] prediction_v1 = rep_matrix(0,N2,M);
  matrix[N2,M] prediction_v2 = rep_matrix(0,N2,M);
  matrix[N2,M] E_deaths_v1  = rep_matrix(0,N2,M);
  matrix[N2,M] E_deaths_v2  = rep_matrix(0,N2,M);
  matrix[N2,M] Rt_v1 = rep_matrix(0,N2,M);
  matrix[N2,M] Rt_v2 = rep_matrix(0,N2,M);
  matrix[N2,M] Rt_adj_immune_v1 = rep_matrix(0,N2,M);
  matrix[N2,M] Rt_adj_immune_v2 = rep_matrix(0,N2,M);
  matrix[N2,M] cumm_sum_v1 = rep_matrix(0,N2,M);
  matrix[N2,M] cumm_sum_v2 = rep_matrix(0,N2,M);
  matrix[N2,M] immune_v1 = rep_matrix(0,N2,M);
  matrix[N2,M] immune_v2 = rep_matrix(0,N2,M);
  matrix[N2,M] alpha_sus1 = rep_matrix(0,N2,M);
  matrix[N2,M] alpha_sus2 = rep_matrix(0,N2,M);
  matrix[N2,M] n1 = rep_matrix(0,N2,M);
  matrix[N2,M] n2 = rep_matrix(0,N2,M);
  matrix[N2,M] pcr_pos_v1 = rep_matrix(0, N2, M);
  matrix[N2,M] pcr_pos_v2 = rep_matrix(0, N2, M);
  matrix[N2,M] E_fraction = rep_matrix(0,N2,M);
  real <lower=0> ifr2[M];
  
  for (m in 1:M)
  {
    // new cases during initial period
    prediction_v1[1:N0,m] = rep_vector(y_v1[m],N0); 
    prediction_v2[T2,m] = y_v2[m]; 
    
    // reproduction numbers
    Rt_v1[,m] = 3.28 * 2 * inv_logit( - weekly_effect[week_index[m],m] );
    Rt_v2[T2:N2,m] = Rt_v1[T2:N2,m] * R_difference; 
    
    // adjusted reproduction number during initial period
    Rt_adj_immune_v1[1:N0,m] = Rt_v1[1:N0,m]; 
    
    for (i in (N0+1):N2) 
    {
      // strain 1 //
      // number of infectious 
      real convolution_v1 = dot_product(sub_col(prediction_v1, 1, m, i-1), tail(SI_rev, i-1));
      // strain 2 //
      real convolution_v2 = 0;
      if ( i > T2 ) 
      {
        // number of infectious 
        convolution_v2 = dot_product(sub_col(prediction_v2, 1, m, i-1), tail(SI_rev, i-1));
      }
      // number of immunes, cross-immunity susceptible and susceptible depletion
      immune_v1[i,m] = dot_product(sub_col(prediction_v1, 1, m, i-1), tail(WI_rev, i-1));
      alpha_sus1[i,m] = (1 - cross_protection) * immune_v1[i,m] / pop[m];
      if ( i > T2 ) 
      {
        // number of immunes, cross_protection-immunity susceptible and susceptible depletion
        immune_v2[i,m] = dot_product(sub_col(prediction_v2, 1, m, i-1), tail(WI_rev, i-1));
        alpha_sus2[i,m] = (1 - cross_protection) * immune_v2[i,m] / pop[m];
      }
      n1[i,m] = immune_v1[i,m] + cross_protection * immune_v2[i,m] * ( 1 - alpha_sus1[i,m] );
      
      // adjusted reproduction number and new cases
      Rt_adj_immune_v1[i,m] = ( 1 - n1[i,m] / pop[m]) * Rt_v1[i,m];
      prediction_v1[i, m] = Rt_adj_immune_v1[i,m] * convolution_v1;
      if ( i > T2 ) 
      {
        n2[i,m] = immune_v2[i,m] + cross_protection * immune_v1[i,m] * ( 1 - alpha_sus2[i,m] );
        
        // adjusted reproduction number and new cases
        Rt_adj_immune_v2[i,m] = ( 1 - n2[i,m] / pop[m]) * Rt_v2[i,m]; 
        prediction_v2[i, m] = Rt_adj_immune_v2[i,m] * convolution_v2;
      }
      
    }
    
    // ensure positive deaths during initial period
    E_deaths_v1[1, m]= 1e-15 * prediction_v1[1,m];
    E_deaths_v2[1, m]= 1e-15 * prediction_v2[1,m];
    
    // ifr of strain 2
    ifr2[m] = ifr1[m] * RR[m];
    
    for (i in 2:N2)
    {
      // strain 1 //
      // number of positive PCR 
      pcr_pos_v1[i,m] = dot_product(sub_col(prediction_v1, 1, m, i-1), tail(PCR_pos_prob_rev[:,m], i-1));
      
      // new deaths
      E_deaths_v1[i,m] = dot_product(sub_col(prediction_v1, 1, m, i-1), tail(f_rev[:,m], i-1));
      E_deaths_v1[i,m] *= ifr1[m];
      
      
      // strain 2 //
      if (i > T2)  
      {
        // number of positive PCR 
        pcr_pos_v2[i,m] = dot_product(sub_col(prediction_v2, 1, m, i-1), tail(PCR_pos_prob_rev[:,m], i-1));
        
        // new deaths
        E_deaths_v2[i,m] = dot_product(sub_col(prediction_v2, 1, m, i-1), tail(f_rev[:,m], i-1));
        E_deaths_v2[i,m] *= ifr2[m];
        
      }
    }
    
    // sum of new cases, immunes and new deaths over the strains
    prediction[:, m] = prediction_v1[:, m] + prediction_v2[:, m]; 
    immune[:, m] = immune_v1[:, m] + immune_v2[:, m];
    E_deaths[:,m] = E_deaths_v1[:,m] + E_deaths_v2[:,m];
    
    // sum of cumulative cases over the strains
    cumm_sum_v1[:,m] = cumulative_sum(prediction_v1[:,m]);
    cumm_sum_v2[:,m] = cumulative_sum(prediction_v2[:,m]);
    cumm_sum[:,m] = cumm_sum_v1[:,m] + cumm_sum_v2[:,m];
    
    // infection ratio
    E_fraction[1,m] = 0.0;
    E_fraction[2:N2,m] = pcr_pos_v2[2:N2,m]./(pcr_pos_v1[2:N2,m] + pcr_pos_v2[2:N2,m]);	
  }
}


model 
{
  R_difference ~ normal(1,1);
  tau ~ exponential(0.03);
  RR ~ lognormal(0,0.5);
  phi ~ normal(0,5);
  y_v1 ~ exponential(1/tau);
  y_v2 ~ normal(0,1);
  weekly_sd ~ normal(0, AR_SD_MEAN);
  weekly_rho ~ normal(0.8, 0.05);
  weekly_rho1 ~ normal(0.1, 0.05);
  for (m in 1:M)
  {
    weekly_effect[3:(W+1), m] ~ normal( weekly_effect[2:W,m]* weekly_rho + weekly_effect[1:(W-1),m]* weekly_rho1,weekly_sd *sqrt(1-pow(weekly_rho,2)-pow(weekly_rho1,2) - 2 * pow(weekly_rho,2) * weekly_rho1/(1-weekly_rho1)));
  }
  weekly_effect[2,:] ~ normal(0,weekly_sd *sqrt(1-pow(weekly_rho,2)-pow(weekly_rho1,2) - 2 * pow(weekly_rho,2) * weekly_rho1/(1-weekly_rho1)));
  weekly_effect[1,:] ~ normal(0, 0.01);
  
  for(m in 1:M)
  {
    deaths[ll_idxs, m] ~ neg_binomial_2(E_deaths[ll_idxs, m] * (1-ur), phi);
    for ( i in 1:phylo_N_len )
    {
      phylo_PSamples[i] ~ binomial(phylo_NSamples[i]+phylo_PSamples[i],E_fraction[phylo_N[i],m]);
    }
  }
}

