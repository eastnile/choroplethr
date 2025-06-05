devtools::load_all()
df_state_demographics = get_state_demographics()
save(df_state_demographics, file = 'data/df_state_demographics.rda')
data(df_state_demographics)
choroplethr::df_state_demographics
