
library(Rilostat)

# Employment by sex and occupation (ISCO level 2)
df <- get_ilostat(id = "EMP_TEMP_SEX_OC2_NB_A",
                  filters = list(sex = c("SEX_M", "SEX_F")))

write.csv(df, "ilo_employment_sex_occupation.csv", row.names = FALSE)
