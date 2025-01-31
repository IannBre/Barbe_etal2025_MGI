-------
title: "Non-lethal management of Felis catus on a protected island: Outcomes of a successful ongoing socio-ecological strategy in Martín García Island Reserve, Argentina"
author: "Ian Barbe, Lucía Inés Rodríguez-Planes, María del Rosario Jacoby, Andrea Szmelc, Gloria Domínguez, Nazareno Asín, María Eugenia Cueto, María Marcela Orozco"
CA_email: "marcelaorozco.vet@gmail.com"
year: 2025
about: "Interviews analisys"
-------

#Table 5. Need to continue the project data###############

#Frequency of affirmative responses related to project continuation on MGI
#Table5 matrix (data from Table5_data.xlsx)
Table5 <- matrix(c(24, 2, 50, 1), nrow = 2, byrow = TRUE)
#chi-square test
projectchisq <- prop.test(Table5)
print(projectchisq)


#Fig6. cat abundance perceptons interviews #############################################################
#Fig.6. Community perceptions of cat abundance at the beginning of the project, by category (Surrounding houses and Island) and year (2019-2021)-------------

#island (data from Fig6. Island data.xlsx) -------------------------------
#2019 
island_2019 <- matrix(c(11, 3, 14), nrow = 1, byrow = TRUE)
colnames(island_2019) <- c("lower", "higher", "total")
# lower vs. higher
result_chi_cuad_island_2019 <- chisq.test(island_2019)
result_chi_cuad_island_2019$expected
print(result_chi_cuad_island_2019)

#island 2021
island_2021 <- matrix(c(20, 7, 27), nrow = 1, byrow = TRUE)
colnames(island_2021) <- c("lower", "higher", "total")
# lower vs. higher
result_chi_cuad_island_2021 <- chisq.test(island_2021)
result_chi_cuad_island_2021$expected
print(result_chi_cuad_island_2021)

#Surrounding houses (data from Fig6. Surrounding houses data.xlsx) ------------------
#2019
surroundings_2019 <- matrix(c(18, 2, 20), nrow = 1, byrow = TRUE)
colnames(surroundings_2019) <- c("lower", "higher", "total")
# lower vs. higher
result_chi_cuad_surroundings_2019 <- chisq.test(surroundings_2019)
print(result_chi_cuad_surroundings_2019)

#2021
surroundings_2021 <- matrix(c(26, 5, 31), nrow = 1, byrow = TRUE)
colnames(surroundings_2021) <- c("lower", "higher", "total")
# lower vs. higher
result_chi_cuad_surroundings_2021 <- chisq.test(surroundings_2021)
print(result_chi_cuad_surroundings_2021)

