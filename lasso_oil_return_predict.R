print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("This is the main file to run the project by L.Zangari")

print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Assemble data...")
source("weekly_study.R")

print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Dealing with monthly data and transformation...")
source("monthly_data_study_and_transformation.R")

print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Dealing with monthly data statistics...")
source("monthly_data_statistic.R")

print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Dealing with monthly data modeling...")
source("modelling.R")

print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("DONE!")