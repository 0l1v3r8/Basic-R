#takes in a list of modules and returns a list of the module means
data = read.table("week2_data.txt", header=TRUE)

module_means = function(modules){
  length= length(modules[,1])
  avg = function(x)
  {
    return(sum(x)/length)
  }
  average = lapply(modules, avg)
  return(average)
}

#
questions = function(data)
{
  answers = list()
  
  #Q1
  id = data[1]
  chem = data[2]
  phy = data[3]
  mth = data[4]
  lit = data[5]
  sex = data[6]
  year = data[7]
  
  #Q2
  modules = data[,2:5]
  male_modules = modules[sex=="Male",]
  male_module_means= module_means(male_modules)
  print(male_module_means)
  female_modules = modules[sex=="Female",]
  female_module_means= module_means(female_modules)
  print(female_module_means)
  
  #Q3
  each_module_max = lapply(modules[,],max)
  print(paste("\n Q3",each_module_max))
  
  #Q4
  all_modules_max = max(modules)
  print(paste("\n Q4",all_modules_max))
  
  #Q5
  avg = function(mod){
    student_average_marks = c()
    for (i in 1:length(mod[,1])){
      student_total = 0
      for (j in 1:length(mod[i,])){
        student_total = student_total + mod[i,j]
      }
      student_average_marks[i] = student_total/length(mod[i,])
    }
    return(student_average_marks)
    }
  
  student_average_marks = avg(modules)
  print("\n Q5")
  print(student_average_marks)
  
  all_student_ranking = rank(student_average_marks)
  year1_rank = rank(student_average_marks[year==1])
  year2_rank = rank(student_average_marks[year==2])
  print(year1_rank)
  print(year2_rank)

  #Q6 standardised grades - mark/avg mark
  print("\n Q6")
  standardised_grades = modules/module_means(modules)
  standardised_avg_grades = avg(standardised_grades)
  print(standardised_avg_grades)
  
  year1std_rank = rank(standardised_avg_grades[year==1])
  year2std_rank = rank(standardised_avg_grades[year==2])
  print(year1std_rank)
  print(year2std_rank)
  
  
  #Q7 assuming the pass mark is 40
  passed_all = id[chem>=40 & phy>=40 & mth>=30 & lit>=40]
  pc_passed_all = length(passed_all)/length(id[,1])
  print(passed_all)
  print(pc_passed_all)
  
  #Q8
  var_stdpassed_all = var(standardised_avg_grades[id[,1]%in%passed_all])
  mean_stdpassed_all = mean(standardised_avg_grades[id[,1]%in%passed_all])
  var_passed_all = var(student_average_marks[id[,1]%in%passed_all])
  mean_passed_all = mean(student_average_marks[id[,1]%in%passed_all])
  print(var_stdpassed_all)
  print(mean_stdpassed_all)
  print(var_passed_all)
  print(mean_passed_all)
  
}

questions(data)
