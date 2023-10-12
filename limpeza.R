pacman::p_load('tidyverse','Desctools',"caTools","ggpubr","grid",'gridExtra','cowplot')

setwd('D:/materias/tcc/resultados')
options(scipen = 999)

data = read.csv('loan_final313.csv')

fim_emprestimo_string = paste0("0", data$final_d)
fim_emprestimo = as.POSIXct(fim_emprestimo_string, format="%d%m%Y", tz="UTC")
data['fim_emprestimo'] = fim_emprestimo
data['inicio_emprestimo'] = as.POSIXct(data$issue_d, format="%d/%m/%Y", tz="UTC")
data['duracao_emprestimo_dias'] = (data['fim_emprestimo'] - data['inicio_emprestimo'])/(60*60*24)
data['duracao_emprestimo_dias'] = sapply(data['duracao_emprestimo_dias'], as.numeric)

"id:     x n
year:    x n
issue_d: x n
final_d: x n
emp_length_int: x 
home_ownership, home_ownership_cat: x
income_category, income_cat:        x
annual_inc: x
loan_amount: x
term, term_cat: x
application_type, application_type_cat: x (talvez retirar, já que somente uma label apresenta valores)
purpose, purpose_cat: x
interest_payments, interest_payment_cat: x
loan_condition, loan_condition_cat: x
interest_rate: x
grade, grade_cat: x
dti: x
total_pymnt: x
total_rec_prncp: x
recoveries: x
installment: x
region: x

17 variáveis 
+ 1 variável duração do empréstimo"

numericas = c('emp_length_int', 'annual_inc','annual_inc','loan_amount','interest_rate','dti',
              'total_pymnt','total_rec_prncp','recoveries','duracao_emprestimo_dias','installment')


categoricas = c('home_ownership','income_category','application_type','purpose','interest_payments',
                'grade','region')

utils_cols = c('emp_length_int', 'home_ownership','income_category',
               'annual_inc','loan_amount','term','application_type','purpose',
               'interest_payments','interest_rate','grade','dti','total_pymnt',
               'total_rec_prncp','recoveries','installment','region','loan_condition','duracao_emprestimo_dias')

new_data = data[utils_cols]

new_data = new_data %>% 
  mutate(
    home_ownership = case_when(
    (home_ownership == "RENT") ~ "Aluguel",
    (home_ownership == "OWN") ~ "Própria",
    (home_ownership == "MORTGAGE") ~ "Hipotecada"
    ),
    
    income_category = case_when(
      (income_category == "Low") ~ "Baixa",
      (income_category == "Medium") ~ "Média",
      (income_category == "High") ~ "Alta"
    ),
    
    application_type = case_when(
      (application_type == "INDIVIDUAL") ~ "Individual",
      (application_type == "JOINT") ~ "Conjunto"
    ),
    
    term = case_when(
      (term == " 36 months") ~ "36 meses",
      (term == " 60 months") ~ "60 meses"
    ),
    
    purpose = case_when(
      (purpose == "credit_card") ~ "Cartão de crédito",
      (purpose == "debt_consolidation") ~ "Término do débito",
      (purpose == "home_improvement") ~ "Reforma da casa",
      (purpose == "other") ~ "Outros",
      (purpose == "major_purchase") ~ "Compras"
    )
    
    
  ) 


write.csv(x = new_data, file = 'data.csv')


