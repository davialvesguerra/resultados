pacman::p_load('tidyverse','Desctools',"caTools","ggpubr","grid",'gridExtra','cowplot')

setwd('D:/materias/tcc/resultados')
options(scipen = 999)

data = read.csv('data.csv')

barras = function(data, var, nome_var){
  
  graf = data %>% 
    rename('var'=var) %>% 
    group_by(var, loan_condition) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% 
    na.omit() %>% 
    ggplot(aes(var, freq, fill = as.factor(loan_condition)))+
    labs(x=nome_var, y='Frequência')+
    geom_bar(stat="identity", position = "stack")+
    theme_bw()+
    theme(text = element_text(size=20),
          legend.position = 'none',
          legend.text=element_text(size=20),
          axis.text.x=element_text(colour="black"),
          axis.text.y=element_text(colour="black"))
  
  ggsave(paste0("../relatorio/imagens/resultados/",var,".pdf"), width = 158, height = 106, units = "mm")
  
  graf
}

legend = get_legend(barras(data, 'home_ownership', 'Tipo de residência'))   
as_ggplot(ggp_legend)

widthDetails(legend)
#18.8987805

heightDetails(legend)
#1.0484168

ggsave(paste0("images/","legenda",".pdf"), width = 11, height = 1.37, units = "cm")


boxplot_ = function(data, var, nome_var, remover_outiler=T){
  
 limites = NULL
 outlier = NULL
 
 if(remover_outiler){
   limites = quantile(data[[var]], c(0.1, 0.9))
   outlier = NA
 }
  
 graf = data %>% 
    rename('var'=var) %>% 
    ggplot(aes(x = as.factor(loan_condition), y = var))+
      geom_boxplot(outlier.shape = outlier)+
      scale_y_continuous(limits = limites)+
      labs(x = "", y = nome_var)+
      scale_x_discrete(labels=c("Ruim", "Bom"))+
      stat_summary( fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
      theme_bw()+
      theme(text = element_text(size=20),
         legend.text=element_text(size=20),
         axis.text.x=element_text(colour="black"),
         axis.text.y=element_text(colour="black"))
  
  ggsave(paste0("../relatorio/imagens/resultados/",var,".jpg"), width = 158, height = 106, units = "mm")
  
  graf
}

#loan_amount: valor do empréstimo concedido em dólares.
boxplot_(data, 'loan_amount','Valor do empréstimo')

#tempo de trabalho do tomador do empréstimo em anos
boxplot_(data, 'emp_length_int','Tempo de trabalho')

#recoveries: valor total de recuperações feitas pelo 
#banco após um empréstimo ter sido inadimplente.
boxplot_(data, 'recoveries', 'Valor de empréstimo \\ recuperado', F)
data %>% 
  filter(loan_condition == "Bad Loan") %>% 
  filter(recoveries < 5000) %>% 
  ggplot(aes(x = loan_condition, y = recoveries))+
    geom_boxplot()


#annual_inc: renda anual do tomador do empréstimo em dólares.
boxplot_(data, 'annual_inc','Renda anual do cliente', T)

#interest_rate: taxa de juros do empréstimo.
boxplot_(data, 'interest_rate','Taxa de interesse') #x

#dti: razão entre as dívidas do tomador do empréstimo e sua renda anual.
boxplot_(data, 'dti','Razão entre a dívida \n e o salário do cliente',T)

#total_pymnt: valor total pago pelo tomador do empréstimo.
boxplot_(data, "total_pymnt", 'Valor total pago')

#installment: valor da parcela mensal do empréstimo.
boxplot_(data, "installment",'Valor da parcela do empréstimo', T)

#total_rec_prncp: valor total do principal pago pelo tomador do empréstimo.
boxplot_(data, "total_rec_prncp",'Valor total pago', T)

#duracao_emprestimo_dias: duração do empréstimo em dias
boxplot_(data, 'duracao_emprestimo_dias', "Duração empréstimo",T)


###barras

#home_ownership: tipo de residência do tomador do empréstimo (própria, alugada, etc.)
barras(data, 'home_ownership', 'Tipo de residência')

#income_category: uma variável categórica que indica a categoria da variável annual_inc.
barras(data, 'income_category','Tipo de renda')

#term: duração do empréstimo em meses (36 ou 60 meses).
barras(data, 'term', "Duração do empréstimo")

#application_type: indica se o empréstimo foi solicitado individualmente ou 
#em conjunto com outro tomador.
barras(data, 'application_type','Condição do empréstimo')

#purpose: finalidade do empréstimo
barras(data, 'purpose','Finalidade do empréstimo')

#grade: nota de risco atribuída ao empréstimo (A, B, C, D, E ou F).
barras(data, 'grade','Risco do empréstimo')

#interest_payments: indica se os pagamentos do empréstimo são com juros simples ou compostos.
barras(data, 'interest_payments','Juros do empréstimo')

#region: região geográfica do país onde o tomador do empréstimo reside.
barras(data, 'region','Região')



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
              'total_pymnt','total_rec_prncp','recoveries','duracao_emprestimo_dias')


categoricas = c('home_ownership','income_category','application_type','purpose','interest_payments',
                'grade','installment','region')

lapply(categoricas,function(col){
  ContCoef(table(unlist(col), unlist(y),useNA = 'no'))
})

cor_num_vars = lapply(data[numericas],function(col){
  (cor.test(col, unlist(data$loan_condition_cat))$estimate)
})

as.data.frame(cor_num_vars) %>% t %>% view %>% xtable()


cor_cat_vars = lapply(data[categoricas],function(col){
  ContCoef(table(unlist(col), unlist(data$loan_condition_cat),useNA = 'no'))
})

barras = function(data, var, nome_var){
  
  graf = data %>% 
    rename('var'=var) %>% 
    group_by(loan_condition, var) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% 
    na.omit() %>% 
    ggplot(aes(as.factor(loan_condition), freq, fill = var)) +
    geom_bar(stat="identity", position = "stack")+
    scale_x_discrete(name="Situação do empréstimo",labels=c("Ruim", "Bom"))+
    labs(x = '', y = "Frequência")+
    scale_fill_discrete(name=nome_var)+
    theme_bw()+
    theme(legend.position = 'bottom',
          text = element_text(size=20),
          legend.text=element_text(size=20),
          axis.text.x=element_text(colour="black"),
          axis.text.y=element_text(colour="black"))
  
  ggsave(paste0("../relatorio/imagens/resultados/",var,".pdf"), width = 158, height = 106, units = "mm")
  
  graf
}