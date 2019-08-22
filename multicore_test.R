for (row in 1:nrow(formulaList)) {  
  modelName <-  formulaList[row, "LongName"]
  Density <- formulaList[row, "Density"]
  group <- formulaList[row, "g0"]
  sig <- formulaList[row, "s"]
  
  modelNameSave <- gsub("\\+", "_", glue("{modelName}"))
  
  print(glue("{modelNameSave} <- list(Capthist,  model = list(D ~ {Density}, g0 ~ {group},sigma ~ {sig}),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')"))
  

}


for (row in 1:nrow(formulaList)) {  
  modelName <-  formulaList[row, "LongName"]
  modelNameSave <- gsub("\\+", "_", glue("{modelName}"))
  print(glue("'{modelNameSave}',\n"))
}

Model01_1_1_1 <- list(Capthist,  model = list(D ~ 1, g0 ~ 1,sigma ~ 1),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model02_1_1_h2 <- list(Capthist,  model = list(D ~ 1, g0 ~ 1,sigma ~ h2),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model03_1_1_b <- list(Capthist,  model = list(D ~ 1, g0 ~ 1,sigma ~ b),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model04_1_1_B <- list(Capthist,  model = list(D ~ 1, g0 ~ 1,sigma ~ B),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model05_1_1_h2_b <- list(Capthist,  model = list(D ~ 1, g0 ~ 1,sigma ~ h2+b),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model06_1_1_h2_B <- list(Capthist,  model = list(D ~ 1, g0 ~ 1,sigma ~ h2+B),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model07_1_h2_1 <- list(Capthist,  model = list(D ~ 1, g0 ~ h2,sigma ~ 1),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model08_1_h2_h2 <- list(Capthist,  model = list(D ~ 1, g0 ~ h2,sigma ~ h2),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model09_1_h2_b <- list(Capthist,  model = list(D ~ 1, g0 ~ h2,sigma ~ b),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model10_1_h2_B <- list(Capthist,  model = list(D ~ 1, g0 ~ h2,sigma ~ B),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model11_1_h2_h2_b <- list(Capthist,  model = list(D ~ 1, g0 ~ h2,sigma ~ h2+b),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model12_1_h2_h2_B <- list(Capthist,  model = list(D ~ 1, g0 ~ h2,sigma ~ h2+B),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model13_1_b_1 <- list(Capthist,  model = list(D ~ 1, g0 ~ b,sigma ~ 1),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model14_1_b_h2 <- list(Capthist,  model = list(D ~ 1, g0 ~ b,sigma ~ h2),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model15_1_b_b <- list(Capthist,  model = list(D ~ 1, g0 ~ b,sigma ~ b),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model16_1_b_h2_b <- list(Capthist,  model = list(D ~ 1, g0 ~ b,sigma ~ h2+b),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model17_1_B_1 <- list(Capthist,  model = list(D ~ 1, g0 ~ B,sigma ~ 1),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model18_1_B_h2 <- list(Capthist,  model = list(D ~ 1, g0 ~ B,sigma ~ h2),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model19_1_B_B <- list(Capthist,  model = list(D ~ 1, g0 ~ B,sigma ~ B),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model20_1_B_h2_B <- list(Capthist,  model = list(D ~ 1, g0 ~ B,sigma ~ h2+B),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model21_1_h2_b_1 <- list(Capthist,  model = list(D ~ 1, g0 ~ h2+b,sigma ~ 1),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model22_1_h2_b_h2 <- list(Capthist,  model = list(D ~ 1, g0 ~ h2+b,sigma ~ h2),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model23_1_h2_b_b <- list(Capthist,  model = list(D ~ 1, g0 ~ h2+b,sigma ~ b),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model24_1_h2_b_h2_b <- list(Capthist,  model = list(D ~ 1, g0 ~ h2+b,sigma ~ h2+b),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model25_1_h2_B_1 <- list(Capthist,  model = list(D ~ 1, g0 ~ h2+B,sigma ~ 1),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model26_1_h2_B_h2 <- list(Capthist,  model = list(D ~ 1, g0 ~ h2+B,sigma ~ h2),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model27_1_h2_B_B <- list(Capthist,  model = list(D ~ 1, g0 ~ h2+B,sigma ~ B),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')
Model28_1_h2_B_h2_B <- list(Capthist,  model = list(D ~ 1, g0 ~ h2+B,sigma ~ h2+B),  trace = TRUE, mask=Mask, detectfn=0,hcov='sex')

fits <- par.secr.fit (c('Model01_1_1_1',
                        'Model02_1_1_h2',
                        'Model03_1_1_b',
                        'Model04_1_1_B',
                        'Model05_1_1_h2_b',
                        'Model06_1_1_h2_B',
                        'Model07_1_h2_1',
                        'Model08_1_h2_h2',
                        'Model09_1_h2_b',
                        'Model10_1_h2_B',
                        'Model11_1_h2_h2_b',
                        'Model12_1_h2_h2_B',
                        'Model13_1_b_1',
                        'Model14_1_b_h2',
                        'Model15_1_b_b',
                        'Model16_1_b_h2_b',
                        'Model17_1_B_1',
                        'Model18_1_B_h2',
                        'Model19_1_B_B',
                        'Model20_1_B_h2_B',
                        'Model21_1_h2_b_1',
                        'Model22_1_h2_b_h2',
                        'Model23_1_h2_b_b',
                        'Model24_1_h2_b_h2_b',
                        'Model25_1_h2_B_1',
                        'Model26_1_h2_B_h2',
                        'Model27_1_h2_B_B',
                        'Model28_1_h2_B_h2_B'), ncores = 6)
AIC(fits)