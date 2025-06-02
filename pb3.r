# a) Reprezentare grafica scatter plot Girth (diametru) vs Volume (volum)

# Datele sunt deja incluse in R
data(trees)

# Scatter plot cu titlu si culoare
plot(trees$Girth, trees$Volume,
     main = "Grafic scatter: Diametru (Girth) vs Volum (Volume) al arborilor",
     xlab = "Diametru (Girth, inch)",
     ylab = "Volum (Volume, cubic feet)",
     col = "forestgreen",
     pch = 19)  # puncte pline

# b) Calculam coeficientul de corelatie Pearson si testam semnificatia acestuia

cor_coef <- cor(trees$Girth, trees$Volume)
cor_test <- cor.test(trees$Girth, trees$Volume)

cat("Coeficientul de corelatie Pearson intre Girth si Volume este:", round(cor_coef, 3), "\n")
cat("Valoarea p pentru testul de semnificatie a corelatiei:", cor_test$p.value, "\n")

if(cor_test$p.value < 0.05){
  cat("Interpretare: Corelatie semnificativa statistic la nivelul esantionului (p < 0.05).\n")
  if(cor_coef > 0){
    cat("Tipul corelatiei: pozitiva — pe masura ce diametrul creste, volumul creste.\n\n")
  } else {
    cat("Tipul corelatiei: negativa.\n\n")
  }
} else {
  cat("Interpretare: Corelatie nesemnificativa statistic la nivelul esantionului.\n\n")
}

cat("Justificare pentru populatie:\n")
cat("Testul de semnificatie al corelatiei sugereaza ca, daca datele sunt reprezentative si conditiile sunt indeplinite,\n")
cat("exista o corelatie semnificativa si in populatia din care provine esantionul.\n\n")

# c) Model de regresie liniara pentru estimarea volumului arborilor in functie de diametru

model <- lm(Volume ~ Girth, data = trees)
summary_model <- summary(model)

cat("Rezumatul modelului de regresie liniara:\n")
print(summary_model)

cat("\nEcuația regresiei liniare pentru estimarea volumului arborilor este:\n")
cat("Volume = ", round(coef(model)[1], 3), " + ", round(coef(model)[2], 3), " * Girth\n\n", sep="")

# Analiza rezultatelor regresiei:
cat("Analiza modelului:\n")
cat("- Interceptul (", round(coef(model)[1], 3), ") reprezinta volumul estimat cand diametrul este 0.\n", sep = "")
cat("- Panta (", round(coef(model)[2], 3), ") indica cresterea volumului pentru fiecare inch in plus la diametru.\n", sep = "")
cat("- R-squared:", round(summary_model$r.squared, 3), "indica procentul din variabilitatea volumului explicat de diametru.\n")
cat("- Valoarea p pentru panta este", summary_model$coefficients[2,4], ",\n  ceea ce indica daca panta este semnificativ diferita de 0.\n\n")

cat("Concluzie:\n")
cat("Exista o corelatie pozitiva si puternica intre diametru si volum, confirmata atat de coeficientul de corelatie,\n")
cat("cat si de modelul de regresie liniara, care explica o proportie mare din variatia volumului.\n")
cat("Astfel, diametrul este un bun predictor pentru volumul arborilor.\n")
