# Setam parametrii problemei
n <- 50          # numarul de produse per lot
p <- 0.7         # probabilitatea ca un produs sa fie conform
num_sim <- 60    # numarul de valori simulate

# a) Generarea datelor - 60 valori binomiale
set.seed(123)  # pentru reproductibilitate
produse_conforme <- rbinom(num_sim, size = n, prob = p)

# Vizualizarea datelor prin histograma
# Alegem 8 clase deoarece intervalul variabil de la min la max este mic (de ex. intre 20 si 45)
hist(produse_conforme,
     breaks = 8,
     main = "Histograma numarului de produse conforme in loturi de 50 bucati",
     xlab = "Numar produse conforme",
     ylab = "Frecventa",
     col = "lightblue",
     border = "black")

# b) Estimarea probabilitatii ca numarul conform sa fie intre 30 si 40
num_intre_30_40 <- sum(produse_conforme >= 30 & produse_conforme <= 40)
prob_estimata <- num_intre_30_40 / num_sim

cat("Probabilitatea estimata ca numarul de produse conforme sa fie intre 30 si 40 este:", prob_estimata, "\n")

# c) Calcularea medianei si decilei 5 (percentila 10%)
mediana <- median(produse_conforme)
decila_5 <- quantile(produse_conforme, 0.10)

cat("Mediana numarului de produse conforme este:", mediana, "\n")
cat("Decila 5 (percentila 10%) este:", decila_5, "\n")

# Concluzii si analiza:

# - Histograma arata distributia numarului de produse conforme pe cele 60 de loturi simulate. 
#   Valorile se concentreaza in jurul mediei teoretice np = 50 * 0.7 = 35, ceea ce este asteptat pentru o distributie binomiala.

# - Probabilitatea estimata ca numarul de produse conforme sa fie intre 30 si 40 este o masura empirica bazata pe simularea noastra.
#   Aceasta ofera o idee asupra sanselor ca loturile sa fie in aceasta zona considerata "normala" sau acceptabila.

# - Mediana reprezinta valoarea centrala a distributiei simulate:
#   jumatate dintre loturi au un numar de produse conforme mai mic sau egal cu mediana,
#   iar cealalta jumatate mai mare. In cazul nostru, aceasta confirma ca numarul conform este in jurul valorii de 35.

# - Decila 5 (percentila 10%) ne indica limita inferioara a distributiei:
#   10% dintre loturi au un numar de produse conforme mai mic sau egal cu aceasta valoare,
#   oferind o perspectiva asupra variatiei minime in performanta loturilor.

