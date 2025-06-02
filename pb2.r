# SUBIECTUL II - PARTEA A

# 1. Incarcam datele din fisierul studenti.csv
studenti <- read.csv("studenti.csv")

# Presupunem ca coloana cu timpii de rezolvare se numeste "timp"
timpi <- studenti$timp

# 2. Calculam dimensiunea esantionului, media si deviația standard
n <- length(timpi)
media_e <- mean(timpi)
sd_e <- sd(timpi)

# 3. Formulam ipotezele pentru testul t unilateral (stanga)
# H0: media = 45 (timpul mediu nu s-a schimbat)
# H1: media < 45 (studentul termina mai rapid decat media istorica)

mu0 <- 45
nivel_neincredere <- 0.02         # nivel de neincredere 2%
nivel_incredere <- 1 - nivel_neincredere

# 4. Calculam statistica t
t_calc <- (media_e - mu0) / (sd_e / sqrt(n))

# 5. Determinam valoarea critica t (percentila 2%, df = n-1)
t_crit <- qt(nivel_neincredere, df = n - 1)

# 6. Afisam rezultatele si decizia testului
cat("Dimensiunea esantionului:", n, "\n")
cat("Media esantionului:", media_e, "\n")
cat("Deviația standard esantion:", sd_e, "\n")
cat("Statistica t calculata:", t_calc, "\n")
cat("Valoarea critica t (nivel 2%):", t_crit, "\n")

if(t_calc < t_crit){
  cat("Decizie: Respinge H0. Studentul finalizeaza testul mai rapid decat media istorica.\n\n")
} else {
  cat("Decizie: Nu se respinge H0. Nu exista dovezi suficiente ca studentul finalizeaza mai rapid.\n\n")
}

# SUBIECTUL II - PARTEA B

# Datele testului asupra variantei timpii de rezolvare
n_b <- 80                     # dimensiunea esantionului
df_b <- n_b - 1               # gradele de libertate
alpha_b <- 0.07               # nivel de semnificatie 7% (93% nivel de incredere)
chi2_calc <- 21.21            # valoarea calculata a statistcii chi-patrat

# Valoarea critica chi-patrat pentru test unilateral (stanga)
chi2_crit <- qchisq(alpha_b, df = df_b)

# Afisam rezultatele si decizia
cat("Valoarea critica chi-patrat (alpha=7%, df=79):", chi2_crit, "\n")
cat("Valoarea calculata chi-patrat:", chi2_calc, "\n")

if(chi2_calc < chi2_crit){
  cat("Decizie: Chi-patrat calculat este in regiunea critica.\n")
  cat("Respingem ipoteza nula: varianta timpii de rezolvare a scazut semnificativ.\n")
} else {
  cat("Decizie: Chi-patrat calculat NU este in regiunea critica.\n")
  cat("Nu putem respinge ipoteza nula: nu exista dovezi suficiente pentru scadere.\n")
}