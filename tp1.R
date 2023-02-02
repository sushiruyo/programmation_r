brutToNet1 <- function(salaire, taux) {
  if (is.numeric(salaire) & is.numeric(taux)){
    return (salaire*(1-taux/100))
  } else {
    return ("ERROR : type not expected")
  }
  
}

print(brutToNet1(1498,22))
print(brutToNet1("1498",22))
print(brutToNet1(1498,"22"))

brutToNet2 <- function(salaire,statut,taux = 7.5) {
  if (is.character(statut)) {
    if (is.numeric(salaire)){
      salaire <- salaire * (1-taux/100)
    }
    if (statut == "cadre") {
      return(brutToNet1(salaire,25))
    } else {
      return(brutToNet1(salaire,22))
    }
  } else {
    return ("ERROR : contract unknown")
  }
}

brutToNet2(1498,"cadre")
brutToNet2(1498,"pas cadre")
brutToNet2("1498","cadre")
brutToNet2(1498,22)

brutToNet3 <- function(salaire,statut,taux = 7.5, temps = 100) {
  if (is.numeric(salaire) & is.numeric(taux) & is.numeric(temps)){
    if (0<=taux & taux<=100 & 0<=temps & temps<=100){
      return (brutToNet2(salaire*temps/100,statut,taux))
    } else {
      return ("ERROR : rate and time must be in range(0,100)")
    }
  } else {
    return ("ERROR : type not expected")
  }
}

brutToNet3(1498,"cadre")
brutToNet3(1498,"pas cadre",17,42)
brutToNet3(1498,"cadre",103)



netAnnuelToImpot <- function(salaire) {
  tranches <- c(10777,27748,78750,168994)
  taux <- c(0.11,0.3,0.41,0.45)
  return(
  min(tranches[2]-tranches[1],max(0,salaire-tranches[1]))*taux[1]  
  + min(tranches[3]-tranches[2],max(0,salaire-tranches[2]))*taux[2]  
  + min(tranches[4]-tranches[3],max(0,salaire-tranches[3]))*taux[3]  
  + max(0,salaire-tranches[4])*taux[4]
  )
  }

netAnnuelToImpot(50000)

juste_prix <- function(mode) {
  essais = 10
  ai_choice <- sample(x = 1:100, size = 1)
  trouve = F
  while (trouve == F & essais != 0) {
    user_choice <- as.integer(readline(prompt = "Saisir un entier naturel : "))
    if (user_choice == ai_choice) {
      trouve = T
    } else {
      if (mode == "difficile"){
        essais <- essais - 1
      }
      
      if (user_choice < ai_choice) {
        print("C'est plus")
      } else {
        print("C'est moins")
      }
    }
  }
  if (trouve == T) {
    print("Bravo !")
  } else {
    print(paste("Perdu, le juste prix était",as.character(ai_choice)))
  }
}

juste_prix("difficile")


pfc <- function(nb_parties) {
  victoires = 0
  while (nb_parties > 0) {
    nb_parties = nb_parties - 1
    user_choice = readline(prompt = "Pierre, Feuille ou Ciseaux ? ")
    ai_choice = sample(x = c("Pierre","Feuille","Ciseaux"), size = 1)
    print(paste("Votre adversaire a choisi",ai_choice))
    if (user_choice == ai_choice){
      print("Match nul !")
    } else{
    if (user_choice == "Pierre"){
      if (ai_choice == "Feuille"){
        print("Votre adversaire a gagné !")
      }else{
        print("Vous avez gagné !")
        victoires = victoires + 1
      }}
    else if (user_choice == "Feuille"){
      if (ai_choice == "Ciseaux"){
        print("Votre adversaire a gagné !")
      }else{
        print("Vous avez gagné !")
        victoires = victoires + 1
      }}
    else {
      if (ai_choice == "Pierre"){
        print("Votre adversaire a gagné !")
      }else{
        print("Vous avez gagné !")
        victoires = victoires + 1
      }
      }
    }
  }
  return (victoires)
    }
  

pfc(3)