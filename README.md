# Compilation

pour compiler le projet il faut installer *cabal* et lancer la commande suivante :

    cabal build

elle génère trois exécutables :

    dist/build/generator/generator
    dist/build/simulator/simulator
    dist/build/optimizer/optimizer

# Generator

l'exécutable crée le fichier *cpu.net*, il s'agit de la netlist du processeur

# Optimizer

l'exécutable prend en paramètre une netlist *netlist.net* et tente d'en réduire la taille. le résultat est enregistré dans *netlist_opt.net*.

# Simulator

l'exécutable prend en paramètre une netlist et l'exécute. lancez *simulator --help* pour plus d'informations sur l'utilisation de la commande

# TODO

une fois terminé, le projet contiendra en plus :
* un compilateur de netlist vers du c
* une architecture (presque) complète de mips
* un fichier de rom contenant un programme simulant une horloge
