# Compilation

pour compiler le projet il faut installer *cabal* et lancer la commande suivante :

    cabal build

elle génère cinq exécutables :

	dist/build/assembly/assembly
	dist/build/compiler/compiler
    dist/build/generator/generator
    dist/build/optimizer/optimizer
    dist/build/simulator/simulator

# Assembly

l'exécutable prend en paramètre un fichier assembleur MIPS *asmfile.s* et crée le fichier binaire MIPS correspondant *asmfile*

# Compiler

l'exécutable prend en paramètre une netlist *netlist.net* et crée le fichier C correspondant *netlist.c*

# Generator

l'exécutable crée le fichier *cpu.net*, il s'agit de la netlist du processeur

# Optimizer

l'exécutable prend en paramètre une netlist *netlist.net* et tente d'en réduire la taille ; le résultat est enregistré dans *netlist_opt.net*

# Simulator

l'exécutable prend en paramètre une netlist et l'exécute ; lancez la commande *simulator --help* pour plus d'informations

# TODO

une fois terminé, le projet contiendra en plus :
* un compilateur de netlist vers du c
* une architecture (presque) complète de mips
* un fichier de rom contenant un programme simulant une horloge
