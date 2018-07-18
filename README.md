# Exécutable d'extraction automatique, depuis des rasters, de linéaires sur la base de leur couleur


## Documentation

Elle est dans le dossier ```doc```

- Le document ```détection_bleu_scan25_historique_document.pdf``` explique comment lancer le programme
- La présentation ```détection_bleu_scan25_historique_presentation.pdf``` est plus technique. Il détaille les différentes étapes lors de l'extraction

## Fichiers de configuration

Il y a deux fichiers de configuration à remplir obligatoirement, et un optionnel :

- ```config.ini``` pour les chemins vers dossiers et applications
- ```couleurs.ini``` pour les couleurs
- ```advanced.ini``` (optionnel) pour gérer des comportements spécifiques de l'outil

## Relance du processus en cas de bloquage

Les temps de calcul pour chaque objet couverture peuvent demander de la patience.  
Au bout d'un certain nombre d'objets de couverture, l'outil peut bloquer.   
Dans ce cas, il suffit de fermer la fenêtre d'exécution et de relancer le processus.  
Les couches qui auront déjà été créées avant ne seront pas recalculées.  

## Précautions d'utilisation
Les couches vectorielles qui sont générées ne se suffisent pas à elles-mêmes et nécessitent d'être validées par l'utilisateur.  
Pour connaître les biais dans les lots générés, veuillez vous reporter à la documentation


*Réalisé le 19 Janvier 2016*  
*Licence Creative Commons BY NC SA (Attribution, pas d'utilisaton commerciale, partage dans les mêmes conditions)*                                     
