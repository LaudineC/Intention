# Intention
Determinants of the intention to use early childcare


1_Sans_recodage: raw data sans aucun recodage - "données brutes"

2_Avec_recodage: avec un minimum de recodage sur la bd machine learning (pas tout dummifé mais merge de certaines catégories comme avant vs après 1 an)

3_Dummies: En mettant tout en dummies sauf les variables numériques; + en mettant tout en dummies même les variables numriques

4_Info and Social resources: les anciens fichiers laissaient ces deux variables qui sont discutables sur le côté, on regarde ce que ça fait avec ces deux variables

5_FAST_IntentionApply : tentative de faire avec une autre fonction qui aille plus vite avortée: grf ne permet pas de tuner avec la fonction proba_forest qui gère les outcomes probabilistes
En revanche, ajout de Normalize en systémtique, du doParallel qui avait disparu par endroit
Et du Intention dans le model application et application et intention dans le model access 