# CompiLustre

Ce projet est réalisé dans le cadre du projet de Système synchrones (MPRI, année 2024).
CompiLustre est un mini transpileur Lustre vers C.

# Compilation

Pour compiler le projet, il suffit de lancer la commande :

```Bash
cargo build
```

Et pour lancer le compilateur :

```Bash
cargo run
```

Par défaut, lancer le compilateur compilera le fichier tests/merge_fby et affichera le code généré.
Pour changer le fichier compilé, il suffis de modifier le fichier main.rs et remplacer tests/merge_fby par le fichier souhaité.
