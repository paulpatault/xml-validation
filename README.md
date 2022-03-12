# Automates & Applications

Vérification de correspondance entre un fichier **XML**
et son type défini dans un fichier *à la* **DTD**.

## Usage

Pour commencer:
```bash
git clone https://github.com/paulpatault/ProjetAuto-Apps
cd ProjetAuto-Apps
```

Exécution directe et simple:
```bash
dune exec bin/validate.exe [fichier dtd] [fichier xml] [racine]
# par exemple :
dune exec bin/validate.exe tests/ex.dtd tests/1.xml hd
```

Récupération de l'exécutable:
```bash
make install
./validate [fichier dtd] [fichier xml] [racine]
# par exemple :
./validate tests/ex.dtd tests/1.xml hd
```

## Installation des dépendances

```bash
# opam
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
# Ocaml et Xml-light
opam install ocaml menhir Xml-light
```

## Explications

Le [rapport](./pdfs/Rapport/Rapport.pdf) se trouve ici : `./pdfs/Rapport/Rapport.pdf`.
Vous y retrouverez les réponses au questions du [sujet](./pdfs/sujet.pdf).
