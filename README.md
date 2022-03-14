# Automates & Applications

Vérification de correspondance entre un fichier **XML**
et son type défini dans un fichier *à la* **DTD**.

Projet de M1 MPRI, pour le cours [Automates et Applications](https://www.lri.fr/~kn/aa_fr.html)
dispensé à l'Université-Paris-Saclay par [M. Kim Nguy&#7877;n](https://www.lri.fr/~kn/).

## Usage

Pour commencer :
```bash
git clone https://github.com/paulpatault/xml-validation
cd xml-validation
```

Exécution directe et simple :
```bash
dune exec bin/validate.exe [fichier dtd] [fichier xml] [racine] [debug-mode]
# par exemple :
dune exec bin/validate.exe tests/ex.dtd tests/1.xml hd
```

Récupération de l'exécutable :
```bash
make install
./validate [fichier dtd] [fichier xml] [racine] [debug-mode]
# par exemple :
./validate tests/ex.dtd tests/1.xml hd
```

## Explications

Le [rapport](./pdfs/Rapport/Rapport.pdf) se trouve ici : `./pdfs/Rapport/Rapport.pdf`.
Vous y retrouverez les réponses au questions du [sujet](./pdfs/sujet.pdf).

---
## Installation des dépendances

```bash
# opam
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
# Ocaml et Xml-light
opam install ocaml menhir Xml-light
```

