# Minesweeper

Le but du projet est d’implémenter le jeu du démineur. Il n’y a rien à installer à part le projet en lui-même, ainsi que Node.js et Yarn (ou NPM).

# Installation du projet

```bash
yarn install  # npm install
```

# Le lancer

```bash
yarn start # npm start
```

# En cas de bug

```bash
yarn global remove elm-app create-elm-app
rm -rf node_modules
yarn global add create-elm-app
yarn install
```

## Les commandes du jeux

* `click gauche` : permet de dévoiler des cellules
* `crtl + click gauche` : permet de placer/retirer un drapeau 

## Les problèmes rencontrés

- Quel type de donnée à utiliser pour représenter la grille
    1) Une bibliothêque : `Array2D`
    2) Une list de list : `List (List Cell)`
    3) pour finir sur list de cellules : `List Cell`

- Initilaliser la grille avec la list des positions des bombes (placer corretement des nombres)
- Dévoiler les cellules suit a un click d'un utilisateur
- Placer les drapeaux suit à une action alternatif : `ctrl`
