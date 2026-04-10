<div align="center">

<h1>𝙒𝙊𝙍𝙈𝙎 🪱</h1>
<h3>Turn-Based Strategy Game Engine</h3>

![Haskell](https://img.shields.io/badge/Haskell-5D4F85?style=for-the-badge&logo=haskell&logoColor=white)
![Cabal](https://img.shields.io/badge/Cabal-blueviolet?style=for-the-badge)

*Academic project developed for **Laboratórios de Informática I** @ University of Minho*  
*Final grade: **17/20** ⭐*

</div>

---

![Worms Game Menu](images/menu.png)

## 🎮 About

This project implements a turn-based strategy game inspired by the classic **Worms** series, developed entirely in **Haskell** using functional programming principles. Players control worms equipped with weapons and must eliminate enemy worms before being eliminated themselves.

The project was developed across multiple tasks, covering game state modelling, physics simulation, entity movement, weapon logic and a fully interactive graphical interface.

## ⚡ Features

- **Turn-based gameplay** - alternating turns between teams, one move at a time
- **Weapon system** - multiple weapons, each with its own mechanics and blast radius
- **Physics engine** - gravity, movement, collision detection and projectile trajectories
- **Dynamic terrain** - destructible maps that change as the game progresses
- **Graphical interface** - full visual rendering powered by Gloss
- **Unit tests** - per-task feedback executables for continuous validation
- **Code coverage** - HPC integration to measure test coverage

---

## 🚀 How to Run

```bash
# Launch the game
cabal run worms-game

# Open the Haskell interpreter with the project loaded
cabal repl

# Run tests for a specific task (replace N with task number)
cabal run tN-feedback
```

---

## 🧪 Code Coverage

```bash
cabal clean
cabal run --enable-coverage t1-feedback
./runhpc.sh t1-feedback

# Or use the shortcut script
./runcoverage.sh t1
```

---

## 📖 Documentation

```bash
cabal haddock-project
```

---

## 🔍 Code Quality

```bash
cabal install homplexity --flags="html"
homplexity-cli --format=HTML lib/ > homplexity.html
```

---

## 🛠️ Tech Stack

`Haskell` · `Functional Programming` · `Gloss` · `Cabal` · `HPC` · `Haddock`

---

## 👩‍💻 Authors

**Carolina Dias** - [@carolinavdias](https://github.com/carolinavdias)  
**Leonor Sousa** - [@Leonor1369](https://github.com/Leonor1369)
