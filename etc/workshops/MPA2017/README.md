# MFAPipe: Open source software for parallel labeling, steady state metabolic flux analysis

The following workshop was developed for [Metabolic Pathways Analysis 2017](http://www.chbe.montana.edu/biochemenglab/MPA2017.html) on July 24-28, 2017 in Bozeman, MT USA.

## Installing MFAPipe

### Prerequisites

* [GLPK](https://www.gnu.org/software/glpk/)
* [Git](https://git-scm.com/)
* [The Haskell Tool Stack](https://git-scm.com/)

**Note:**
On macOS, GLPK can be installed using the `homebrew/science/glpk` package for [Homebrew](https://brew.sh/).

### Installation Guide

1. Clone MFAPipe using Git with `git clone https://github.com/EMSL-NMR-EPR/Haskell-MFAPipe-Executable.git`

2. Enter into the MFAPipe folder with `cd Haskell-MFAPipe-Executable`

3. Build MFAPipe using The Haskell Tool Stack with `stack setup && stack build`

4. Once MFAPipe finishes building, check the version with `stack exec mfaPipe -- --version` which should be `MFAPipe v0.1.0.0, (C) 2016 Pacific Northwest National Laboratory`

## Background

### Citric acid cycle

The citric acid cycle (also known as the tricarboxylic acid cycle or the Krebs cycle) is a metabolic pathway (a set of chemical reactions) that is used by aerobic organisms to release energy through the oxidation of acetyl coenzyme A into carbon dioxide and chemical energy.

### Stoichiometry and atom transitions

The model is depicted in Figure 1. The model is depicted as a network, where: vertices are labeled with abbreviations for metabolites; and, edges are labeled with metabolic flux variables and their expected values. Metabolic flux variables are denoted "v<sub>n</sub>", where n is an identifier. Edges represent either: irreversible reactions; or, left-to-right or right-to-left directions of reversible reactions (with respect to the arrow). For example, the edge labeled "v<sub>5</sub>" represents an irreversible reaction, and the edges labeled "v<sub>6</sub>" and "v<sub>7</sub>" represent the two directions of a reversible reaction.

> ![Figure 1](https://raw.githubusercontent.com/EMSL-NMR-EPR/Haskell-MFAPipe-Executable/master/etc/workshops/MPA2017/fig1.jpg)
>
> **Figure 1.** Simplified model of the tricarboxylic acid cycle. Abbreviations of metabolites: OAC, oxaloacetate; Asp, aspartate; AcCoA, acetyl coenzyme A; Cit, citrate; AKG, &alpha;-ketoglutarate; Glu, glutamate; Fum, fumarate. Suc, succinate. The assumed fluxes have arbitrary units.
>
> Figure and caption reproduced from [Antoniewicz et al., 2007, Fig. 13](http://www.sciencedirect.com/science/article/pii/S109671760600084X#fig13).

Stoichiometry and atom transitions for the model are given in Figure 2.

> ![Figure 2](https://raw.githubusercontent.com/EMSL-NMR-EPR/Haskell-MFAPipe-Executable/master/etc/workshops/MPA2017/fig2.png)
>
> **Figure 2.** Stoichiometry and atom transitions for reactions of the TCA cycle.

Figure and caption reproduced from [Antoniewicz et al., 2007, Tbl. 5](http://www.sciencedirect.com/science/article/pii/S109671760600084X#tbl5).

The modified model is depicted in Figure 3. Two modifications are made: First, exchange reactions from the original model are moved to the "intracellular" major-body fluid compartment of the organelle. Second, "extracellular" metabolites and corresponding reactions are added to the model to describe substrate uptake and product transport. By convention (see Rios and Lange, 2007), metabolic flux variables for transport reactions are denoted "b<sub>n</sub>", where n is an identifier.

> ![Figure 3](https://raw.githubusercontent.com/EMSL-NMR-EPR/Haskell-MFAPipe-Executable/master/etc/workshops/MPA2017/fig3.png)
>
> **Figure 3.** Modified version of Figure 1.

### Stoichiometry matrix

The stoichiometry matrix for the modified model is

| | b<sub>1</sub> | b<sub>2</sub> | b<sub>3</sub> | b<sub>4</sub> | v<sub>1</sub> | v<sub>2</sub> | v<sub>3</sub> | v<sub>4</sub> | v<sub>5</sub> | v<sub>6</sub> | v<sub>7</sub> | v<sub>8</sub> |
| :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: |
| **AKG** | 0 | | 0 | | 0 | | 0 | | 0 | | 1 | | -1 | | -1 | | 0 | | 0 | | 0 | | 0 |
| **AcCoA** | 1 | 0 | 0 | 0 | -1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| **Asp** | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | -1 |
| **CO2** | 0 | 0 | -1 | 0 | 0 | 1 | 0 | 1 | 0 | 0 | 0 | 0 |
| **Cit** | 0 | 0 | 0 | 0 | 1 | -1 | 0 | 0 | 0 | 0 | 0 | 0 |
| **Fum** | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | -1 | 1 | 0 |
| **Glu** | 0 | 0 | 0 | -1 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 0 |
| **OAC** | 0 | 0 | 0 | 0 | -1 | 0 | 0 | 0 | 0 | 1 | -1 | 1 |
| **Suc** | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | -1 | 0 | 0 | 0 |
| <td colspan=6 style="text-align:center;">&uarr; Intracellular</td><td colspan="6" style="text-align:center;">&darr; Extracellular</td>
| **AcCoA** | -1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| **Asp** | 0 | -1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| **CO2** | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| **Glu** | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |

### Null-space matrix

The null-space matrix for the modified model is

| | u<sub>1</sub> | u<sub>2</sub> | u<sub>3</sub> |
| :-: | :-: | :-: | :-: |
| **b<sub>1</sub>** | 1 | 0 | 0 |
| **b<sub>2</sub>** | 0 | 1 | 0 |
| **b<sub>3</sub>** | 2 | -1 | 0 |
| **b<sub>4</sub>** | 0 | 1 | 0 |
| **v<sub>1</sub>** | 1 | 0 | 0 |
| **v<sub>2</sub>** | 1 | 0 | 0 |
| **v<sub>3</sub>** | 0 | 1 | 0 |
| **v<sub>4</sub>** | 1 | -1 | 0 |
| **v<sub>5</sub>** | 1 | -1 | 0 |
| **v<sub>6</sub>** | 1 | -1 | 1 |
| **v<sub>7</sub>** | 0 | 0 | 1 |
| **v<sub>8</sub>** | 0 | 1 | 0 |

where u<sub>1</sub>, u<sub>2</sub> and u<sub>3</sub> are independent variables.

The relationship between independent variables and metabolic flux variables is summarized by

| Independent Variable | Transport Metabolic Flux Variable | Exchange Metabolic Flux Variable |
| :-: | :-: | :-: |
| u<sub>1</sub> | b<sub>1</sub> | v<sub>1</sub><br/>v<sub>2</sub> |
| u<sub>2</sub> | b<sub>2</sub><br/>b<sub>4</sub> | v<sub>3</sub><br/>v<sub>8</sub> |
| u<sub>3</sub> | | v<sub>7</sub> |

where b<sub>3</sub>, v<sub>4</sub>, v<sub>5</sub>, and v<sub>6</sub> are linear functions of the independent variables.

The net metabolic flux of the reversible reaction (v<sub>6</sub> - v<sub>7</sub>) is given by (u<sub>1</sub> - u<sub>2</sub>) = v<sub>4</sub> = v<sub>5</sub>.

## Experiment

The experiment is to use a chemostat to study the citric acid cycle of an aerobic organism.

Data is reproduced from [Antoniewicz et al., 2007, Sec. 3.2](http://www.sciencedirect.com/science/article/pii/S109671760600084X#aep-section-id38), [Antoniewicz et al., 2007, Fig. 13](http://www.sciencedirect.com/science/article/pii/S109671760600084X#fig13), and [Antoniewicz et al., 2007, Fig. 17](http://www.sciencedirect.com/science/article/pii/S109671760600084X#fig17).

### Substrate labeling

| Metabolite (MFAPipe Syntax) | Mixture Percentage | Isotopic Distribution |
| - | - | - |
| `AcCoA.ext` | 50% | Natural abundance |
| `AcCoA.ext` | 25% | C2 <sup>13</sup>C-labeled |
| `AcCoA.ext` | 25% | Uniform <sup>13</sup>C-labeled |
| `Asp.ext` | 100% | Natural abundance |

### Substrate uptake rate

| Metabolite (MFAPipe Syntax) | Metabolic Flux Variable (MFAPipe Syntax) | Metabolic Flux |
| - | - | - |
| `AcCoA.ext` | `b1.f` | 100 |
| `Asp.ext` | `b2.f` | 50 |

### Product transport rate

| Metabolite (MFAPipe Syntax) | Metabolic Flux Variable (MFAPipe Syntax) | Metabolic Flux |
| - | - | - |
| `CO2.ext` | `b3.f` | 150 |
| `Glu.ext` | `b4.f` | 50 |

### GC-MS of head-space gas

| Elementary Metabolite Unit (MFAPipe Syntax) | M+0 | M+1 |
| - | - | - |
| `CO2#1` | 0.8958 | 0.1042 |

### GC-MS of intracellular metabolites

| Elementary Metabolite Unit (MFAPipe Syntax) | M+0 | M+1 | M+2 | M+3 |
| - | - | - | - | - |
| `Fum#2` | 0.7667 | 0.2333 | - | - |
| `OAC#2` | 0.8333 | 0.1667 | - | - |
| `OAC#3` | 0.8333 | 0.1667 | - | - |
| `Fum#23` | 0.5917 | 0.3500 | 0.0583 | - |
| `OAC#23` | 0.7083 | 0.2500 | 0.0417 | - |
| `Fum#123` | 0.5698 | 0.2698 | 0.1385 | 0.0219 |
| `OAC#123` | 0.6927 | 0.1927 | 0.0990 | 0.0156 |
| `OAC#234` | 0.6927 | 0.1927 | 0.0990 | 0.0156 |

### GC-MS of extracellular metabolites

| Elementary Metabolite Unit (MFAPipe Syntax) | M+0 | M+1 | M+2 | M+3 | M+4 | M+5 |
| - | - | - | - | - | - | - |
| `Glu.ext#12345` | 0.3464 | 0.2695 | 0.2708 | 0.0807 | 0.0286 | 0.0039 |

### NMR of extracellular metabolites

[Borkum et al., 2017](https://link.springer.com/article/10.1186/s13321-017-0201-7) gives a method for fitting the values of metabolic flux variables to NMR data, where NMR-derived expressions are representations of the conditional probability of observation given detection. (The formulation is shown to be backwards compatible with MS-derived expressions if it is assumed that the probability of detection of an MS-derived observation is always equal to unity.)

![Equation 1](https://raw.githubusercontent.com/EMSL-NMR-EPR/Haskell-MFAPipe-Executable/master/etc/workshops/MPA2017/eqn1.gif)

The chemical structure of glutamate is depicted in Figure 4.

> ![Figure 4](https://raw.githubusercontent.com/EMSL-NMR-EPR/Haskell-MFAPipe-Executable/master/etc/workshops/MPA2017/fig4.png)
>
> **Figure 4.** Depiction of the chemical structure of glutamate. The carbon atoms of glutamate are numbered according to IUPAC convention.

The conditional probabilities for observation given detection for each pair of <sup>13</sup>C-labeled carbon atoms in glutamate is as follows:

| P(&darr; &vert; &rarr;) | `Glu.ext#1,1` | `Glu.ext#2,1` | `Glu.ext#3,1` | `Glu.ext#4,1` | `Glu.ext#5,1` |
| - | - | - | - | - | - |
| `Glu.ext#1,1` | 1.0000 | 0.4374 | 0.1878 | 0.1042 | 0.1040 |
| `Glu.ext#2,1` | 0.6998 | 1.0000 | 0.2502 | 0.1666 | 0.1668 |
| `Glu.ext#3,1` | 0.3005 | 0.2502 | 1.0000 | 0.1666 | 0.1668 |
| `Glu.ext#4,1` | 0.5002 | 0.4998 | 0.4998 | 1.0000 | 1.0000 |
| `Glu.ext#5,1` | 0.2496 | 0.2502 | 0.2502 | 0.5000 | 1.0000 |

Analyzing the first row of conditional probabilities:

| Observation Criteria | Observation Expression (MFAPipe Syntax) | Detection Criteria | Detection Expression (MFAPipe Syntax) | Expression (MFAPipe Syntax) | Value |
| - | - | - | - | - | - |
| C1 is <sup>13</sup>C-labeled | `Glu.ext#1,1` | C1 is <sup>13</sup>C-labeled | `Glu.ext#1,1` | `Glu.ext#1,1 / Glu.ext#1,1` | 1.0000 |
| C1 is <sup>13</sup>C-labeled | `Glu.ext#1,1` | C2 is <sup>13</sup>C-labeled | `Glu.ext#2,1` | `Glu.ext#12,11 / Glu.ext#2,1` | 0.4374 |
| C1 is <sup>13</sup>C-labeled | `Glu.ext#1,1` | C3 is <sup>13</sup>C-labeled | `Glu.ext#3,1` | `Glu.ext#13,11 / Glu.ext#3,1` | 0.1878 |
| C1 is <sup>13</sup>C-labeled | `Glu.ext#1,1` | C4 is <sup>13</sup>C-labeled | `Glu.ext#4,1` | `Glu.ext#14,11 / Glu.ext#4,1` | 0.1042 |
| C1 is <sup>13</sup>C-labeled | `Glu.ext#1,1` | C5 is <sup>13</sup>C-labeled | `Glu.ext#5,1` | `Glu.ext#15,11 / Glu.ext#5,1` | 0.1040 |

## Exercises

### Exercise 1

The model from [Antoniewicz et al., 2007](http://www.sciencedirect.com/science/article/pii/S109671760600084X) is distributed with MFAPipe as the [`examples/Antoniewicz2007.json`](https://github.com/EMSL-NMR-EPR/Haskell-MFAPipe-Executable/blob/master/examples/Antoniewicz2007.json) file.

The file includes stoichiometry and atom transitions for the modified model (depicted in Figure 3), along with substrate labeling data, and GC-MS data for specific intra- and extracellular metabolites.

1. Invoke MFAPipe with `stack exec mfaPipe -- mfa-levmar --input=examples/Antoniewicz2007.json --output=examples/Antoniewicz2007.zip`

2. Extract the contents of the output file, a zip archive, to a new folder with `mkdir -p examples/Antoniewicz2007 && unzip examples/Antoniewicz2007.zip -d examples/Antoniewicz2007`

3. Verify the fitted values of metabolic flux variables with `cat examples/Antoniewicz2007/results/flux.csv`

### Exercise 2

Modify the model (e.g., add/remove constraints, add/remove experimental data, and add/remove reactions). How does it affect the result of the fit?
