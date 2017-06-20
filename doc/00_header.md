% Categories and Haskell
% Jan-Willem Buurlage

---
tagline: An introduction to the mathematics behind modern functional programming
date: \today
toc: true
numbersections: true
documentclass: memoir
bibliography: mendeley.bib
classoption: 12pt,openany,oneside
header-includes:
    - \usepackage{tikz-cd}
    - \usepackage{amsthm}
    - \usepackage[T1]{fontenc}
    - \usepackage{palatino}
    - \usepackage[scaled]{beramono}
    - \usepackage{float}
    - \usepackage{epigraph}
    - \usepackage{hyperref}
    - \usepackage{subcaption}
    - \usepackage{xcolor}
    - \definecolor{lightblue}{HTML}{27aae1}
    - \definecolor{lightred}{HTML}{e12727}
    - \hypersetup{colorlinks, linkcolor=lightred, urlcolor=lightblue}
    - \urlstyle{tt}
    - \setlength{\epigraphwidth}{0.8\textwidth}
    - \newtheoremstyle{custom}
        {0.5 cm}
        {0.5 cm}
        {}
        {}
        {\bfseries}
        {.}
        {.5em}
        {}
    - \theoremstyle{custom}
    - \newtheorem{theorem}{Theorem}
    - \newtheorem{lemma}{Lemma}
    - \newtheorem{definition}{Definition}
    - \newtheorem{proposition}{Proposition}
    - \newtheorem{corollary}{Corollary}
    - \newtheorem{example}{Example}
    - \newtheorem{exercise}{Exercise}
    - \newcounter{common}
    - \usepackage{aliascnt}
    - \makeatletter
        \let\c@theorem\relax
        \let\c@lemma\relax
        \let\c@definition\relax
        \let\c@example\relax
        \let\c@proposition\relax
        \let\c@corollary\relax
        \let\c@proposition\relax
        \makeatother
    - \newaliascnt{theorem}{common}
    - \newaliascnt{lemma}{common}
    - \newaliascnt{definition}{common}
    - \newaliascnt{example}{common}
    - \newaliascnt{proposition}{common}
    - \newaliascnt{corollary}{common}
    - \numberwithin{common}{chapter}
    - \numberwithin{exercise}{chapter}
    - \newcommand{\cd}[1]{
        \begin{figure}
        \centering
        \begin{tikzcd}
            {#1}
        \end{tikzcd}
        \end{figure}}
    - \newcommand{\concat}{\ensuremath{+\!\!+\,}}
    - \renewcommand{\thetheorem}{\thechapter.\arabic{theorem}}
    - \renewcommand{\thelemma}{\thechapter.\arabic{lemma}}
    - \renewcommand{\thedefinition}{\thechapter.\arabic{definition}}
    - \renewcommand{\theexample}{\thechapter.\arabic{example}}
    - \renewcommand{\theproposition}{\thechapter.\arabic{proposition}}
    - \renewcommand{\thecorollary}{\thechapter.\arabic{corollary}}
    - \renewcommand{\theexercise}{\thechapter.\arabic{exercise}}
    - \newcommand{\alongtop}{\raisebox{-0.5em}{\tikz{\draw[->] (0,0) -- (0.9em,0); \draw[->] (1em,0) -- (1em,-0.9em);}}}
    - \newcommand{\alongbottom}{\raisebox{-0.5em}{\tikz{\draw[->] (0,0) -- (0,-0.9em); \draw[->] (0, -1em) -- (0.9em,-1.0em);}}}
    - \newcommand{\lbanana}{(\!|}
    - \newcommand{\rbanana}{|\!)}
    - \setlength{\parskip}{0.3cm}
    - \setlength{\parindent}{0.0cm}
    - \usepackage{geometry}
    - \geometry{margin=3.2cm}
    - \usetikzlibrary{backgrounds}
    - \newcommand{\room}{\hspace{0.5cm}}
---


