# Econometrics Study Guide

An interactive, plain-language study guide for an introductory Econometrics course.
It is written for low-overwhelm studying: one lecture per tab, the three key ideas first,
formulas translated into words, and a learning path in every lecture.

**Open it:** https://sweetheartapi.github.io/learn_econometrics/

## What's inside

| Tab | Topic | Interactive demo |
| --- | --- | --- |
| Basics | Plain definitions (mean, variance, SE, p-value, log, Greek letters), with search | |
| 1 | Regression basics | Regression playground: noise, heteroskedasticity, omitted variable, outlier |
| 2 | Maximum likelihood | Likelihood explorer: score, curvature, standard error |
| 3 | Bootstrap | Resampling demo with sample-size choice and a coverage check |
| 4 | Intro to causality | Coin flip vs self-selection: ATT + selection bias |
| 5 | Instrumental variables | 500 simulated studies: weak and invalid instruments |
| 6 | Regression discontinuity | Bandwidth simulator with placebo cut-offs |
| 7 | Difference-in-differences | Parallel trends and what happens when they fail |
| 8 | IV, RDD & DiD extras | Complier calculator, kink simulator, doubly robust DiD with two safety nets |
| Cheat sheet | Methods side by side, how they connect, "choose the design" scenarios | |
| Exam mode | Mixed, optionally timed questions with results by topic | |
| Final test | 33 new questions on everything, graded at the end, with a personal study plan | |

Every lecture follows the same five steps: **Understand → See it → Apply → Challenge → Review**.
It also has learning objectives marked Foundational / Intermediate / Advanced, a "method in five
questions" card (problem, solution, assumptions, failures, checks), Simple / Intuitive / Technical
explanations of tricky concepts, scored practice with feedback on each wrong answer, and an optional
"Try it in R" lab with the exact output to expect.

Progress has three stages per lecture: **studied** (ticked), **practised** (every practice question
answered) and **mastered** (at least 80 % right in exam mode, or on that lecture's part of the final test).

The **final test** is meant for the end: 33 questions not used anywhere else, answers can be changed until
submitting, and the result shows a score per lecture, feedback on every answer and a study plan that links
back to the exact sections to reread, weakest lecture first. It is saved in the browser only.

Other features: whole-guide search (press `/`), "Just the gist" mode, light and dark themes,
a phone-friendly layout, keyboard navigation and a printable cheat sheet.

## Files

- `index.html`: the whole site (no build step).
- `r/`: one R script per lab (simulated data with fixed seeds, so the output matches the guide).
- `data/`: the simulated data from each lab as CSV, for use in other software.
- `Econometrics-Learnbook-for-Dummies.pdf`: the printable Econometrics Learnbook for Dummies (third edition, with two practice exam papers).
- `tests/check.cjs`: automated checks, run on every push by GitHub Actions
  (`.github/workflows/check.yml`).

## Checks

`npm install && npx playwright install chromium && npm test` opens the site in a headless browser and
checks that every page loads without errors, that each practice question accepts its own answer, that the
numbers in worked examples recompute, that the demos, exam mode and search work, that nothing scrolls
sideways on a phone, and that axe-core finds no serious accessibility problems in light or dark mode.

## Sources

Based on the course lecture slides, *Mastering 'Metrics* (Angrist & Pischke) and
*Linear Models with R* (Faraway). The R labs need `ggplot2`, `sandwich`, `lmtest`, `AER`, `boot`,
`rdrobust`, `rddensity` and `dplyr`.
