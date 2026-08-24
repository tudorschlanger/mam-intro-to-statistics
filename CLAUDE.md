# CLAUDE.MD -- Academic Project Development with Claude Code

<!-- HOW TO USE: Replace [BRACKETED PLACEHOLDERS] with your project info.
     Customize Beamer environments for your theme.
     Keep this file under ~150 lines — Claude loads it every session.
     See the guide at guide/workflow-guide.md for full documentation. -->

**Project:** MAM Intro to Statistics
**Institution:** Yale University, School of Management
**Branch:** main

---

## Core Principles

- **Plan first** -- enter plan mode before non-trivial tasks; save plans to `quality_reports/plans/`
- **Verify after** -- compile/render and confirm output at the end of every task
- **Single source of truth** -- Beamer `.tex` is authoritative
- **Quality gates** -- nothing ships below 80/100
- **[LEARN] tags** -- when corrected, save `[LEARN:category] wrong → right` to MEMORY.md

---

## Folder Structure

```
MAM-Intro-to-Statistics/
├── CLAUDE.MD                    # This file
├── .claude/                     # Rules, skills, agents, hooks
├── Bibliography_base.bib        # Centralized bibliography
├── input/                       # Input data (sources from outside the project)
├── output/                      # Output data, figures, tables, slides, paper drafts (produced by scripts in this project)
├── scripts/                     # All codes 
├── quality_reports/             # Plans, session logs, merge reports
├── templates/                   # Session log, quality report templates
└── docs/                        # Papers, emails, technical documentation, and existing slides
```

---

## Commands

```bash
# LaTeX (3-pass, XeLaTeX only)
cd output && TEXINPUTS=../scripts/latex_preambles:$TEXINPUTS xelatex -interaction=nonstopmode file.tex
BIBINPUTS=..:$BIBINPUTS bibtex file
TEXINPUTS=../scripts/latex_preambles:$TEXINPUTS xelatex -interaction=nonstopmode file.tex
TEXINPUTS=../scripts/latex_preambles:$TEXINPUTS xelatex -interaction=nonstopmode file.tex
```

### LaTeX Headers
- Slides: always use `scripts/latex_preambles/header_slides.tex`
- Documents: always use `scripts/latex_preambles/header_doc.tex`

## Stata
- Always run Stata to verify code and table output rather than generating blindly
- Stata executable path: `[PATH TO STATA - configure in .claude/settings.local.json]`
- Run do-files with: `[STATA PATH] -b do filename.do`
- Logs are written to the same directory as the do-file; always check the `.log` file for errors after running


---

## Quality Thresholds

| Score | Gate | Meaning |
|-------|------|---------|
| 80 | Commit | Good enough to save |
| 90 | PR | Ready for deployment |
| 95 | Excellence | Aspirational |

---

## Skills Quick Reference

| Command | What It Does |
|---------|-------------|
| `/commit [msg]` | Stage, commit, create PR, and merge to main |
| `/compile-latex [file]` | Compile Beamer slide deck with XeLaTeX (3 passes + bibtex) |
| `/context-status` | Show session health and context usage |
| `/create-lecture` | Create new Beamer lecture from papers and materials |
| `/data-analysis [dataset]` | End-to-end R data analysis from exploration through regression |
| `/deep-audit` | Deep consistency audit of the entire repository infrastructure |
| `/devils-advocate` | Challenge slide design with 5-7 pedagogical questions |
| `/extract-tikz [LectureN]` | Extract TikZ diagrams from Beamer source, compile to PDF, convert to SVG |
| `/format-tables` | Apply consistent LaTeX table style for table output |
| `/format-tables-reg` | Apply consistent LaTeX regression table style for Stata output |
| `/interview-me [topic]` | Interactive interview to formalize a research idea |
| `/learn [skill-name]` | Extract reusable knowledge into a persistent skill |
| `/lit-review [topic]` | Structured literature search and synthesis with citation extraction |
| `/pedagogy-review [file]` | Holistic pedagogical review: narrative arc, prerequisites, notation, pacing |
| `/proofread [file]` | Grammar, typo, overflow, consistency review (produces report) |
| `/research-ideation [topic]` | Generate research questions, hypotheses, and empirical strategies |
| `/review-paper [file]` | Comprehensive manuscript review: structure, specification, citations |
| `/review-r [file]` | R code quality, reproducibility, and domain correctness review |
| `/slide-excellence [file]` | Multi-agent slide review (visual, pedagogy, proofreading) |
| `/validate-bib` | Cross-reference citations against bibliography |
| `/visual-audit [file]` | Adversarial visual audit: overflow, font consistency, box fatigue |

---

## Beamer Custom Environments

| Environment | Effect | Use Case |
|---|---|---|
| `questionbox` | Orange background box | Pose questions to the audience |
| `answerbox` | Green border box | Highlight answers or solutions |
| `insightbox` | Orange left-accent bar | Key insights and takeaways |
| `definitionbox[Title]` | Blue bordered titled box | Formal definitions and theorems |
| `methodbox` | Sky blue background box | Methodology or estimation approach |
| `quotebox` | Purple left-accent bar | Quotes, citations, references |

---

## Current Project State

<!-- Update this table as you add lectures/content. -->

| Deck | Beamer | Key Content |
|------|--------|-------------|
| Part 1 | `drafts/slides/part1_random_variables.tex` | Random variables, pdf/cdf, Bernoulli, Binomial, continuous RVs, fat tails. 42 frames. Figures: `scripts/R/part1_figures.R` |
| Part 2 | `drafts/slides/part2_moments.tex` | Built around one question (portfolio weights): expected value, LLN, variance, covariance/correlation, skewness/kurtosis. 25 frames + appendix. Figures: `scripts/R/part2_figures.R` |
| Part 3 | `drafts/slides/part3_inference.tex` | Populations and samples, estimators (unbiased/efficient/consistent), standard errors, CLT, hypothesis testing, $p$-values, multiple testing, confidence intervals. 42 frames. Figures: `scripts/R/part3_figures.R` |

**Note:** LaTeX preambles live in `drafts/latex_files/` (not `scripts/latex_preambles/`).
Compile slides from `drafts/slides/` with `TEXINPUTS=../latex_files:` and
`BIBINPUTS=../latex_files:`.
