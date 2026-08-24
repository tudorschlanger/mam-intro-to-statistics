---
name: compile-beamer
description: Compile a Beamer LaTeX slide deck with XeLaTeX (3 passes + bibtex). Use when compiling lecture slides.
argument-hint: "[filename without .tex extension]"
allowed-tools: ["Read", "Bash", "Glob"]
---

# Compile Beamer LaTeX Slides

Compile a Beamer slide deck using XeLaTeX with full citation resolution.

## Steps

1. **Navigate to drafts/slides/ directory** and compile with 3-pass sequence:

```bash
cd drafts/slides
TEXINPUTS=../latex_files:$TEXINPUTS xelatex -interaction=nonstopmode $ARGUMENTS.tex
BIBINPUTS=../latex_files:$BIBINPUTS BSTINPUTS=../latex_files:$BSTINPUTS bibtex $ARGUMENTS
TEXINPUTS=../latex_files:$TEXINPUTS xelatex -interaction=nonstopmode $ARGUMENTS.tex
TEXINPUTS=../latex_files:$TEXINPUTS xelatex -interaction=nonstopmode $ARGUMENTS.tex
```

**Alternative (latexmk):**
```bash
cd drafts/slides
TEXINPUTS=../latex_files:$TEXINPUTS BIBINPUTS=../latex_files:$BIBINPUTS BSTINPUTS=../latex_files:$BSTINPUTS latexmk -xelatex -interaction=nonstopmode $ARGUMENTS.tex
```

2. **Check for warnings:**
   - Grep output for `Overfull \\hbox` warnings
   - Grep for `undefined citations` or `Label(s) may have changed`
   - Report any issues found

3. **Open the PDF** for visual verification:
   ```bash
   open drafts/slides/$ARGUMENTS.pdf          # macOS
   # xdg-open drafts/slides/$ARGUMENTS.pdf    # Linux
   ```

4. **Report results:**
   - Compilation success/failure
   - Number of overfull hbox warnings
   - Any undefined citations
   - PDF page count

## Why 3 passes?
1. First xelatex: Creates `.aux` file with citation keys
2. bibtex: Reads `.aux`, generates `.bbl` with formatted references
3. Second xelatex: Incorporates bibliography
4. Third xelatex: Resolves all cross-references with final page numbers

## Important
- **Always use XeLaTeX**, never pdflatex
- **TEXINPUTS** is required: your Beamer preamble lives in `drafts/latex_files/`
- **BIBINPUTS** is required: your `.bib` file lives in `drafts/latex_files/`
