---
name: compile-paper
description: Compile a LaTeX paper/document with pdflatex using MacTeX. Use when compiling research papers or documents in drafts/documents/.
argument-hint: "[filename without .tex extension, default: main]"
allowed-tools: ["Read", "Bash", "Glob"]
---

# Compile LaTeX Paper

Compile a research paper or document using MacTeX pdflatex with preambles from `drafts/latex_files/`.

## Steps

1. **Set the filename** — use `$ARGUMENTS` if provided, otherwise default to `main`.

2. **Compile from `drafts/documents/`** using MacTeX pdflatex (never TinyTeX):

```bash
cd drafts/documents
TEXINPUTS=../latex_files:$TEXINPUTS /Library/TeX/texbin/pdflatex -interaction=nonstopmode $FILENAME.tex
```

3. **If the document has a bibliography** (check for `\bibliography{}`), run bibtex and recompile:

```bash
cd drafts/documents
BIBINPUTS=../latex_files:$BIBINPUTS /Library/TeX/texbin/bibtex $FILENAME
TEXINPUTS=../latex_files:$TEXINPUTS /Library/TeX/texbin/pdflatex -interaction=nonstopmode $FILENAME.tex
TEXINPUTS=../latex_files:$TEXINPUTS /Library/TeX/texbin/pdflatex -interaction=nonstopmode $FILENAME.tex
```

4. **Check for warnings** in the `.log` file:
   - Grep for `Overfull \\hbox` warnings
   - Grep for `undefined citations` or `Label(s) may have changed`
   - Report any issues found

5. **Report results:**
   - Compilation success/failure
   - Number of overfull hbox warnings
   - Any undefined citations
   - PDF page count

## Important
- **Always use MacTeX** at `/Library/TeX/texbin/pdflatex` — never TinyTeX (`/usr/local/bin/pdflatex`)
- **TEXINPUTS** includes `drafts/latex_files/` so `\input{header_doc}` resolves correctly
- Available preambles: `header_doc.tex` (articles/papers), `header_slides.tex` (Beamer)
- Output PDF lands in `drafts/documents/`
