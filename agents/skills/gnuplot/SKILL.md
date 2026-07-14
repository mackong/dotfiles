---
name: gnuplot
description: 'This skill should be used when the user invokes "/gnuplot" to plot data from the current context using gnuplot and output the resulting image path.'
tools: Bash
disable-model-invocation: true
---

# Plot data with gnuplot

Plot data from the most recent interaction context using gnuplot. Generate a PNG image with a transparent background, using a fixed palette designed to stay legible on both light and dark page backgrounds, and output it as a markdown image so it renders inline.

## How to plot

1. Extract or derive plottable data from the current context.
2. Write a gnuplot script to a temporary file using the template below.
3. Run gnuplot on the script.
4. Output the result as a markdown image on its own line:
   ```
   ![description](/tmp/agent-plot-XXXX.png)
   ```

```sh
gnuplot /tmp/agent-plot-XXXX.gp
```

## Cross-environment palette

Gnuplot has no "shape fill" to anchor text against, so everything sits on the transparent canvas. The palette therefore picks colors with enough contrast on both white and dark backgrounds:

- **Chrome** (border, ticks, axis labels, title, key text) — medium accent `#5DADE2`.
- **Grid** (if used) — subdued gray `#7F8C8D` so it sits behind the data without dominating.
- **Data series** — a five-color cycle (`#5DADE2`, `#F39C12`, `#48C9B0`, `#AF7AC5`, `#E74C3C`). Each is medium-saturation and stays visible on both light and dark page backgrounds.

This matches the [[plantuml]] skill's accent color so diagrams and plots share a look.

## Gnuplot script template

```gnuplot
set terminal pngcairo transparent enhanced size 800,500
set output "/tmp/agent-plot-XXXX.png"

# Cross-environment palette — legible on both light and dark backgrounds
FG   = "#5DADE2"  # axes, ticks, labels, title, key
GRID = "#7F8C8D"  # optional grid

set border lc rgb FG
set key textcolor rgb FG
set xlabel textcolor rgb FG
set ylabel textcolor rgb FG
set title textcolor rgb FG
set xtics textcolor rgb FG
set ytics textcolor rgb FG

# Uncomment if you want a grid:
# set grid lc rgb GRID lw 1

# Data-series colors — medium saturation, readable on both backgrounds
set linetype 1 lc rgb "#5DADE2" lw 2
set linetype 2 lc rgb "#F39C12" lw 2
set linetype 3 lc rgb "#48C9B0" lw 2
set linetype 4 lc rgb "#AF7AC5" lw 2
set linetype 5 lc rgb "#E74C3C" lw 2

# ... plot commands using the data ...
```

## Rules

- Use the fixed palette above for every plot — do **not** query the user's terminal foreground color. The palette is designed to be environment-agnostic.
- Always use `pngcairo transparent` terminal. Readability comes from color choice, not the page background.
- Always use a timestamp in the filename (e.g., `/tmp/agent-plot-$(date +%s).png`). Never use descriptive names like `agent-plot-lorenz.png`.
- Use inline data (`$DATA << EOD ... EOD`) when practical. For large datasets, write a separate data file.
- Prefer the predefined `linetype` cycle for data series so colors stay cross-environment. Only override per-plot when you have a specific reason (e.g. a "good"/"bad" semantic encoding).
- After gnuplot runs successfully, output a terminal image (`![description](path)`) on its own line.
- Choose an appropriate plot type for the data (lines, bars, histogram, scatter, etc.).
- Include a title, axis labels, and a legend when they add clarity.
- Use `enhanced` text mode for subscripts/superscripts when needed.
- If no plottable data exists in the recent context, inform the user.
