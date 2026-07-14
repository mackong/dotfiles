---
name: matplotlib
description: 'This skill should be used when the user invokes "/matplotlib" to plot data from the current context using matplotlib (via uv) and output the resulting image path.'
tools: Bash
disable-model-invocation: true
---

# Plot data with matplotlib

Plot data from the most recent interaction context using matplotlib. Generate a PNG image with a transparent background, using a fixed palette designed to stay legible on both light and dark page backgrounds, and output it as a markdown image so it renders inline.

## How to plot

1. Extract or derive plottable data from the current context.
2. Write a Python script to a temporary file using the template below.
3. Run the script with `uv run --with matplotlib`.
4. Output the result as a markdown image on its own line:
   ```
   ![description](/tmp/agent-plot-XXXX.png)
   ```

```sh
uv run --with matplotlib /tmp/agent-plot-XXXX.py
```

## Cross-environment palette

Like the [[gnuplot]] skill, matplotlib plots sit on the transparent canvas with no shape fills to anchor text, so the palette picks colors with contrast on both white and dark backgrounds:

- **Chrome** (spines, ticks, axis labels, title) — medium accent `#5DADE2`.
- **Grid** — subdued gray `#7F8C8D` with alpha so it sits behind the data.
- **Data series** — five-color cycle (`#5DADE2`, `#F39C12`, `#48C9B0`, `#AF7AC5`, `#E74C3C`).
- **Legend** — solid dark fill (`#2C3E50`) with light text (`#ECF0F1`) and accent edge so legend entries stay readable even when they sit on top of plot data; matches the [[plantuml]] "filled shape + light text" pattern.

## Python script template

```python
import matplotlib.pyplot as plt

# Cross-environment palette — legible on both light and dark backgrounds
FG    = "#5DADE2"  # spines, ticks, axis labels, title
GRID  = "#7F8C8D"  # gridlines
FILL  = "#2C3E50"  # solid fill behind the legend
LIGHT = "#ECF0F1"  # text on the solid fill
CYCLE = ["#5DADE2", "#F39C12", "#48C9B0", "#AF7AC5", "#E74C3C"]

fig, ax = plt.subplots(figsize=(10, 6))
fig.patch.set_alpha(0)
ax.set_facecolor('none')
ax.set_prop_cycle(color=CYCLE)

ax.spines['bottom'].set_color(FG)
ax.spines['left'].set_color(FG)
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.tick_params(colors=FG)
ax.xaxis.label.set_color(FG)
ax.yaxis.label.set_color(FG)
ax.title.set_color(FG)
ax.grid(True, alpha=0.3, color=GRID)

# ... plot commands using the data ...

# If a legend is used, anchor its text on a solid fill so it stays readable:
# ax.legend(facecolor=FILL, edgecolor=FG, labelcolor=LIGHT)

plt.tight_layout()
plt.savefig("/tmp/agent-plot-XXXX.png", dpi=150, transparent=True)
```

## Rules

- Use the fixed palette above for every plot — do **not** query the user's terminal/Emacs foreground color. The palette is designed to be environment-agnostic.
- Always use `fig.patch.set_alpha(0)` and `ax.set_facecolor('none')` for transparent background, and `transparent=True` in `savefig`. Readability comes from color choice, not the page background.
- Always use a timestamp in the filename (e.g., `/tmp/agent-plot-$(date +%s).png`). Never use descriptive names like `agent-plot-lorenz.png`.
- Always run scripts with `uv run --with matplotlib`. Do not use `pip install`.
- Prefer the predefined `CYCLE` colors via `ax.set_prop_cycle` so data series stay cross-environment. Only override per-plot when you have a specific reason (e.g. a "good"/"bad" semantic encoding).
- When a legend is used, give it a solid fill (`facecolor=FILL, edgecolor=FG, labelcolor=LIGHT`) so its text never sits on the bare transparent canvas.
- After the script runs successfully, output a markdown image (`![description](path)`) on its own line.
- Choose an appropriate plot type for the data (line, bar, scatter, histogram, pie, heatmap, etc.).
- Include a title, axis labels, and a legend when they add clarity.
- Hide top and right spines for a cleaner look (already in the template).
- If no plottable data exists in the recent context, inform the user.
