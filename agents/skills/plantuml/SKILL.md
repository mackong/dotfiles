---
name: plantuml
description: 'This skill should be used when the user invokes "/plantuml" to create a diagram from the current context using PlantUML and output the resulting image path.'
tools: Bash
disable-model-invocation: true
---

# Create diagrams with PlantUML

Create a diagram from the most recent interaction context using PlantUML. Generate a PNG image with a transparent background, using a fixed palette designed to stay legible on both light and dark backgrounds, and output it as a markdown image so it renders inline.

## How to create a diagram

1. Extract or derive diagrammable data from the current context.
2. Write a PlantUML file to a temporary file using the template below.
3. Run PlantUML on the file.
4. Output the result as a markdown image on its own line:
   ```
   ![description](/tmp/agent-diagram-XXXX.png)
   ```

```sh
plantuml -tpng /tmp/agent-diagram-XXXX.puml
```

## Cross-environment palette

The template avoids the dark-vs-light readability trap by never relying on the page background:

- **Shape fills** are dark (`#2C3E50`) so light text on them is always legible.
- **Text inside shapes** is light (`#ECF0F1`).
- **Borders, arrows, lifelines, and free-floating labels** use a medium accent (`#5DADE2`) that retains contrast on both white and dark backgrounds.
- **Group/divider backgrounds** use a lighter slate (`#34495E`) so headers sit on a solid fill rather than on the transparent canvas.

## PlantUML template

```plantuml
@startuml
skinparam backgroundColor transparent
skinparam shadowing true
skinparam roundcorner 10
skinparam defaultFontName "Helvetica"
skinparam defaultFontColor #ECF0F1
skinparam defaultTextAlignment center

' Shape fills — dark so light text on them is readable everywhere
skinparam actorBackgroundColor #2C3E50
skinparam participantBackgroundColor #2C3E50
skinparam collectionsBackgroundColor #2C3E50
skinparam classBackgroundColor #2C3E50
skinparam componentBackgroundColor #2C3E50
skinparam interfaceBackgroundColor #2C3E50
skinparam packageBackgroundColor #34495E
skinparam noteBackgroundColor #34495E
skinparam rectangleBackgroundColor #2C3E50
skinparam usecaseBackgroundColor #2C3E50
skinparam stateBackgroundColor #2C3E50
skinparam activityBackgroundColor #2C3E50
skinparam objectBackgroundColor #2C3E50
skinparam databaseBackgroundColor #2C3E50
skinparam queueBackgroundColor #2C3E50
skinparam storageBackgroundColor #2C3E50

' Text INSIDE filled shapes — light for contrast against dark fills
skinparam participantFontColor #ECF0F1
skinparam collectionsFontColor #ECF0F1
skinparam classFontColor #ECF0F1
skinparam classAttributeFontColor #ECF0F1
skinparam componentFontColor #ECF0F1
skinparam interfaceFontColor #ECF0F1
skinparam packageFontColor #ECF0F1
skinparam noteFontColor #ECF0F1
skinparam stateFontColor #ECF0F1
skinparam activityFontColor #ECF0F1
skinparam usecaseFontColor #ECF0F1
skinparam objectFontColor #ECF0F1
skinparam rectangleFontColor #ECF0F1

' Labels that float on the transparent canvas (actor/database/etc render their
' name BELOW the shape) — use the medium accent so they stay legible on both
' light and dark page backgrounds
skinparam actorFontColor #5DADE2
skinparam databaseFontColor #5DADE2
skinparam queueFontColor #5DADE2
skinparam storageFontColor #5DADE2

' Borders, arrows, free-floating lines — medium accent visible on both backgrounds
skinparam actorBorderColor #5DADE2
skinparam participantBorderColor #5DADE2
skinparam collectionsBorderColor #5DADE2
skinparam classBorderColor #5DADE2
skinparam componentBorderColor #5DADE2
skinparam interfaceBorderColor #5DADE2
skinparam packageBorderColor #5DADE2
skinparam noteBorderColor #5DADE2
skinparam rectangleBorderColor #5DADE2
skinparam stateBorderColor #5DADE2
skinparam activityBorderColor #5DADE2
skinparam usecaseBorderColor #5DADE2
skinparam objectBorderColor #5DADE2
skinparam arrowColor #5DADE2
skinparam arrowFontColor #5DADE2
skinparam sequenceLifeLineBorderColor #5DADE2
skinparam sequenceArrowColor #5DADE2

' Sequence group/divider — backgrounds keep header text legible on transparent canvas
skinparam sequenceGroupHeaderFontColor #ECF0F1
skinparam sequenceGroupBorderColor #5DADE2
skinparam sequenceGroupBackgroundColor #34495E
skinparam sequenceGroupBodyBackgroundColor #34495E
skinparam sequenceDividerFontColor #ECF0F1
skinparam sequenceDividerBorderColor #5DADE2
skinparam sequenceDividerBackgroundColor #34495E
skinparam sequenceReferenceBackgroundColor #34495E
skinparam sequenceReferenceBorderColor #5DADE2
skinparam sequenceReferenceFontColor #ECF0F1
skinparam sequenceBoxBackgroundColor #34495E
skinparam sequenceBoxBorderColor #5DADE2
skinparam sequenceBoxFontColor #ECF0F1

' Title — accent color so it stands out without depending on background
skinparam titleFontColor #5DADE2

' Stereotype text inside shapes
skinparam stereotypeCBackgroundColor #34495E
skinparam stereotypeIBackgroundColor #34495E
skinparam stereotypeNBackgroundColor #34495E
skinparam stereotypeABackgroundColor #34495E
skinparam stereotypeEBackgroundColor #34495E

' ... diagram content ...
@enduml
```

## Rules

- Use the fixed palette above for every diagram — do **not** query the user's terminal/Emacs foreground color. The palette is designed to be environment-agnostic.
- Always keep `skinparam backgroundColor transparent`. Readability comes from the shapes' own fills, not from the page background.
- Always use a timestamp in the filename (e.g., `/tmp/agent-diagram-$(date +%s).png`). Never use descriptive names.
- Anything that floats on the transparent canvas (arrows, free-standing labels, group headers) must use either the medium accent (`#5DADE2`) for lines/labels or sit on a `#34495E` fill — never plain dark or plain light text on transparent.
- After PlantUML runs successfully, output a markdown image (`![description](path)`) on its own line.
- Choose an appropriate diagram type for the data (sequence, class, component, activity, state, etc.).
- Include a title when it adds clarity.
- If no diagrammable data exists in the recent context, inform the user.
