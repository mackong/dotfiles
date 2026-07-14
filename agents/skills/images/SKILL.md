---
name: images
description: 'This skill should be used when the user invokes "/images" to generate an image from the current context using OpenAI''s images generation endpoint and output the resulting image path so Emacs renders it inline.'
tools: Bash
disable-model-invocation: true
---

# Generate images with OpenAI

Generate an image from the most recent interaction context using OpenAI's `POST /api/v1/images/generations` endpoint. Save the resulting PNG to a temporary file and output it as a markdown image so Emacs renders it inline.

## How to generate an image

1. Derive a clear, specific prompt from the current context. If the context has none, ask the user what to generate instead of guessing.
2. Build the JSON request body (see fields below). Default to `gpt-image-2`, `1024x1024`, `quality: "high"`, `n: 1`.
3. Call the endpoint with `curl`, parse the base64 response, and write each image to a timestamped temp file.
4. Output one markdown image per file on its own line:
   ```
   ![prompt summary](/tmp/agent-image-XXXX.png)
   ```

## curl invocation

```sh
: "${OPENAI_API_KEY:?set OPENAI_API_KEY before running /images}"
BASE_URL="${OPENAI_BASE_URL:-https://zenmux.ai/api/v1}"

TS=$(date +%s)
REQ=$(mktemp)
RESP=$(mktemp)

cat > "$REQ" <<'JSON'
{
  "model": "gpt-image-2",
  "prompt": "<derived prompt goes here>",
  "size": "1024x1024",
  "n": 1,
  "quality": "high"
}
JSON

curl -sS "$BASE_URL/images/generations" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -d @"$REQ" \
  -o "$RESP"

# Surface API errors instead of writing an empty PNG
if jq -e '.error' "$RESP" > /dev/null; then
  jq -r '.error | "image generation failed: \(.code // "error") — \(.message)"' "$RESP"
  exit 1
fi

# Write each returned image to its own timestamped file, then emit markdown.
# Stream the base64 directly from jq into base64 -d — never put it in a shell
# variable, since a single 1024x1024 high-quality PNG can encode to >1 MB and
# blow past ARG_MAX when passed as a command-line argument.
COUNT=$(jq '.data | length' "$RESP")
for i in $(seq 1 "$COUNT"); do
  OUT="/tmp/agent-image-${TS}-${i}.png"
  jq -r ".data[$((i-1))].b64_json" "$RESP" | base64 -d > "$OUT"
  [ -s "$OUT" ] || { echo "empty image written to $OUT"; exit 1; }
  echo "![<short caption from prompt>]($OUT)"
done
```

The script intentionally fails loudly on API errors and never announces an empty file.

## Request fields

| Field | Default here | Notes |
|-------|--------------|-------|
| `model` | `gpt-image-2` | Stable image model. `gpt-image-1.5` also work; their `size`/`quality` enums differ. |
| `prompt` | — | Required. Be specific — subject, composition, style, lighting. |
| `size` | `1024x1024` | `gpt-image-2`: `1024x1024`, `1024x1536`, `1536x1024`, or `auto`. |
| `n` | `1` | Number of images. `gpt-image-2` supports 1–10. |
| `quality` | `high` | `gpt-image-2`: `low`, `medium`, `high`, `auto`. |
| `background` | omit | `gpt-image-2` only: `transparent`, `opaque`, or `auto`. |
| `output_format` | omit (defaults to png) | `gpt-image-2` only: `png`, `jpeg`, `webp`. Change the file extension to match. |
| `response_format` | omit | `gpt-image-2` always returns `b64_json` — do **not** send this field for it. |

The response always has `data[].b64_json` for `gpt-image-2`.

## Environment

- **`OPENAI_API_KEY`** — required. Never hard-code keys in the script.
- **`OPENAI_BASE_URL`** — optional override (e.g. for an OpenAI-compatible proxy). Defaults to `https://zenmux.ai/api/v1`. The path appended is always `/images/generations`.

## Rules

- Always write each PNG to a timestamped temporary file (e.g. `/tmp/agent-image-$(date +%s)-1.png`). Never use descriptive names — multiple runs in the same session must not collide.
- Always decode the base64 response with `jq -r '.data[].b64_json' | base64 -d`. The script must stay self-contained — no helper scripts, no external decoders. (`-d` works on both GNU `base64` and modern BSD `base64`; the legacy BSD `-D` form is not portable.)
- Always check for `.error` in the response before writing files. Surface the API's error message to the user instead of silently producing an empty image.
- After curl finishes successfully, verify each file is non-empty before announcing it.
- Output one markdown image (`![caption](path)`) per generated file, each on its own line, so Emacs renders them inline.
- Use a short, descriptive caption derived from the prompt (≤ ~60 chars). Do not paste the entire prompt into the alt text if it's long.
- If `OPENAI_API_KEY` is missing, stop and instruct the user to set it instead of running the request.
- If `n > 1`, generate all images in one API call (cheaper and faster) — do not loop curl.
- If no prompt can be derived from the recent context, ask the user what to generate.
- Match the file extension to `output_format` when set (`.png`, `.jpg`, `.webp`).
