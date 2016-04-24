# Elm Encounters

This is an encounter builder for Dungeons & Dragons 5th edition written in Elm.

## Next Steps

1. Current build is broken because I can't figure out how to extract CharacterList properly. Figure that out and then extract MonsterList.
2. All of the UI design and CSS.

## Development

Start local server in root directory:

    python -m SimpleHTTPServer 5000  (or `npm run server`)

Build project:

    npm run build  (or `npm run watch` for auto-rebuild on fs change)

Navigate to <http://localhost:5000> (which loads your `index.html`).

**Note:** Make your edits to `index.template.html` and consider `index.html` an ephemeral file. This is because index.html gets overwritten easily. For example, `elm make` overwrites it by default.
