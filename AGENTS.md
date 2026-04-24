# krikit-agent-ops — agent notes

Ops tooling for the krikit agent host, written in Haskell.

## Layout

- Single cabal package, library + multiple executables.
- `src/Krikit/*` holds shared logic.
- `app/<tool>/Main.hs` is the entrypoint for each binary.
- `test/Spec.hs` is the shared test suite.

## Adding a tool

1. Create `app/<tool>/Main.hs`.
2. Add an `executable <tool>` stanza to `krikit-agent-ops.cabal`
   mirroring `krikit-smoke`'s stanza.
3. `cabal build` — new binary appears under `dist-newstyle/`.
4. `cabal install` — lands in `$HOME/.local/bin/<tool>`.

## Style conventions

- Strict ADTs for domain types. Tier status, subprocess outcomes,
  config keys — never stringly-typed.
- Effects at the edges: pure tier logic inside, `IO`-heavy
  subprocess/HTTP wrappers at the boundary.
- Exhaustive pattern matching on every sum type. No catch-all `_`
  branches for closed variants.
- No partial functions. Return `Either`/`Maybe`; parse at the edges.

## Build/test loop

    cabal build    # compile all targets
    cabal test     # run the test suite
    cabal run krikit-smoke -- --help
