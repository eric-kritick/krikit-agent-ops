# krikit-agent-ops

Haskell-based ops tooling for the krikit agent host.

## Tools

| Binary         | Purpose                                                     |
|----------------|-------------------------------------------------------------|
| `krikit-smoke` | End-to-end smoke test of the agent stack (see Playbook 23). |

More to follow; each new tool ships as an additional `executable`
stanza in `krikit-agent-ops.cabal`.

## Build

Requires GHC 9.4+ and cabal 3.10+. Install via
[ghcup](https://www.haskell.org/ghcup/); see Playbook 25 for the
mac-mini-side setup.

    cabal build
    cabal run krikit-smoke

## Install

    cabal install --installdir "$HOME/.local/bin" \
                  --install-method copy \
                  --overwrite-policy=always

This drops native binaries straight into PATH. `krikit-update-ops`
(Playbook 19, future addition) wraps `git pull && cabal install`.

## Structure

- `src/Krikit/Agent/Ops/*` — shared library (subprocess, JSON, output helpers).
- `app/<tool>/Main.hs` — one executable per tool.
- `test/Spec.hs` — HSpec + QuickCheck.

All modules live under `Krikit.Agent.Ops`. See `AGENTS.md` for
full conventions (types, effects, errors, style, anti-patterns).
