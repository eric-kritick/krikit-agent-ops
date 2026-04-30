# krikit-agent-ops task runner.
#
# Install the `just` binary: `brew install just` (or `ghcup install just` if
# ghcup ever ships it). Then run `just` with no args to list recipes, or
# `just <recipe>` to execute one.

# Default recipe: list everything that's available.
default:
    @just --list --unsorted

# -----------------------------------------------------------------------------
# Build / test / install
# -----------------------------------------------------------------------------

# Compile library + executables + tests (no install).
build:
    cabal build all

# Run the hspec test suite.
test:
    cabal test

# Run a quick syntax + warning check without producing artifacts.
check:
    cabal build --dry-run

# Install every built executable into $HOME/.local/bin via copy.
install:
    cabal install --installdir "{{env_var('HOME')}}/.local/bin" \
                  --install-method copy \
                  --overwrite-policy=always

# Why mirror at all: agentops's launchd context can't traverse
# opsadmin's home (mode 700), so binaries living at
# `/Users/opsadmin/.local/bin/` are unreachable from the
# `ai.krikit.*` daemons. The system-path mirror is root-owned and
# world-traversable, so every daemon (regardless of UserName) can
# exec from there.
#
# Mirrored set covers the 9 launchd-driven binaries: `krikit-monitor`
# (runs as agentops), `krikit-update-status` + `krikit-regen-summary`
# (invoked from monitor's digest path), and the 6 regen + verify
# daemons (PB 6 / PB 26; run as opsadmin but exec from the same
# system path so the wrapper set is uniform).
#
# Opsadmin-only tools (krikit-{health,restart,smoke,summary,update,
# versions}) are deliberately NOT mirrored -- they're operator
# CLIs, run from opsadmin's PATH, and don't need a system home.
#
# Same as `install`, plus mirror every launchd-driven binary to /usr/local/bin/krikit/. Use on the mini.
install-system: install _mirror-system

# (private) Mirror every launchd-driven binary from `~/.local/bin/`
# to `/usr/local/bin/krikit/`. Idempotent. Used by `install-system`
# and `update-system`.
_mirror-system:
    #!/usr/bin/env bash
    set -euo pipefail
    sudo install -d /usr/local/bin/krikit
    for bin in \
        krikit-monitor \
        krikit-update-status \
        krikit-regen-summary \
        krikit-regen-system-state-mini \
        krikit-regen-repo-inventory \
        krikit-regen-cross-reference-index \
        krikit-verify-reading-order \
        krikit-verify-llm-channel-consistency \
        krikit-verify-repo-inventory \
    ; do
        sudo install -m 755 \
            "${HOME}/.local/bin/${bin}" \
            "/usr/local/bin/krikit/${bin}"
    done

# Delete all build artifacts.
clean:
    cabal clean
    rm -rf dist-newstyle

# -----------------------------------------------------------------------------
# Update flow (mini + admin laptop)
# -----------------------------------------------------------------------------

# Pull from origin, rebuild, reinstall. The common `git pull && cabal build
# && cabal install` loop after someone pushes a change. Safe to run
# repeatedly; cabal install --overwrite-policy=always silently replaces.
update:
    git pull --ff-only
    cabal update
    cabal build all
    just install
    @echo ""
    @echo "Installed binaries:"
    @which krikit-smoke || echo "  (krikit-smoke not on PATH -- see PB 25 Phase 6)"

# Same as `update` but skip the `git pull` (useful after local edits).
rebuild: build install

# Use this on the mini -- it's the canonical "pull the latest
# agent-ops chain and put it where launchd can run it" recipe.
# See `install-system` above for the full mirror set.
#
# Same as `update`, plus mirror every launchd-driven binary to /usr/local/bin/krikit/.
update-system: update _mirror-system

# -----------------------------------------------------------------------------
# Run / inspect
# -----------------------------------------------------------------------------

# Invoke krikit-smoke via cabal (skips reinstall; good for dev iteration).
# Pass args after the recipe name: `just smoke --fast`.
smoke *ARGS:
    cabal run krikit-smoke -- {{ARGS}}

# Print krikit-smoke's usage block without exercising the stack.
smoke-help:
    cabal run krikit-smoke -- --help

# GHCi REPL with the library loaded.
repl:
    cabal repl krikit-agent-ops

# Tail what GHC + cabal + ghcup think is installed.
versions:
    @ghc --version
    @cabal --version | head -1
    @ghcup --version

# -----------------------------------------------------------------------------
# Style
# -----------------------------------------------------------------------------

# Format the codebase with fourmolu (install: `brew install fourmolu`).
fmt:
    fourmolu -i src app test

# Check formatting without modifying files. Exits non-zero on drift;
# useful for CI when we eventually add it.
fmt-check:
    fourmolu --mode check src app test
