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

# Same as `install`, plus mirror krikit-update-status to a system path
# readable by all users. Needed on the mini because monitor.py runs as
# agentops and invokes krikit-update-status from the daily digest --
# /Users/opsadmin/.local/bin/ may not be traversable by agentops on
# hardened hosts.
install-system: install
    sudo install -d /usr/local/bin/krikit
    sudo install -m 755 \
        "{{env_var('HOME')}}/.local/bin/krikit-update-status" \
        /usr/local/bin/krikit/krikit-update-status

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

# Same as `update` but also mirrors update-status to /usr/local/bin/krikit/.
# Use this on the mini after pulling a change to the update-status binary.
update-system: update
    sudo install -m 755 \
        "{{env_var('HOME')}}/.local/bin/krikit-update-status" \
        /usr/local/bin/krikit/krikit-update-status

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
