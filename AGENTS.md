# krikit-agent-ops — conventions

How code is written in this repo. Applies to any agent (Claude, Codex,
human) working here. Adapts the PureScript-flavored principles in
`~/.claude/CLAUDE.md` (types first, effects at the edges, make illegal
states unrepresentable) to Haskell specifics.

This file is the source of truth; `CLAUDE.md` is a symlink to it so
tools looking for either name find the same content.

---

## Scope

This repo holds Haskell-based CLI tooling for the krikit agent host.
Typical binary: a short-lived operator command (`krikit-smoke`, future
`krikit-update-ops`, etc.) that orchestrates subprocess calls, HTTP
probes, and JSON I/O, and prints a pass/fail report.

Expected weight: 200–2000 lines of Haskell per tool. No web servers,
no long-running daemons, no concurrency fanning out to thousands of
workers. If a future tool genuinely needs that shape, revisit the
conventions below rather than stretching them.

---

## Module namespace and layout

All modules live under `Krikit.Agent.Ops.*`.

```
src/Krikit/Agent/Ops/
    Version.hs               shared: package version
    Process.hs               shared: typed-process wrappers
    Output.hs                shared: pretty + JSON Lines formatters
    Http.hs                  shared: http-client wrappers
    Smoke/                   smoke-test-specific modules
        Tier.hs              tier ADTs + results
        Run.hs               orchestration
        Report.hs            formatter/history

app/smoke/Main.hs            krikit-smoke entrypoint
app/<future-tool>/Main.hs    one directory per executable

test/Spec.hs                 hspec + quickcheck test suite
```

Rules:

- Top-level namespace is `Krikit.Agent.Ops`. Never `Krikit.Smoke` or
  `Ops.Something` — keep the namespace stable so future tools slot
  in without rename churn.
- One dominant type per module. If a file has three coequal domain
  types, split.
- Shared helpers go under `Krikit.Agent.Ops.<Helper>`; tool-specific
  logic goes under `Krikit.Agent.Ops.<Tool>.<Part>`.
- Executables (under `app/`) are thin: `main = run <$> parseArgs` and
  little more. All real logic lives in the library so tests can import
  it.

---

## Types and domain modeling

Mirror what the global CLAUDE.md says for PureScript, translated to
Haskell:

### Use sum types for closed sets of cases

```haskell
-- Good
data TierStatus
    = Pass
    | Fail Text          -- reason
    | Skip Text
    deriving stock (Eq, Show)

-- Bad — stringly-typed, no exhaustiveness
tierStatus :: Text
tierStatus = "pass"   -- or "fail"? "FAIL"? "failed"? compiler can't help
```

Derive `(Eq, Show)` by default. Add `Ord` when needed for `Set`/`Map`
keys. Use `deriving stock` explicitly so `DerivingStrategies` doesn't
ambush you — GHC2021 implies it.

### Use `newtype` for typed identifiers and validated values

```haskell
newtype AgentName = AgentName Text
    deriving stock   (Eq, Show)
    deriving newtype (IsString)   -- if you want "main" :: AgentName

newtype TimeoutSeconds = TimeoutSeconds Int
    deriving stock   (Eq, Show, Ord)

-- Smart constructor for validated values:
mkTimeout :: Int -> Maybe TimeoutSeconds
mkTimeout n | n > 0 && n <= 3600 = Just (TimeoutSeconds n)
            | otherwise          = Nothing
```

Don't pass bare `Int`/`Text`/`String` through the domain layer when
the value has a meaning. A function signature of
`runTier :: AgentName -> TimeoutSeconds -> IO TierResult` tells the
reader (and the compiler) what each argument is; `Text -> Int -> IO _`
tells them nothing.

### Numbers with a unit always get a newtype

Whenever a number in the domain carries a unit — seconds,
milliseconds, bytes, lines, count — wrap it. The cost is a newtype
declaration plus one helper; the payoff is a compile-time error the
first time someone tries to pass milliseconds where seconds were
expected (exactly the bug `timeout (n * 1_000_000)` is prone to).

```haskell
newtype Seconds = Seconds Int
    deriving stock   (Eq, Show)
    deriving newtype (Ord)

newtype Milliseconds = Milliseconds Int
    deriving stock   (Eq, Show)
    deriving newtype (Ord)

secondsToMicros :: Seconds -> Int
secondsToMicros (Seconds n) = n * 1_000_000
```

Deliberately **no** `Num` instance — that would let `Seconds *
Seconds` typecheck (dimensionally wrong), which defeats the whole
point. If arithmetic within the unit is needed, export named
helpers (`addSeconds`, `halfSeconds`) rather than typeclass magic.

Pattern: `Krikit.Agent.Ops.Units` holds the shared units for this
repo. Add new units there. Conversions between units are explicit
named functions, never implicit.

### Exhaustive pattern matching

`-Wall` turns on `-Wincomplete-patterns` — non-exhaustive matches are
warnings, and `-Werror` in CI (future) promotes them to failures. Don't
suppress with catch-all `_` on a closed sum:

```haskell
-- Good — compiler catches the missing Skip case if you add one
formatStatus :: TierStatus -> Text
formatStatus = \case
    Pass          -> "OK"
    Fail reason   -> "FAIL: " <> reason
    Skip reason   -> "SKIP: " <> reason

-- Bad — adding a new TierStatus constructor silently falls into the _
formatStatus :: TierStatus -> Text
formatStatus Pass = "OK"
formatStatus _    = "not OK"
```

Catch-all patterns are only acceptable for:

- **Types you don't own** with many stable constructors (`Aeson.Value`,
  `IOError`, `ExitCode`). Even here, handle the specific cases you
  care about explicitly and let the rest fall through, with a comment.
- **Intentional "everything else is handled the same"** branches (like a
  polling loop where every error means "try again"). In those cases,
  write the branches out anyway — `Right _ -> retry; Left _ -> retry`
  is only four more lines than `_ -> retry` and makes the intent
  obvious.

For every ADT we control in this repo (`Tier`, `Service`, `Agent`,
`TierStatus`, `ProcError`, `ProbeError`, `LogLevel`, ...), write
every case.

### Records with named fields

Use records over tuples for anything with ≥2 fields where order isn't
inherent. GHC2021 supports `NoFieldSelectors` + `DuplicateRecordFields`
+ `OverloadedRecordDot` if name collisions become painful; introduce
those flags per-module only when the pain arrives.

```haskell
data TierResult = TierResult
    { tierName    :: !Text
    , tierStatus  :: !TierStatus
    , tierElapsed :: !Milliseconds
    , tierDetails :: ![Text]
    }
    deriving stock (Eq, Show)
```

Strictness annotations (`!`) on record fields in short-lived ops tools
are cheap insurance against accumulator-style laziness bugs. Default
them on.

### Make illegal states unrepresentable

If a `TierResult` with `Pass` status shouldn't carry a reason, encode
that: put the reason inside the `Fail`/`Skip` constructors (see
`TierStatus` above), not as a separate `tierReason :: Maybe Text`
field. The latter allows `TierResult { tierStatus = Pass, tierReason =
Just "broken" }`, a nonsense value the compiler can't reject.

---

## Errors and partiality

### `Maybe` for "might not be there"

```haskell
lookup :: Eq k => k -> [(k, v)] -> Maybe v
```

Use `Maybe` when absence is a normal outcome, not an error.

### `Either e a` for "expected failure with context"

```haskell
parseTimeout :: Text -> Either ParseError TimeoutSeconds
```

Use `Either` when the caller needs to know *why* something failed.
Define a sum type for the error:

```haskell
data ParseError
    = NotAnInteger  Text
    | OutOfRange    Int
    deriving stock (Eq, Show)
```

Not `Either String a`. Strings are a parsing-output anti-pattern;
callers can't pattern-match on them or act differently on different
failure modes.

### `IO` exceptions only cross the `IO` boundary

If a pure function can fail, it returns `Either e a` or `Maybe a`.
`throwIO` / `try` is only for wrapping libraries (e.g. `http-client`
throws `HttpException`; we `try` it at the edge and convert to our own
`Either ProbeError a`).

### No `error`, no `undefined` in production code

```haskell
-- Bad
findAgent :: AgentName -> [Agent] -> Agent
findAgent name agents =
    case filter ((== name) . agentName) agents of
        (a : _) -> a
        []      -> error "agent not found"   -- will crash production

-- Good
findAgent :: AgentName -> [Agent] -> Maybe Agent
findAgent name = find ((== name) . agentName)
```

`error` is only acceptable in "genuinely unreachable by construction"
branches where the type system should have made it so but didn't —
and then it's a signal to refactor toward a type that makes the
impossibility explicit, not a solution.

### No partial standard functions

Avoid `head`, `tail`, `init`, `last`, `!!`, `fromJust`, `read` from
`Prelude`. Use:

- `Data.List.NonEmpty` when the list is always non-empty by
  construction.
- `listToMaybe`, `uncons`, `readMaybe` for partial versions with
  `Maybe`.
- Pattern matching for destructuring.

Only pull in a partial function when the exhaustive alternative is
genuinely worse — and justify in a comment.

---

## Effects

Use **`effectful`** (not `polysemy`, not plain mtl transformers) when
a tool has more than one effect worth separating (subprocess + HTTP +
logging, etc.). Plain `IO` is fine when there's only one.

### Effect declaration

```haskell
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies   #-}

module Krikit.Agent.Ops.Probe where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Data.Text (Text)

data Probe :: Effect where
    HttpGet :: String -> Probe m (Either ProbeError Text)

data ProbeError = NetworkError Text | HttpStatus Int
    deriving stock (Eq, Show)

makeEffect ''Probe
```

### Handler per interpretation

Always write at least two: the real one (IO) and a mock (pure).
Mocks are what make effectful worth the ceremony.

```haskell
runProbeHttp :: IOE :> es => Eff (Probe : es) a -> Eff es a
runProbeHttp = interpret $ \_ -> \case
    HttpGet url -> liftIO $ doRealHttpGet url

runProbeMock :: Text -> Eff (Probe : es) a -> Eff es a
runProbeMock canned = interpret $ \_ -> \case
    HttpGet _ -> pure (Right canned)
```

### Business logic never names `IO`

```haskell
-- Good: constrained, not IO-coupled
checkOllama :: (Probe :> es, Log :> es) => Eff es TierResult

-- Bad: couples logic to IO and a specific HTTP library
checkOllama :: Manager -> IO TierResult
```

The constraint `(Probe :> es, Log :> es)` is the "push effects to the
edges" principle in type form.

### Stack composition in `main`

```haskell
main :: IO ()
main = runEff
     . runLogStdout
     . runProbeHttp
     $ runSmoke   -- the whole thing
```

Read bottom-up: `runProbeHttp` interprets `Probe`, `runLogStdout`
interprets `Log`, `runEff` escapes the `Eff` monad back into plain
`IO`. Add/remove effects by adding/removing handler lines. Tests swap
real handlers for mocks without touching business logic.

---

## Text, not `String`

Use `Text` from `Data.Text` for any string that represents human-
readable content, JSON strings, paths, log messages. Use `ByteString`
from `Data.ByteString` for raw bytes (HTTP bodies before decoding,
binary file content).

`String` is `[Char]` — fine for trivial utility code, the output of
`show`, or when interop demands it (e.g. `FilePath` is currently
`String`). Never carry `String` through the domain layer.

Enable `OverloadedStrings` per-module where it pays:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

Then `"krikit-smoke"` typechecks as `Text`, `ByteString`, or `String`
depending on context.

---

## Recursion and stack safety

- Prefer library folds (`foldr`, `foldl'`, `foldMap`) over hand-written
  recursion for list-shaped traversals.
- Use `foldl'` from `Data.List` (strict accumulator) when building up
  a result; never plain `foldl` (lazy accumulator is a space-leak pit).
- For custom recursive functions over large inputs, write tail-recursive
  accumulator-passing helpers. Annotate the accumulator strict (`!`).
- For traversals of recursive ADTs, pattern-match; don't reach for
  `fix` or `recur` — Haskell's recursion is already first-class.

Ops scripts rarely process large inputs, so stack safety is rarely
load-bearing here — but the habits carry over to non-ops Haskell work.

---

## Language extensions

`default-language: GHC2021` is already set in the cabal file. GHC2021
gives us `DeriveDataTypeable`, `DeriveFoldable`, `DeriveFunctor`,
`DeriveGeneric`, `DeriveLift`, `DeriveTraversable`, `DerivingStrategies`,
`EmptyCase`, `EmptyDataDecls`, `FlexibleContexts`, `FlexibleInstances`,
`GADTSyntax`, `GeneralisedNewtypeDeriving`, `HexFloatLiterals`,
`ImportQualifiedPost`, `InstanceSigs`, `KindSignatures`,
`NamedFieldPuns`, `NamedWildCards`, `NumericUnderscores`,
`PolyKinds`, `PostfixOperators`, `RankNTypes`,
`ScopedTypeVariables`, `StandaloneDeriving`, `StandaloneKindSignatures`,
`TupleSections`, `TypeApplications`, `TypeOperators`,
`TypeSynonymInstances`.

Add per-module only what's missing:

- `OverloadedStrings` — string literals polymorphic over Text / ByteString / String
- `LambdaCase` — `\case` for one-arg pattern matches
- `DataKinds` + `TypeFamilies` + `GADTs` + `TemplateHaskell` — when declaring `effectful` effects
- `OverloadedRecordDot` — `foo.bar.baz` record access (nice for deeply-nested records; skip otherwise)
- `StrictData` — if a whole module's records should default strict (we do this inline with `!` on fields)

Don't enable extensions you don't use. Every extension is a choice
that costs a little reader-comprehension tax.

---

## Style and formatting

- 4-space indent. Don't mix tabs and spaces.
- Top-level type signatures required on every exported function and on
  every non-trivial local `let`/`where` binding.
- `let` for short local bindings inside a `do`; `where` for longer
  helpers at the end of a function. Don't mix styles arbitrarily
  inside one function.
- Use `<>` for `Semigroup` append, not `++` — it works on `Text`,
  `ByteString`, `Builder`, and lists uniformly.
- Prefer explicit `return` = `pure` for `Monad` contexts; both work,
  `pure` reads better.
- Formatter: **fourmolu** (`brew install fourmolu`). Run on save via
  HLS in your editor, or manually before commit:

  ```bash
  fourmolu -i src app test
  ```

  Config: whatever fourmolu defaults to (we haven't felt pain yet; add
  a `fourmolu.yaml` if we do).

- GHC options for this repo are set in `common warnings` in the cabal
  file (`-Wall -Wcompat -Wincomplete-record-updates ...`). Don't
  silence these per-module without a comment stating why.

### Naming

- Types: `UpperCamelCase` — `TierStatus`, `AgentName`, `ProbeError`.
- Values / functions: `lowerCamelCase` — `runSmoke`, `checkOllama`, `tierName`.
- Constants: `lowerCamelCase` — `defaultTimeout` (not `DEFAULT_TIMEOUT`).
- Constructors: `UpperCamelCase` — `Pass`, `Fail`, `HttpGet`.
- Record fields: `lowerCamelCase`, prefixed with type when several
  records share a short field name (`tierName`, `agentName`) until
  `OverloadedRecordDot` makes prefixes unnecessary.

---

## Imports

- Explicit import lists on everything. `import Data.Text (Text, pack, unpack)`
  beats `import Data.Text` — you see at-a-glance what a module uses.
- Qualified imports for ambiguous namespaces: `Data.Text`, `Data.Map`,
  `Data.Set`, `Data.ByteString`, `Data.ByteString.Lazy`. Short aliases:

  ```haskell
  import qualified Data.Text      as T
  import qualified Data.Text.IO   as TIO
  import qualified Data.Map.Strict as Map
  import qualified Data.ByteString.Lazy as LBS
  ```

- Group imports: standard library, third-party, local. Blank line
  between groups. Alphabetical within a group.

- Use `import Data.Text (Text)` — type unqualified, functions qualified
  (`T.pack`, `T.unpack`). Avoids name clashes with `Prelude`.

---

## Dependencies

Canonical picks for this repo's problem domain. Deviating needs a
reason in the cabal file comment.

| Need | Library |
|------|---------|
| Text | `text` |
| ByteString | `bytestring` |
| Containers | `containers` (Map, Set), `unordered-containers` (HashMap) |
| JSON | `aeson` (parsing + encoding) |
| HTTP client | `http-client` + `http-client-tls` |
| Subprocess | `typed-process` |
| Effect system | `effectful` + `effectful-th` |
| CLI args | `optparse-applicative` |
| Logging / formatted output | `prettyprinter` + `text` (or just `T.putStrLn`) |
| Testing | `hspec` + `QuickCheck` + `hspec-expectations` |
| Time | `time` |
| Environment | `envy` or just `System.Environment` for simple cases |

Don't pull in a library for something the stdlib already does well
(showing an `Int`, splitting on a character with `T.splitOn`).

---

## Testing

- `cabal test` runs the suite.
- `hspec` for describe-it integration tests.
- `QuickCheck` for properties on pure functions: parsers, formatters,
  encoders (`encode . decode ≡ id` on well-formed inputs).
- Effectful handlers get tested with mocked variants (see `runProbeMock`
  above). Real handlers are tested by running them against local
  fixtures — don't hit real services in unit tests.

Structure:

```haskell
-- test/Spec.hs
module Main (main) where

import Test.Hspec

import qualified Krikit.Agent.Ops.Smoke.TierSpec
import qualified Krikit.Agent.Ops.OutputSpec

main :: IO ()
main = hspec $ do
    Krikit.Agent.Ops.Smoke.TierSpec.spec
    Krikit.Agent.Ops.OutputSpec.spec
```

Per-module spec files under `test/Krikit/Agent/Ops/...Spec.hs`.

---

## Build / workflow

```
cabal update                            # refresh Hackage index
cabal build                             # compile everything
cabal run krikit-smoke -- --help        # run exe with args
cabal test                              # run test suite
cabal repl krikit-agent-ops             # GHCi with lib loaded
cabal install --installdir $HOME/.local/bin \
              --install-method copy \
              --overwrite-policy=always # ship binaries to PATH
```

`-- --help` passes the `--help` flag to the binary, not to cabal.

For REPL-driven development:

```
cabal repl krikit-agent-ops
ghci> :l src/Krikit/Agent/Ops/Smoke/Tier.hs
ghci> :reload    -- after each edit
```

HLS (Haskell Language Server) provides inline errors / types / refactor
actions in any modern editor. Install via ghcup on the admin laptop:
`ghcup install hls recommended`.

---

## Adding a new tool

1. Create `app/<tool>/Main.hs`. Keep it one function: parse args,
   invoke library code, return exit code.
2. Add an `executable <tool>` stanza to `krikit-agent-ops.cabal`
   mirroring `krikit-smoke`'s stanza.
3. Create `src/Krikit/Agent/Ops/<Tool>.hs` (or subdirectory if it grows)
   for the actual logic.
4. Add a spec under `test/Krikit/Agent/Ops/<Tool>Spec.hs`.
5. `cabal build && cabal test`.
6. `cabal install ...` to ship.
7. Update this repo's `README.md` tool table + whatever playbook
   references it.

---

## Common recipes

### Defining a validated newtype

```haskell
newtype Port = Port Int
    deriving stock   (Eq, Ord, Show)
    deriving newtype (ToJSON, FromJSON)

mkPort :: Int -> Maybe Port
mkPort n | n > 0 && n < 65536 = Just (Port n)
         | otherwise          = Nothing

-- Smart constructor prevents bad values; only export mkPort + unPort
```

### Parsing at the edge

```haskell
-- Bad: validate deep in business logic
runTier (rawConfig :: Aeson.Value) = case rawConfig of ... -- at every use

-- Good: decode once at the boundary, carry typed values
decodeConfig :: ByteString -> Either String Config
decodeConfig = Aeson.eitherDecodeStrict

runTier :: Config -> Eff es TierResult
runTier cfg = ... -- cfg is already validated
```

### An effectful tier check

```haskell
data Smoke :: Effect where
    CheckOllama    :: Smoke m TierResult
    CheckSentry    :: Smoke m TierResult

makeEffect ''Smoke

runSmoke :: (Probe :> es, Log :> es) => Eff (Smoke : es) a -> Eff es a
runSmoke = interpret $ \_ -> \case
    CheckOllama -> checkOllamaImpl
    CheckSentry -> checkSentryImpl
```

### JSON Lines history append

```haskell
appendHistory :: FilePath -> SmokeRun -> IO ()
appendHistory path run =
    BS.appendFile path (LBS.toStrict (Aeson.encode run) <> "\n")
```

---

## Anti-patterns

Flag these in review; don't ship them:

- `String` in domain types instead of `Text`.
- `Int` or `Integer` used for things that have a name
  (`TimeoutSeconds`, `Port`, `AgentId`).
- Catch-all `_` branches on sum types we control.
- `head` / `tail` / `!!` / `fromJust` / `read` from `Prelude`.
- `error` or `undefined` outside explicitly unreachable branches.
- `Either String a` when a proper `Either MyError a` would do.
- Long `do` blocks (>30 lines) — factor into named helpers.
- Business logic with `IO` directly in its signature when it could
  be effect-polymorphic.
- Boolean-blindness: `Bool`-returning predicates where a more specific
  sum type carries the "why" with the "yes/no".
- Silently ignoring `Left`/`Nothing` — every failure case gets logged
  or surfaced.
- Unused imports, unused language extensions, unused `build-depends:`
  entries. `-Wunused-packages` catches the last one.

---

## Inspiration / reading

- *Thinking with Types* — Sandy Maguire. Deep on type-level Haskell.
- *Parse, don't validate* — Alexis King's blog post. Applies
  1-to-1 to how we structure parsers.
- *Haskell Design Patterns* — not a book; search the `design-patterns`
  tag on the Haskell discourse.
- `effectful` README + examples repo. Best source for effect-system
  recipes that aren't out of date.

---

## When to revise this file

This doc is a convention, not a cage. If a pattern here actively
hurts readability or maintenance, propose a change in a PR, explain
why, and update both the doc and the code in the same commit. The
rules serve the code, not the other way around.
