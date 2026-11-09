# oglyad

repository metadata and health checker for [Terra](https://terra.fyralabs.com).
"огляд" is Ukrainian for "review" or "inspection".

```sh
git clone https://github.com/terrapkg/packages ../packages
cabal run oglyad -- -m ../packages            # warnings and errors
cabal run oglyad -- -m ../packages -v         # plus info findings
cabal run oglyad -- -m ../packages -u         # plus upstream version drift
cabal run oglyad -- -m ../packages --fedora   # plus overlap with Fedora
cabal run oglyad -- -m ../packages --only drift --json
cabal test
```

it exits non-zero when there are error findings, so it can gate CI.
upstream lookups use `GITHUB_TOKEN`, or `gh auth token` when that's unset.

## checks

| check | severity | what it means |
|---|---|---|
| `build-lag` | error / warn / info | never published, merged but not built, or the repo is newer than your checkout |
| `drift` | warn | upstream released something newer than the spec (`-u`) |
| `duplicate-name` | warn | two directories build the same package name into the same repo, so they overwrite each other |
| `stale` | info, warn with `-u` | last build is older than `--stale-days`, warn if upstream also moved on |
| `upstream-error` | warn | the upstream from update.rhai 404s or fails, so `anda update` is failing too |
| `fedora-overlap` | warn | Fedora ships the same source package (`--fedora`). subrepos are exempt |
| `no-autoupdate` | info | no update.rhai |
| `unparsed-rhai` | info | update.rhai does something oglyad can't follow, like custom scraping |
| `collect` | error | rpmspec failed, or a repo couldn't be fetched |

## how it works

```
 ┌──────────── IO shell (Collect) ──┐      ┌──── pure core ────┐     ┌─ IO ─┐
 monorepo: rpmspec, git ──┐
 Terra repodata + subrepos├─► Snapshot ───► [Check] ─► [Finding] ─► Report
 Fedora repodata ─────────┤
 upstream APIs ───────────┘
```

checks are `Snapshot -> Package -> [Finding]` and never do IO, so each one is
tested with a fake `Snapshot` in `test/Spec.hs`.

- **specs** are evaluated with `rpmspec`
- **update.rhai** is lexed and pattern-matched, not run. it follows `let`
  bindings into `rpm.version(...)` / `rpm.global("ver", ...)` and recognizes
  the andax helpers (`gh`, `pypi`, `crates`, `gitlab`, ...)
- **upstream lookups** hit the same endpoints anda does, so oglyad sees what
  `anda update` would
- **versions** compare with a port of rpm's `rpmvercmp`, plus some forgiveness
  for respelled tags (`v1.2`, `1.2-rc1` vs `1.2~rc1`, `2026-07-27` vs `20260727`)

## caching

everything lives in `~/.cache/oglyad`.

| file | reused while |
|---|---|
| `specs.json` | the package dir's files, the dist tag, and the installed rpm macros are unchanged |
| `commits.json` | HEAD hasn't moved |
| `repodata-*.json` | the repo's primary.xml checksum is unchanged |
| `upstream.json` | younger than `--cache-hours` (default 6), successes only |

a run with warm caches takes about 0.4s. a cold run takes about 5s, almost
all of it in rpmspec.

## formatting

code is formatted with [ormolu](https://github.com/tweag/ormolu) 0.9.0.0, and
CI fails on unformatted files. install the matching version from its
[release page](https://github.com/tweag/ormolu/releases/tag/0.9.0.0), then:

```sh
ormolu --mode inplace $(git ls-files '*.hs')
```

## license

GPL-3.0-or-later. see [LICENSE](LICENSE).
