# oglyad

repository metadata and health checker for [Terra](https://terra.fyralabs.com).
"огляд" is Ukrainian for "review" or "inspection," but also for a health checkup.

```sh
git clone https://github.com/terrapkg/packages ../packages
oglyad -m ../packages            # warnings and errors, every maintained release
oglyad -m ../packages -v         # plus info findings
oglyad -m ../packages -u         # plus upstream version drift
oglyad -m ../packages --fedora   # plus overlap with Fedora
oglyad -m ../packages -r 44 -r rawhide   # just these releases
oglyad -m ../packages -r rawhide --worktree   # your local edits, against rawhide
oglyad -m ../packages --only drift --format json
```

from a checkout, `cabal run oglyad -- <args>` and `cabal test`. it needs
`rpmspec` (rpm-build), `git`, and the `zstd`, `gzip` and `xz` commands.
upstream lookups use `GITHUB_TOKEN`, or `gh auth token` when that's unset.

## releases

by default oglyad checks every Fedora release
[Bodhi](https://bodhi.fedoraproject.org/releases/) hasn't archived (the
current ones, a branched one before its release, and rawhide), so branching
and EOL need no changes here. `-r`/`--release` picks releases instead, as
`43`, `f43` or `rawhide`, and can be repeated.

each release is checked against its own Terra repo (`terra43`,
`terrarawhide`) and its own branch of terrapkg/packages (`origin/f43`,
`origin/frawhide`), with that release's `%{dist}` (`.fc43`, `.fcrawhide`)
and `%fedora`. branches are read with `git archive` into
`~/.cache/oglyad/trees`, so your checkout is never touched. nothing is
fetched either, so `git fetch` first if the branches are old. `--remote`
picks a remote other than `origin`.

`--worktree` reads specs from the checkout as it is, local edits included,
and needs exactly one `--release`. `--repo` points one release at a
different Terra repo.

findings from several releases are merged. one that comes up in only some
of them is tagged, like `anki [f44, f45]: ...`, and messages that only
differ by dist tag are merged with `%{dist}` in its place:
`never published (spec is at 26.09.4-1%{dist})`.

## output and exit codes

`--format` picks `text` (default), `markdown` (made for GitHub job
summaries) or `json`. the json is a versioned envelope:

```json
{"schema": 2, "oglyad": "0.1.0.0", "generated": "...",
 "releases": [{"release": "43", "label": "f43", "ref": "origin/f43",
               "commit": "...", "terra_repo": "https://repos.fyralabs.com/terra43"}],
 "upstream": true,
 "summary": {"error": 0, "warn": 0, "info": 0, "ignored": 0},
 "findings": [{"check": "...", "package": "...", "path": "...", "severity": "...",
               "message": "...", "releases": ["43", "44"]}],
 "ignored": [...]}
```

`schema` changes on any breaking change to the json. schema 2 replaced
`monorepo_head` and `terra_repo` with `releases`, and added `releases` to
findings. `ref` is `null` with `--worktree`.

| exit | meaning                                                                            |
| ---- | ---------------------------------------------------------------------------------- |
| 0    | ran, nothing at or above `--fail-on` (default `error`, or `warn`, `info`, `never`) |
| 1    | ran, and found something at or above `--fail-on`                                   |
| 2    | couldn't run: bad flags, unreadable ignore file, not a monorepo                    |

## ignoring known findings

some findings are intentional, like a bootstrap package sharing its real
package's name. there are two ways to silence one, and ignored findings
don't count toward the exit code.

an ignore file, passed with `--ignore-file`. every entry needs a reason, and
names one check plus a monorepo `path` or a `package` name:

```json
{
  "ignore": [
    {
      "check": "duplicate-name",
      "path": "anda/langs/zig/master-bootstrap",
      "reason": "bootstrap build of zig-master"
    }
  ]
}
```

or a label in the package's own `anda.hcl`:

```hcl
labels {
  oglyad_ignore = "duplicate-name, stale"
}
```

file entries that no longer match anything in any checked release are
reported as `unused-ignore`, so exceptions don't outlive their reason. `--no-ignores` shows everything.
[`ignores.json`](ignores.json) is the file the nightly report uses.

## checks

| check            | severity                                 | what it means                                                                                                                                                                                                                                                                                  |
| ---------------- | ---------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `build-lag`      | error / warn / info                      | never published, merged but not built, or the repo is newer than your checkout                                                                                                                                                                                                                 |
| `drift`          | warn                                     | upstream released something newer than the spec (`-u`). scripts oglyad can't follow on its own are run with the real anda, see below. packages with a `nightly` or `quaterdaily` label build from a branch head, so they're only checked for a mangled version, not held to the latest release |
| `duplicate-name` | warn                                     | two directories build the same package name into the same repo, so they overwrite each other                                                                                                                                                                                                   |
| `stale`          | info, warn with `-u`                     | last build is older than `--stale-days`, warn if upstream also moved on                                                                                                                                                                                                                        |
| `upstream-error` | warn                                     | the upstream from update.rhai 404s or fails, so `anda update` is failing too                                                                                                                                                                                                                   |
| `fedora-overlap` | warn                                     | Fedora ships the same source package (`--fedora`). subrepos are exempt                                                                                                                                                                                                                         |
| `no-autoupdate`  | info                                     | no update.rhai, or one with only comments in it                                                                                                                                                                                                                                                |
| `unparsed-rhai`  | info, warn when anda can't run it either | update.rhai looks a version up in a way oglyad can't follow, like custom scraping, and anda didn't answer for it (no `-u`, or anda isn't installed). scripts that only bump Release, track a commit, or copy Version from another spec aren't listed                                           |
| `collect`        | error                                    | rpmspec failed, a repo couldn't be fetched, or an upstream rate-limited us (warn)                                                                                                                                                                                                              |
| `unused-ignore`  | info / warn                              | an ignore entry matches nothing, or an `oglyad_ignore` label names an unknown check                                                                                                                                                                                                            |

### scripts oglyad can't follow

oglyad reads most update.rhai scripts itself: it finds the helper the version
comes from (`gh`, `pypi`, `crates`, ...), asks that upstream, and replays the
script's guards and edits over the answer, regexes included. scripts that do
more than that, like scraping a page with `get()` and `find()`, filtering
`gh_tags()`, or reading a file with `gh_rawfile()`, go to the real anda
instead. with `-u` and `anda` on the PATH, oglyad copies the release's tree
to a throwaway directory, runs `anda update --labels branch=<f43> <project>`
for each of those, and reads the Version rpmspec sees in the rewritten spec.
a script that throws shows up as `upstream-error`. answers are cached like
upstream ones.

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

| file              | reused while                                                                                      |
| ----------------- | ------------------------------------------------------------------------------------------------- |
| `specs.fcNN.json` | the package dir's files, the dist tag, and rpm's whole macro state (`rpm --showrc`) are unchanged |
| `commits.json`    | HEAD hasn't moved                                                                                 |
| `repodata-*.json` | the repo's primary.xml checksum is unchanged                                                      |
| `upstream.json`   | younger than `--cache-hours` (default 6), successes only. entries unused for 30 days are dropped  |

a run with warm caches takes about 0.4s. a cold run takes about 5s, almost
all of it in rpmspec. `--fedora` adds about 8s and peaks around 200MB of
memory the first time, since Fedora's repodata is 260MB decompressed. after
that it's cached until Fedora publishes again.

## security

`rpmspec` runs the shell code in specs (`%(...)`, `%{lua:...}`), so running
oglyad on a checkout means running that checkout's code as you. child
processes get an allowlisted environment, so `GITHUB_TOKEN` and other
secrets in oglyad's environment never reach them, but code running as your
user can still read your files.

the one exception is anda. with `-u`, update.rhai scripts oglyad can't follow
run under the real anda, which refuses GitHub helpers without
`GITHUB_TOKEN`, so anda gets the token. those scripts can run shell commands
with `sh()`, and those commands see the token too. use a read-only token.

**don't run oglyad on untrusted code, like an unmerged pull request, with
credentials around.** the nightly report only checks out terrapkg/packages'
default branch, and its token is read-only and expires with the job.

the GitHub token is only ever sent to `api.github.com`, and is dropped if a
request gets redirected anywhere else.

## nightly report

[`report.yml`](.github/workflows/report.yml) runs every night in Terra's
builder image (`ghcr.io/terrapkg/builder:f45`), so specs evaluate with the
same macros Terra builds with. it builds `oglyad-git` with anda, checks
every maintained release's branch of terrapkg/packages with `-u --fedora`,
writes the markdown
report to the run's summary, and keeps the markdown and json for 30 days.
caches carry over between nights.

## packaging

built the same way as anda itself. [`anda.hcl`](anda.hcl) has two projects:

| project      | spec                                 | builds                                                        |
| ------------ | ------------------------------------ | ------------------------------------------------------------- |
| `oglyad`     | [`oglyad.spec`](oglyad.spec)         | the `v<version>` release tag                                  |
| `oglyad-git` | [`oglyad-git.spec`](oglyad-git.spec) | the checked-out commit, versioned `<version>^<date>.<commit>` |

```sh
anda build oglyad-git --package rpm -c terra-45-x86_64
```

both build in mock, offline, against Fedora's Haskell libraries with
`%ghc_bin_build`, run the test suite in `%check`, and ship one self-contained
binary. [`package-git.yml`](.github/workflows/package-git.yml) builds
`oglyad-git` on x86_64 and aarch64 for every push to main and every pull
request that touches the code.

## releasing

1. bump `version` in `oglyad.cabal` and `oglyad.spec`, and add a
   `CHANGELOG.md` entry
2. push a `v<version>` tag, e.g. `v0.1.0.0`

[`hackage.yml`](.github/workflows/hackage.yml) checks the tag matches the
cabal version, builds and tests the sdist, and publishes it to Hackage. the
same tag is what `oglyad.spec` downloads. to preview a release first, run the
workflow by hand, which uploads a Hackage package candidate instead.

it needs a `HACKAGE_TOKEN` secret (an API token from your Hackage account) in
a GitHub environment named `hackage`.

## formatting

code is formatted with [ormolu](https://github.com/tweag/ormolu) 0.9.0.0, and
CI fails on unformatted files. install the matching version from its
[release page](https://github.com/tweag/ormolu/releases/tag/0.9.0.0), then:

```sh
ormolu --mode inplace $(git ls-files '*.hs')
```

## license

GPL-3.0-or-later. see [LICENSE](LICENSE).
