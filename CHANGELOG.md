# changelog

## 0.1.2.0

- checks every maintained release by default (today f43, f44, f45 and
  rawhide), as listed by Fedora's Bodhi. `-r`/`--release` takes `43`, `f43`
  or `rawhide` and can be repeated. the default used to be just 44
- each release reads its own terrapkg/packages branch (`origin/f43`,
  `origin/frawhide`) through `git archive`, without touching the checkout.
  `--worktree` checks the checkout as it is against one release, which is
  what oglyad used to do. `--remote` picks the remote
- rawhide uses `terrarawhide`, `.fcrawhide` and Fedora's rawhide repo
- findings are merged across releases and tagged with the releases they
  came up in. messages that only differ by dist tag show `%{dist}`
- json schema 2: `releases` replaces `monorepo_head` and `terra_repo`, and
  each finding lists its `releases`
- an ignore file entry is only reported unused when it matched nothing in
  every release
- upstream answers fetched earlier in the same run are reused, so checking
  four releases asks each upstream once
- `unparsed-rhai` only lists scripts that really look up a version oglyad
  can't follow. kmod packages copying Version from their kmod-common spec,
  scripts that only bump Release or write files, and scripts tracking a
  commit from `get()` are classified as such instead. whether a script sets
  the version is judged by the macros the spec's `Version:` line uses, so
  `%global tag` feeding `Version: %(echo %tag | ...)` still counts
- an update.rhai with only comments in it counts as `no-autoupdate`
- `drift` and `stale` don't compare packages with a `nightly` or
  `quaterdaily` label in anda.hcl against the latest release, since they
  build from a branch head.
  a mangled version is still flagged
- anda.hcl values can be bare (`nightly = 1`) as well as quoted
- with `-u`, update.rhai scripts oglyad can't follow (scraping with `get()`,
  `gh_tags()` filters, `gh_rawfile()`, ...) run under the real anda on a
  throwaway copy of the tree, and the Version left in the spec is compared.
  failing scripts show up as `upstream-error`, and answers are cached like
  upstream ones. anda gets `GITHUB_TOKEN`, since it needs it for GitHub
  helpers
- `drift` replays the version-setting part of update.rhai over the upstream
  value, so guards (`if v.starts_with("cli-v")`, `if v < "5"`), edits
  (`crop`, `pop`, `truncate`, `replace`), `terminate()` and try/catch apply
  before comparing. tags a script would skip no longer show up as drift
- andax's regex helpers (`find`, `find_all`, `sub`) work in that replay,
  through a small engine for Rust's regex dialect that gives the same
  matches and captures as the regex crate
- `drift` treats a snapshot suffix (`^20260805git...`, `.gitfc62ae7`) as part
  of the spec's spelling, so `2.0.0-alpha-1-20250926` matches
  `2.0.0~alpha~1~20250926.gitfc62ae7`
- `drift` warns when update.rhai would produce a version that doesn't start
  with a letter or digit, like `crop(1)` turning `6.0.1` into `.0.1`
- `unparsed-rhai` warns about scripts with unbalanced brackets, which anda
  can't run at all
- `opam()` lookups skip versions that aren't semver, like anda does, so old
  Jane Street versions like `113.24.00` no longer win
- `--help` and `--version` print to stdout instead of stderr, so tools like
  help2man can read them. bad arguments still go to stderr and exit 2

## 0.1.1.0

prepare for release on Hackage

## 0.1.0.0

first release.

- checks: `build-lag`, `drift`, `duplicate-name`, `fedora-overlap`,
  `no-autoupdate`, `stale`, `unparsed-rhai`, `upstream-error`
- text, markdown and json output, with a versioned json envelope
- `--fail-on` and distinct exit codes for automation
- ignore files and `oglyad_ignore` labels in anda.hcl
- caching of spec evaluations, repodata, commit times and upstream answers
