# the test suite runs offline and only needs git, rpm and the decompressors
%bcond tests 1

%global _version 0.1.2.0

Name:           oglyad
Version:        %{_version}^%{autogitversion}
Release:        1%{?dist}
Summary:        Repository metadata and health checker for Terra

License:        GPL-3.0-or-later
URL:            https://github.com/terrapkg/oglyad
# keep %version the plain cabal version, like anda-git.spec does
%global version %{_version}
Source0:        %{url}/archive/%{autogitcommit}.tar.gz

BuildRequires:  ghc-rpm-macros
BuildRequires:  ghc-Cabal-devel
BuildRequires:  ghc-aeson-devel
BuildRequires:  ghc-async-devel
BuildRequires:  ghc-base-devel
BuildRequires:  ghc-base16-bytestring-devel
BuildRequires:  ghc-bytestring-devel
BuildRequires:  ghc-conduit-devel
BuildRequires:  ghc-containers-devel
BuildRequires:  ghc-cryptohash-sha256-devel
BuildRequires:  ghc-deepseq-devel
BuildRequires:  ghc-directory-devel
BuildRequires:  ghc-filepath-devel
BuildRequires:  ghc-http-client-devel
BuildRequires:  ghc-http-client-tls-devel
BuildRequires:  ghc-http-types-devel
BuildRequires:  ghc-megaparsec-devel
BuildRequires:  ghc-mtl-devel
BuildRequires:  ghc-optparse-applicative-devel
BuildRequires:  ghc-text-devel
BuildRequires:  ghc-time-devel
BuildRequires:  ghc-typed-process-devel
BuildRequires:  ghc-vector-devel
BuildRequires:  ghc-xml-conduit-devel
BuildRequires:  ghc-xml-types-devel
%if %{with tests}
BuildRequires:  ghc-hspec-devel
BuildRequires:  ghc-QuickCheck-devel
BuildRequires:  ghc-temporary-devel
BuildRequires:  git-core
BuildRequires:  gzip
BuildRequires:  rpm-build
BuildRequires:  xz
BuildRequires:  zstd
%endif

# rpmspec evaluates specs, git reads the monorepo history, and repodata is
# decompressed with the zstd, gzip and xz commands
Requires:       git-core
Requires:       gzip
Requires:       rpm-build
Requires:       xz
Requires:       zstd
# picked up for GitHub API auth when GITHUB_TOKEN isn't set
Recommends:     gh

Packager:       Cypress Reed <cypress@fyralabs.com>

%description
oglyad checks a terrapkg/packages checkout against Terra's published
repodata and upstream releases. It reports packages that never got built,
stale builds, upstream version drift, broken update.rhai scripts, and
overlap with Fedora.


%prep
%autosetup -n %{name}-%{autogitcommit}


%build
%ghc_bin_build


%install
%ghc_bin_install


%check
%if %{with tests}
%cabal_test
%endif


%files
%license LICENSE
%doc README.md
%{_bindir}/%{name}


%changelog
%autochangelog
