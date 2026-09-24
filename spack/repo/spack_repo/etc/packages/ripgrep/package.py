# Copyright Spack Project Developers. See COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

import platform

from spack_repo.builtin.build_systems.generic import Package

from spack.package import *


_platforms = {
    "Darwin-arm64": "aarch64-apple-darwin",
    "Linux-x86_64": "x86_64-unknown-linux-gnu",
}

_checksums = {
    "15.2.0": {
        "aarch64-apple-darwin": "3750b2e93f37e0c692657da574d7019a101c0084da05a790c83fd335bad973e4",
        "x86_64-unknown-linux-gnu": "0019dfc4b32d63c1392aa264aed2253c1e0c2fb09216f8e2cc269bbfb8bb49b5",
    },
}


class Ripgrep(Package):
    """A line-oriented search tool that recursively searches directories
    for a regex pattern."""

    homepage = "https://github.com/BurntSushi/ripgrep"

    maintainers("sykloid")

    license("MIT OR Unlicense")

    _platform = _platforms.get("{0}-{1}".format(platform.system(), platform.machine()))

    if _platform is not None:
        for _ver, _sums in _checksums.items():
            _sha = _sums.get(_platform)
            if _sha is not None:
                version(
                    _ver,
                    sha256=_sha,
                    url="https://github.com/BurntSushi/ripgrep/releases/download/"
                    "{0}/ripgrep-{0}-{1}.tar.gz".format(_ver, _platform),
                )

    def install(self, spec, prefix):
        mkdirp(prefix.bin)
        install("rg", prefix.bin)

        mkdirp(prefix.share.man.man1)
        install("doc/rg.1", prefix.share.man.man1)
