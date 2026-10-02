# Copyright Spack Project Developers. See COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

import platform

from spack_repo.builtin.build_systems.generic import Package

from spack.package import *


# Prebuilt release archives, keyed by "<system>-<machine>" as reported by
# platform.system()/platform.machine(), mapping to the upstream asset's
# platform token.
_platforms = {
    "Darwin-arm64": "darwin-arm64",
    "Linux-x86_64": "linux-x64",
}

_checksums = {
    "1.0.0": {
        "darwin-arm64": "97291e7d2eb2d7d95ab1f67d26de7902302201bc8786c132bbbc9e53fa8526cc",
        "linux-x64": "8fd5543a52a889d60ad57ccbf6c969e73c75c5240aae18ac40b506947a63dc38",
    },
    "0.85.1": {
        "darwin-arm64": "d5f70e3c0cf7398eac239fd0261ee074d98b7ba7f6b43fe3617f052ed5b79d06",
        "linux-x64": "494e498f47d74d21f40b3386f6a5e921a3d49531a169cab55bbdaca0ea1fe25a",
    },
}


class Pi(Package):
    """Terminal-based AI coding agent."""

    homepage = "https://github.com/earendil-works/pi"

    maintainers("sykloid")

    license("MIT")

    _platform = _platforms.get("{0}-{1}".format(platform.system(), platform.machine()))

    if _platform is not None:
        for _ver, _sums in _checksums.items():
            _sha = _sums.get(_platform)
            if _sha is not None:
                version(
                    _ver,
                    sha256=_sha,
                    url="https://github.com/earendil-works/pi/releases/download/"
                    "v{0}/pi-{1}.tar.gz".format(_ver, _platform),
                )

    def install(self, spec, prefix):
        # The archive contains the standalone binary plus runtime assets (wasm,
        # theme, node_modules, etc.) that it loads relative to its own
        # location. Spack strips the leading "pi/" directory during extraction,
        # so the staged sources are the contents themselves. Install the whole
        # tree into libexec and expose the binary on PATH via a symlink.
        install_tree(".", prefix.libexec)
        mkdirp(prefix.bin)
        symlink(join_path(prefix.libexec, "pi"), join_path(prefix.bin, "pi"))
