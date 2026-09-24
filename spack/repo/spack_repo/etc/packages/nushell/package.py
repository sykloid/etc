# Copyright Spack Project Developers. See COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

import glob
import platform

from spack_repo.builtin.build_systems.generic import Package

from spack.package import *


# Prebuilt release binaries, keyed by "<system>-<machine>" as reported by
# platform.system()/platform.machine(), mapping to the checksum of the
# matching upstream release tarball triple.
_triples = {
    "Darwin-arm64": "aarch64-apple-darwin",
    "Linux-x86_64": "x86_64-unknown-linux-gnu",
}

_checksums = {
    "0.115.1": {
        "aarch64-apple-darwin": "2e6ed1eb043869ff05b5f2448a8c443e4d3a93557ba4303b21008a0523c96734",
        "x86_64-unknown-linux-gnu": "d11d825241f6504a3617c535fa725a9dd6d009c86d7b19fb3168b47635b9d8b0",
    },
}


class Nushell(Package):
    """A new type of shell."""

    homepage = "https://www.nushell.sh"

    maintainers("sykloid")

    license("MIT")

    _triple = _triples.get("{0}-{1}".format(platform.system(), platform.machine()))

    if _triple is not None:
        for _ver, _sums in _checksums.items():
            _sha = _sums.get(_triple)
            if _sha is not None:
                version(
                    _ver,
                    sha256=_sha,
                    url="https://github.com/nushell/nushell/releases/download/"
                    "{0}/nu-{0}-{1}.tar.gz".format(_ver, _triple),
                )

    def install(self, spec, prefix):
        # The tarball extracts to a single top-level directory containing the
        # nu binary, its plugins, and license/readme files.
        mkdirp(prefix.bin)
        for exe in glob.glob("nu") + glob.glob("nu_plugin_*"):
            install(exe, prefix.bin)
