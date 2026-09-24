# Copyright Spack Project Developers. See COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

from spack_repo.builtin.packages.zsh.package import Zsh as BuiltinZsh

from spack.package import *


class Zsh(BuiltinZsh):
    # Applying pointer-types.patch bumps a source timestamp above the shipped
    # configure script, which makes the build regenerate it with autoconf. The
    # upstream recipe does not declare the autotools needed for that regen.
    depends_on("autoconf", type="build")
    depends_on("automake", type="build")
    depends_on("libtool", type="build")
    depends_on("m4", type="build")
