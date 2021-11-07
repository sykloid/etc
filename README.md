Hello, and welcome to my `~/etc`. This is where I put my configuration files,
for a large number of the programs I use on a regular basis.

Points of Interest:

* My typical development workflow uses the files in this repository to provision
  a docker container, within which I mount project sources and do my work. See
  [Dockerfile](./Dockerfile) for that image definition.
* The Docker image is populated using `nix`; the corresponding derivation can be
  found in [flake.nix](./flake.nix). This ensures that configuration files and
  the applications that use them remain in sync.
* The configuration files for the various applications are provisioned in the
  image using ansible; the corresponding playbook is at
  [ansible/main.yml](./ansible/main.yml).
* The [justfile](./justfile) defines common tasks for this repository.
