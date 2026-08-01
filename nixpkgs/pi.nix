{
  lib,
  buildNpmPackage,
  fetchurl,
  makeWrapper,
  nodejs_22,
  pkg-config,
  pixman,
  cairo,
  pango,
  libpng,
  libjpeg,
  giflib,
  librsvg,
  python3,
}:

let
  # Workspace packages that coding-agent depends on at runtime.
  # Maps npm package suffix to monorepo directory name.
  workspaceDeps = {
    "pi-tui" = "tui";
    "pi-ai" = "ai";
    "pi-agent-core" = "agent";
  };

in
buildNpmPackage (finalAttrs: {
  pname = "pi-coding-agent";
  version = "0.83.0";

  src = fetchurl {
    url = "https://github.com/earendil-works/pi/releases/download/v${finalAttrs.version}/pi-${finalAttrs.version}-source.tar.gz";
    hash = "sha256-8iW4fsO0gl3VuU6SKoYpVYrdyjGhtNLCBq5Zio4mksA=";
  };

  sourceRoot = "pi-${finalAttrs.version}";

  npmDepsHash = "sha256-AbSfP1Ion8bN309NUBQb1QSn2cIIUjNONmZgls9vnYE=";

  nodejs = nodejs_22;

  # The generate-models script fetches from external APIs, which fails in the
  # sandbox. The release source tarball includes pre-generated model data, so
  # just skip the generate step and go straight to build:offline.
  postPatch = ''
    substituteInPlace packages/ai/package.json \
      --replace-fail '"build": "npm run generate-models && npm run build:offline"' '"build": "npm run build:offline"'
  '';

  nativeBuildInputs = [ pkg-config python3 makeWrapper ];

  # Native deps for the `canvas` npm package (dev dep that still runs install scripts).
  buildInputs = [ pixman cairo pango libpng libjpeg giflib librsvg ];

  # Build only the workspace packages that coding-agent needs, in order.
  buildPhase = ''
    runHook preBuild
    npm run --workspace=packages/tui build
    npm run --workspace=packages/ai build
    npm run --workspace=packages/agent build
    npm run --workspace=packages/coding-agent build
    runHook postBuild
  '';

  # Custom install — npm prune is unnecessary.
  dontNpmPrune = true;

  installPhase = ''
    runHook preInstall

    local pi_dir="$out/lib/node_modules/@earendil-works/pi-coding-agent"
    mkdir -p "$pi_dir"

    # Copy coding-agent package contents
    cp -r packages/coding-agent/{dist,package.json} "$pi_dir/"
    cp -r packages/coding-agent/docs "$pi_dir/" 2>/dev/null || true
    cp -r packages/coding-agent/examples "$pi_dir/" 2>/dev/null || true
    cp packages/coding-agent/CHANGELOG.md "$pi_dir/" 2>/dev/null || true

    # Copy third-party node_modules
    cp -r node_modules "$pi_dir/"

    # npm workspaces creates symlinks from node_modules/<pkg> -> packages/<pkg>.
    # Replace the ones coding-agent needs with actual built content.
    ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: dir: ''
      rm -f "$pi_dir/node_modules/@earendil-works/${name}"
      mkdir -p "$pi_dir/node_modules/@earendil-works/${name}"
      cp -r "packages/${dir}/dist" "$pi_dir/node_modules/@earendil-works/${name}/"
      cp "packages/${dir}/package.json" "$pi_dir/node_modules/@earendil-works/${name}/"
    '') workspaceDeps)}

    # Remove remaining broken workspace symlinks (orchestrator, etc.)
    find "$pi_dir/node_modules" -type l ! -exec test -e {} \; -delete

    # Wrap cli.js so that npm/node are on PATH at runtime
    # (pi spawns `npm install` for package management)
    mkdir -p "$out/bin"
    makeWrapper "$pi_dir/dist/cli.js" "$out/bin/pi" \
      --prefix PATH : "${finalAttrs.nodejs}/bin"

    runHook postInstall
  '';

  meta = {
    description = "Terminal-based AI coding agent";
    homepage = "https://github.com/earendil-works/pi";
    changelog = "https://github.com/earendil-works/pi/releases/tag/v${finalAttrs.version}";
    license = lib.licenses.mit;
    mainProgram = "pi";
    platforms = lib.platforms.all;
  };
})
