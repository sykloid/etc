{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
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
  version = "0.62.0";

  src = fetchFromGitHub {
    owner = "badlogic";
    repo = "pi-mono";
    tag = "v${finalAttrs.version}";
    hash = "sha256-nUK7R9kPkULg8eP9lwyqUpzPGRJeSRv3mDgBkHacf8I=";
  };

  sourceRoot = "${finalAttrs.src.name}";

  npmDepsHash = "sha256-mzFtHU3xGFZxIaQ1XTkYLmQ4UCcn9HhPVfNJ0DHi7Ps=";

  nodejs = nodejs_22;

  nativeBuildInputs = [ pkg-config python3 ];

  # Native deps for the `canvas` npm package (transitive dep via pi-ai).
  buildInputs = [ pixman cairo pango libpng libjpeg giflib librsvg ];

  # The generate-models script fetches from external APIs, which fails in the
  # sandbox and produces a reduced fallback that breaks tsgo type-checking.
  # The committed models.generated.ts has the full model set — just use it.
  postPatch = ''
    substituteInPlace packages/ai/package.json \
      --replace-fail '"build": "npm run generate-models && tsgo' '"build": "tsgo'
  '';

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

    local pi_dir="$out/lib/node_modules/@mariozechner/pi-coding-agent"
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
      rm -f "$pi_dir/node_modules/@mariozechner/${name}"
      mkdir -p "$pi_dir/node_modules/@mariozechner/${name}"
      cp -r "packages/${dir}/dist" "$pi_dir/node_modules/@mariozechner/${name}/"
      cp "packages/${dir}/package.json" "$pi_dir/node_modules/@mariozechner/${name}/"
    '') workspaceDeps)}

    # Remove remaining broken workspace symlinks (mom, pods, web-ui, etc.)
    find "$pi_dir/node_modules" -type l ! -exec test -e {} \; -delete

    # cli.js has a shebang; just symlink it
    mkdir -p "$out/bin"
    ln -s "$pi_dir/dist/cli.js" "$out/bin/pi"

    runHook postInstall
  '';

  meta = {
    description = "Terminal-based AI coding agent";
    homepage = "https://github.com/badlogic/pi-mono";
    changelog = "https://github.com/badlogic/pi-mono/releases/tag/v${finalAttrs.version}";
    license = lib.licenses.mit;
    mainProgram = "pi";
    platforms = lib.platforms.all;
  };
})
