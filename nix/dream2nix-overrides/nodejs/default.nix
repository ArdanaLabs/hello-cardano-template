{ lib
, pkgs
, # dream2nix
  satisfiesSemver
, ...
}:
let
  # The `dist` folder is not included with lighthouse when trying to compile it
  # from source, and it's not clear how to produce it. So this function fetches
  # it from the npm registry. Conversely, the lockfile is not included when
  # fetching from the registry, so this is a hard position to be in. We may be
  # able to fix this hack in future.
  getLighthouseDist = version: pkgs.stdenv.mkDerivation {
    name = "lighthouse-dist";
    src = pkgs.fetchurl {
      url = "https://registry.npmjs.org/lighthouse/-/lighthouse-${version}.tgz";
      sha256 = "sha256-QDjBxZ6ZvGJn6s7eG+/Y2QPXRs3LwnhMMs/z0wzzZfM=";
    };
    dontBuild = true;
    installPhase = ''
      mv dist $out
    '';
  };
in
{
  lighthouse.build = {
    nativeBuildInputs = old: old ++ [
      pkgs.makeWrapper
    ];
    overrideAttrs = old: {
      preBuild = ''
        cp -r ${getLighthouseDist old.version} ./dist
      '';
      postInstall = ''
        wrapProgram $out/bin/lighthouse \
          --set CHROME_PATH "${pkgs.chromium}/bin/chromium"
        wrapProgram $out/bin/smokehouse \
          --set CHROME_PATH "${pkgs.chromium}/bin/chromium"
        wrapProgram $out/bin/chrome-debug \
          --set CHROME_PATH "${pkgs.chromium}/bin/chromium"
      '';
    };
  };
  puppeteer.build = {
    PUPPETEER_SKIP_DOWNLOAD = true;
  };
}
