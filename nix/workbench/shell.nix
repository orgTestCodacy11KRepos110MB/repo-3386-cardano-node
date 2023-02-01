{ pkgs
, lib
, haskellLib
, project
##
, setLocale, haveGlibcLocales, commandHelp
##
, cardano-mainnet-mirror
##
, workbench-runner
, workbenchDevMode ? false
##
, profiled ? false
, withHoogle ? true
, withMainnet ? true
}:

with lib;

let
    inherit (workbench-runner) profileName profileNix backend;

    shellHook = { profileName, backend, profiled, workbenchDevMode, withMainnet }: ''
      while test $# -gt 0
      do shift; done       ## Flush argv[]

      echo 'workbench shellHook:  profileName=${profileName} backendName=${backend.name} useCabalRun=${toString backend.useCabalRun} workbenchDevMode=${toString workbenchDevMode} profiled=${toString profiled} '
      export WB_BACKEND=${backend.name}
      export WB_DEPLOYMENT_NAME=''${WB_DEPLOYMENT_NAME:-$(basename $(pwd))}
      export WB_SHELL_PROFILE=${profileName}
      export WB_SHELL_PROFILE_DATA=${profileNix}

      ${optionalString
        workbenchDevMode
        ''
      export WB_CARDANO_NODE_REPO_ROOT=$(git rev-parse --show-toplevel)
      export WB_EXTRA_FLAGS=

      function wb() {
        $WB_CARDANO_NODE_REPO_ROOT/nix/workbench/wb $WB_EXTRA_FLAGS "$@"
      }
      export -f wb
        ''}

      ${optionalString
        backend.useCabalRun
        ''
      . nix/workbench/lib.sh
      . nix/workbench/lib-cabal.sh ${optionalString profiled "--profiled"}
        ''}

      export CARDANO_NODE_SOCKET_PATH=run/current/node-0/node.socket

      function workbench_atexit() {
          if wb backend is-running run/current
          then stop-cluster
          fi
      }
      trap workbench_atexit EXIT

      ${optionalString
        withMainnet
        ''
      export CARDANO_MAINNET_MIRROR=${cardano-mainnet-mirror.outputs.defaultPackage.x86_64-linux.outPath}
        ''}

      ${setLocale}
      ${commandHelp}
      '';

in project.shellFor {
  name = "workbench-shell";

  shellHook = shellHook { inherit profileName backend profiled workbenchDevMode withMainnet; };

  inherit withHoogle;

  # The workbench shell uses cabalWrapped, which removes the `source-repository-package` stanzas
  # from `cabal.project`. The idea is to use prebuilt ones provided by haskell.nix. However,
  # haskell.nix is clever enough to not include `source-repository-package`s in the shell
  # package db, because it knows that cabal will rebuild them. So you just end up with nothing!
  # We can work around this by overriding haskell.nix's selection of which packages the shell
  # is prepared for, so that it *doesn't* include the `source-repository-package` ones
  # (the default is *local* packages which includes them, we select *project* pacakges which doesn't)
  packages = ps: builtins.attrValues (haskellLib.selectProjectPackages ps);

  tools = {
  };

  # These programs will be available inside the nix-shell.
  nativeBuildInputs = with pkgs; with haskellPackages; with cardanoNodePackages; [
    cardano-ping
    db-analyser
    pkgs.graphviz
    graphmod
    weeder
    nixWrapped
    pkgconfig
    profiteur
    profiterole
    ghc-prof-flamegraph
    sqlite-interactive
    tmux
    pkgs.git
    pkgs.hlint
    pkgs.moreutils
    pkgs.pstree
    pkgs.time
    workbench-interactive-start
    workbench-interactive-stop
    workbench-interactive-restart
  ]
  # Backend packages take precendence.
  ++ backend.extraShellPkgs
  ++ [
      # Publish
      bench-data-publish
      # Publish tunnel
      yq nomad vault-bin norouter socat
      # Debugging
      postgresql
  ]
  ++ lib.optional haveGlibcLocales pkgs.glibcLocales
  ++ lib.optionals (!backend.useCabalRun) [ cardano-topology cardano-cli locli ]
  ++ lib.optionals (!workbenchDevMode) [ workbench.workbench ]
  ;

} // { inherit shellHook;
     }
