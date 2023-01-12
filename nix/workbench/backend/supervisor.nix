{ pkgs
, lib
, useCabalRun
, ...
}:
let
  name = "supervisor";

  extraShellPkgs = with pkgs;
    [
      python3Packages.supervisor
    ]
  ++ lib.optionals ( useCabalRun)
    (with haskellPackages; [
      cabalWrapped
      ghcid
      haskellBuildUtils
      cabal-plan
    ])
  ## Workbench's main script is called directly in dev mode.
  ++ lib.optionals (!useCabalRun)
    (with cardanoNodePackages; [
      cardano-node
      cardano-tracer
      tx-generator
    ]);

  # Backend-specific Nix bits:
  materialise-profile =
    { stateDir, profileNix }:
      let supervisorConf = import ./supervisor-conf.nix
        { inherit profileNix;
          inherit pkgs lib stateDir;
          inetHttpServerPort = "127.0.0.1:9001";
        };
      in pkgs.runCommand "workbench-backend-output-${profileNix.profileName}-${name}"
        {supervisorConfPath = supervisorConf.INI;}
        ''
        mkdir $out
        cp    $supervisorConfPath           $out/supervisor.conf
        '';

  overlay =
    proTopo: self: super:
    {
    };
in
{
  inherit name extraShellPkgs materialise-profile overlay useCabalRun;
}
