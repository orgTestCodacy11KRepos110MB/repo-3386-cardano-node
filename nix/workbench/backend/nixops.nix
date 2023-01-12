{ pkgs
, lib
, ...
}:
with lib;
let
  name = "nixops";

  plugins = [ "nixops-aws"
              "nixops-libvirtd"
            ];
  sourcePaths = import ./nixops/sources.nix { inherit pkgs; };

  nixops = (import (sourcePaths.nixops-core + "/release.nix") {
    nixpkgs = sourcePaths.nixpkgs-nixops;
    nixpkgsConfig.permittedInsecurePackages =
      [
        "libvirt-5.9.0"
      ];
    pluginsSources = sourcePaths;
    p = lib.attrVals plugins;
    withManual = false;
  }).build.${pkgs.stdenv.system};

  extraShellPkgs = with pkgs;
    [
      nixops
    ];

  materialise-profile =
    { stateDir, profileNix }:
      let
      in pkgs.runCommand "workbench-backend-output-${profileNix.profileName}-${name}"
        {}
        ''
        mkdir $out
        touch $out/empty
        '';

  mkGlobals =
    profileNix: pkgs: _:
    let
      inherit (profileNix) topology genesis;
      inherit (pkgs) globals;

      ## WARNING: IFD
      topologyNix = __fromJSON (__readFile "${topology.files}/topology-nixops.json");

      mkNodeOverlay =
        machineOverlay: nodeConfigOverlay:
        recursiveUpdate
        {
          documentation = {
            man.enable = false;
            doc.enable = false;
          };
          networking.firewall.allowPing = mkForce true;
          services.cardano-node = {
            eventlog = mkForce true;
            extraNodeConfig = mkExtraNodeConfig nodeConfigOverlay;
            rtsArgs =
              mkForce ([ "-N2" "-A16m" "-qg" "-qb" "-scardano-node.gcstats" ]
                       ++
                       (profileNix.value.node.rts_flags_override or []));
            useNewTopology = profileNix.value.node.p2p or false;
            usePeersFromLedgerAfterSlot = if profileNix.value.node.useLedgerPeers then 0 else -1;
          };
        } machineOverlay;

      mkExtraNodeConfig =
        { TraceBlockFetchProtocol ? false
        , ... }:
        recursiveUpdate
          (removeAttrs globals.environmentConfig.nodeConfig
            ["ByronGenesisHash"
             "ShelleyGenesisHash"
             "AlonzoGenesisHash"])
          (recursiveUpdate
            (benchmarkingLogConfig "node")
            ({
               TracingVerbosity = "NormalVerbosity";
               minSeverity = "Debug";
               TurnOnLogMetrics = true;

               TestEnableDevelopmentHardForkEras = true;
               TestEnableDevelopmentNetworkProtocols = true;

               inherit TraceBlockFetchProtocol;

               TraceMempool               = true;
               TraceTxInbound             = true;
               TraceBlockFetchClient      = true;
               TraceBlockFetchServer      = true;
               TraceChainSyncHeaderServer = true;
               TraceChainSyncClient       = true;
            } //
            (profileNix.value.node.extra_config or {})));

      benchmarkingLogConfig = name: {
        defaultScribes = [
          [ "StdoutSK" "stdout" ]
          [ "FileSK"   "logs/${name}.json" ]
        ];
        setupScribes = [
          {
            scKind     = "StdoutSK";
            scName     = "stdout";
            scFormat   = "ScJson"; }
          {
            scKind     = "FileSK";
            scName     = "logs/${name}.json";
            scFormat   = "ScJson";
            scRotation = {
              ## 1. Twice-daily, so not too large, but also most benchmarks
              ##    would be covered by that
              rpMaxAgeHours   = 12;
              ## 2. Ten per epoch, for two last epochs
              rpKeepFilesNum  = 20;
              ## 3. 10GB/file to prevent file-size cutoff from happening,
              ##    and so most benchmarks will have just 1 file
              rpLogLimitBytes = 10*1000*1000*1000;
            }; }
        ];
        options = {
          mapBackends = {
            "cardano.node.resources" = [ "KatipBK" ];
            "cardano.node.metrics"   = [ "EKGViewBK" ];
          };
        };
      };
      envConfigBase = pkgs.cardanoLib.environments.testnet;
      AlonzoGenesisFile  = "${genesis.files}/genesis.alonzo.json";
      ShelleyGenesisFile = "${genesis.files}/genesis-shelley.json";
      ByronGenesisFile   = "${genesis.files}/byron/genesis.json";
      requireEnv = name:
        let value = builtins.getEnv name;
        in if value == ""
           then abort "${name} environment variable is not set"
           else value;
    in
    rec {
      profile = profileNix;

      deploymentName = "${builtins.baseNameOf ../../../..}";
      networkName = "Benchmarking, size ${toString (__length topologyNix.coreNodes)}";

      withMonitoring = true;
      withExplorer = false;
      withSnapshots = false;
      withSubmitApi = false;
      withFaucet = false;
      withFaucetOptions = {};
      withSmash = false;

      dnsZone = "dev.cardano.org";
      relaysNew = "relays-new.${globals.domain}";
      domain = "${globals.deploymentName}.${globals.dnsZone}";

      deployerIp = requireEnv "DEPLOYER_IP";

      cardanoNodePort = 3001;
      cardanoNodePrometheusExporterPort = 12798;
      cardanoExplorerPrometheusExporterPort = 12698;

      systemDiskAllocationSize = 15;
      nodeDbDiskAllocationSize = 15;
      nbInstancesPerRelay = 1;

      environmentName = "bench-${profileNix.profileName}";
      environmentVariables = {};
      environmentConfig = rec {
        relays = "relays.${globals.domain}";

        edgePort = globals.cardanoNodePort;
        private = true;
        networkConfig = (removeAttrs envConfigBase.networkConfig ["AlonzoGenesisHash"]) // {
          Protocol = "Cardano";
          inherit  AlonzoGenesisFile;
          inherit ShelleyGenesisFile;
          inherit   ByronGenesisFile;
        };
        nodeConfig = (removeAttrs envConfigBase.nodeConfig ["AlonzoGenesisHash"]) // {
          Protocol = "Cardano";
          inherit  AlonzoGenesisFile;
          inherit ShelleyGenesisFile;
          inherit   ByronGenesisFile;
        } // {
          shelley =
            { TestShelleyHardForkAtEpoch = 0;
            };
          allegra =
            { TestShelleyHardForkAtEpoch = 0;
              TestAllegraHardForkAtEpoch = 0;
            };
          mary =
            { TestShelleyHardForkAtEpoch = 0;
              TestAllegraHardForkAtEpoch = 0;
              TestMaryHardForkAtEpoch    = 0;
            };
          alonzo =
            { TestShelleyHardForkAtEpoch = 0;
              TestAllegraHardForkAtEpoch = 0;
              TestMaryHardForkAtEpoch    = 0;
              TestAlonzoHardForkAtEpoch  = 0;
            };
          babbage =
            { TestShelleyHardForkAtEpoch = 0;
              TestAllegraHardForkAtEpoch = 0;
              TestMaryHardForkAtEpoch    = 0;
              TestAlonzoHardForkAtEpoch  = 0;
              TestBabbageHardForkAtEpoch = 0;
            };
        }.${profileNix.value.era};
        txSubmitConfig = {
          inherit (networkConfig) RequiresNetworkMagic;
          inherit AlonzoGenesisFile ShelleyGenesisFile ByronGenesisFile;
        } // pkgs.iohkNix.cardanoLib.defaultExplorerLogConfig;

        ## This is overlaid atop the defaults in the tx-generator service,
        ## as specified in the 'cardano-node' repository.
        generatorConfig = (traceValSeqN 3 profileNix).value.generator;
      };

      topology = {
        relayNodes = map
          (recursiveUpdate
            (mkNodeOverlay
              ## 1. nixos machine overlay
              {
                ## XXX: assumes we have `explorer` as our only relay.
                imports = [
                  pkgs.cardano-ops.roles.tx-generator
                  # ({ config, ...}: {
                  # })
                ];
                systemd.services.dump-registered-relays-topology.enable = mkForce false;
                services.cardano-node.systemdSocketActivation = mkForce false;
                services.cardano-node.tracerSocketPathConnect =
                  if !profileNix.value.node.withNewTracing then null
                  else "/var/lib/cardano-node/tracer.socket";
                services.cardano-tracer.networkMagic = profileNix.value.genesis.protocol_magic;
                ## Generator can only use new tracing:
                services.tx-generator.tracerSocketPath =
                  "/var/lib/cardano-node/tracer.socket";
              }
              ## 2. cardano-node service config overlay
              {
                ## This allows tracking block contents on the explorer.
                TraceBlockFetchProtocol = true;
              }))
          (topologyNix.relayNodes or []);
        coreNodes = map
          (recursiveUpdate
            (mkNodeOverlay
              ## 1. nixos machine overlay
              {
                stakePool = true;
                services.cardano-node.systemdSocketActivation = mkForce false;
                services.cardano-node.tracerSocketPathConnect =
                  if !profileNix.value.node.withNewTracing then null
                  else "/var/lib/cardano-node/tracer.socket";
                services.cardano-tracer.networkMagic = profileNix.value.genesis.protocol_magic;
              }
              ## 2. cardano-node service config overlay
              {
              }
            )) (topologyNix.coreNodes or []);
      };

      ec2 = with pkgs.iohk-ops-lib.physical.aws;
        {
          instances = {
            core-node = c5-2xlarge;
            relay-node = c5-2xlarge;
          };
          credentials = {
            accessKeyIds = {
              IOHK = "dev-deployer";
              dns = "dev-deployer";
            };
          };
        };
    };

  overlay = profileNix: self: super:
    {
      globals = mkGlobals profileNix self super;
      cardano-ops = {
        modules = {
          base-service = import ./nixops/module-base-service.nix self;
          common       = import ./nixops/module-common.nix       self;
        };
        roles = {
          core         = import ./nixops/role-core.nix         self;
          relay        = import ./nixops/role-relay.nix        self;
          tx-generator = import ./nixops/role-tx-generator.nix self;
        };
      };
    };
in
{
  inherit name extraShellPkgs materialise-profile overlay;

  useCabalRun = false;
}
