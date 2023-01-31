pkgs: nodeId: {config, name, ...}:
with pkgs;
let

  leftPad = number: width: lib.fixedWidthString width "0" (toString number);

  genDir = commonLib.requireEnv "WB_GENESIS_DIR";

  signingKey = genDir + "/byron/delegate-keys.${leftPad (nodeId - 1) 3}.key";
  delegCert  = genDir + "/byron/delegation-cert.${leftPad (nodeId - 1) 3}.json";
  vrfKey     = genDir + "/node-keys/node-vrf${toString nodeId}.skey";
  kesKey     = genDir + "/node-keys/node-kes${toString nodeId}.skey";
  opCert     = genDir + "/node-keys/node${toString nodeId}.opcert";
  bulkCreds  = genDir + "/node-keys/bulk-${toString nodeId}.creds";

  keysConfig = rec {
    RealPBFT = {
      _file = ./core.nix;
      services.cardano-node = {
        signingKey            = "/var/lib/keys/cardano-node-signing";
        delegationCertificate = "/var/lib/keys/cardano-node-delegation-cert";
      };
      systemd.services."cardano-node" = {
        after = [ "cardano-node-signing-key.service" "cardano-node-delegation-cert-key.service" ];
        wants = [ "cardano-node-signing-key.service" "cardano-node-delegation-cert-key.service" ];
      };
      deployment.keys = {
        "cardano-node-signing" = builtins.trace ("${name}: using " + (toString signingKey)) {
            keyFile = signingKey;
            user = "cardano-node";
            group = "cardano-node";
            destDir = "/var/lib/keys";
        };
        "cardano-node-delegation-cert" = builtins.trace ("${name}: using " + (toString delegCert)) {
            keyFile = delegCert;
            user = "cardano-node";
            group = "cardano-node";
            destDir = "/var/lib/keys";
        };
      };
    };
    TPraos =
      {
        _file = ./core.nix;

        services.cardano-node =
          {
            kesKey = "/var/lib/keys/cardano-node-kes-signing";
            vrfKey = "/var/lib/keys/cardano-node-vrf-signing";
            operationalCertificate = "/var/lib/keys/cardano-node-operational-cert";
          };

        systemd.services."cardano-node" =
          {
            after = [ "cardano-node-vrf-signing-key.service" "cardano-node-kes-signing-key.service" "cardano-node-operational-cert-key.service" ];
            wants = [ "cardano-node-vrf-signing-key.service" "cardano-node-kes-signing-key.service" "cardano-node-operational-cert-key.service" ];
            partOf = [ "cardano-node-vrf-signing-key.service" "cardano-node-kes-signing-key.service" "cardano-node-operational-cert-key.service" ];
          };

        deployment.keys =
          {
            "cardano-node-vrf-signing" = builtins.trace ("${name}: using " + (toString vrfKey)) {
              keyFile = vrfKey;
              user = "cardano-node";
              group = "cardano-node";
              destDir = "/var/lib/keys";
            };
            "cardano-node-kes-signing" = builtins.trace ("${name}: using " + (toString kesKey)) {
              keyFile = kesKey;
              user = "cardano-node";
              group = "cardano-node";
              destDir = "/var/lib/keys";
            };
            "cardano-node-operational-cert" = builtins.trace ("${name}: using " + (toString opCert)) {
              keyFile = opCert;
              user = "cardano-node";
              group = "cardano-node";
              destDir = "/var/lib/keys";
            };
          };
      };
    Cardano =
      if !(builtins.pathExists signingKey) then TPraos
      else if !(builtins.pathExists vrfKey) then RealPBFT
      else lib.recursiveUpdate TPraos RealPBFT;
  };

in {

  imports = [
    cardano-ops.modules.base-service
    keysConfig.${globals.environmentConfig.nodeConfig.Protocol}
  ];

  users.users.cardano-node.extraGroups = [ "keys" ];

  deployment.ec2.ebsInitialRootDiskSize = globals.systemDiskAllocationSize
    + globals.nodeDbDiskAllocationSize;

}
