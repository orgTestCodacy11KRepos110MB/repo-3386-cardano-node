{ pkgs
, instances
, ...
}:
with pkgs;
with lib;
let

  cardanoNodes = listToAttrs (concatLists [
    (map mkCoreNode  globals.topology.coreNodes)
    # (map mkRelayNode globals.topology.relayNodes)
  ]);

  mkCoreNode =  def:
  {
    inherit (def) name;
    value = mkNode {
      _file = ./deployment.nix;
      imports = [
        (def.instance or instances.core-node)
        (cardano-ops.roles.core def.nodeId)
      ];
      node = {
        roles = {
          isCardanoCore = true;
          class = "pool";
        };
        inherit (def) org nodeId;
      };
      services.cardano-node.allProducers = def.producers;
      deployment.ec2.region = def.region;
      security.wrappers.unix_chkpwd.source = mkForce "${pkgs.pam}/bin/unix_chkpwd";
    } def;
  };

  mkRelayNode = def:
  {
    inherit (def) name;
    value = mkNode {
      _file = ./deployment.nix;
      imports = [
        (def.instance or instances.relay-node)
        cardano-ops.roles.relay
      ];
      node = {
        roles = {
          isCardanoRelay = true;
          class = "relay";
        };
        inherit (def) org nodeId;
      };
      services.cardano-node.allProducers = def.producers;
      deployment.ec2.region = def.region;
    } def;
  };

  mkNode = args: def:
    recursiveUpdate (
      recursiveUpdate {
        deployment.targetEnv = instances.targetEnv;
        nixpkgs.pkgs = pkgs;
      } (args // {
        imports = (args.imports or []) ++ (def.imports or []);
      }))
      (builtins.removeAttrs def [
        "imports"
        "name"
        "org"
        "region"
        "nodeId"
        "producers"
        "staticRoutes"
        "dynamicSubscribe"
        "stakePool"
        "instance"
        "pools"
        "ticker"
        "public"
      ]);

in {
  network.description = globals.networkName;
  network.enableRollback = true;
} // cardanoNodes
