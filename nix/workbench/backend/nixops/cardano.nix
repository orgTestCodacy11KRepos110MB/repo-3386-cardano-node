{ pkgs
, instances
, ...
}:
with pkgs;
with lib;
let

  inherit (globals) topology byronProxyPort;
  inherit (topology) coreNodes relayNodes;
  privateRelayNodes = topology.privateRelayNodes or [];
  inherit (lib) recursiveUpdate mapAttrs listToAttrs imap1 concatLists;

  cardanoNodes = listToAttrs (concatLists [
    (map mkCoreNode coreNodes)
    (map mkRelayNode (relayNodes ++ privateRelayNodes))
  ]);

  nodes = cardanoNodes;

  mkCoreNode =  def:
  {
    inherit (def) name;
    value = mkNode {
      _file = ./cardano.nix;
      node = {
        inherit (def) org nodeId;
        roles = {
          isCardanoCore = true;
          class = "pool";
        };
      };
      deployment.ec2.region = def.region;
      imports = [
        (def.instance or instances.core-node)
        (cardano-ops.roles.core def.nodeId)
      ];
      services.cardano-node.allProducers = def.producers;
    } def;
  };

  mkRelayNode = def:
  {
    inherit (def) name;
    value = mkNode {
      _file = ./cardano.nix;
      node = {
        roles = {
          isCardanoRelay = true;
          class = "relay";
        };
        inherit (def) org nodeId;
      };
      services.cardano-node.allProducers = def.producers;
      deployment.ec2.region = def.region;
      imports =
        [(def.instance or instances.relay-node)
         cardano-ops.roles.relay];
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
  network.description =
    globals.networkName
      or
    "Cardano cluster - ${globals.deploymentName}";
  network.enableRollback = true;
} // nodes
