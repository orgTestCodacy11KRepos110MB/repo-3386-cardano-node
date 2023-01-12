let nix = import ../../..
  { localCluster = {
      profileName = (__fromJSON (__readFile <profileJson>)).name;
      backendName = "nixops";
      useCabalRun = false;
    };
    withHoogle = false;
  };
in
with nix;

import ./cardano.nix {
  instances = with iohk-ops-lib.physical.libvirtd; {
    inherit targetEnv;
    core-node   = medium;
    relay-node  = large;
    test-node   = large;
    smash       = medium;
    faucet      = medium;
    metadata    = medium;
    explorer    = medium;
    explorer-gw = small;
    monitoring  = medium;
  };
  inherit (nix) pkgs;
} // lib.optionalAttrs (builtins.getEnv "BUILD_ONLY" == "true") {
  defaults = {
    users.users.root.openssh.authorizedKeys.keys = lib.mkForce [""];
  };
}
