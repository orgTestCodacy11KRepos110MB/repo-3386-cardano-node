{ pkgs, lib, cardanoLib
, runCommand
, runJq, jsonFilePretty
, runWorkbenchJqOnly,  runWorkbench
}:

rec {
  profileJson = { profileName }:
    runWorkbenchJqOnly "profile-${profileName}.json"
      "profile json ${profileName}";

  topologyFiles = { profileName, profileJson }:
    import ../topology/topology.nix
      { inherit pkgs profileName profileJson; };

  nodeSpecsJson = { profileName, profileJson }:
    runWorkbenchJqOnly "node-specs-${profileName}.json"
                       "profile node-specs ${profileJson}";

  genesisFiles = { profileName, profileJson, nodeSpecsJson }:
    import ../genesis/genesis.nix
      { inherit pkgs profileName profileJson nodeSpecsJson; };

  servicesConfig = { basePort, stateDir, useCabalRun }:
    import ../service/config.nix
      {
        inherit lib runWorkbench;
        inherit basePort;
        inherit stateDir;
        inherit useCabalRun;
      };

  services = { servicesConfig, profile, nodeSpecs, topologyFiles }:
    rec {
      inherit (pkgs.callPackage
               ../service/nodes.nix
               {
                 inherit runJq servicesConfig profile nodeSpecs topologyFiles;
                 baseNodeConfig = cardanoLib.environments.testnet.nodeConfig;
               })
        node-services;

      inherit (pkgs.callPackage
               ../service/generator.nix
               {
                 inherit jsonFilePretty;
                 inherit profile nodeSpecs servicesConfig node-services;
               })
        generator-service;

      inherit (pkgs.callPackage
               ../service/tracer.nix
               { inherit runJq servicesConfig profile nodeSpecs; })
        tracer-service;
    };

  ## WARNING:  IFD !!
  profile = { profileName
            , basePort, stateDir, useCabalRun }:
    rec {
      inherit profileName;

      JSON = profileJson { inherit profileName; };
      value = __fromJSON (__readFile JSON);                         ## IFD !!

      topology.files =
        topologyFiles { inherit profileName; profileJson = JSON; };

      node-specs  =
        {
          JSON = nodeSpecsJson
            { inherit profileName;
              profileJson = JSON;
            };
          value = __fromJSON (__readFile node-specs.JSON);          ## IFD !!
        };

      genesis.files =
        genesisFiles
          { inherit profileName;
            profileJson = JSON;
            nodeSpecsJson = node-specs.JSON;
          };

      inherit (services
        {
          servicesConfig = servicesConfig
            { inherit basePort stateDir useCabalRun; };
          profile = value;
          nodeSpecs = node-specs.value;
          topologyFiles = topology.files;
        })
        node-services
        generator-service
        tracer-service;
    };

  profileData = { profile }:
    let inherit (profile) profileName;
    in
    pkgs.runCommand "workbench-profile-${profileName}"
      { buildInputs = [];
        profileConfigJsonPath = profile.JSON;
        nodeSpecsJsonPath = profile.node-specs.JSON;
        nodeServices =
          __toJSON
          (lib.flip lib.mapAttrs profile.node-services
            (name: svc:
              with svc;
              { inherit name;
                service-config = serviceConfig.JSON;
                start          = startupScript.JSON;
                config         = nodeConfig.JSON;
                topology       = topology.JSON;
              }));
        generatorService =
          with profile.generator-service;
          __toJSON
          { name           = "generator";
            service-config = serviceConfig.JSON;
            start          = startupScript.JSON;
            run-script     = runScript.JSON;
          };
        tracerService =
          with profile.tracer-service;
          __toJSON
          { name                 = "tracer";
            tracer-config        = tracer-config.JSON;
            nixos-service-config = nixos-service-config.JSON;
            config               = config.JSON;
            start                = startupScript.JSON;
          };
        passAsFile = [ "nodeServices" "generatorService" "tracerService" ];
      }
      ''
      mkdir $out
      cp    $profileConfigJsonPath        $out/profile.json
      cp    $nodeSpecsJsonPath            $out/node-specs.json
      cp    $nodeServicesPath             $out/node-services.json
      cp    $generatorServicePath         $out/generator-service.json
      cp    $tracerServicePath            $out/tracer-service.json
      ''
  // profile;

  profile-names-json =
    runWorkbenchJqOnly "profile-names.json" "profiles list";

  profile-names =
    __fromJSON (__readFile profile-names-json);

  materialise-profile =
    # `workbench` is the pinned workbench in case there is one.
    { basePort, stateDir, useCabalRun, profileName }:
    let
      mkProfileData = profileName:
        profileData {
          profile = profile { inherit profileName basePort stateDir useCabalRun; };
        };
      ps = lib.genAttrs profile-names mkProfileData;
    in
      ps."${profileName}"
        or (throw "No such profile: ${profileName};  Known profiles: ${toString (__attrNames ps)}");
}
