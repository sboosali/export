##################################################
{ nixpkgs ? (import ./nixpkgs)
}:

##################################################
let

inherit (nixpkgs) pkgs;

packages = with pkgs; [
 ffmpeg-full
 ];

in
##################################################
pkgs.buildEnv
  {
      name                  = "media-programs";

      paths                 = packages;
      pathsToLink           = [ "/" "/bin" ];

      buildInputs           = packages;
      extraOutputsToInstall = [ "out" ];

  }
##################################################