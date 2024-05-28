{channels, ...}: final: prev: {
  inherit (channels.unstable) sketcybar;
}

# { stdenv
# , sources
# , clang
# , gcc
# , readline
# }:

# stdenv.mkDerivation {
#     pname = "sketchybar-lua";
#     version = "1.0.0";
#     src = sources.sketchybar-lua;
#     nativeBuildInputs = [ clang gcc ];
#     inputs = [ readline ];
#     installPhase = ''
#         mv bin "$out"
#     '';
# }
