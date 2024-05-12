{ stdenv
, sources
, clang
, gcc
, readline
}:

stdenv.mkDerivation {
    pname = "sketchybar-lua";
    version = "0.0.0.0";
    src = sources.sketchybar-lua;
    nativeBuildInputs = [ clang gcc ];
ldInputs = [ readline ];
    installPhase = ''
        mv bin "$out"
    '';
}
