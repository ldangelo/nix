# overlays/acfs.nix
# ACFS tools: ntm, ubs, mcp-agent-mail
# Pass flake inputs so ubs can be pulled from its own flake output.
inputs:

final: prev: {

  # ntm — named-tmux-manager (Go TUI for multi-agent coordination)
  ntm = prev.buildGoModule {
    pname = "ntm";
    version = "unstable-2026-03-06";
    src = prev.fetchFromGitHub {
      owner = "Dicklesworthstone";
      repo  = "ntm";
      rev   = "9b0b3389783f5fda8548c1eba67d1a97c6468a70";
      hash = "sha256-TyHTFT+mQDwG73YCrHqk1IyliZKt2nPIl/BcFs62aOk=";
    };
    vendorHash = "sha256-7YMwW0lDsrqW5i6+o9Iv9kwmgRkNQijoAxw58Pov2UE=";
    doCheck = false;
    meta = with prev.lib; {
      description = "Spawn and coordinate multiple AI coding agents across tmux panes";
      homepage    = "https://github.com/Dicklesworthstone/ntm";
      license     = licenses.mit;
      platforms   = platforms.unix;
    };
  };

  # ubs — Ultimate Bug Scanner (shell meta-runner; pulled from its own flake)
  ubs = inputs.ubs.packages.${final.system}.default;

  # mcp-agent-mail — async multi-agent MCP coordination server (Python)
  mcp-agent-mail = prev.python3.pkgs.buildPythonApplication {
    pname   = "mcp-agent-mail";
    version = "0.3.0";
    format  = "pyproject";
    src = prev.fetchFromGitHub {
      owner = "Dicklesworthstone";
      repo  = "mcp_agent_mail";
      rev   = "79b25db8738dc8526545197e3223633b359b55a0";
      hash = "sha256-GtsHQMLZnKXrsaiAX1LACAOG80h7gxo2urK6rbB1nUo=";
    };
    nativeBuildInputs = with prev.python3.pkgs; [ hatchling ];
    propagatedBuildInputs = with prev.python3.pkgs; [
      fastapi
      pyyaml
      pathspec
      uvicorn
    ];
    doCheck = false;
    # Many deps (fastmcp, litellm, etc.) are not in nixpkgs; skip runtime check
    env.dontCheckRuntimeDeps = "1";
    meta = with prev.lib; {
      description = "Async multi-agent coordination MCP server";
      homepage    = "https://github.com/Dicklesworthstone/mcp_agent_mail";
      license     = licenses.mit;
      platforms   = platforms.unix;
    };
  };

}
