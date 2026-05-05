# Linux Nix Configuration

This directory contains the Linux configuration for Leo's Nix setup.

## Usage

To use this configuration on a Linux machine (like the ASUS GX10):

1. Install Nix:
```bash
curl -L https://nixos.org/nix/install | sh
```

2. Clone this repo (if not already done):
```bash
gh repo clone ldangelo/nix ~/nix
```

3. Run home-manager:
```bash
nix run github:nix-community/home-manager -- switch --configuration ~/nix/modules/linux/flake.nix
```

Or for a test build:
```bash
nix run github:nix-community/home-manager -- build --configuration ~/nix/modules/linux/flake.nix
```

## Pi Agent Configuration

The Pi Agent is configured through the `pi-agent.nix` module which:
- Sets up the `settings.json` file
- Creates the `bin` directory with required tools (fd, rg)
- Manages Pi Agent packages

The configuration is shared between macOS and Linux through the home-manager module system.
