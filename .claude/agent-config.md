# Agent Mesh Configuration for nix-darwin Repository

This document defines optimized agent configurations and workflows specific to this Nix-darwin system configuration repository.

## Project Profile

**Type**: Infrastructure as Code (Nix)
**Primary Language**: Nix
**Domain**: System Configuration Management
**Complexity**: Medium (39 .nix files, modular structure)
**Active Development**: Terminal workflow optimization, package management

## Recommended Agent Workflows

### 1. Infrastructure Development

**Agent**: `ensemble-infrastructure:infrastructure-developer`

**Use Cases**:
- Modifying Nix module configurations
- Adding/updating packages in `modules/home-manager/packages.nix`
- Creating custom overlays in `overlays/`
- Updating flake inputs in `flake.nix`
- System-level darwin settings in `modules/darwin/`

**Context Requirements**:
- Always read existing module files before modification
- Preserve commented packages (intentional documentation)
- Test with `just deploy-nc` after flake changes
- Reference nix-darwin and home-manager option documentation

**Example Invocation**:
```
Use the infrastructure-developer agent to add the `ripgrep-all` package to my home-manager configuration
```

### 2. Documentation Management

**Agent**: `ensemble-development:documentation-specialist`

**Use Cases**:
- Updating technical documentation in `docs/TRD/`
- Maintaining product requirements in `docs/PRD/`
- Keeping migration guides current (`BREWFILE_MIGRATION.md`, `CHEZMOI_MIGRATION.md`)
- Documenting new system configurations
- Creating keybinding references (like `ZELLIJ_KEYBINDINGS.md`)

**Context Requirements**:
- Cross-reference with actual implementation
- Include file paths with line numbers
- Provide code examples in documentation
- Link to external resources when relevant

**Example Invocation**:
```
Use the documentation-specialist agent to create a TRD for the new Ghostty terminal configuration
```

### 3. Git Workflow Automation

**Agent**: `ensemble-git:git-workflow`

**Use Cases**:
- Creating conventional commits for nix changes
- Managing feature branches with git-town
- Preparing configuration change commits
- Following semantic commit patterns

**Context Requirements**:
- Commit message format: `<type>(<scope>): <description>`
- Common types: `feat`, `fix`, `refactor`, `docs`, `chore`
- Common scopes: `nix`, `home-manager`, `darwin`, `packages`, `secrets`
- Include co-authorship: `Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>`

**Example Invocation**:
```
Use the git-workflow agent to commit these nix configuration changes with a proper conventional commit message
```

### 4. Technical Planning

**Agent**: `Plan` (built-in)

**Use Cases**:
- Planning multi-file configuration changes
- Architecting new module structures
- Designing package migration strategies
- Planning integration of new development tools

**Context Requirements**:
- Review existing module patterns
- Consider dependency impacts
- Plan testing strategy (`just deploy-nc`, `nix flake check`)
- Document rollback procedures

**Example Invocation**:
```
Enter plan mode to design a migration strategy for moving all shell configurations to home-manager modules
```

## Domain-Specific Patterns

### Nix Configuration Changes

**Standard Workflow**:
1. Read existing configuration file
2. Identify insertion point
3. Preserve existing patterns (comments, formatting, grouping)
4. Add/modify configuration
5. Test with `just deploy-nc` (no cache)
6. Verify system behavior
7. Commit with conventional message

**Common Pitfalls to Avoid**:
- Never assume Nix file structure without reading first
- Don't remove commented packages without asking
- Always test flake changes with `deploy-nc` (not `deploy`)
- Never suggest plaintext secrets (use sops-nix paths)

### Package Management

**Decision Tree**:
```
Is package available in nixpkgs?
├─ Yes → Add to modules/home-manager/packages.nix
└─ No → Check if available in Homebrew
    ├─ Yes → Add to modules/darwin/homebrew.nix
    └─ No → Consider creating custom overlay
```

**Package Addition Pattern**:
```nix
# In modules/home-manager/packages.nix
home.packages = with pkgs; [
  # ... existing packages (keep grouped by category)

  # <Category Name>
  new-package            # Description
  # unavailable-package - not in nixpkgs, use brew
];
```

### Secrets Management

**Always**:
- Reference secrets via `/run/secrets/<key>` paths
- Use `./edit_secrets.sh` for modifications
- Never commit plaintext secrets
- Provide fallback values: `"$(cat /run/secrets/key 2>/dev/null || echo "")"`

**Never**:
- Suggest hardcoding API keys
- Commit sensitive values
- Skip sops-nix encryption

### System Configuration

**macOS Defaults Pattern**:
```nix
# modules/darwin/system.nix
system.defaults = {
  NSGlobalDomain = {
    # Global settings
  };

  dock = {
    # Dock-specific settings
  };

  finder = {
    # Finder-specific settings
  };
};
```

**Launchd Services Pattern**:
```nix
# modules/darwin/services.nix
launchd.daemons.<service-name> = {
  serviceConfig = {
    ProgramArguments = [ ... ];
    RunAtLoad = true;
    KeepAlive = true;
  };
};
```

## Context Management Strategy

### High-Priority Context (Always Retain)

1. **Flake Structure**:
   - Input sources (nixpkgs, home-manager, nix-darwin)
   - Current versions from `flake.lock`
   - Overlay configurations

2. **Active Integrations**:
   - Zellij + Neovim navigation (vim-zellij-navigator)
   - Aerospace window manager (no keybinding conflicts)
   - Atuin history (Ctrl+P conflict resolution)
   - sops-nix secrets management

3. **System Architecture**:
   - Target: aarch64-darwin (Apple Silicon)
   - User: ldangelo
   - Hostname: Leos-MacBook-Pro
   - Installation: Determinate Nix Installer

4. **Critical Configuration Patterns**:
   - 2-space indentation for Nix
   - Catppuccin theming everywhere
   - Just commands preferred over raw darwin-rebuild
   - Test with `deploy-nc` after flake changes

### Medium-Priority Context (Reference as Needed)

1. **Package Lists**:
   - Nix packages in `modules/home-manager/packages.nix`
   - Homebrew formulas/casks in `modules/darwin/homebrew.nix`
   - Unavailable packages (documented in comments)

2. **Module Structure**:
   - Darwin modules organization
   - Home-manager modules organization
   - Import patterns

3. **Development Tools**:
   - Installed editors (Neovim, Emacs, VSCode)
   - Terminal stack (Zellij, Ghostty, Zsh)
   - Version control tools (Git, Jujutsu, Git Town)

### Low-Priority Context (On-Demand Only)

1. **Historical Information**:
   - Brewfile migration details
   - Chezmoi migration process
   - Deprecated configurations (Doom Emacs, spacemacs)

2. **Detailed Configuration**:
   - Specific Zsh rc files
   - Individual dotfile contents
   - Full package descriptions

## Agent Selection Matrix

| Task Type | Recommended Agent | Alternative |
|-----------|------------------|-------------|
| Add Nix package | infrastructure-developer | None (simple task) |
| Modify system defaults | infrastructure-developer | None |
| Create custom overlay | infrastructure-developer | Plan mode first |
| Update documentation | documentation-specialist | None |
| Write TRD/PRD | documentation-specialist | None |
| Commit changes | git-workflow | None |
| Multi-module refactor | Plan mode | infrastructure-developer |
| New feature integration | Plan mode | infrastructure-developer |
| Debug build failures | None (direct) | Explore mode |
| Understand codebase | Explore mode | None |

## Optimization Settings

### Token Budget Management

**Strategies**:
- Prefer targeted file reads over full module scans
- Use `Glob` for file discovery, then `Read` specific files
- Cache frequently referenced files in memory (CLAUDE.md, flake.nix)
- Summarize large configuration files when context is sufficient

**Token Priorities**:
1. Current task requirements
2. Active file modifications
3. Related module dependencies
4. Reference documentation (as links, not full text)
5. Historical context (minimal)

### Error Recovery Patterns

**Build Failures**:
1. Parse error message for specific failure
2. Read relevant configuration file
3. Identify issue (syntax, missing dependency, etc.)
4. Apply fix
5. Suggest `just deploy-nc` for retry

**Flake Lock Issues**:
1. Suggest `just up` to update all inputs
2. If specific input: `just upp i=<input-name>`
3. Check for version conflicts in error output
4. Review `flake.lock` for actual versions

**Homebrew Conflicts**:
1. Check if package exists in nixpkgs first
2. Verify Homebrew formula name is correct
3. Consider using Nix overlay if custom build needed

## Quality Gates

Before completing any configuration change:

- [ ] Configuration syntax is valid Nix
- [ ] File follows existing patterns and conventions
- [ ] Comments preserved or added for clarity
- [ ] No plaintext secrets introduced
- [ ] Testing command provided (`just deploy-nc` or `just deploy`)
- [ ] Rollback procedure mentioned if significant change
- [ ] Related documentation updated if needed

## Integration Points

### Claude Code IDE Features

**Available**:
- Emacs xref (find references, definitions)
- Tree-sitter syntax analysis
- Imenu symbol navigation
- Flycheck/Flymake diagnostics

**Usage Patterns**:
- Use xref to find where options are defined
- Use imenu to list all modules in a file
- Reference coordinates: line (1-based), column (0-based)

### MCP Server Capabilities

**Claude in Chrome**:
- Look up nix-darwin documentation
- Search home-manager options
- Find nixpkgs package details
- Research Nix best practices

**Emacs Tools**:
- Navigate project structure
- Analyze symbol references
- Get diagnostic information

## Success Metrics

Track these to measure optimization effectiveness:

1. **Configuration Quality**:
   - First-time deployment success rate
   - Number of rollbacks needed
   - Build time improvements

2. **Agent Efficiency**:
   - Token usage per task
   - Number of tool calls required
   - Task completion time

3. **Documentation Quality**:
   - Accuracy of CLAUDE.md context
   - Completeness of TRD/PRD documents
   - Migration guide usability

4. **Development Velocity**:
   - Time to add new packages
   - Time to modify system settings
   - Time to integrate new tools

---

**Configuration Version**: 1.0.0
**Last Updated**: 2026-01-15
**Compatible with**: Claude Code (claude-sonnet-4-5-20250929)
