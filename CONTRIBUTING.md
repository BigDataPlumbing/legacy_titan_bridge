# Contributing to Legacy Titan Bridge

Thank you for your interest in contributing to Legacy Titan Bridge! This document provides guidelines and information for contributors.

## Code of Conduct

By participating in this project, you agree to maintain a respectful and inclusive environment for everyone.

## How to Contribute

### Reporting Issues

1. **Check existing issues** - Search [GitHub Issues](../../issues) to avoid duplicates
2. **Use issue templates** - Provide clear reproduction steps
3. **Include environment details** - OS, Rust version, Docker version, etc.

### Submitting Changes

1. **Fork the repository**
2. **Create a feature branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```
3. **Make your changes** following our coding standards
4. **Write/update tests** as needed
5. **Submit a Pull Request**

## Development Setup

### Prerequisites

- Rust 1.75+ (`rustup install stable`)
- GnuCOBOL 3.x
- Docker & Docker Compose
- Node.js 18+ (for Solidity tooling)

### Local Development

```bash
# Clone your fork
git clone https://github.com/YOUR_USERNAME/legacy-titan-bridge.git
cd legacy-titan-bridge

# Build COBOL library
cd cobol
cobc -m -o libcorebanking.so core_banking.cbl

# Build and test Rust sidecar
cd ../sidecar
cargo build
cargo test

# Run locally
cargo run
```

## Coding Standards

### Rust

- Follow [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)
- Run `cargo fmt` before committing
- Run `cargo clippy` and address warnings
- Add documentation for public APIs

### COBOL

- Use uppercase for keywords and paragraph names
- Include clear `AUTHOR` and purpose comments
- Follow IBM COBOL naming conventions

### Solidity

- Follow [Solidity Style Guide](https://docs.soliditylang.org/en/latest/style-guide.html)
- Use NatSpec comments for functions
- Include comprehensive test coverage

## Testing

```bash
# Rust tests
cd sidecar
cargo test

# Integration tests (requires Docker)
docker-compose -f docker-compose.test.yml up --abort-on-container-exit
```

## Pull Request Guidelines

1. **Keep PRs focused** - One feature/fix per PR
2. **Update documentation** - README, inline docs, etc.
3. **Add tests** - New features require test coverage
4. **Write clear commit messages** - Use conventional commits format

### Commit Message Format

```
type(scope): description

[optional body]

[optional footer]
```

Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

Example:
```
feat(merkle): add batch verification endpoint

Adds /verify-proof endpoint for individual transaction proof verification.

Closes #42
```

## Architecture Decisions

Major architectural changes require an ADR (Architecture Decision Record). Create a new file in `docs/adr/` following the template.

## Questions?

Open a [Discussion](../../discussions) or reach out to the maintainers.

---

Thank you for contributing to Legacy Titan Bridge! 🦀
