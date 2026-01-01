# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |

## Reporting a Vulnerability

We take security seriously. If you discover a security vulnerability, please follow responsible disclosure:

### Do NOT

- Open a public GitHub issue
- Disclose the vulnerability publicly before we've had a chance to address it

### Do

1. **Email us directly** at security@bdp.dev with:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact assessment
   - Any suggested fixes (optional)

2. **Expect a response** within 48 hours acknowledging receipt

3. **Work with us** on remediation timeline

## Security Considerations for Deployment

> ⚠️ **This is a demonstration project.** Production deployment requires additional security hardening.

### Required for Production

| Component | Requirement |
|-----------|-------------|
| Private Keys | Use HSM or HashiCorp Vault - never environment variables |
| API Authentication | Add JWT/OAuth2 before exposing endpoints |
| Network | TLS termination, network segmentation, WAF |
| COBOL Library | Security audit of FFI boundary |
| Rate Limiting | Implement to prevent DoS |

### Threat Model

This system handles high-value financial transactions. Key threats include:

1. **Private Key Exposure** - Mitigate with HSM integration
2. **Transaction Replay** - Mitigate with unique TX IDs in Merkle tree
3. **Memory Safety** - Mitigated by Rust's ownership model
4. **COBOL Buffer Overflows** - Requires security audit of legacy code

## Security Updates

Subscribe to GitHub releases for security updates.
