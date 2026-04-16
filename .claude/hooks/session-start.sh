#!/bin/bash
set -euo pipefail

# Only run on Claude Code remote (web sessions)
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# Install system dependencies and Haskell toolchain
apt-get update -qq 2>/dev/null
apt-get install -y -qq \
  ghc \
  haskell-stack \
  libffi-dev \
  libgmp-dev \
  libtinfo-dev \
  zlib1g-dev \
  libncurses-dev \
  2>/dev/null

cd "$CLAUDE_PROJECT_DIR"

# Build project dependencies and the project itself.
# Use -j1 to avoid DNS cache overflow in restricted network environments.
# --system-ghc: use the apt-installed GHC (9.4.7, compatible with lts-21.25's 9.4.8)
# --skip-ghc-check: accept the minor version mismatch (9.4.7 vs 9.4.8)
stack build --system-ghc --skip-ghc-check --pedantic -j1
