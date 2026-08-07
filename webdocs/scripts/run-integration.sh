#!/usr/bin/env bash
set -euo pipefail

API_PORT=31204
READONLY_PORT=8080
BACKEND_PIDS=()

cleanup() {
  for pid in ${BACKEND_PIDS[@]+"${BACKEND_PIDS[@]}"}; do
    if kill -0 "$pid" 2>/dev/null; then
      echo "Stopping backend (PID $pid)..."
      kill "$pid"
      wait "$pid" 2>/dev/null || true
    fi
  done
}
trap cleanup EXIT

free_port() {
  if lsof -ti ":$1" > /dev/null 2>&1; then
    echo "Killing existing process on port $1..."
    kill $(lsof -ti ":$1") 2>/dev/null || true
    sleep 1
  fi
}

# Waits for a backend to answer /api/toc, failing fast if it dies first.
await_backend() {
  local port=$1 pid=$2
  local waited=0
  local max_wait=120
  echo "Waiting for backend on port ${port} to be ready..."
  while ! curl -sf "http://localhost:${port}/api/toc" > /dev/null 2>&1; do
    if ! kill -0 "$pid" 2>/dev/null; then
      echo "Backend on port ${port} exited unexpectedly."
      exit 1
    fi
    if [ "$waited" -ge "$max_wait" ]; then
      echo "Timed out waiting for backend on port ${port} after ${max_wait}s."
      exit 1
    fi
    sleep 1
    waited=$((waited + 1))
  done
  echo "Backend on port ${port} ready after ${waited}s."
}

free_port "$API_PORT"
free_port "$READONLY_PORT"

cd "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"

# Start the Catln backend API server.
# --cached builds once at startup and serves from that build. Without it the live
# provider re-runs ctssBuildAll on every request (see the TODO in mkLiveWDProvider),
# costing ~60s per call and timing out every test. These tests only check the API
# contract over a fixed file set, so live reloading buys nothing here.
echo "Starting Catln backend on port ${API_PORT}..."
stack run catln -- doc test/Integration/code/id.ct --api --cached &
BACKEND_PIDS+=($!)
await_backend "$API_PORT" "${BACKEND_PIDS[0]}"

# A second, --readonly instance so the tests can check that writes are refused.
# It has no --api flag because the port is only selectable that way; the serve port
# exposes the same API routes. Started only after the first is up, since two
# concurrent stdlib compiles contend badly for CPU and memory.
echo "Starting readonly Catln backend on port ${READONLY_PORT}..."
stack run catln -- doc test/Integration/code/id.ct --cached --readonly &
BACKEND_PIDS+=($!)
await_backend "$READONLY_PORT" "${BACKEND_PIDS[1]}"

# Run integration tests
cd webdocs
npx vitest run --config vitest.integration.config.ts "$@"
