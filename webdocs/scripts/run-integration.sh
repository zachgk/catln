#!/usr/bin/env bash
set -euo pipefail

API_PORT=31204
API_URL="http://localhost:${API_PORT}/api/toc"
BACKEND_PID=""

cleanup() {
  if [ -n "$BACKEND_PID" ] && kill -0 "$BACKEND_PID" 2>/dev/null; then
    echo "Stopping backend (PID $BACKEND_PID)..."
    kill "$BACKEND_PID"
    wait "$BACKEND_PID" 2>/dev/null || true
  fi
}
trap cleanup EXIT

# Kill any existing process on the API port
if lsof -ti ":${API_PORT}" > /dev/null 2>&1; then
  echo "Killing existing process on port ${API_PORT}..."
  kill $(lsof -ti ":${API_PORT}") 2>/dev/null || true
  sleep 1
fi

# Start the Catln backend API server
echo "Starting Catln backend on port ${API_PORT}..."
cd "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
stack run catln -- doc test/Integration/code/id.ct --api &
BACKEND_PID=$!

# Wait for the backend to be ready
echo "Waiting for backend to be ready..."
MAX_WAIT=120
WAITED=0
while ! curl -sf "$API_URL" > /dev/null 2>&1; do
  if ! kill -0 "$BACKEND_PID" 2>/dev/null; then
    echo "Backend process exited unexpectedly."
    exit 1
  fi
  if [ "$WAITED" -ge "$MAX_WAIT" ]; then
    echo "Timed out waiting for backend after ${MAX_WAIT}s."
    exit 1
  fi
  sleep 1
  WAITED=$((WAITED + 1))
done
echo "Backend ready after ${WAITED}s."

# Run integration tests
cd webdocs
npx vitest run --config vitest.integration.config.ts "$@"
