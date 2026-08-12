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

# Waits for a backend to answer, failing fast if it dies first. /api/config is the endpoint that
# needs nothing built, so it answers as soon as the server is listening.
await_backend() {
  local port=$1 pid=$2
  local waited=0
  # Startup compiles the whole stdlib. Measured across runs on one dev machine: 114-138s,
  # so the previous 120s budget aborted at random and reported it as a backend failure.
  local max_wait=300
  echo "Waiting for backend on port ${port} to be ready..."
  while ! curl -sf "http://localhost:${port}/api/config" > /dev/null 2>&1; do
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

# Answering /api/config does not mean the backend is warm. --cached forces the typecheck
# pipeline, but buildSCCPipeline leaves evalBuildAll (step 7) and evalAnnots (step 8) as
# thunks, so the FIRST /api/page pays the tree building and annotation evaluation for that
# page. Measured on a dev machine: 25s for the first load, 0s for every one after. Without
# this the test that happens to make that first call eats the cost and blows the 15s default
# timeout, which reads as a failure of whatever test ran first rather than a cold cache.
#
# The unscoped #sidebar is warmed first because it is what the app itself asks for first, and it
# pays the same kind of cost over every loaded page rather than one. The page to warm is the one
# the server says it opens at, which is the page the app loads next.
warm_backend() {
  local port=$1 page
  curl -sf "http://localhost:${port}/api/annot?annot=%23sidebar" > /dev/null || true
  page=$(curl -sf "http://localhost:${port}/api/config" | node -e '
    let s = ""
    process.stdin.on("data", d => s += d).on("end", () => {
      const res = JSON.parse(s)
      if (res.tag !== "Success") return
      const root = res.contents[0].rootPage
      if (root) process.stdout.write(encodeURIComponent(root))
    })')
  if [ -n "$page" ]; then
    curl -sf "http://localhost:${port}/api/page?prgmName=${page}" > /dev/null || true
    echo "Backend on port ${port} warmed."
  else
    echo "The backend on port ${port} reports no page it opens at; skipping warmup."
  fi
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
# obsidian.ct is loaded beside id.ct so the vault it imports is among the pages. It is what gives
# the server notes, and so tasks, to answer a #taskList with; without it that annotation could only
# be checked against a doc server with nothing to list.
stack run catln -- doc test/Integration/code/id.ct test/Integration/code/obsidian.ct --api --cached &
BACKEND_PIDS+=($!)
await_backend "$API_PORT" "${BACKEND_PIDS[0]}"
warm_backend "$API_PORT"

# A second, --readonly instance so the tests can check that writes are refused.
# It has no --api flag because the port is only selectable that way; the serve port
# exposes the same API routes. Started only after the first is up, since two
# concurrent stdlib compiles contend badly for CPU and memory.
echo "Starting readonly Catln backend on port ${READONLY_PORT}..."
stack run catln -- doc test/Integration/code/id.ct --cached --readonly &
BACKEND_PIDS+=($!)
await_backend "$READONLY_PORT" "${BACKEND_PIDS[1]}"
warm_backend "$READONLY_PORT"

# Run integration tests
cd webdocs
npx vitest run --config vitest.integration.config.ts "$@"
