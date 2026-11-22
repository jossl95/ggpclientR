#!/bin/sh
# Deploy the documentation website. GitHub Actions (.github/workflows/docs.yml) renders docs/ with Quarto and
# publishes it to GitHub Pages; this script pushes main, makes sure that workflow runs, and waits for the result.
# Needs: git and python3. Optional: quarto (renders first, so errors show before the push), gh (starts a run when
# the push itself does not, e.g. nothing in docs/ changed).
set -eu
cd "$(dirname "$0")"

repo=$(git remote get-url origin | sed -E 's#^(git@github\.com:|https://github\.com/)##; s#\.git$##')
site="https://${repo%%/*}.github.io/${repo#*/}/"
runs="repos/$repo/actions/workflows/docs.yml/runs"

[ "$(git branch --show-current)" = main ] || { echo "Switch to main first."; exit 1; }
if [ -n "$(git status --porcelain -- docs)" ]; then
  echo "docs/ has uncommitted changes, which would not be deployed. Commit them first:"
  git status --short -- docs
  exit 1
fi
if command -v quarto >/dev/null 2>&1; then
  echo "Rendering docs/ ..."
  quarto render docs --quiet || { echo "quarto render failed: fix it before deploying."; exit 1; }
fi

git fetch -q origin main
[ "$(git rev-list --count HEAD..origin/main)" = 0 ] || { echo "GitHub has commits you do not have: git pull first."; exit 1; }
since=$(python3 -c 'import datetime as d; print((d.datetime.now(d.timezone.utc) - d.timedelta(seconds=10)).strftime("%Y-%m-%dT%H:%M:%SZ"))')
git push -q origin main

if command -v gh >/dev/null 2>&1 && gh auth status >/dev/null 2>&1; then
  get() { gh api "$1"; }  # logged in: 5000 requests an hour
else
  get() { curl -fs "https://api.github.com/$1"; }  # anonymous: 60 requests an hour
fi
latest() {  # "<id> <status> <conclusion>" of the newest docs run started since the push, "none", or nothing if GitHub is unreachable
  get "$runs?per_page=1&created=%3E%3D$since" 2>/dev/null | python3 -c '
import json, sys
runs = json.load(sys.stdin)["workflow_runs"]
print(*(runs[0]["id"], runs[0]["status"], runs[0]["conclusion"]) if runs else ["none"])' 2>/dev/null || true
}

i=0; run=$(latest)
while { [ -z "$run" ] || [ "$run" = none ]; } && [ $i -lt 6 ]; do sleep 5; i=$((i + 1)); run=$(latest); done
if [ -z "$run" ]; then
  echo "Pushed, but cannot reach the GitHub API to follow the deploy: see https://github.com/$repo/actions"
  exit 1
fi
if [ "$run" = none ]; then  # the push changed nothing in docs/, so it started no run
  if command -v gh >/dev/null 2>&1; then
    gh workflow run docs.yml -R "$repo" --ref main
  else
    echo "Nothing in docs/ changed, so no deploy started. Start one with 'Run workflow' at"
    echo "https://github.com/$repo/actions/workflows/docs.yml (or install gh and run this again)."
    exit 1
  fi
fi

echo "Deploying (https://github.com/$repo/actions/workflows/docs.yml) ..."
i=0
while [ $i -lt 120 ]; do  # at most 30 minutes
  run=$(latest)
  case "$run" in *" completed "*) break ;; esac
  sleep 15; i=$((i + 1))
done
case "$run" in *" completed "*) ;; *) echo "Still not finished, see https://github.com/$repo/actions"; exit 1 ;; esac
set -- $run
if [ "$3" = success ]; then echo "Deployed: $site"; else echo "Deploy $3: https://github.com/$repo/actions/runs/$1"; exit 1; fi
