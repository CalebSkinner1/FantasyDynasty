#!/bin/bash
# Runs Workflow/Frequent Updates.R and logs it. launchd starts this every day at 10:00
# (see com.fantasydynasty.frequent-updates.plist and install_schedule.sh in this folder).
# Nothing is committed or pushed to GitHub; review the changes yourself.
#
# Output:
#   Workflow/logs/frequent_updates_<date>.log   full output of the run
#   Workflow/logs/last_run.txt                  status of the latest run (success/failed)

set -u

REPO="$(cd "$(dirname "$0")/../.." && pwd)"
LOG_DIR="$REPO/Workflow/logs"
mkdir -p "$LOG_DIR"

STAMP="$(date +%Y-%m-%d_%H%M)"
LOG="$LOG_DIR/frequent_updates_$STAMP.log"
STATUS="$LOG_DIR/last_run.txt"
LOCK="$LOG_DIR/.running"

# launchd starts jobs with a bare environment: add the usual R locations and a UTF-8 locale
export PATH="/usr/local/bin:/opt/homebrew/bin:/Library/Frameworks/R.framework/Resources/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

RSCRIPT="$(command -v Rscript)"
if [ -z "$RSCRIPT" ]; then
  printf "status: failed\nreason: Rscript not found\nstarted: %s\n" "$(date)" > "$STATUS"
  exit 1
fi

# skip if a previous run is still going
if ! mkdir "$LOCK" 2>/dev/null; then
  echo "$(date): skipped, a previous run is still going" >> "$LOG_DIR/skipped.log"
  exit 0
fi
trap 'rmdir "$LOCK"' EXIT

START="$(date)"
printf "status: running\nstarted: %s\nlog: %s\n" "$START" "$LOG" > "$STATUS"

cd "$REPO" || exit 1
echo "Frequent Updates started $START (Rscript: $RSCRIPT)" > "$LOG"

# keep the Mac awake until this script finishes (including the wait below)
caffeinate -i -w $$ &

# wait for the internet, up to 5 minutes: right after the Mac wakes, Wi-Fi can take a while
# to reconnect, and a run that starts first fails at its first download. Any answer from
# Sleeper counts as online.
REASON=""
WAITED=0
until curl -sS --max-time 10 -o /dev/null "https://api.sleeper.app/v1/state/nfl" 2>/dev/null; do
  if [ "$WAITED" -ge 300 ]; then
    REASON="no internet connection (api.sleeper.app unreachable for 5 minutes), so nothing ran"
    break
  fi
  sleep 15
  WAITED=$((WAITED + 15))
done

if [ -n "$REASON" ]; then
  echo "$REASON" >> "$LOG"
  CODE=2
else
  if [ "$WAITED" -gt 0 ]; then
    echo "waited $WAITED seconds for the internet" >> "$LOG"
  fi
  "$RSCRIPT" "Workflow/Frequent Updates.R" >> "$LOG" 2>&1
  CODE=$?
fi

END="$(date)"
if [ "$CODE" -eq 0 ]; then
  RESULT="success"
  MESSAGE="Frequent Updates finished and the app was deployed."
elif [ -n "$REASON" ]; then
  RESULT="failed"
  MESSAGE="Frequent Updates didn't run: no internet connection."
else
  RESULT="failed"
  MESSAGE="Frequent Updates failed (exit $CODE). Check the log."
fi

{
  printf "status: %s\n" "$RESULT"
  if [ -n "$REASON" ]; then
    printf "reason: %s\n" "$REASON"
  fi
  printf "exit code: %s\n" "$CODE"
  printf "started: %s\n" "$START"
  printf "finished: %s\n" "$END"
  printf "log: %s\n" "$LOG"
  printf "\nlast lines of the log:\n"
  tail -n 15 "$LOG"
} > "$STATUS"

echo "Frequent Updates $RESULT at $END" >> "$LOG"

# desktop notification
osascript -e "display notification \"$MESSAGE\" with title \"Fantasy Dynasty\"" >/dev/null 2>&1

# keep two months of logs
find "$LOG_DIR" -name 'frequent_updates_*.log' -mtime +60 -delete 2>/dev/null

exit "$CODE"
