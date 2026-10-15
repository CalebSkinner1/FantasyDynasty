#!/bin/bash
# Runs Workflow/Frequent Updates.R and logs it. launchd starts this every Tuesday at 10:00
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

# caffeinate keeps the Mac awake until the run finishes
caffeinate -i "$RSCRIPT" "Workflow/Frequent Updates.R" >> "$LOG" 2>&1
CODE=$?

END="$(date)"
if [ "$CODE" -eq 0 ]; then
  RESULT="success"
  MESSAGE="Frequent Updates finished and the app was deployed."
else
  RESULT="failed"
  MESSAGE="Frequent Updates failed (exit $CODE). Check the log."
fi

{
  printf "status: %s\n" "$RESULT"
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
