#!/bin/bash
# One-time setup: schedules run_frequent_updates.sh every Tuesday at 10:00 (your Mac's local
# time, so daylight saving is handled) with launchd. Run it from Terminal:
#
#   bash "<path to FantasyDynasty>/Workflow/Scheduled/install_schedule.sh"
#
# If the Mac is asleep or off at 10:00, launchd runs the job when it next wakes. To wake the
# Mac on its own at 9:55 on Tuesdays (it must be plugged in; replaces any other pmset repeat
# schedule), also run:
#
#   sudo pmset repeat wakeorpoweron TU 09:55:00
#
# Run it now (to test):   launchctl kickstart -k gui/$(id -u)/com.fantasydynasty.frequent-updates
# Remove the schedule:    launchctl bootout gui/$(id -u)/com.fantasydynasty.frequent-updates
#                         rm ~/Library/LaunchAgents/com.fantasydynasty.frequent-updates.plist

set -eu

LABEL="com.fantasydynasty.frequent-updates"
SCHEDULED_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$SCHEDULED_DIR/../.." && pwd)"
SCRIPT="$SCHEDULED_DIR/run_frequent_updates.sh"
PLIST="$HOME/Library/LaunchAgents/$LABEL.plist"

chmod +x "$SCRIPT"
mkdir -p "$HOME/Library/LaunchAgents" "$HOME/Library/Logs/FantasyDynasty"

cat > "$PLIST" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>$LABEL</string>
  <key>ProgramArguments</key>
  <array>
    <string>/bin/bash</string>
    <string>$SCRIPT</string>
  </array>
  <key>StartCalendarInterval</key>
  <dict>
    <key>Weekday</key>
    <integer>2</integer>
    <key>Hour</key>
    <integer>10</integer>
    <key>Minute</key>
    <integer>0</integer>
  </dict>
  <key>StandardOutPath</key>
  <string>$HOME/Library/Logs/FantasyDynasty/launchd.log</string>
  <key>StandardErrorPath</key>
  <string>$HOME/Library/Logs/FantasyDynasty/launchd.log</string>
</dict>
</plist>
EOF

# reload if it was already installed
launchctl bootout "gui/$(id -u)/$LABEL" 2>/dev/null || true
launchctl bootstrap "gui/$(id -u)" "$PLIST"

echo "Scheduled: every Tuesday at 10:00 -> $SCRIPT"
echo "Logs: $REPO/Workflow/logs (launchd itself logs to ~/Library/Logs/FantasyDynasty)"
echo
echo "Next steps:"
echo "  1. System Settings > Privacy & Security > Full Disk Access: add /bin/bash"
echo "     (the project is on your Desktop, which background jobs can't read without it)."
echo "  2. Optional, to wake the Mac:  sudo pmset repeat wakeorpoweron TU 09:55:00"
echo "  3. Test now:  launchctl kickstart -k gui/\$(id -u)/$LABEL"
