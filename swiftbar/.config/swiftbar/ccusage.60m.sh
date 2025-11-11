#!/bin/bash

# <xbar.title>Claude Code Usage Tracker</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Thiago Perrotta</xbar.author>
# <xbar.desc>Displays daily Claude Code spending in the menu bar</xbar.desc>

# Get today's date in YYYYMMDD format
TODAY=$(date +%Y%m%d)

# Fetch today's usage data using ccusage
JSON_OUTPUT=$(/opt/homebrew/bin/ccusage daily --json --since "$TODAY" --until "$TODAY" --offline 2>/dev/null)

# Parse today's cost from JSON
# If no data exists, default to 0
if echo "$JSON_OUTPUT" | grep -q '"daily"'; then
    TODAY_COST=$(echo "$JSON_OUTPUT" | grep -o '"totalCost": [0-9.]*' | head -1 | sed 's/"totalCost": //')
else
    TODAY_COST="0"
fi

# Default to 0 if empty
TODAY_COST=${TODAY_COST:-0}

# Format cost with 2 decimal places
FORMATTED_COST=$(printf "%.2f" "$TODAY_COST")

# Determine color based on spending thresholds
if (( $(echo "$TODAY_COST < 10" | bc -l) )); then
    COLOR="#4CAF50"  # Green
elif (( $(echo "$TODAY_COST < 25" | bc -l) )); then
    COLOR="#FFC107"  # Yellow
elif (( $(echo "$TODAY_COST < 50" | bc -l) )); then
    COLOR="#FF9800"  # Orange
else
    COLOR="#F44336"  # Red
fi

# Menu bar display
echo "💰 \$$FORMATTED_COST | color=$COLOR"

# Dropdown menu
echo "---"
echo "Today's Usage | size=14"
echo "Total Cost: \$$FORMATTED_COST | color=$COLOR"
echo "---"

# Last 7 Days Summary
echo "Last 7 Days | size=14"

# Get weekly data (using BSD date syntax for macOS)
WEEK_START=$(/bin/date -v-7d +%Y%m%d 2>/dev/null || date -d "7 days ago" +%Y%m%d 2>/dev/null)
WEEK_JSON=$(/opt/homebrew/bin/ccusage daily --json --since "$WEEK_START" --until "$TODAY" --offline 2>/dev/null)

if echo "$WEEK_JSON" | grep -q '"daily"'; then
    # Parse and display each day
    echo "$WEEK_JSON" | grep -o '"date": "[^"]*"' | while IFS= read -r line; do
        DATE=$(echo "$line" | sed 's/"date": "//;s/"//')

        # Get cost for this date
        COST=$(echo "$WEEK_JSON" | grep -A 20 "\"date\": \"$DATE\"" | grep -o '"totalCost": [0-9.]*' | head -1 | sed 's/"totalCost": //')

        # Format date as Mon DD
        FORMATTED_DATE=$(/bin/date -j -f "%Y-%m-%d" "$DATE" "+%b %d" 2>/dev/null || echo "$DATE")
        FORMATTED_DAY_COST=$(printf "%.2f" "$COST")

        # Highlight today
        if [ "$DATE" = "$(date +%Y-%m-%d)" ]; then
            echo "$FORMATTED_DATE: \$$FORMATTED_DAY_COST | color=$COLOR font=Monaco"
        else
            echo "$FORMATTED_DATE: \$$FORMATTED_DAY_COST | font=Monaco"
        fi
    done

    # Calculate and display weekly total (from totals section)
    WEEK_TOTAL=$(echo "$WEEK_JSON" | grep '"totals"' -A 10 | grep -o '"totalCost": [0-9.]*' | head -1 | sed 's/"totalCost": //')
    FORMATTED_WEEK_TOTAL=$(printf "%.2f" "${WEEK_TOTAL:-0}")
    echo "---"
    echo "7-Day Total: \$$FORMATTED_WEEK_TOTAL | size=12"
else
    echo "No data available"
fi

echo "---"
echo "Open Full Report | bash='/opt/homebrew/bin/ccusage' param1='daily' terminal=true"
echo "Refresh | refresh=true"
