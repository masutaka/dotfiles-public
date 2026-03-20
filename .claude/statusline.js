#!/usr/bin/env node

// Read JSON from stdin
let input = '';
process.stdin.on('data', chunk => input += chunk);
process.stdin.on('end', () => {
  try {
    const path = require('path');
    const data = JSON.parse(input);

    // Extract values
    const model = data.model?.display_name || 'Unknown';
    const currentDir = path.basename(data.workspace?.current_dir || data.cwd || '.');

    // Gauges
    const cwPct = Math.round(data.context_window?.used_percentage || 0);
    const fiveHourPct = Math.round(data.rate_limits?.five_hour?.used_percentage || 0);
    const sevenDayPct = Math.round(data.rate_limits?.seven_day?.used_percentage || 0);
    const gaugeDisplay = [
      ['cw', cwPct],
      ['5h', fiveHourPct],
      ['7d', sevenDayPct],
    ].map(([label, pct]) => `${label} ${formatGauge(pct)}`).join(' | ');

    // Build status line
    const statusLine = `⚡ ${model} | 📁 ${currentDir} | ${gaugeDisplay}`;

    console.log(statusLine);
  } catch (error) {
    // Fallback status line on error
    console.log('[Error] 📁 .');
  }
});

const colorByPercentage = (text, pct) => {
  if (pct >= 90) return `\x1b[31m${text}\x1b[0m`;
  if (pct >= 70) return `\x1b[33m${text}\x1b[0m`;
  return `\x1b[32m${text}\x1b[0m`;
};

const pieIcon = (pct) => {
  if (pct >= 75) return '●';
  if (pct >= 50) return '◕';
  if (pct >= 25) return '◑';
  if (pct > 0)   return '◔';
  return '○';
};

const formatGauge = (pct) => {
  const icon = pieIcon(pct);
  return colorByPercentage(`${icon} ${pct}%`, pct);
};
