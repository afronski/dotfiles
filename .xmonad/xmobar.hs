Config {
  font = "xft:benis uushi-8",

  bgColor = "#000000",
  fgColor = "#ffffff",

  position = Static { xpos = 0, ypos = 0, width = 2368, height = 16 },
  lowerOnStart = True,

  commands = [
      Run Weather "EPKT" [
          "-t", "<tempC>°C <skyCondition>",
          "-L", "0",
          "-H", "30",
          "-n", "#CEFFAC",
          "-h", "#FFB6B0",
          "-l", "#96CBFE"
        ] 600,
      Run BatteryP [ "BAT0" ] [
          "-t", "<acstatus>",
          "-L", "10",
          "-H", "80",
          "-l", "#FFB6B0",
          "-h", "#CEFFAC",
          "--",
          "-O", "AC <left>%",
          "-i", "IDLE",
          "-o", "BATT <left>%"
        ] 600,
      Run MultiCpu [
          "-t", "CPU: <total0>% <total1>% <total2>% <total3>%",
          "-L", "30",
          "-H", "60",
          "-h", "#FFB6B0",
          "-l", "#CEFFAC",
          "-n", "#FFFFCC",
          "-w", "3"
        ] 10,
      Run Memory [
          "-t", "MEM: <used>/<total> MB",
          "-H", "16384",
          "-L", "4096",
          "-h", "#FFB6B0",
          "-l", "#CEFFAC",
          "-n", "lightblue"
        ] 10,
      Run MPD [
          "-t", "<artist> <title> <statei>",
          "--",
          "-P", "(PLAYING)",
          "-Z", "(PAUSED)",
          "-S", "(MPD)"
        ] 10,
      Run CoreTemp [
          "-t", "T: <core0>°C <core1>°C <core2>°C <core3>°C",
          "-L", "40",
          "-H", "60",
          "-l", "lightblue",
          "-n", "#FFFFCC",
          "-h", "#FFB6B0"
        ] 50,
      Run Wireless "wlp3s0" [
          "-t", "(<essid>)",
          "-x", "NO ESSID"
        ] 10,
      Run Network "wlp3s0" [
          "-t", "WIFI: <rx>/<tx> ",
          "-H", "250",
          "-L", "50",
          "-h", "#FFB6B0",
          "-l", "#CEFFAC",
          "-n", "#FFFFCC",
          "-x", ""
        ] 10,
      Run Network "enp0s31f6" [
          "-t", " ETH: <rx>/<tx>",
          "-H", "250",
          "-L", "50",
          "-h", "#FFB6B0",
          "-l", "#CEFFAC",
          "-n", "#FFFFCC",
          "-x", ""
        ] 10,
      Run Com "/home/afronski/.user-scripts/mixer-state" [] "mixer" 10,
      Run Com "/home/afronski/.user-scripts/governor" [] "governor" 10,
      Run Date "%a, %d %b %Y, %H: %M" "date" 10,
      Run StdinReader
  ],

  sepChar = "%",
  alignSep = "}{",

  template = "%StdinReader% }{%mpd% %mixer% | %multicpu% (%governor%) | %coretemp% | %memory% | %battery% | %wlp3s0%%wlp3s0wi%%enp0s31f6% | <action=/home/afronski/.user-scripts/toggle-clock><fc=#FFFFCC>%date%</fc></action> | %EPKT%"
}
