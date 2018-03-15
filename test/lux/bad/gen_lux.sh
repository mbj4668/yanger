#!/bin/sh

set -e

# pick up any extra flags to yanger needed for this module
YF=`sed -n 's/^[^:]*YANGER_EXTRA_FLAGS:\(.*\)$/\1/p' $1`
CMD0="yanger --smiv2-detect-duplicate-oids $YF"
CMD="$CMD0 --print-error-code"

# NOTE: We mustn't send command lines longer than 80 chars below, since some
# shells output a CR at column 80 to deal with the vt100/xterm cursor issue.
# Thus we fold the command with '\' (need 2 since single backslash is consumed
# by lux, means 4 in the "here doc" - using echo is impossible due to broken
# /bin/sh implementations) and match only the second line - but this means we
# must match on the PS2 prompt, otherwise it may appear *after* the echo of
# the second line. Sigh...

cat <<EOF
[doc Test errors in $1]

[shell yanger]
  !export PS2=CONT:
  ?SH-PROMPT:
  !$CMD \\\\
  ?CONT:
  !$1 2>&1 | grep $1
  -SH-PROMPT
"""?
$1 2>&1 \\| grep $1
EOF

grep -n LINE $1 /dev/null | awk '
/LINE:/ { split($0, a, "LINE: "); printf "%s([0-9]+:)? (\\(.*\\): )?%s\n", $1, a[2] }
'

cat <<EOF
"""
  # make sure there are no extra errors reported
  -$1
  ?SH-PROMPT:

  # verify that a correctly formatted message is given w/o --print-error-code
  # (i.e. no crash, and no "unregistered" code)
  !$CMD0 \\\\
  ?CONT:
  !$1 2>&1 | grep $1
  -SH-PROMPT
"""?
$1 2>&1 \\| grep $1
EOF

grep -n LINE $1 /dev/null | awk '
/LINE:/ { printf "%s([0-9]+:)? (\\(.*\\): )?(error|warning): \\S+.*\n", $1 }
'

cat <<EOF
"""
  # make sure there are no extra errors reported
  -$1
  ?SH-PROMPT:
EOF
