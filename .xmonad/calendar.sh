#!/bin/bash

x="-1"
y="10"

if [ ! -z $1 ] ; then
    x=$1
fi

if [ ! -z $2 ] ; then
    y=$2
fi

TODAY=$(expr `date +'%d'` + 0)
MONTH=`date +'%m'`
YEAR=`date +'%Y'`
 
(
#date +'^bg(red)^fg(#111111)%A, %d.%m.%Y %H:%M^fg()^bg()'
 
# current month, highlight header and today
cal | sed -r -e "1,2 s/.*/^fg(white)&^fg()/" \
             -e "s/(^| )($TODAY)($| )/\1^bg(white)^fg(#111)\2^fg()^bg()\3/"
 
echo
# next month, hilight header
[ $MONTH -eq 12 ] && YEAR=`expr $YEAR + 1`
cal `expr \( $MONTH + 1 \) % 12` $YEAR \
    | sed -e "1,2 s/.*/^fg(white)&^fg()/"
) \
    | dzen2 -p 60 -x "$x" -y "$y" -w 190 -l 17 -sa l -e 'onstart=uncollapse;button1=exit'
