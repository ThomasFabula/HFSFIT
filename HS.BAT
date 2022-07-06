echo=off
hgc full hprint
:nochmal
echo HFSFIT & SPEKTRUM
pause
kedit hfs.in
hfs
htos
s
goto nochmal
echo=on
