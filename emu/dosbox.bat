@echo off

rem Just a shortcut to start DOSBox-X in the same environment/settings as
rem makedb.bat. Set path to DOSBox-X in env.bat first!

pushd "%~dp0"
call env.bat
"%dosbox%" -c mount .. -conf dosbox-x.conf
popd
