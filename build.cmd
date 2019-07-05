@echo off

.paket\paket.exe restore -v
if errorlevel 1 (
  exit /b %errorlevel%
)

FAKE.exe build %*