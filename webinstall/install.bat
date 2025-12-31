@echo off

:: Copyright (c) 2024-2026 the Qob authors.

:: This program is free software; you can redistribute it and/or modify
:: it under the terms of the GNU General Public License as published by
:: the Free Software Foundation; either version 3, or (at your option)
:: any later version.

:: This program is distributed in the hope that it will be useful,
:: but WITHOUT ANY WARRANTY; without even the implied warranty of
:: MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
:: GNU General Public License for more details.

:: You should have received a copy of the GNU General Public License
:: along with this program.  If not, see <https://www.gnu.org/licenses/>.

::: Commentary:
::
:: TODO(everyone): Keep this script simple and easily auditable.
::

set URL=https://github.com/cl-qob/binaries/raw/master/win-x64.zip
set QOB_BIN_DIR=%USERPROFILE%\.local\bin
set ZIP=%QOB_BIN_DIR%\qob.zip

mkdir %QOB_BIN_DIR%

curl.exe -fsSL %URL% -o %ZIP%

tar.exe -xf %ZIP% -C %QOB_BIN_DIR%

del %ZIP%

echo.
echo ✓ Qob is installed in %QOB_BIN_DIR%.
echo.
echo Don't forget to add %QOB_BIN_DIR% to PATH environment variable:
echo.
echo     set PATH=%QOB_BIN_DIR%;%%PATH%%
echo.
