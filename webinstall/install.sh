#!/bin/sh

# Copyright (c) 2024-2026 the Qob authors.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Commentary:
#
# TODO(everyone): Keep this script simple and easily auditable.
#

set -e

if ! command -v unzip >/dev/null; then
  echo "Error: unzip is required to install Qob." 1>&2
  exit 1
fi

if [ "$OS" = "Windows_NT" ]; then
  ext="zip"
else
  ext="tar.gz"
fi

if [ "$OS" = "Windows_NT" ]; then
  target="win-x64"
else
  case $(uname -sm) in
    "Darwin x86_64") target="macos-x64" ;;
    "Darwin arm64") target="macos-arm64" ;;
    "Linux aarch64") target="linux-arm64" ;;
    *) target="linux-x64" ;;
  esac
fi

qob_uri="https://github.com/cl-qob/binaries/raw/master/${target}.${ext}"

qob_bin_dir=~/.local/bin
dwd_file=$qob_bin_dir/qob.${ext}

mkdir -p $qob_bin_dir

curl -fsSL $qob_uri -o $dwd_file

if [ "$OS" = "Windows_NT" ]; then
  unzip -d "$qob_bin_dir" -o "$dwd_file"
else
  tar -xvzf "$dwd_file" -C "$qob_bin_dir"
fi

rm $dwd_file

echo
echo "✓ Qob is installed in ${qob_bin_dir}."
echo
echo "Don't forget to add ${qob_bin_dir} to PATH environment variable:"
echo
echo "    export PATH=\"${qob_bin_dir}:\$PATH\""
echo
