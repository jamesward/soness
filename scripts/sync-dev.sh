#!/usr/bin/env bash
# mirrors soness's sources into ../soness-dev (its own sbt server and target), for building while an eval runs:
# src/ exactly (files only the mirror has are removed), the rest copied over
set -e
mkdir -p ~/projects/soness-dev
cd ~/projects/soness
rm -rf ~/projects/soness-dev/src
tar --exclude=./target --exclude=./.bsp --exclude=./project/target --exclude=./project/project --exclude=./.soness -cf - . | (cd ~/projects/soness-dev && tar xf -)
