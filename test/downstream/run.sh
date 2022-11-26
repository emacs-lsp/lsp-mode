#!/usr/bin/env bash

cd $(dirname "$0")

tree

eask install
eask install-deps --dev --allow-error
