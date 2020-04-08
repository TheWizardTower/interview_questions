#!/usr/bin/env bash

tr -s ' ' '\n' | dos2unix | sort | uniq -c | sort -n | tail
