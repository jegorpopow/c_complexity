#!/bin/bash

echo "Analyzing function $2 from file '$2'"
python3 frontend/patterns.py "$1" "$2" | ./backend/main
