#!/bin/bash

cat LINKS.log | xargs git checkout
rm LINKS.log