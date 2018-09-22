#!/usr/bin/env bash

export PGPASSWORD=TheSuperuserPassword
export PGHOST=postgres
export PGUSER=postgres

"$@"
