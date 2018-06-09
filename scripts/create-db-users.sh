#!/usr/bin/bash

psql -U postgres postgres <<OMG
  CREATE USER moot password 'moot';
  ALTER USER moot WITH SUPERUSER;
OMG
