package = moot

stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

build: frontend-build
	$(stack) build $(package)

build-fast:
	$(stack) build -j4 --fast --no-terminal

build-dirty:
	$(stack) build --force-dirty $(package)

build-profile:
	$(stack) --work-dir .stack-work-profiling --profile build

backend-watch:
	$(stack) exec -- yesod devel

frontend-build:
	cd frontend && npm run-script build

frontend-watch:
	cd frontend && npm start

backend-deps:
	$(stack) install yesod-bin alex happy

frontend-deps:
	cd frontend && npm install

deps: backend-deps frontend-deps

run:
	$(stack) build --fast && $(stack) exec -- $(package)

install:
	$(stack) install

ghci:
	$(stack) ghci $(package):lib --ghci-options='-j4 +RTS -A128m'

test:
	$(stack) test $(package)

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests

bench:
	$(stack) bench $(package)

ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind' --main-is $(package):$(package)"

dev-deps:
	stack install ghcid

reset-database: destroy-create-db migration fixtures

reset-data: truncate-tables fixtures

drop-databases:
	-sudo -u postgres dropdb moot_dev
	-sudo -u postgres dropdb moot_test

create-db-user: drop-databases
	-sudo -u postgres dropuser moot
	sudo -u postgres bash ./scripts/create-db-users.sh

destroy-create-db: drop-databases
	sudo -u postgres createdb -O moot moot_dev
	sudo -u postgres createdb -O moot moot_test

# These variants should behave themselves better on Mac and Windows. You
# might need to setup a pgpass.conf for automatic authentication
# since you're not using sudo.
drop-databases-mac-windows:
	-dropdb -U postgres moot_dev
	-dropdb -U postgres moot_test

create-db-user-mac-windows: drop-databases-mac-windows
	-dropuser -U postgres moot
	bash ./scripts/create-db-users.sh

destroy-create-db-mac-windows: drop-databases-mac-windows
	createdb -U postgres -O moot moot_dev
	createdb -U postgres -O moot moot_test

recreate-db: create-db-user destroy-create-db

recreate-db-mac-windows: create-db-user-mac-windows destroy-create-db-mac-windows

psql:
	sudo -u postgres psql moot_dev

psql-mac-windows:
	psql -U postgres moot_dev

migration: build
	stack exec -- migration

fixtures: build
	stack exec -- fixtures

truncate-tables: build
	stack exec -- truncate

build-image:
	sudo docker build -t moot-build-image .
	sudo docker tag moot-build-image:latest registry.gitlab.com/lorepub/moot/moot-build-image:latest

docker-shell:
	docker run -u 1000:1000 -v `pwd`:/builds/lorepub/moot -it moot-build-image /bin/bash

push-image:
	sudo docker push registry.gitlab.com/lorepub/moot/moot-build-image

.PHONY: build build-dirty run install ghci test test-ghci ghcid dev-deps build-image push-image
