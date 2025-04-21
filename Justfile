#!/usr/bin/env just --justfile
# https://github.com/casey/just

set dotenv-load

# Update git submodules
update:
	git submodule update --remote
	pre-commit autoupdate

# Run ansible playbook
ansible:
	ansible-galaxy install -r ansible/requirements.yml
	ANSIBLE_CONFIG=ansible/ansible.cfg ansible-playbook ansible/bootstrap.yml -i ansible/inventory.ini --tags untagged

