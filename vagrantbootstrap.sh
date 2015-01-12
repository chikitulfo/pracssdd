#!/bin/bash

apt-get update
apt-get install -y yaws erlang couchdb memcached
ln -s /vagrant /home/vagrant/pracssdd
update-rc.d yaws disable
service yaws stop
