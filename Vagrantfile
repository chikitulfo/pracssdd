Vagrant.configure(2) do |config|
  config.vm.define "Distribuidos"
  config.vm.box = "ubuntu/trusty32"
  config.vm.provision :shell, path: "vagrantbootstrap.sh"
  config.vm.network "forwarded_port", guest: 8888, host: 8888
end
