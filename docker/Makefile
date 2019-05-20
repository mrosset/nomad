start: dock
	-docker rm -f nomad
	docker run --privileged --name nomad -d -P -e TERM=$(TERM) -e DISPLAY=$(DISPLAY) -v cache:/var/cache -v /tmp/.X11-unix:/tmp/.X11-unix:rw -v $(PWD):/home/nomad/nomad -it nomad:strings

dock: Dockerfile
	docker build -t nomad:strings .

attach: start
	docker container attach nomad
