FROM ubuntu:disco
ENV USER nomad
RUN rm /etc/apt/apt.conf.d/docker-clean
RUN useradd -m $USER -G sudo
RUN echo $USER:pass | chpasswd
RUN echo root:pass | chpasswd
RUN apt update
RUN apt install -y tzdata keyboard-configuration
RUN apt install -y sudo git-core less bash-completion build-essential emacs-nox autoconf automake
RUN apt install -y wget file ccache m4
RUN apt install -y ncurses-bin
RUN apt install -y guile-2.2-dev qtbase5-dev qtchooser qtwebengine5-dev qtquickcontrols2-5-dev qtbase5-dev-tools qml-module-qtquick2 qml-module-qtquick-controls qml-module-qtwebengine qml-module-qtwebchannel qml-module-qtquick-layouts libqtermwidget5-0-dev
USER $USER
RUN mkdir -p /home/$USER/nomad
WorkDir /home/$USER/nomad
CMD /bin/bash --login
