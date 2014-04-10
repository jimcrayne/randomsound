#!/bin/sh
### BEGIN INIT INFO
# Provides:          randomsound
# Required-Start:    $remote_fs $syslog alsa-utils
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Random Sound
# Description:       ALSA sound card related entropy gathering daemon.
### END INIT INFO

PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
DAEMON=/usr/sbin/randomsound
NAME=randomsound
DESC="ALSA Entropy gatherer"

test -x $DAEMON || exit 0
if [ ! -e /proc/asound/cards ]; then
	echo "No /proc/asound/cards: is alsa configured?"
	exit 0
fi

# Include randomsound defaults if available
if [ -f /etc/default/randomsound ] ; then
	. /etc/default/randomsound
fi

case "$1" in
  start)
	echo -n "Starting $DESC: "
	start-stop-daemon --start --quiet --exec $DAEMON -- -D $DAEMON_OPTS > /dev/null
	echo "$NAME."
	;;
  stop)
	echo -n "Stopping $DESC: "
	start-stop-daemon --stop  --quiet --pidfile /var/run/$NAME.pid 
	echo "$NAME."
	;;
  force-reload)
	# check wether $DAEMON is running. If so, restart
	start-stop-daemon --stop --test --quiet --pidfile \
		/var/run/$NAME.pid --exec $DAEMON \
	&& $0 restart \
	|| exit 0
	;;
  restart)
    echo -n "Restarting $DESC: "
	start-stop-daemon --stop  --quiet --pidfile /var/run/$NAME.pid 
	sleep 1
	start-stop-daemon --start --quiet --exec $DAEMON -- -D $DAEMON_OPTS > /dev/null
	echo "$NAME."
	;;
  *)
	N=/etc/init.d/$NAME
	echo "Usage: $N {start|stop|restart|force-reload}" >&2
	exit 1
	;;
esac

exit 0
