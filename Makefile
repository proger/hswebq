

mon:
	while true; do \
		pgrep hswebq | xargs ps -o pid=,stime=,cputime=,%cpu=,vsz=,rss=,wchan=,args=; curl -s :3000/waitq; \
		sleep 1; done

spin:
	./spin.sh
