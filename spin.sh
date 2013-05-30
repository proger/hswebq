
key=key1

# this script has a race :)
for i in $(seq 5); do
	curl -s :3000/waitq/$key?$i &
done

curl -v -X POST :3000/waitq/$key

wait $!
