if [ "$IN_DEV_CONTAINER" ]; then
  # Already in container, nothing to do
  :
else
  # Script was run from outside the container, re-exec inside the container
  # with the same arguments
  docker compose build
  exec docker compose run --rm dev $0 "$@"
fi
