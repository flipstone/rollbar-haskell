if [ "$IN_DEV_CONTAINER" ]; then
  # Already in container, nothing to do
  :
else
  EXTRA_ARGS=()

  if [ "$RUN_AS_SERVICE" ]; then
    EXTRA_ARGS+=(--service-ports)
    EXTRA_ARGS+=(--use-aliases)
  fi

  # Script was run from outside the container, re-exec inside the container
  # with the same arguments
  docker-compose build
  exec docker-compose run --rm "${EXTRA_ARGS[@]}" dev $0 "$@"
fi
