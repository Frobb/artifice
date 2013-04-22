artifice
========

Artificial life simulation.

# Building

From a terminal do:

    $ make

This will automatically download dependencies and compile the `artifice` application.

# Running

First, start serving the client files in the background. From the root directory, do:

    $ cd client
    $ ./serve.sh >/dev/null &

Then, go back to the root directory and start the Artifice application in development mode:

    $ ./start-dev.sh

Alternatively, you can build a release and start it with:

    $ make rel
    $ rel/artifice/bin/artifice console

The main difference is that when starting in development mode,
BEAM files are automatically reloaded as they are recompiled.

# Connecting

Assuming you've completed the steps in building and running, point your browser to
`http://127.0.0.1:1234`. You should see a green (grass-ish) world map.
Click anywhere on the map to spawn a creature.
