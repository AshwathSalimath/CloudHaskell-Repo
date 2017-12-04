The purpose of this project is to provide a baseline demonstration of the use of cloudhaskell in the context of the
code complexity measurement individual programming task. The cloud haskell platform provides an elegant set of
features that support the construction of a wide variety of multi-node distributed systems commuinication
architectures. A simple message passing abstraction forms the basis of all communication.

This project provides a command line switch for starting the application in master or worker mode. It is implemented
using the work-stealing pattern described in http://www.well-typed.com/blog/71/. Comments below describe how it
operates. A docker-compose.yml file is provided that supports the launching of a master and set of workers.

To use, build and do somethign like the following to start some clients:

```
stack exec use-cloudhaskell-exe worker localhost 8000 &
stack exec use-cloudhaskell-exe worker localhost 8001 &
stack exec use-cloudhaskell-exe worker localhost 8002 &
stack exec use-cloudhaskell-exe worker localhost 8003 &
```
Or alternative, a docker-compose.yml file is provided that supports the launching of a set of workers:

```
docker-compose up
```

And then start the manager as follows:

```
stack exec use-cloudhaskell-exe manager localhost 8005 500
```

You will see console output of this form from from the manager node:

```
Starting Node as Manager
[Manager] Workers spawned
1376
```

and console output of this form from the worker nodes:

```
[Node pid://localhost:8000:0:11] given work: 1
[Node pid://localhost:8000:0:11] finished work.
[Node pid://localhost:8000:0:11] given work: 2
[Node pid://localhost:8000:0:11] finished work.
[Node pid://localhost:8000:0:11] given work: 3
[Node pid://localhost:8000:0:11] finished work.
```
To understand the ouput, consult the code.

Note that to make the above work, you will need to build and install an image in your docker instance. Consult documentation for details 
but the short cut is to do something like the following:

```
stack docker pull
stack --docker build
stack --docker image container
```

Note also that to make this repository work with docker you will need to set the resolver in the `stack.yml` file to an `LTS` release
rather than a `nightly` release. Fro example, lts-8.24 should work fine.

__Docker-Compose__

The basic architecture of the work stealing pattern has a manager node as a central component surrounded by a set of
worker nodes. Each worker node is presumed to execute on a different node such that the total computational capacity of
the worker node set are available to us to deliver processing. 

Launching worker nodes individually is inconvenient and so I have added a `docker-compose.yml` file to the project. To
launch a set of worker nodes, run:

```
docker-compose up
```

One may now launch a manager node to passwork for these nodes:

``` 
stack exec use-cloudhaskell-exe manager localhost 8085 100
```

where the final parameter is the size of the number range (see the code to see the specifics on what the project is calculating). Note that when you execute the system in this way you will not see console output from the worker nodes as the worker function has not been written to gather output to the console.