title: Docker for Java Enterprise Architects, Designerns 'n' Coders


# What's an (application) container?

Example 1)  JBoss, WebSphere

Java Enterprise Application Server runs Java Enterprise Applications packaged in enterprise "containers".

- `.ear` = Enterprise Application Archive (just a zip with zips inside)   - or -
- `.war` = Web Archive (just a zip) 

What's wrong?


# What's an (application) container?  (Cont.)

What's wrong
with Java Enterprise Application Servers (e.g. JBoss, WebSphere, etc.)?

Basically runs only **Java, Java, Java**. (*)

(*): Might be Ruby for Java (JRuby), Clojure, Scala or any other language compiling to Java byte code
running on the Java virtual machine.



# What's an (application) container?

Example 2) Docker

Runs **ANY** application. (*) 

PHP, anyone? Yes. Node.js, anyone? Yes. Java, anyone? Yes. COBOL, anyone? Yes.
PostgreSQL, anyone? Yes. Redis, anyone? Yes. And Ruby, anyone? Yes!

(*): Remember its all 0 and 1 all the way down (including Java) ;-)


# What's an (application) container?

Ok.

What's the docker package format? Any guess?


# What's an (application) container?

Use what works - don't reinvent the wheel, the kitchen sink or the toothbrush.

What works for more than 50+ years?


# What's an (application) container?

Yes. Scripts. Scripts. Scripts.

What's the docker package format (e.g. `Dockerfile`)?  And the winner is...

Linux Shell Scripts (*)

(*) No zips just plain old vanilla text



# Dockerfile - Docker Container "Package Format" Example

Example: Hello world in a container

`Dockerfile`:

```
FROM ubuntu

RUN echo 'Hello world'
```

Proof of the pudding. Let's build it:


```
$ docker build .

Sending build context to Docker daemon  2.048kB
Step 1/2 : FROM ubuntu
Step 2/2 : RUN echo 'Hello world'
Hello world
Successfully built 09953f45072b


$ docker images

REPOSITORY           TAG                 IMAGE ID            CREATED             SIZE
<none>               <none>              09953f45072b        2 minutes ago       73.9MB
```


# Virtual Machines vs Containers vs Java Enterprise Application Servers

One Container is One Process

Every container is "isolated" - own protected (secure, safe) memory space and own "virtual" server


Virtual Machine (e.g. VMBox, VirtualBox, KVM, etc.)

Not the Java "Virtual Machine" is NOT a virtual machine e.g. you cannot boot any operation system.



# Virtual Machines vs Containers

Bootup (Startup) Time

How long does it take to bootup (startup) a virtual machine image?

- 10 secs
- 30 secs
- 1 min
- 2 mins

How long does it take to bootup (startup) a container image?

- 0.015 secs
- 0.029 secs
- 0.042 secs


# Virtual Machines vs Containers

10 000 x Faster!  - Yes, ten thousand times faster!

Wow. Wow. Wow.  What's the catch? How is this possible?

Is it all Java? ;-)



# Virtual Machines vs Containers

It is all Linux ;-)

No magic. A container (application) always (re)uses the same (one and only) Linux kernel.

No need to bootup / startup an operating system kernel image.

Uses overlay file system, copy-on-write and much more
for the high speed.



# Dockerfile - Docker Container "Package Format" Example

Example: Node.js hello world web app in a container

`hello.js`:

```
var express = require('express');
var app = express.createServer();

// respond with "hello world" when a GET request is made
app.get('/', function(req, res) {
  res.send( 'hello world' );
});

app.listen( 8080 );
```


`package.json`:

```
{
  "name": "nodejs-sample",
  "version": "1.0.0",
  "dependencies": {
    "express": "^2.3.0"
  }
}
```

# Dockerfile - Docker Container "Package Format" Example

Example: Node.js hello world web app in a container (Cont.)

`Dockerfile`:

```
FROM ubuntu

# Install Node.js and npm
RUN apt-get install -y nodejs npm

# Install app dependencies
COPY package.json /src/package.json
RUN cd /src; npm install --production

# Bundle app source
COPY hello.js /src/hello.js

# App binds to port 8080 
EXPOSE  8080

# Last but not least, define the command to run your app
CMD ["node", "/src/hello.js"]
```


# Dockerfile - Docker Container "Package Format" Example

Example: Node.js hello world web app in a container (Cont.)

Done. Build the image and run.

```
$ docker build -t hello_nodejs .

$ docker images

REPOSITORY           TAG                 IMAGE ID            CREATED             SIZE
hello_nodejs         latest              00362d5176e4        2 minutes ago       944MB

$ docker run -p 8080:8080 -d hello_nodejs
```


# Yes, You Can! - Get Started Today - Try Docker

Step 1. Try it out.

Works on Windows (requires 64-bit) runs Docker Server
in the Windows Subsystem for Linux v2 (WSL2).

Step 2. Use Docker for development and testing.

Step 3. Share Dockerfiles (and images) with your (co-)workers or the world.

Step 4. Use Docker in production?




# Appendix I -- Minimial Bare Bone Linux Images for Containers

- Atomic Linux
- Alpine Linux
- CoreOS Linux
- and others


# Appendix II -- €€€ - Economics 101 - £££ - Follow the Money - $$$

One Server

Example 1) Containers

Sell X Containers 

Example 2) Virtual Machines

Sell Y Virtual "Root" Machines 


Who will "win" in a (turbo) captialistic system?

Is X or Y bigger?

=> Container are more "efficient"  e.g. lets you sell more units (e.g. "virtual" servers)
per "physical" server



# Appendix III -- Best Practices for Container Applications (Images)

Twelf-Factor Applications (As Used by Heroku) - see [12factor.net](http://12factor.net)

**I. Codebase**
One codebase tracked in revision control, many deploys

**II. Dependencies**
Explicitly declare and isolate dependencies

**III. Config**
Store config in the environment

**IV. Backing Services**
Treat backing services as attached resources

**V. Build, release, run**
Strictly separate build and run stages

**VI. Processes**
Execute the app as one or more stateless processes

**VII. Port binding**
Export services via port binding

**VIII. Concurrency**
Scale out via the process model

**IX. Disposability**
Maximize robustness with fast startup and graceful shutdown

**X. Dev/prod parity**
Keep development, staging, and production as similar as possible

**XI. Logs**
Treat logs as event streams

**XII. Admin processes**
Run admin/management tasks as one-off processes

