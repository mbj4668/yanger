yanger
======

Extensible YANG validator.

Prerequisites
=============
erlang
libxml2-dev

Installation of Yanger on OS X Example
=============

Tested on OS X 10.13.5 (High Sierra)

1. Install [brew package manager](https://brew.sh/) (if not present)


2. Install erlang and coreutils using brew
```
brew install erlang coreutils

```

3. (optional) install lux to verify test cases 

```
##deps
brew update
brew upgrade
brew install erlang coreutils

##make local env
mkdir ~/local
cd ~/local
git clone https://github.com/hawk/lux.git

##export path
export PATH=$PATH:~/local/lux/bin
(put the above in .bash_profile or .profile)

##rebuild to use brew erlang
cd ~/local/lux
./bin/lux --make

##test
./bin/lux examples/intro.lux
```

4. git clone yanger and build it, and source the env bash

```
git clone https://github.com/mbj4668/yanger.git
cd yanger
source env.sh
make
```

Installation of Yanger using Docker
=============

Checkout this repository (described above) and build the image:

```sh
docker build -t mbj4668/yanger .
```

Execute the images as below:

```sh
$ docker run -it mbj4668/yanger
Usage: yanger [-h] [-v] [--print-error-code] [-p <path>]
              [-P <plugindir>] [-c] [--strict] [-e] [-W <warning>]
              [-w <no_warning>] [-E <error>] [-f <format>]
              [-t <transform>] [--max-status <max_status>]
              [--deviation-module <deviation_module>] [-F <features>]
              [-C <conformance>] [-o <outfile>] [--no-deviation-apply]
              [--print] [--print-all] <file>...
```

Execute the example below:

```sh
docker run -it -v ${PWD}:/workdir mbj4668/yanger -t expand -f swagger test_leaf.yang -o test_leaf_now_swagger.json
```

Sample Usage
-----------------

To create a Swagger JSON file from a yang file, you must first have a yang file, 
for example, create the following `test_leaf.yang` file in your current working directory:

```
module test_leaf {
  namespace "http://example.com/ns/example/test_leaf";
  prefix ns;

container test_vars{
    leaf single_value
    {
      type string;
    }
   }
}
```

Next you must tell yanger which format to output the yang into, in this example, 
it is the swagger JSON format, giving the new file output name `test_leaf_now_swagger.json`:

```
yanger -t expand -f swagger test_leaf.yang -o test_leaf_now_swagger.json
```

You will now have a JSON file created from the yang file:
```
$ more test_leaf_now_swagger.json
{
  "swagger": "2.0",
  "info": {
    "title": "test_leaf",
    "description": "",
    "version": "2018-10-15"
  },
  "basePath": "/restconf",
  "tags": [
    {
      "name": "root",
.....
```

You can for example take the Swagger file to the [Swaggerhub](https://app.swaggerhub.com/home) and auto create an SDK from 
it in a python flask format or other languages. The Swaggerhub requires free registration. 


For running tests
-----------------
lux (https://github.com/hawk/lux)


Contributing
============

Code style
----------

Do not introduce trailing whitespace.

Do not introduce lines longer than 80 characters.

Do not use tabs for indentation.

Hint for emacs users:

(setq whitespace-style (quote (face trailing tabs lines)))

and use whitespace-mode.
