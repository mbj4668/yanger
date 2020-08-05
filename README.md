yanger
======

Extensible YANG validator.

Prerequisites
=============
Build requirements:
- erlang
- libxml2-dev

Test requirements:
- python
- [pyang](https://github.com/mbj4668/pyang)
- [lux](https://github.com/hawk/lux)

Installation of Yanger on macOS/Linux
=============

1.Install erlang and libxml2-dev
------------------------------
- **macOS**

Install erlang and coreutils using brew (if not present, install [brew package manager](https://brew.sh/))
```
# OS X 10.13.5 (High Sierra)
$ brew install erlang coreutils libxml2
```
- **Linux**

Most linux distributions provide a package that can be installed using the system package manager,for example:
```
# Debian, Ubuntu, ...
$ apt-get install erlang libxml2-dev
```
```
# CentOS, RHEL, ...
$ yum install erlang libxml2-devel
```
```
# openSUSE
$ zypper install erlang libxml2-devel
```
```
# Fedora
$ dnf install erlang libxml2-devel
```

2.(optional)Install lux and pyang to verify test cases
-------------------------------
- install lux
```
# make local env
$ mkdir ~/local
$ cd ~/local
$ git clone https://github.com/hawk/lux.git

# export path
$ export PATH=$PATH:~/local/lux/bin
(put the above in .bash_profile or .profile)

# rebuild to use erlang
$ cd ~/local/lux
$ ./bin/lux --make

# test
$ ./bin/lux examples/intro.lux
```
- git clone pyang and source the env bash
```
$ git clone https://github.com/mbj4668/pyang.git
$ cd pyang
$ source env.sh
```

3.Building
--------
```
$ git clone https://github.com/mbj4668/yanger.git
$ cd yanger
$ source env.sh
$ make
```

4.Running Unit Tests
------------------
```
$ make test
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

For more information about the `yanger` usage, please use the command `yanger --help` as below:
```
$ yanger --help
Usage: yanger [-h] [-v] [-V] [--print-error-code] [-p <path>]
              [-P <plugindir>] [--pa <pa>] [--pz <pz>] [-c]
              [--strict] [-e] [-W <warning>] [-w <no_warning>]
              [-E <error>] [-f <format>] [-t <transform>]
              [--max-status <max_status>]
              [--deviation-module <deviation_module>] [-F <features>]
              [--ignore-unknown-features <ignore_unknown_features>]
              [-C <conformance>] [-o <outfile>] [--no-deviation-apply]
              [--print] [--print-all] <file>...

  -h, --help                        Show this help message and exit.
  -v, --version                     Show version number and exit.
  -V, --verbose                     Verbose output.
.....
```


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



Debugging
=========
If you wanted to use the erl-shell in order to debug you could:

```
erl -pa ./ebin
yanger:main(["-p","<<YANG_PATH>>","-f","swagger","<<YANG_PATH>>/tailf-ncs.yang","-o","tailf-ncs.json"]).
```

