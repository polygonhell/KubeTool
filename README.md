# KubeTool

Tool similar in scope to odo, to manage developing, packaging and deploying applications to kubernetes clusters.


## Quickstart

TODO

## Overview

all commands have the form

    kt <context> <command> [arguments]

Where context is one of

* config
* env
* namespace
* project
* push

### config

Used to control the project configuration, changes are saved in the config.yaml file.

#### set

    kt config set <name>=<value>

Supported names are currently

* name
  * The name of the project
* author
  * The author of the project

### env

Used to control settings for the current environment, changes are saved to the .kube_env file.

#### set

    kt env set <name>=<value>

Supported names are currently

* namespace
  * The Kubernetes namespace the project will be deployed to

### namespace

A collection of helpers for working with Kubernetes namespaces.

#### create

Create a new namespace

    kt namespace create <name>

#### delete

Delete an existing namespace and all contained items

    kt namespace delete <name>

#### list

Get a list of all kubernetes namespaces on the current K8's cluster, the current active namespace for kt is preceded by a '*'.

    kt namespace list

### project

Tools for manipulating projects

#### list

list projects

#### set

Supported names are currently

* name


### push

Push and launch a project on the cluster.

Will create or update the projects StatefulSet, Service and Pods.

    kt push

## Status

Work in progress
Currently NOT FUNCTIONAL
