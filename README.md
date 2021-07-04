# KubeTool

Tool similar in scope to odo, to manage developing, packaging and deploying applications to kubernetes clusters.


## Known issues

* Only one template
* Only supports a single project in the config
* Only supports ClusterIP services
* Changing a running project's name leaves the existing projects running when you use push

## Quickstart

### CreateReactApp Typescript template

Use create-react-app to create a project.

    npx create-react-app testproj --template typescript

cd to the project

    cd testproj

Initialise the kube tool configuration (config.yaml).

    kt init -n testproj -t TSCreateReactApp

Create a namespace for the project in your current kubernetes cluster - this will also set the namespace for the project.

     kt namespace create testproj

Push project to the kubernetes cluster

    kt watch

The first time push or watch is called it may take a couple of minutes, it will create the StatefulSet and Service associated with the project, once the Pod is created it will copy the source files to the pod and run the build/launch command.
Subsequent invocations to Push will just copy changed files and restart the build/launch command, and should be fast unless many large files have changed.

In a different shell use kubectl port-forward to forward the project service locally

    kubectl port-forward service/testproj 3000:3000 --namespace testproj

Check the service is running by connecting a browser - you may have to wait a minute or two as the initial npm i completes.

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

Will create or update the projects StatefulSet, Service and Pods, any changed files will be copied to the k8's cluster and the pushCommand will be invoked.

    kt push

### watch

Push and launch a project on the cluster.

Will create or update the projects StatefulSet, Service and Pods, copy changed files to the cluster and invoke the pushCommand. The files specified in the srcFiles part of the template are then monitored for changes, if any occur, changed files will be copied to the k8's cluster and if a watchCommand is specified it will be invoked on the pod.

    kt watch

## Status

Work in progress
Usable for simple projects
