# Docker integration

## Reason
You can use docker-wrapped both the language server and debug server and connect to them using LSP/DAP.
Imagine that you have a short-time project (or a sideproject) and don't want to bring any software packages to your local machine.
Now it is simple - wrap the language server and debugger you need in `docker` and use `lsp-docker` for LSP and `dap-mode` for DAP.

## Features
Most of the `lsp-mode` and `dap-mode` features, but with the ability to use containerized environments.  
  
*Note: some of the features may yet to be tested, so any help finding and fixing any bugs in them is highly appreciated!*

## Components: Language Server / LSP support
You can use manually built language containers or images hosting language server(s), just follow a few simple rules (shown below).  
  
*Note: this info is based on the readme in the original project, take a look at [lsp-docker](https://github.com/emacs-lsp/lsp-docker) for additional info.*

### Building a container (or an image):
You have 2 constraints:

A language server must be launched in stdio mode (other types of communication are yet to be supported)
A docker container (only container subtype, see the configuration below) must have your language server as an entrypoint (basically you have to be able to launch it with `docker start -i <container_name>` as it is launched this way with `lsp-docker`)
When you have sucessfully built a language server, you have to register it with either a configuration file or a .dir-locals file.

### Registering a language server using a persistent configuration file:
A configuration file is a yaml file that is named `.lsp-docker.yml` or `.lsp-docker.yaml` and looks generally like this:
``` yaml
lsp:
  server:
    type: docker
    subtype: container # Or image. container subtype means launching an existing container
    # image subtype means creating a new container each time from a specified image
    name: not-significant-but-unique-name # Must be unique across all language servers
    server: server-id-of-the-base-server # Server id of a registered server (by lsp-mode) 
    launch_command: "launch command with arguments" # Launch command of the language server
    # (selected by a server id specified above) in stdio mode
    # Note: launch_command is not used with container subtype servers
    # as a command is embedded in a container itself and serves as an entrypoint
  mappings:
    - source: "/your/host/source/path"
      destination: "/your/local/path/inside/a/container"
```

### Registering a language server using a .dir-locals file:
Just refer to the source code and general conventions of using .dir-locals. The variable you need is lsp-docker-persistent-default-config, its content is merged with the lsp section from a configuration file (if present).

## Components: Debug Server / DAP
  
*Note: this info is based on the readme in the original project, take a look at [dap-mode](https://github.com/emacs-lsp/dap-mode) for additional info.*

Configure it either with a `.dir-locals` file or drop an `.lsp-docker.yml` configuration file (use `lsp-docker` for general reference). Basically you have one function `dap-docker-register` that performs all the heavy lifting (finding the original debug template, patching it, registering a debug provider e.t.c). This function examines a configuration file or falls back to the default configuration (which can be patched using the `.dir-locals` approach, take a note that the default configuration doesn’t provide any sane defaults for debugging) and then operates on the combination of the two. This mechanism is the same as in `lsp-docker`.  

*Note: currently you cannot use this mode when using a network connection to connect to debuggers (this part is yet to be implemented). Still want to talk to debuggers over network? In order to do so you have to look at the launch-args patching done by `dap-docker--dockerize-start-file-args`, you have to somehow assign `nil` to `dap-server-path` before it is passed further into session creation.*

If you want to stick to a configuration file, take a look at the example below:
``` yaml
lsp:
  server:
    # 'lsp-docker' fields
  mappings:
    - source: "/your/host/source/path" # used both by 'lsp-docker' and 'dap-docker'
      destination: "/your/local/path/inside/a/container" # used both by 'lsp-docker' and 'dap-docker'
  debug:
    type: docker # only docker is supported
    subtype: image # or 'container'
    name: <docker image or container that has the debugger in> # you can omit this field
    # in this case the 'lsp-docker' ('server' section) image name is used
    enabled: true # you can explicitly disable 'dap-docker' by using 'false'
    provider: <your default language debug provider, double quoted string>
    template: <your default language debug template, double quoted string>
    launch_command: <an explicit command if you want to override a default one provided by the debug provider>
    # e.g. if you have installed a debug server in a different directory, not used with 'container' subtype debuggers
```
