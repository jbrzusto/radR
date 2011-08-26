##    example.desc.R - a brief description of the "example" plugin
##
## We keep this information separate from example.plugin.R because
## the plugin file might be large and complicated to parse (and might
## have side-effects), while this file is meant to be short and simple
## since it will automatically be read when radR starts.
## The example.plugin.R file is only read if/when the plugin is loaded.

list (
     ## the plugin version
     version = "0.01",
     
     ## a label that appears in the "Load a plugin" menu, describing the plugin
     label = "an example plugin template for showing how to create plugins"
)

