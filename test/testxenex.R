## test the xenex shared library
dyn.load("plugins/interfaces/xenex.dll")
ant<-.Call("get_antennas")
p<-.Call("open_usbvp", ant[6], 20)
.Call("set_main_mode", p, TRUE)
