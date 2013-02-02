# Opa on Windows #


## Overview ##

Opa is a new generation of cloud development platform. It is a new programming 
language, a new web server, a new database and a new distributed execution
engine, all of them tightly integrated to provide a great experience for cloud
developers.

## Mandatory Software

There are mandatory softwares that a post-install script is currently helping you to install right now (see Need Help if not).
	
0) MongoDb : Please follow the step-by-step installation
(http://docs.mongodb.org/manual/tutorial/install-mongodb-on-windows)

1) Nodejs : in order to use the nodejs backend, you must install nodejs (>= 0.8.1). 
Make sure that your NODE_PATH user environment variable contains %APPDATA%\npm\node_modules.
For instance, do right click on "Computer" -> "Properties" -> "Advanced system parameter" -> "Environment variables" and then in user variable add NODE_PATH=%NODE_PATH%;%USERPROFILE%\AppData\[Roaming\]\npm\node_modules
(http://nodejs.org/#download)

## Recommended Editing Software

We recommend the use of Sublime Text 2 to edit your Opa code. 
It is avalaible at http://www.sublimetext.com/
We provide a plugin with syntax hightlighting and build command for this editor.

Syntax hightlighting mode are also provided for emacs and vim.
(http://doc.opalang.org/#!/manual/Getting-Opa/Setting-up-your-editor).



## First steps in Opa ?

A mini-tutorial is included in Opa plugin for ST2.
It will help you to write your first "Hello World" Opa Web server 

0) start ST2

1) open a new file with "opa" as extension, e.g. "hello.opa"

2) write TUTORIAL_01, and use completion to expand to the tutorial instructions.

3) Follow the magically appeared instructions



## Need help ?

0) To install automatically Opa modes, after installing Sublime Text 2 or emacs,
    or reinstall nodejs or mongodb, you can call the post-install script:

	[Start Menu -> Search on %OPABASEDIR%\share\opa\post_install.cmd]

1) Documentation about opa can be found here:
   
	doc.opalang.org 

2) Questions can get answers quickly on our real-time forum:
   
	forum.opalang.org

3) If you fall in love with Opa, following us can be your love declaration:

	https://lists.owasp.org/mailman/listinfo/opa
	https://twitter.com/opalang