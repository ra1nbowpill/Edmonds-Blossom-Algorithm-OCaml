#!/bin/bash

if (($# != 1))
then
	echo "Usage : ./compile.sh source.ext"
	exit
fi

file=$1

ocamlbuild -plugin-tag "package(js_of_ocaml.ocamlbuild)" -use-ocamlfind $file


if test "${file##*.}" = "js"
then
	echo '<html>
<head>
	<title> Observation of leftist heap </title>
	<script type="text/javascript" defer="defer" src="_build/Exec/'$1'"> </script>
	<link type"text/css" href="static/observer.css" rel="stylesheet"> </script>
</head>

<body>
</body>
</html>' > index.html
fi
