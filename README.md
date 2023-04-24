# homemade_c_compiler_v2


## How to use it?
	###  Compilation
	```
	$ make
	```
	### Usage
	```
	$ ./main.byte source.c > output.s
	```
	Then use SPIM to execute the code:
	```
	spim -file output.s
	```
	You can try with "test.c".
## What Works?
	- functions
	- conditions
	- return
	- function call
	- comments 
	
	types : 
		- void
		- string
		- int 
		- variables

## Work in progress

	- pointers ( made parts) 
	- standard lib ( made parts)
	- loops ( almost done)
	
	
## Exemple of code that works
```
int seconde ( int x) {
	int jus = x ;
	bool pomme = true;
	return jus;
} ;

void main ( void) {
	int v = 10;
	bool zed = true;
	str w = "this is a string";
	#int x = 10 + 3;
	int x;
	if (true){
		x = seconde ( 5 );
	} else {
		x = seconde ( 3 );
	};
	#while ( 0 ){ x = v;};
	return x;
	
	
} ;

```	
