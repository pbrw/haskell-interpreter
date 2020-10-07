# Interpreter description

## How to run a program?
```
make 
./interpreter source_code_file
```
or you can just run `./interpreter` and write source code as an input

## Modules
- **Definitions.hs** - contains data types declarations for **environments**, **stores** and **values**
- **Semantic.hs** - describes what is the effect of particular statements and expressions
- **Utils.hs** - auxiliary functions used across the project, e.g. changing state, checking types
- **Interpreter.hs** - top-level module which starts and ends the execution of the program
- **Main.hs** - parsing code and running interpreter on abstract syntax tree
- **AbsGrammar.hs** - definition of abstract syntax tree (auto-generated)
- **grammar.cf** - formal description of the language grammar in EBNF notation

## Main idea

### State
In order to execute a program interpreter processes instructions which changes current **state** of execution. **State** is in fact a pair **(environment, store)**. First element tells which location in **store** is assigned to a particular identifier. Second represents a memory where **values** are stored.    

### Value
Represents a single value which can be stored in **store**. Can be in one of 4 forms:	
- I *integer* - integer value
- B *bool* - boolean value
- S *string* - string value
- V - void

### Environment
Carries an information about identifiers bindings to **store** locations in the current scope. Especially useful in handling variables shadowing. **Environment** is a composition of three separated parts:
- variable environment (venv) - for storing actual variables names
- procedure environment (penv) - for storing actual procedure/function names
- cost environment (cenv) - for storing actually opened cost blocks 

### Store
Used as a simple memory which stores **values** at specific locations. Is made of two parts:
- variable store (vst) - for storing variables values and special values (e.g. return values)
- cost store (cst) - for storing statistics needed by cost blocks

## Grammar conflicts
There is one conflict in the language grammar related to nested if-else constructs.
Consider code block `if (a) if (b) f(); else g();` It can't be determined if `else` belongs to first or second `if` statement based on set of rules. Interpreter will parse it as
```
if (a) {
	if (b) {
		print "X"; 
	} else {
		print "Y";
	}
}
```
## Issues
Cost block turned out to be a little bit harder to implement than I originally thought. It works with some exceptions. Cost block must be defined inside a function. Calculating memory allocated used works only for integer and boolean types. Because string's length can change multiple times it requires more complicated approach to track the used memory.








