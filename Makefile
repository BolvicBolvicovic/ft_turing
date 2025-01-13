C = ocamlopt
TEST = test
MAIN = ft_turing

TEST_FILE = src/tests.ml
MAIN_FILE = src/main.ml
FILES = $(addprefix src/, turing.ml)

# The list of compiled objects
CMX_FILES = $(FILES:.ml=.cmx)

all: ${MAIN}

tester: ${TEST}

# Compile the main program
${MAIN}: ${CMX_FILES} ${MAIN_FILE}
	${C} -o ${MAIN} $^

# Compile the test program
${TEST}: ${FILES} ${TEST_FILE}
	${C} -o ${TEST} $^

# Compile each .ml file into .cmx
%.cmx: %.ml
	${C} -c $<

clean:
	rm -rf $(addprefix src/, *.cmi *.cmx *.o)
	rm -rf $(addprefix tests/, *.cmi *.cmx *.o)

fclean: clean
	rm -f ${TEST} ${MAIN}

.PHONY: all test clean fclean

