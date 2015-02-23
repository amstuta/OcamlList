##
## Makefile for my_list in /home/amstuta/rendu/My_list
##
## Made by arthur
## Login   <amstuta@epitech.net>
##
## Started on  Mon Feb 23 15:21:06 2015 arthur
## Last update Mon Feb 23 15:40:12 2015 arthur
##

RM	= rm -f

CAMLC	= ocamlc

SRCS	= my_list.ml \
	  main.ml

NAME	= tests

OBJS	= $(SRCS:.ml=.cmo)

all:	$(NAME)

$(NAME):$(OBJS)
	$(CAMLC) $(OBJS) -o $(NAME)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) -c $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -c $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli
	$(CAMLC) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -c $*.mli
	$(CAMLOPT) -c $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

clean:
	$(RM) *.cm[iox] *~ .*~
	$(RM) $(EXEC)

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

.PHONY:	all clean fclean re
