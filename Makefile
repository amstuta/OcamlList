##
## Makefile for my_list in /home/amstuta/rendu/My_list
##
## Made by arthur
## Login   <amstuta@epitech.net>
##
## Started on  Mon Feb 23 15:21:06 2015 arthur
## Last update Mon Feb 23 15:45:51 2015 arthur
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

.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	$(CAMLC) -c $<

clean:
	$(RM) *.cm[iox] *~ .*~
	$(RM) $(EXEC)

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

.PHONY:	all clean fclean re
